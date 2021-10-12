# ECON 483 
# CJ Robinson
# Cleaning and analysis of ATUS data for econometrics paper

# Set-up --------------------------------------------------------------

library(MASS)
library(stargazer)
library(tidyverse)
library(lubridate)
library(ipumsr)
library(broom)
library(emmeans)


setwd("/Users/cj/Dropbox/Academics and Research/UW Undergraduate/Year Three/ECON 483")



# Load in Data --------------------------------------------------------------
# Needs both ATUS .xml file and data file from extract site

ddi <- read_ipums_ddi("data/atus_00011.xml")
data <- read_ipums_micro(ddi)



# Data Joins  --------------------------------------------------------------
# The data is split in odd ways, so in order to get data in a workable form, 
# I create an individual demographics dataset, a house demographics dataset,
# and a work travel description dataset, all ultimately joined by CASEID and 
# based off of the "RECTYPE" variable

# Create indiviudal dataset and select relevant variables
ind_demographics <- data %>% 
  filter(RECTYPE == 2) %>% 
  select(CASEID, YEAR, DAY, AGE, SEX, RACE, HISPAN, ASIAN, EDUCYRS, EMPSTAT, 
         MULTJOBS, PAIDHOUR, HOURWAGE, UHRSWORKT, FULLPART, HOURWAGE_CPS8, WT06, NCHILD, MARST)

# Create household dataset and select relevant variables
house_demographics <-  data %>% 
  filter(RECTYPE == 1) %>% 
  select(CASEID, METRO, MSASIZE, COUNTY, FAMINCOME)

# Join to make overall demographics dataset for each individual with
# additional household charateristics attatched
demographics <- merge(ind_demographics, house_demographics)

# Create travel time to work dataset and select relevant variables
worktravel <- data %>% 
  filter(ACTIVITY == 180501) %>%
  select(CASEID, WHERE, DURATION_EXT, START)

# Join to create full dataset
full_data_not_clean <- merge(demographics, worktravel) 



# Data Cleaning  --------------------------------------------------------------
# Below is several iterations of coding variables from the codebook and creating
# new variables for my own analysis. Refer to the codebook for questions and
# verification. 

# Data desc:
# "full_data" is all the data that could be applicable 
# "travel_data" is the sample I use in regression. 
# Each is used in future analysis. 

full_data <- full_data_not_clean %>% 
  # some observations have locations rather than methods of transport - this filters these out
  filter(WHERE >=200 & WHERE < 300) %>% 
  # creates variables as time to pull hours from later
  mutate(STARTTIME = parse_date_time(START, orders = 'HMS'), 
         DAY = factor(DAY, labels = c("sun", "mon", "tues", "wed", "thurs", "fri", "sat")),
         SEX = factor(SEX, labels = c("male", "female")),
         # this variable is all races that mention "black"
         BLACK_dummy = ifelse(RACE == 0110 | RACE == 0200 | RACE == 0210 | RACE == 0211 | RACE == 0212 | 
                                RACE == 0301 | RACE == 0302 | RACE == 0330 | RACE == 0331 | RACE == 0340 | 
                                RACE == 0400 | RACE == 0401 | RACE == 0402 | RACE == 403 | RACE == 0500, 1, 0),
         BLACK = factor(BLACK_dummy, labels = c("non black", "black")),
         MULTJOBS = factor(MULTJOBS), 
         FULLPART = factor(FULLPART, labels = c("full", "part", "na")),
         PAIDHOUR = factor(PAIDHOUR, labels = c("hourly", "salaried", "na")),
         METRO = factor(METRO, labels = c("Metropolitan, central city", "Metropolitan, balance of MSA",
                                          "Metropolitan, not identified", "Nonmetropolitan", "Not identified")),
         MSASIZE = factor(MSASIZE, labels = c("Not identified or non-metropolitan", "100,000 - 249,999", "250,000 - 499,999",
                                              "500,000 - 999,999", "1,000,000 - 2,499,999", "2,500,000 - 4,999,999",
                                              "5,000,000+")),
         COUNTY = factor(COUNTY),
         METHOD = factor(WHERE, labels = c("Car, truck, or motorcycle", "Car, truck, or motorcycle",
                                           "Walking", "Bus", "Subway/train", "Bicycle", "Boat/ferry", "Taxi/limousine service",
                                           "Airplane", "Other mode of transportation")),
         INCOME_FACTOR = factor(FAMINCOME, labels = c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                                                      "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999",
                                                      "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
                                                      "$35,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                                                      "$60,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999",
                                                      "$150,000 and over", "na", "na", "na")),
         INCOME_MEDIAN = case_when(FAMINCOME == 1 ~ 2500,
                                   FAMINCOME == 2 ~ 6250, 
                                   FAMINCOME == 3 ~ 8750,
                                   FAMINCOME == 4 ~ 11250,
                                   FAMINCOME == 5 ~ 13750,
                                   FAMINCOME == 6 ~ 17500,
                                   FAMINCOME == 7 ~ 22500,
                                   FAMINCOME == 8 ~ 27500,
                                   FAMINCOME == 9 ~ 32500,
                                   FAMINCOME == 10 ~ 37500,
                                   FAMINCOME == 11 ~ 45000,
                                   FAMINCOME == 12 ~ 55000,
                                   FAMINCOME == 13 ~ 67500,
                                   FAMINCOME == 14 ~ 82500,
                                   FAMINCOME == 15 ~ 125000,
                                   FAMINCOME == 16 ~ 200000),
         EDUCYEARS = case_when(EDUCYRS == 101 ~ 1,
                               EDUCYRS == 102 ~ 3,
                               EDUCYRS == 105 ~ 6,
                               EDUCYRS == 107 ~ 8,
                               EDUCYRS == 109 ~ 9,
                               EDUCYRS == 110 ~ 10,
                               EDUCYRS == 111 ~ 11,
                               EDUCYRS == 112 ~ 12,
                               EDUCYRS == 213 ~ 13,
                               EDUCYRS == 214 ~ 14,
                               EDUCYRS == 215 ~ 15,
                               EDUCYRS == 216 ~ 16,
                               EDUCYRS == 217 ~ 16,
                               EDUCYRS == 316 ~ 18,
                               EDUCYRS == 317 ~ 17,
                               EDUCYRS == 318 ~ 18,
                               EDUCYRS == 319 ~ 19,
                               EDUCYRS == 320 ~ 18,
                               EDUCYRS == 321 ~ 21),
         EDUC_CATEGORY = case_when(EDUCYRS > 100 & EDUCYRS < 200 ~ "high school",
                                   EDUCYRS > 200 & EDUCYRS <= 216 ~ "some college",
                                   EDUCYRS == 217 ~ "college",
                                   EDUCYRS > 300 & EDUCYRS < 400 ~ "post grad"),
         SUBWAY_OR_BUS = case_when(METHOD == "Subway/train"~ 1,
                                   METHOD =="Bus"  ~ 1,
                                   TRUE ~ 0),
         HOUR_num = hour(STARTTIME),
         peak_dummy = case_when(HOUR_num >= 6 & HOUR_num <= 9 ~ 1,
                                HOUR_num >= 16 & HOUR_num <= 19 ~ 0,
                                TRUE ~ 0),
         HOUR = factor(HOUR_num),
         PEAK = factor(peak_dummy, labels = c("peak", "not peak")),
         YEAR = factor(YEAR),
         TRAVEL_DUMMY = factor(ifelse(DURATION_EXT < 22, 0, 1)),
         LOGHOURWAGE = log(HOURWAGE),
         LOGTRAVELTIME = log(DURATION_EXT),
         MARRIED = factor(ifelse(MARST == 1 | MARST == 2, "married", "not married"))) %>% 
  rename(TRAVELTIME = DURATION_EXT)


# Cleaning for the desired sample, mainly unwanted NAs, hourly workers, a
# and city size
travel_data <- full_data %>% 
  filter(METHOD != "Airplane", #not applicable
         METHOD != "Other mode of transportation",
         METHOD != "Boat/ferry",
         METHOD != "Taxi/limousine service",
         TRAVELTIME < 150, #outliers
         INCOME_FACTOR != "na",
         FULLPART != "na",
         HOURWAGE != 999.99,
         HOURWAGE != 99.99,
         HOURWAGE != 0,
         EMPSTAT != 2,
         UHRSWORKT != 9995, #na
         MSASIZE == "5,000,000+", # size of city for sample
         METRO == "Metropolitan, central city") #delineation for sample



# Visualizations  -------------------------------------------------------------

# Scatterplot of dep and indep variables
travel_data %>% 
  ggplot(aes(x = LOGHOURWAGE, y = LOGTRAVELTIME)) +
  xlab("Log Wage") +
  ylab("Log Travel Time") +
  geom_point(alpha = .3) +
  theme_minimal()

ggsave("scatterplot.png", path = "./final_images")

# Log Wage Histogram
travel_data %>% 
  ggplot(aes(x = LOGHOURWAGE, weight = WT06)) +
  xlab("Log Wage") +
  ylab("Count") +
  geom_histogram(bins = 15) +
  theme_minimal()

ggsave("logwage.png", path = "./final_images", width = 4, height = 3)

# Log Travel Time Histogram
travel_data %>% 
  ggplot(aes(x = LOGTRAVELTIME, weight = WT06)) +
  geom_histogram(bins = 10) +
  xlab("Log Travel Time") +
  ylab("Count") +
  theme_minimal()

ggsave("logtraveltime.png", path = "./final_images", width = 4, height = 3)

# Use by method
travel_data %>% 
  ggplot(aes(x = fct_infreq(METHOD), weight = WT06)) +
  geom_bar(stat = "count") +
  xlab("Method of Transportation") +
  ylab("Weighted Count") +
  coord_flip() +
  theme_minimal()

# Use by method in sample
travel_data %>% 
  ggplot(aes(x = fct_infreq(METHOD))) +
  geom_bar(stat = "count") +
  xlab("Method of Transportation") +
  ylab("Count") +
  coord_flip() +
  theme_minimal()

# Average travel time by method of transport
travel_data %>% 
  group_by(METHOD) %>% 
  summarize(AVERAGETRAVELTIME = weighted.mean(TRAVELTIME, WT06)) %>% 
  ggplot(aes(x = reorder(METHOD, -AVERAGETRAVELTIME), y = AVERAGETRAVELTIME)) +
  geom_bar(stat = "identity") +
  xlab("Method of Transportation") +
  ylab("Average Travel Time (in minutes)") +
  theme_minimal() +
  coord_flip()

ggsave("meantraveltime.png", path = "./final_images", width = 6, height = 4)

# Average bus/subway ridership by income category 
# (INCOME CATEGORY IS FAMILY INCOME)
# This better describes how people ride transportation in the sample, rather 
# than describing job placement, which I find wage to be better at.
travel_data %>%
  filter(INCOME_FACTOR != "na",
         INCOME_FACTOR != "$150,000 and over") %>% 
  group_by(INCOME_FACTOR) %>% 
  summarize(SUBWAYANDBUS = sum(SUBWAY_OR_BUS * WT06),
            weight_pop = sum(WT06),
            PERCENTPUBLIC = SUBWAYANDBUS / weight_pop) %>% 
  ggplot(aes(x = reorder(INCOME_FACTOR, desc(INCOME_FACTOR)), y = PERCENTPUBLIC)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  xlab("Income Level") +
  ylab("Public Transit Usage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip()

ggsave("transitusebyincome.png", path = "./final_images", width = 6, height = 4)


# Average bus/subway ridership by income category  AND RACE
# (INCOME CATEGORY IS FAMILY INCOME)
# This better describes how people ride transportation in the sample, rather 
# than describing job placement, which I find wage to be better at.
travel_data %>%
  filter(INCOME_FACTOR != "na",
         INCOME_FACTOR != "$150,000 and over") %>% 
  group_by(INCOME_FACTOR, BLACK) %>% 
  summarize(count = sum(WT06),
            SUBWAYORBUS = sum(SUBWAY_OR_BUS * WT06),
            pubtrans = SUBWAYORBUS / count) %>% 
  ggplot(aes(x = reorder(INCOME_FACTOR, desc(INCOME_FACTOR)), y = pubtrans, fill = BLACK)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  xlab("Income Level") +
  ylab("Public Transit Usage") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_discrete(name = "Race", labels = c("Non-Black", "Black")) +
  coord_flip()

ggsave("transitusebyincome_byrace.png", path = "./final_images", width = 6, height = 4)


# Regression --------------------------------------------------------------

simple_reg <- lm(LOGTRAVELTIME ~ LOGHOURWAGE + BLACK, travel_data, weights = WT06)
summary(simple_reg)

robust_1 <- lm(LOGTRAVELTIME ~ LOGHOURWAGE + BLACK + METHOD, travel_data, weights = WT06)
summary(robust_1)

robust_2 <- lm(LOGTRAVELTIME ~ LOGHOURWAGE + SEX + BLACK + METHOD + EDUCYEARS + 
                 SEX + AGE + MULTJOBS + UHRSWORKT + FULLPART + NCHILD + MARRIED, travel_data, weights = WT06)
summary(robust_2)

robust_3 <- lm(LOGTRAVELTIME ~ LOGHOURWAGE + BLACK + METHOD + EDUCYEARS + 
                 SEX + AGE + MULTJOBS + UHRSWORKT + FULLPART + NCHILD + MARRIED + HOUR + DAY + YEAR + COUNTY , 
               travel_data,  weights = WT06)
summary(robust_3)

multi_reg <- lm(LOGTRAVELTIME ~ LOGHOURWAGE * BLACK + METHOD + EDUCYEARS + 
                  SEX + AGE + MULTJOBS + UHRSWORKT + FULLPART + HOUR + DAY + YEAR + COUNTY + NCHILD + MARRIED, 
                travel_data,  weights = WT06)
summary(multi_reg)



# Regression Visualization  --------------------------------------------------------------
# Creates visualizations for interaction terms and categorical variables 

# Create a datatable from regression
reg_table <- tidy(multi_reg)

# Calculate standard error
reg_table <- reg_table %>% 
  mutate(lowerci = estimate - std.error * 1.96,
         upperci = estimate + std.error * 1.96)

# Visualizing difference in effect of wage between methods of transportation
reg_table %>% 
  filter(str_detect(term, "^METHOD")) %>% 
  mutate(term = substring(term, 7)) %>% 
  filter(term != "METHODBoat/ferry") %>% 
  ggplot(aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = lowerci, ymax = upperci), width = .5) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  ylab("Coefficient Estimate") +
  xlab("Method of Transportation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# Creating Tables  ---------------------------------------------------

stargazer(multi_reg, align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE)

stargazer(simple_reg, robust_2, multi_reg, align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE)

stargazer(travel_data)



# Visualization Graveyard   ---------------------------------------------------
# Not used, still fun

# Density of income by METRO delineation
full_data %>% 
  ggplot(aes(x = INCOME_FACTOR, y = ..density.., weight = WT06)) +
  facet_wrap(~METRO) + 
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), stat = "count")

# Histogram of wage by MSASIZE
full_data %>% 
  filter(HOURWAGE != 999.99,
         HOURWAGE != 99.99) %>% 
  ggplot(aes(x = HOURWAGE, weight = WT06)) +
  geom_histogram() +
  facet_wrap(~METRO) + 
  theme_minimal()

# Mean wage by MSASIZE
full_data %>% 
  filter(HOURWAGE != 999.99,
         HOURWAGE != 99.99) %>% 
  group_by(MSASIZE) %>% 
  summarize(mean = weighted.mean(HOURWAGE, WT06)) %>% 
  ggplot(aes(x = MSASIZE, y = mean)) +
  geom_histogram(stat = "identity") +
  coord_flip() +
  theme_minimal()

# Black vs. Non-Black Histogram by MSASIZE
full_data %>% 
  ggplot(aes(x = BLACK, weight = WT06)) +
  facet_wrap(~MSASIZE) + 
  geom_histogram(aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), stat = "count") +
  theme_minimal()

# Scatterplot by race
travel_data %>% 
  ggplot(aes(x = LOGHOURWAGE, y = log(TRAVELTIME))) +
  geom_point(aes(color = BLACK), alpha = .3) +
  geom_smooth(aes(color = BLACK)) +
  theme_minimal()

# Income histogram
travel_data %>% 
  ggplot(aes(x = fct_rev(INCOME_FACTOR), weight = WT06)) +
  geom_bar() +
  xlab("Income") +
  coord_flip() +
  theme_minimal()

# Start time histogram
ggplot(travel_data, aes(x = HOUR, weight = WT06)) +
  geom_histogram(stat = "count") +
  theme_minimal()

# Black count
travel_data %>% 
  group_by(BLACK) %>% 
  summarize(count = sum(WT06)) %>% 
  ggplot(aes(x = BLACK, y = count)) +
  geom_bar(stat = "identity") +
  theme_minimal()

# Visualizing travel time by race and method when wage is 0
reg_table %>% 
  filter(str_detect(term, "^METHOD|BLACKblack:METHOD"),) %>% 
  mutate(BLACK = factor(ifelse(str_detect(term, "BLACK"), 1, 0))) %>% 
  mutate(term = ifelse(str_detect(term, "BLACK"), substring(term, 12), term)) %>% 
  mutate(term = substring(term, 7)) %>% 
  ggplot(aes(x = term, y = estimate, fill = BLACK)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin = lowerci, ymax = upperci), position = position_dodge(width=0.5), width = .5) +
  theme_minimal() +
  ylab("Coefficient Estimate") +
  xlab("Method of Transportation") +
  scale_fill_brewer(palette="Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
