## Group project r code

getwd()
setwd("Documents/Masters/MSc DS/Modules /STAT0032")

## tidyverse
library(tidyverse)

day <- read.csv("Group project/Bike-Sharing-Dataset/day.csv")
head(day)
colnames(day)

hour <- read.csv("Group project/Bike-Sharing-Dataset/hour.csv")
colnames(hour)[!is.element(colnames((hour)), colnames(day))]

head(hour)
## The columns of the data are as follows: 
# [1] "instant"    "dteday"     "season"     "yr"        
# [5] "mnth"       "holiday"    "weekday"    "workingday"
# [9] "weathersit" "temp"       "atemp"      "hum"       
# [13] "windspeed"  "casual"     "registered" "cnt"       


## Day vs hour?
day[1,] ## 1st Jan 2011
hour %>% filter(dteday == "2011-01-01") %>% summarise(sum(casual)) ## hour is the more granular version of day 

## We will proceed to use Hour data from here on

## Task 1 --- distribution of bike hires 

# We only need the following columns: season, cnt, workingday 

colnames(hour)
data <- hour %>% select(season, cnt, workingday, hr)
table(data$season)
table(data$hr)

## Both seasons, working days, evenings 
peak_commute_evening <- data %>% filter(season == 2 | season == 3) %>%## season 2 = spring, season 3 = summer
                        filter(workingday == 1) %>%
                        filter(is.element(hr,(16:19)))


# Plot -----------------------------------------
## 1 Histogram + Normal fit plot - summer + spring
## 1.1 Side by side
peak_commute_evening %>%  
  select(cnt, season)%>% ggplot(aes(x = cnt)) + 
  geom_histogram(aes(y = ..density..),col = "red") +
  #stat_function(fun = dnorm, 
  #              args = list(mean = mean(peak_commute_evening$cnt),
  #                          sd = sd(peak_commute_evening$cnt))) +
  #geom_vline(xintercept = mean(peak_commute_evening$cnt))+
  facet_wrap(~season)

## 1.2 Overlaid
peak_commute_evening %>%  
  select(cnt, season)%>% ggplot(aes(x = cnt, fill = as.factor(season))) + 
  geom_histogram()


peak_commute_evening %>% summarise(mean(cnt), var(cnt))

## 2 Histogram + Normal fit plot - spring only
pce_spring <- peak_commute_evening %>% filter(season == 2)
pce_spring%>%  
  select(cnt)%>% ggplot(aes(x = cnt)) + 
  geom_histogram(aes(y = ..density..),col = "red") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(pce_spring$cnt),
                            sd = sd(pce_spring$cnt))) +
  geom_vline(xintercept = mean(pce_spring$cnt),linetype = "dashed", 
             col = "blue") +
  annotate('text', label =  "sample mean",
         x = 600, y = 0.0025, col = "blue") + 
  labs(title = "Spring count data")

## Estimates for Spring
peak_commute_evening %>% filter(season == 2) %>%
  summarise(mean(cnt), var(cnt)) 

## Histogram + Normal fit plot - summer only
pce_summer <- peak_commute_evening %>% filter(season == 3)
pce_summer%>%  
  select(cnt)%>% ggplot(aes(x = cnt)) + 
  geom_histogram(aes(y = ..density..),col = "red") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(pce_summer$cnt),
                            sd = sd(pce_summer$cnt))) +
  geom_vline(xintercept = mean(pce_summer$cnt),linetype = "dashed", 
             col = "blue") +
  annotate('text', label =  "sample mean",
           x = 600, y = 0.0026, col = "blue") +
  labs(title = "Summer count data")


## -----------------------------------------
## Quantitative test for normality of data

# 1: Kolmogorov Smirnov test 
ks.test(pce_spring$cnt, "pnorm") ## small pvalue, reject normality
ks.test(pce_summer$cnt, "pnorm") ## small pvalue, reject normality

ks.test(pce_summer$cnt, pce_spring$cnt) ## small pvalue, reject null that says come from same distribution


# 2. Chi Squared Test 
pce_sample_statistics <- pce_spring %>% summarise(count = n(),mean =  mean(cnt), sd = sd(cnt))
sim_normal_data_spring <- rnorm(pce_sample_statistics$count,
                                pce_sample_statistics$mean,
                                pce_sample_statistics$sd)
sort(sim_normal_data_spring)
sort(pce_spring$cnt)

chisq.test(sort(pce_spring$cnt), sort(sim_normal_data_spring))






# -------- ignore
data %>% filter(season == 3) %>%## season 2 = spring, season 3 = summer
  filter(workingday == 1) %>%
  filter(is.element(hr,(16:19))) %>% ggplot(aes(cnt)) +
  geom_histogram(col = "red") 

peak_commute_evening %>% ggplot(aes(cnt, fill = as.factor(season))) +
  geom_histogram()

peak_communte_summer <- data %>% filter(season == 3) %>%## season 2 = spring, season 3 = summer
  filter(is.element(hr,(16:19))) %>% ggplot(aes(cnt)) +
  geom_histogram(col = "red") 

data %>% filter(cnt > 500 & cnt <800) %>% tabulate(season)


