#####################
# load libraries
# set wd
# clear global .envir
#####################

# Get working directory
getwd()

# Set working directory 
setwd("/Users/carty/OneDrive/Documents/GitHub/StatsI_Fall2023/problemSets/PS01/my_answers")
getwd()

# remove objects
rm(list=ls())

# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# QUESTION 1: find a 90% Confidence Interval for the average IQ in the school
# below is a random sample of 25 students' IQ scores

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# let's look at key data

sort(y) # reorder scores
mean(y) # Central tendency, mean
var(y) # Variability, variance
sd(y) # Variability, standard deviation
sd(y) / sqrt(length(y))  # Variability, standard **error**

summary(y)

# Some quick visualizations, to look at distribution
hist(y, 
     breaks = 5,
     main = "Students' IQ scores",
     xlab = "IQ score")

plot(y,
     main="Students' IQ scores",
     ylab="IQ scores")

#To find the 90% Confidence Interval, we look for the Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?
mean(y) # Central tendency, mean - a point estimate
sd(y) / sqrt(length(y))  # Variability, the standard error

# How to find the multiple? 
# Looking at the normal distribution, we see that 90% of observations lie within +/-1.64 
# standard errors of the point estimate 

?qnorm
qnorm(0.05) # value for first 5%
qnorm(0.95) # value last 5%

# Solution method 1: The **approximate** solution for 90% confidence level

upper_90 = mean(y)+(1.64*sd(y) / sqrt(length(y)))
lower_90 = mean(y)-(1.64*sd(y) / sqrt(length(y)))

#Summary
lower_90
mean(y)
upper_90

# SOlution method 2: The **precise** solution, using normal distribution for 90% confidence level

lower_90_n <- qnorm(0.05, 
                    mean = mean(y), 
                    sd = (sd(y) / sqrt(length(y))))

# Upper bound, 95 confidence level
upper_90_n <- qnorm(0.95,
                    mean = mean(y),
                    sd = (sd(y) / sqrt(length(y))))


# On this basis, we can conclude that the confidence interval lies between 94.13283 and 102.7472


# QUESTION 2: is the average student IQ in the school higher than the average IQ score (100) 
# in schools across the country?

# As the sample size is less than 30 we conduct a T-test

# It will be a one-sided test as we are looking to see if the student IQ in the school 
# is higher than the pop average

# The NULL Hypothesis is that there is no difference 

# Alternative Hypothesis is that the IQ is higher in the school

# The significance level is set at 0.05

# First step: calculate the standard error

standard_error <-  sd(y) / sqrt(length(y))  # Variability, standard error

# Find T statistic

t_statistic <- abs((mean(y) - 100) / standard_error)

# Find probability score

prob <- pt(t_statistic, (length(y)-1))

# p_value = 0.7215383

# Therefore we have evidence to accept the null hypothesis, as prob > 0.05

# METHOD 2

# calculate the t_score for a one-sided test, with confidence level of 0.05

t.test(y, mu = 100, conf.level = 0.05, alternative = "greater")

# Update Histogram 

hist(y, 
     breaks = 5,
     abline(v=mean(y),col="black"),
     main = "Students' IQ scores",
     xlab = "IQ score")

abline(v=mean(y),col="black")
abline(v=lower_90,col="black",lty="dashed")
abline(v=upper_90,col="black",lty="dashed")



#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

#Please plot the relationships among Y, X1, X2, and X3 ? What are the correlations
# among them (you just need to describe the graph and the relationships among them)?

exp_per_capita <- expenditure$Y
state_income <- expenditure$X1
fin_secure <- expenditure$X2
urban_areas <- expenditure$X3
region <-  expenditure$Region
factor_region <- factor(region, levels = c("1", "2", "3", "4"))
levels(factor_region) <- c("Northeast", "North Central", "South", "West")

install.packages("ggplot2")       
install.packages("GGally")
library("ggplot2")                     
library("GGally")

data.frame(expenditure)
ggpairs(expenditure, columns = 2:5)

# the strongest associations observed are between per capital expenditure on shelters/housing assistance in the state (Y) 
# and per capita personal income in state (X1) and also the number of people living in urban areas in the state (X3)

# the weakest associations are between the per capital personal income in state (X1) and number of residents per 100,00 that are 
# "financially secure" in state (X2), as well as between number of residents that are financially secure (X2)
#and the number of people residing in urban areas of the state


#Please plot the relationship between Y and Region? On average, which region has the
#highest per capita expenditure on housing assistance?

ggplot(expenditure, aes(x = exp_per_capita, y = factor_region)) +
  geom_boxplot() +
  labs(x = "per capita expenditure on shelters/housing assistance in state",
       y = "regions") 


# region 4 has the highest expenditure on housing assistance.

#Please plot the relationship between Y and X1 ? Describe this graph and the relationship. #Reproduce the above graph including one more variable Region and display
#different regions with different types of symbols and colors

ggplot(expenditure, aes(x = state_income, y = exp_per_capita)) +
  geom_point(size = 2, color = "red") +
  labs(x = "per capita personal income in state", y = "per capita expenditure on shelters/housing assistance in state" )

cor(exp_per_capita, state_income)

# with Region added

ggplot(expenditure, aes(x = state_income, y = exp_per_capita, color = factor_region, shape = factor_region)) +
  geom_point(size = 2) +
  labs(x = "per capita personal income in state", 
       y = "per capita expenditure on shelters/housing assistance in state", 
       color = "Regions", 
       shape = "Regions")


# The correlation is 0.532 and you can see a strong correlation in particular across region 3, and then region 2. 

#Regions 1 and 4 appear to have a less strong correlation.