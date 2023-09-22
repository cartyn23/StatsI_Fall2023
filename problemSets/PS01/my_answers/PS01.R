#####################
# load libraries
# set wd
# clear global .envir
#####################

# Get working directory
getwd()

# Set working directory 
setwd("/Users/carty/OneDrive/Documents/Stats1 Fall Prep")
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

sd(y) / sqrt(length(y))  # Variability, standard error

# calculate the t_score for a one-sided test, with confidence level of 0.05

#Right side critical value 95% confidence level, as looking to see if score is higher −

t_score <- abs(qt(0.05, df=(length(y)-1)))

#Test score = 1.710882, df = length((y)-1)


#find one-tail p-value with right sided test

P_value <- pt(q = 1.710882, df = (length(y)-1), lower.tail = FALSE)

#The p-value of 0.05000001 is approximately equal to the significance level of 0.05

# for this reason, we found evidence that we can reject the null hypothesis, as with repeated sampling, 
# we can expect the p-value to fall within the area of significance


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
