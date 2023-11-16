#####################
# load libraries
# set wd
# clear global .envir
#####################

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

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)}

if(!require(stargazer)){
  install.packages("stargazer")
  library(stargazer)}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

#We are interested in knowing how the difference in campaign spending between incumbent
# and challenger affects the incumbent’s vote share.

#After we load our dataset into our working environment, we execute our regression
#model in which the incumbent's vote share is explained by the difference in campaign
# spending between incumbent and challenger.`

# `Question 1: Run a regression where the outcome variable is voteshare and the explanatory variable
# is difflog

# Set our hypothesis ----

# Null hypothesis: 
#A difference in campaign spending between incumbents
# and challengers has no impact on incumbent vote share

# Alternative hypothesis: 
# an increase in incumbent campaign spending either increases (or decreases) their vote share

# run the regression

model <- lm(inc.sub$voteshare ~ inc.sub$difflog, data=inc.sub)

# we then run a summary to check the coefficients

summary(model)

# the slope for the difference in campaign spending is equal to 0.041666
# So we have evidence to support the view that a one unit increase in spending 
# leads to a 0.04 increase in vote share

# The estimated coefficient is statistically differentiable from
# zero at the α = 0.05 level because the p-value < 0.05 (≈2e-16).

# (1.b.) Descriptive analysis -------

# Scatter plot
scatter <-
  ggplot(data = inc.sub,
         mapping = aes(x = difflog, 
                       y = voteshare)) +
  geom_point() +
  labs(x = "Campaign Spending", 
       y = "Incumbent Vote Share") +
  
  geom_smooth(method='lm',col="red")

ggsave(scatter, file = "vote_share_incumbent_scatter.png")

# Print plot object
scatter

# (1.c.) Residuals -------

# Save the residuals of the model in a separate object

vote_residuals <- model$residuals
vote_residuals

# (1.d.) Write the prediction equation ----

# income_pred = intercept (0.579031) + slope(0.041666) * vote share

# Make predictions for first observation 
head(inc.sub)
0.579031 +  0.041666 * 1 # predicted outcome = 0.620697
model$fitted.values
0.6027841 - (0.579031 +  0.041666 * 1) # error for first residual
model$residuals

# Make predictions for a range of x values
predicted_model <- predict(model, newdata=data.frame(voteshare = seq(min(inc.sub$voteshare), max(inc.sub$voteshare), by=1)))

predicted_model


# `Question 2: Run a regression where the outcome variable is voteshare and the explanatory variable
# is presvote

# Set our hypothesis ----

# Null hypothesis: 
#A difference in campaign spending between incumbents
# and challengers has no impact on president's party vote share

# Alternative hypothesis: 
# an increase in incumbent campaign spending either increases (or decreases) their vote share

# run the regression

model_pres <- lm(inc.sub$presvote ~ inc.sub$difflog, data=inc.sub)

# we then run a summary to check the coefficients

summary(model_pres)

# the slope for the difference in campaign spending is equal to 0.023837  
# So we have evidence to support the view that a one unit increase in spending 
# leads to a 0.023837 increase in the president party's vote share

# The estimated coefficient is statistically differentiable from
# zero at the α = 0.05 level because the p-value < 0.05 (≈2e-16).

# (2.b.) Descriptive analysis -------

# Scatter plot
scatter_pres <-
  ggplot(data = inc.sub,
         mapping = aes(x = difflog, 
                       y = presvote)) +
  geom_point() +
  labs(x = "Campaign Spending", 
       y = "Presidential candidate of the incumbent's party vote share") +
  
  geom_smooth(method='lm',col="green")

ggsave(scatter_pres, file = "vote_share_pres_scatter.png")

# Print plot object
scatter_pres

# Now we add the regression line

# (2.c.) Residuals -------

# Save the residuals of the model in a separate object

pres_residuals <- model_pres$residuals
pres_residuals

# (2.d.) Write the prediction equation ----



# `Question 3: Run a regression where we examine whether the 
# vote share of the presidential candidate of the incumbent’s party 
# is associated with the incumbent’s electoral success.


# Set our hypothesis ----

# Null hypothesis: 
#There is no association between the 
# vote share of the presidential candidate of the incumbent’s party 
# and the incumbent’s electoral success

# Alternative hypothesis: 
#There is an association between the 
# vote share of the presidential candidate of the incumbent’s party 
# and the incumbent’s electoral success

# run the regression

model_vote <- lm(inc.sub$voteshare ~ inc.sub$presvote, data=inc.sub)

# we then run a summary to check the coefficients

summary(model_vote)

# the slope for the difference in the incumbent's electoral success is equal to 0.388018   
# So we have evidence to support the view that a one unit increase in the incumbent's elctoral success 
# leads to a 0.388018 increase in the vote share of the President's party

# The estimated coefficient is statistically differentiable from
# zero at the α = 0.05 level because the p-value < 0.05 (≈2e-16).

# (3.b.) Descriptive analysis -------

# Scatter plot
scatter_vote <-
  ggplot(data = inc.sub,
         mapping = aes(x = inc.sub$presvote, 
                       y = inc.sub$voteshare)) +
  geom_point() +
  labs(x = "President Party vote share", 
       y = "Incumbent electoral success") +
  
  geom_smooth(method='lm',col="yellow")

ggsave(scatter_vote, file = "vote_share2_scatter.png")

# Print plot object
scatter_vote

# Now we add the regression line

# (3.c.) Residuals -------

# Save the residuals of the model in a separate object

new_vote_residuals <- model_vote$residuals

# (3.d.) Write the prediction equation ----

# Test for correlation

cor.test(inc.sub$presvote, inc.sub$voteshare)


# Question 4 -----

#the residuals from part (a) tell us how much of the variation in voteshare is not explained
#by the difference in spending between incumbent and challenger. The residuals in part (b)
#tell us how much of the variation in presvote is not explained by the difference in spending
#between incumbent and challenger in the district.

#1. Run a regression where the outcome variable is the residuals from Question 1 and the
#explanatory variable is the residuals from Question 2.

# Null hypothesis: 
#There is no association between the residuals

# Alternative hypothesis: 
#There is an association between the residuals

# run the regression

model_residuals <- lm(vote_residuals ~ pres_residuals, data=inc.sub)

# we then run a summary to check the coefficients

summary(model_residuals)

# the slope is equal to -2.059e-01   
# So we do not have enough evidence to reject the null hypothesis in this instance.

# Note that the estimated coefficient is statistically differentiable from
# zero at the α = 0.05 level because the p-value < 0.05 (≈2e-16).

# (4.b.) Descriptive analysis -------

# Scatter plot
scatter_residuals <-
  ggplot(data = inc.sub,
         mapping = aes(x = pres_residuals, 
                       y = vote_residuals)) +
  geom_point() +
  labs(x = "Presidntial Party candidate Residuals", 
       y = "Vote Residuals") +
  
  geom_smooth(method='lm',col="purple")

ggsave(scatter_residuals, file = "residuals_scatter.png")

# Print plot object
scatter_residuals

# (4.c.) Write the prediction equation ----


