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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages(car)
library(car)
data(Prestige)
help(Prestige)

df <- data.frame(Prestige)

# Create dummy variables 
df$professional <- ifelse(df$type == "prof", 1, 0)
df

# Run a linear model 
model1 = lm(df$prestige ~ df$income + df$professional, data = Prestige)
summary(model1)

sprintf("%.20f",3.062e+01) 
#30.62
sprintf("%.20f",1.371e-03) 
#0.001
sprintf("%.20f",2.276e+01) 
#22.76

# Write the prediction equation 

# Y = B0 + B1X1 + B2D1

# Prestige = intercept + income*Slope1 + profstatus*Slope2

# Y = 30.62 + 0.001 *Income + 22.76 * ProfStatus

# B0 = 30.62 is the intercept, 
# which is the predicted Y value when both income = 0 and Profstatus = 0

### (d) Interpret the coefficient for income ---

#B1 = 0.001 is the slope associated with income, when controlling for profstatus

### (e) Interpret the coefficient for professional ---
#B2 = 22.76 is the effect associated with professional status when 
# controlling for income

# Controlling for income, blue and white colour workers exhibit
# on average, a 22.76 unit drop in income compared to those with the professional status.

# Controlling for income, those with professional status, exhibit, on average
# a 22.76 unit increase in income compared to blue and white colured workers.
