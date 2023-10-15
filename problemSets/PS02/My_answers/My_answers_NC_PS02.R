# Applied Statistical Analysis I      
# Problem Set 2: Niall Carty (Student no. 00369004)

# Get working directory
getwd()

# Set working directory 
setwd("/Users/carty/OneDrive/Documents/GitHub/StatsI_Fall2023/problemSets/PS02/My_answers")
getwd()

#############################
### PROBLEM SET 2 ###
#############################

#Statistical independence: Two variables are statistically independent 
#if the conditional distributions of the population are identical across categories.


#Answer 1 (part 1)

#OBSERVED VALUES
Not_Stopped <- c(14,7)
Bribed <- c(6,7)
Stopped_Given_Warning <- c(7,1)

df = data.frame (Not_Stopped, Bribed, Stopped_Given_Warning )
row.names(df) <- c("Upper Class", "Lower Class")

#EXPECTED VALUES
Not_Stopped_E <- c(13.5,7.5)
Bribed_E <- c(8.357,4.642)
Stopped_Given_Warning_E <- c(5.142, 2.857)

df_E = data.frame (Not_Stopped_E, Bribed_E, Stopped_Given_Warning_E )
row.names(df_E) <- c("Upper Class", "Lower Class")

chi_test_n <- chisq.test(df, df_E)
chi_test_n

?chisq.test

#Answer 1 (part 2)

# Calculate P Value when Chi square test = 3.7925

Chi_Result <- 3.7925

pchisq(Chi_Result, df = 2, lower.tail=FALSE)

P <- 0.1501306

#Answer 1 (part 3)

chi_test_n$residuals 

stdnew_residuals <- chi_test_n$stdres
stdnew_residuals

# Answer 2 (part 1)



## is there a relationship between the reservation policy in a district and irrigation policy in place?
  
## The null hypothesis is that there is no association.

## The alternative hypothesis is that there is a correlation between the reservation policy and women leaders in a district (either positive or negative)
## and the irrigation policies in place.


#First, let's look at  representation of women in the GPs reserved for women.

data_India <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summary(data_India)

#Looking at the GPs where the reservation policy is in place, 
#we can see that there are female candidates in all of them. 
# In only another 16 GPs, are there female candidates in unreserved GPs.

length(which(data_India$reserved == 1)) #there arr 108 GPs with a reservation policy in place
length(which(data_India$reserved == 0)) #there are 214 GPs without a reservation policy in place

#all the reserved states have female candidates.
length(which(data_India$reserved == 1 & data_India$female == 1))  

#another 16 out of 214 remaining states have female candidates
data_India$matched <- ifelse(data_India$reserved == data_India$female, "yes" , "no" )
length(data_India$matched)

num_no <- length(which(data_India$matched == "no"))
num_no



# Answer 2 (part 2)

#Run a bivariate regression to test this hypothesis in R (include your code!).

data_female <- (data_India$female == 1)
data_male <- (data_India$female == 0)

model_water <- summary(lm(data_India$water~data_female))
print(model_water)

plot(data_India$water ~ data_female, col = "blue", xlab = "female", ylab = "water policy"  )
abline(coef(lm(data_India$water~data_female)), col = "green")


# Step 1: Assumptions #one categorical value and one continuous value
# Step 2: Hypotheses #slope = 0
# Step 3: Test statistic # if slope reject the null hypothesis, and there is strong evidence that there is a relationship between education and income 
#note: can't interpret the size! #if so, need to standardize.
# Step 4: P-value
# Step 5: Conclusion  

# t-test for the slope of the regression line: T-value = 2.049

# P-value = 0.0413




