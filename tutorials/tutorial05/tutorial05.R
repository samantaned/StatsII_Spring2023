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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establishjust as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)
install.packages("MASS")
library(MASS)
install.packages("nnet")
library(nnet)
workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?
workingMoms$attitude <- factor(workingMoms$attitude,levels = c("SD", "D", "A", "SA"), 
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))

model <- polr(attitude ~ year + gender + race + age + prestige, data = workingMoms, Hess = TRUE)

summary(model)
# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.
test <- multinom(attitude ~ year + gender + race + age + prestige, data = workingMoms)
summary(test)
 #the coefficients for each predictor variable are similar, 
#but the standard errors are generally smaller in the multinomial model. 
#The residual deviance and AIC are also smaller in the multinomial model, indicating a better fit to the data.

# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?

model_interact <- polr(attitude ~ year * gender + race * gender + age * gender + prestige * gender, data = workingMoms, Hess = TRUE)
summary(model_interact)

#he coefficient for gender has changed to -0.9124, 
#indicating that being male is associated with a lower odds of having a positive attitude towards working mothers, all else being equal
