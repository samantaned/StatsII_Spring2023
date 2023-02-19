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
library(stargazer)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load("/Users/samantanedzinskaite/Documents/GitHub/StatsII_Spring2023/problemSets/PS02/template/climateSupport.RData")
data <- climateSupport

#have to encode the 'choice' data to make binary/binomial variables

new_data <- as.data.frame(ifelse(data$choice == "Supported", 1, 0))


#the countries variable is a categorical variable with 3 levels, and the 
#sanctions variable is a categorical variable with 4 levels. 
#have to convert them into a dummy variable for the model. (1 if it matches the categorical variable, 0 if it does not.) 
new_data$countries <- model.matrix(~ countries - 1, data = data)
new_data$sanctions <- model.matrix(~ sanctions - 1, data = data)

#rename columns for convenience
colnames(new_data)[1] <- "choice"
colnames(new_data)[2] <- "countries"
colnames(new_data)[3] <- "sanctions"

#now we can fit an additive model
model <- glm(choice ~ ., data = new_data, family = binomial(link = "logit"))
#summary output
summary(model)
stargazer(model, title = "Model Summary", out = "summary_output.html")

#global hypothesis 
null_model <- glm(choice ~ 1, data = new_data, family = binomial)
global_hypothesis <- anova(null_model, model, test = "Chisq")

stargazer(global_hypothesis, title = "Global Hypothesis", out = "global_hypothesis.html")


#Question 2

#c) would an interaction be appropriate?
#a model with interaction

model2 <- glm(choice ~ countries * sanctions, data = new_data, family = binomial(link = "logit"))
#compare it to our original additive model
test_for_model_comparison <- anova(model, model2, test = "Chisq")
summary(test_for_model_comparison)
stargazer(test_for_model_comparison, title = "Model Comparison", out = "Model Comparison.html")

#the p-value is 0.3912, which is greater than the typical level of 0.05. 
#there is not strong evidence to suggest that Model 2 with the interaction term 
#is a better fit for the data than Model 1 with only the main effects.

#b)

#probability = 1 / (1 + exp(-(intercept + b1*countries80of192 + b2*sanctionsNone)))
probability <- 1 / (1 + exp(-(-0.072 + (-0.312)*1 + 0.304*1)))
probability <- 1 / (1 + exp(-0.228))
print(probability)