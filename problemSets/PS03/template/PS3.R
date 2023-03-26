library(readr)
library(nnet)
library(MASS)
#######################

## QUESTION 1 ##

#######################

data <- read_csv("~/Documents/GitHub/StatsII_Spring2023/datasets/gdpChange.csv")

##response variable to negative/positive/no change
data$GDPwdiff <- ifelse(data$GDPWdiff<0, "negative",
               ifelse(data$GDPWdiff>0, "positive", 
                      "no change"))

#'no change' is reference category 
data$GDPwdiff <- factor(data$GDPwdiff, levels = c("no change", "negative", "positive"))

#unordered multinomial model 
multinom_model1 <- multinom(GDPwdiff ~ REG + OIL, data = data)
summary(multinom_model1)
exp(coef(multinom_model1))

# get p values
z <- summary(multinom_model1)$coefficients/summary(multinom_model1)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

#get cut-off points using predicted probabilities 
pred_probs <- predict(multinom_model1, type = "probs")
cutoff_points <- t(apply(pred_probs, 2, function(x) quantile(x, probs = 0.5)))
#print cutoff points
colnames(cutoff_points) <- c("no change", "negative", "positive")
rownames(cutoff_points) <- names(multinom_model1$coefficients)
print(cutoff_points)


#ordered multinomial model
ord_log <- polr(GDPwdiff ~ REG + OIL, data = data, Hess = TRUE)
summary(ord_log)

# Calculate a p value
ctable <- coef(summary(ord_log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

#get cut-off points using predicted probabilities 
pred_probs2 <- predict(ord_log, type = "probs")
cutoff_points2 <- t(apply(pred_probs2, 2, function(x) quantile(x, probs = 0.5)))
#print cutoff points
colnames(cutoff_points2) <- c("no change", "negative", "positive")
rownames(cutoff_points2) <- names(multinom_model1$coefficients)
print(cutoff_points2)

#######################

## QUESTION 2 ##

#######################

df <- read_csv("~/Documents/GitHub/StatsII_Spring2023/datasets/MexicoMuniData.csv")

with(df, 
     list(mean(PAN.visits.06), var(PAN.visits.06)))

# The variance is MUCH greater than the mean...
# This suggests that we will have over-dispersion in the model.

mod_ps <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = df, family = poisson)
summary(mod_ps)

# Based on the output of the Poisson regression, we can see that the coefficient estimate for competitive.district is negative (-0.08135) 
#and not statistically significant (p-value = 0.6336), 
#which suggests that there is no evidence to support the claim that PAN presidential candidates visit swing districts more.

#The coefficient for marginality.06 in the Poisson regression model is -2.08014. 
#This indicates that for a one-unit increase in marginality, holding all other variables constant,
#the expected log count of PAN presidential candidate visits decreases by 2.08014.

#The coefficient for PAN.governor.06 is -0.31158. 
#This means that for a one-unit increase in the presence of a PAN governor in a district, holding all other variables constant, 
#the expected log count of PAN presidential candidate visits decreases by 0.31158.

mean_visits <- exp(-3.81023 - 0.08135*1 - 2.08014*0 - 0.31158*1)
mean_visits



