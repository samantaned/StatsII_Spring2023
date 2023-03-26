# Reading in the data - option 1, using stringsAsFactors
graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)
# Option 2 - parse column names as a vector to colClasses
graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         colClasses = c("hsgrad" = "factor", 
                                        "nonwhite" = "factor",
                                        "mhs" = "factor",
                                        "fhs" = "factor",
                                        "intact" = "factor"))

summary(graduation)

# Another option: coercing hsgrad from a character vector to a logical vector
as.logical(as.numeric(as.factor(graduation$hsgrad))-1) 

# Run the logit regression
reg <- glm(hsgrad ~ ., # period functions as omnibus selector (kitchen sink additive model)
           data = graduation, 
           family = "binomial")

reg <- glm(hsgrad ~ ., 
           data = graduation, 
           family = binomial(link = "logit")) # same as above (logit is default arg)

summary(reg)

# Likelihood ratio test
reg_null <- glm(as.factor(hsgrad) ~ 1, data = graduation, family = "binomial") # 1 = fit an intercept only (i.e. sort of a "mean")
anova(reg_null, reg, test = "Chisq")

# Extracting confidence intervals (of the coefficients)
?confint
exp(confint(reg)) # Transform to odds ratio using exp()

# An option for making a data.frame of conf ints and coefficients
conf_reg <- data.frame(cbind(lower = exp(confint(reg)[,1]), 
                             coefs = exp(coef(reg)), 
                             upper = exp(confint(reg)[,2])))

# Then using this to make a plot
ggplot(data = conf_reg, mapping = aes(x = row.names(conf_reg), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip()

# Factor vs numeric
# The nsibs variable was parsed as an integer. The model.matrix() function is used under the
# hood by lm() and glm() to create a design matrix of the model. See the difference compared
# to when we input nsibs as an integer and a factor
?model.matrix

model.matrix( ~ unique(nsibs), data = graduation)
model.matrix( ~ as.factor(unique(nsibs)), data = graduation)

# As a side note, we can use unique() with model.matrix() to create a matrix of different
# combinations of factor levels to use with predict(). Another function that can help with 
# this is expand.grid()

with(graduation, expand.grid(nonwhite = unique(nonwhite),
                             mhs = unique(mhs),
                             fhs = unique(fhs)))

# Consider for instance if we had a model just consisting of factors:
fac_reg <- glm(hsgrad ~ nonwhite + mhs + fhs, data = graduation, family = "binomial")

predicted_data <- with(graduation, expand.grid(nonwhite = unique(nonwhite),
                                               mhs = unique(mhs),
                                               fhs = unique(fhs)))
  
predicted_data <- cbind(predicted_data, predict(fac_reg, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

# Now we can use the code in Jeff's lecture to fill out the confidence intervals and 
# predicted probability (see lecture)
predicted_data <- within(predicted_data,
                         {
                           PredictedProb <- plogis(fit)
                           LL <- plogis(fit - (1.96 * se.fit))
                           UL <- plogis(fit + (1.96 * se.fit))
                         })

# As an alternative to coercing an interval variable as a factor, with one level for each
# unique value, we can "bin" the variable into a smaller number of categories using cut()
?cut
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

reg_3 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs", "nsibs_f")], 
             family = "binomial")
summary(reg_3)
summary(reg)

# Extract confidence intervals around the estimates
conf_reg_2 <- data.frame(cbind(lower = exp(confint(reg_3)[,1]), 
                               coefs = exp(coef(reg_3)), 
                               upper = exp(confint(reg_3)[,2])))

# Plot the estimates and confidence intervals
ggplot(data = conf_reg_2, mapping = aes(x = row.names(conf_reg_2), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") +
  coord_flip()
