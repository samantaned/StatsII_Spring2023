####################################################################################
#AUTHOR: Shom Mazumder
#AFFILIATION: Harvard University Dept. of Government
#DATE LAST UPDATED: March 12th, 2018
#PURPOSE: analyze effect of historical protests on contemporary public opinion 
#DETAILS: this script checks the effect of protests on individual attitudes
####################################################################################
rm(list = setdiff(ls(),"root")) #clear workspace

############PACKAGES############
library(foreign)
library(ggplot2)
library(stargazer)
library(lmtest)
library(dplyr)
library(readr)
library(causalsens)
library(sandwich)
#### SET WORKING DIRECTORY ####

####SETUP####
setwd('/Users/samantanedzinskaite/Desktop/replication')

#read data
protests <- read_csv('/Users/samantanedzinskaite/Desktop/replication/data/civil_rights_protest_final.csv')
abs.cces.white <- read_csv('/Users/samantanedzinskaite/Desktop/replication/data/abs-jop-cces-white-countydata.csv')


#merge Acharya, Blackwell, and Sen's CCES data with protests data
merged <- inner_join(abs.cces.white,protests,by='fips')

####OLS EFFECTS####
ols.dem <- lm(dem ~ protest_indicator + pctblack + pcturban + logTotPop1960 +
                medincome + avg.dem.vshare.pre + state.abb,
              data = merged,
              weights = sample.size)

ols.dem$rse <- vcovHC(ols.dem,type='HC2')

ols.affirm <- lm(affirm ~ protest_indicator + pctblack + pcturban + 
                   logTotPop1960 + medincome + avg.dem.vshare.pre + state.abb,
                 data=merged,
                 weights = sample.size)
ols.affirm$rse <- vcovHC(ols.affirm,type='HC2')

ols.resent <- lm(resent ~ protest_indicator + pctblack + pcturban + 
                   logTotPop1960 + medincome + avg.dem.vshare.pre + state.abb,
                 data=merged,
                 weights = sample.size)
ols.resent$rse <- vcovHC(ols.resent,type='HC2')



vcov.ols.list <- list(ols.resent$rse,ols.affirm$rse,ols.dem$rse)#store vcov mats in a list
se.list.ols <- lapply(vcov.ols.list,FUN = function(x){return(sqrt(diag(x)))})#collect standard errors

# Extract the residuals from the dem.ols model
residuals <- resid(ols.dem)

# Plot a density plot of the residuals
plot(density(residuals), main = "Density Plot of Residuals (ols.dem)")


# Extract the residuals from the dem.ols model
residuals2 <- resid(ols.affirm)

# Plot a density plot of the residuals
plot(density(residuals2), main = "Density Plot of Residuals (ols.affirm)")


# Extract the residuals from the dem.ols model
residuals3 <- resid(ols.resent)

# Plot a density plot of the residuals
plot(density(residuals3), main = "Density Plot of Residuals (ols.resent)")






#output table
ols.tab <- stargazer(ols.resent, ols.affirm, ols.dem,
                     keep = c("protest_indicator", "pctblack", "pcturban", "logTotPop1960",
                              "medincome", "avg.dem.vshare.pre"), style = "ajps",
                     out = "original_summary_output.html",
                     omit.stat = c("adj.rsq","ll", "F", "ser"),
                     covariate.labels = c("Protest"), 
                     dep.var.labels = c("Racial Resentment", "Affirm. Action", "Prop. Democrat"), 
                     column.sep.width = "0pt", float = TRUE, header = FALSE, 
                     multicolumn = TRUE,
                     se = se.list.ols,
                     star.cutoffs = c(0.05,0.01,0.001),
                     label = 'tab:olscces',
                     title = 'Effect of Historical Civil Rights Protests on Contemporary Political Attitudes, OLS',
                     notes = c("Outcome variables constructed by pooling annd computing county-averages among whites from the 2006-2011 CCES."))



#### SENSITIVITY ANALYSIS ####

#estimate probability of treatment
treat <- glm(protest_indicator ~ pctblack + pcturban + medincome + avg.dem.vshare.pre + state.abb,
             data = merged,family = binomial(link = 'logit'))

#run the sensitivity analysis to unobserved confounding for racial resentment
#Blackwell (2014)
alpha <- seq(-0.4,0.4,by=0.005) #set confounding parameter
sens.rr <- causalsens(ols.resent,treat,~pctblack + pcturban + medincome + avg.dem.vshare.pre,data = merged,
                   confound = one.sided.att,alpha = alpha) #estimate sensitivity

#sensitivity plots
pdf(file = 'figures/absolute_sensitivity.pdf')
plot(sens.rr, type = "raw", bty = "n")
dev.off()

pdf(file = 'figures/partial_sensitivity.pdf')
plot(sens.rr, type = "r.squared", bty = "n") 
dev.off()