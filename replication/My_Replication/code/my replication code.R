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

#read data
protests <- read_csv('/Users/samantanedzinskaite/Desktop/replication/data/civil_rights_protest_final.csv')
abs.cces.white <- read_csv('/Users/samantanedzinskaite/Desktop/replication/data/abs-jop-cces-white-countydata.csv')


colnames(abs.cces.white)
#merge Acharya, Blackwell, and Sen's CCES data with protests data
merged <- inner_join(abs.cces.white,protests,by='fips')

####OLS EFFECTS####
ols.dem.interact <- lm(dem ~ protest_indicator * pcturban * avg.dem.vshare.pre + 
                pctblack + logTotPop1960 + medincome + state.abb,
              data = merged,
              weights = sample.size)
ols.dem.interact$rse <- vcovHC(ols.dem.interact,type='HC2')

ols.affirm.interact <- lm(affirm ~ protest_indicator * pctblack + pcturban + 
                   logTotPop1960 + medincome + avg.dem.vshare.pre + state.abb,
                 data=merged,
                 weights = sample.size)
ols.affirm.interact$rse <- vcovHC(ols.affirm.interact,type='HC2')

ols.resent.interact <- lm(resent ~ protest_indicator * pctblack * medincome + pcturban + 
                   logTotPop1960 + avg.dem.vshare.pre + state.abb,
                 data=merged,
                 weights = sample.size)
ols.resent.interact$rse <- vcovHC(ols.resent.interact,type='HC2')

vcov.ols.list <- list(ols.resent.interact$rse,ols.affirm.interact$rse,ols.dem.interact$rse)
se.list.ols <- lapply(vcov.ols.list,FUN = function(x){return(sqrt(diag(x)))})





#output table
ols.tab <- stargazer(ols.resent.interact, ols.affirm.interact, ols.dem.interact,
                     keep = c("protest_indicator", "pctblack", "pcturban", "logTotPop1960",
                              "medincome", "avg.dem.vshare.pre"),
                     out = "replication_summary_output.html",
                     omit.stat = c("adj.rsq","ll", "F", "ser"),
                     dep.var.labels = c("Racial Resentment", "Affirm. Action", "Prop. Democrat"), 
                     column.sep.width = "0pt", float = TRUE, header = FALSE, 
                     multicolumn = TRUE,
                     se = se.list.ols,
                     star.cutoffs = c(0.05, 0.01, 0.1)
                     label = 'tab:olscces',
                     title = 'Effect of Historical Civil Rights Protests on Contemporary Political Attitudes, OLS',
                     notes = c("Outcome variables constructed by pooling annd computing county-averages among whites from the 2006-2011 CCES."))

##density plots
# Extract the residuals from the dem.ols model
residuals <- resid(ols.dem.interact)

# Plot a density plot of the residuals
plot(density(residuals), main = "Density Plot of Residuals (ols.dem.interact)")


# Extract the residuals from the dem.ols model
residuals2 <- resid(ols.affirm.interact)

# Plot a density plot of the residuals
plot(density(residuals2), main = "Density Plot of Residuals (ols.affirm.interact)")


# Extract the residuals from the dem.ols model
residuals3 <- resid(ols.resent.interact)

# Plot a density plot of the residuals
plot(density(residuals3), main = "Density Plot of Residuals (ols.resent.interact)")
