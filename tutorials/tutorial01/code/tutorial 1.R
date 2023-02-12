library(readr)
mydata <- X960b13b7_ee18_45fa_beae_59ca1255b988_Data

#removing uneccessary columns
tidy_data <-subset(mydata, select = -c(2,4))

#removing everything within the square brackets 
names(tidy_data) <- sub("\\[[^][]*]", "", names(tidy_data))

library(tidyverse)
library(ggplot2)
library(dplyr)

tidy_data$`Ease of doing business rank (1=most business-friendly regulations) `

regression <- lm(`GDP per capita (current US$) ` ~ `Tax revenue (% of GDP) `, tidy_data)

ggplot(tidy_data, aes(x = `Tax revenue (% of GDP) `, y = `GDP per capita (current US$) `)) + 
  geom_point() +
  geom_smooth(method = "lm")



regression <- lm(`GDP per capita (current US$) ` ~ `Ease of doing business rank (1=most business-friendly regulations) `, tidy_data)

ggplot(tidy_data, aes(x = `Ease of doing business rank (1=most business-friendly regulations) `, y = `GDP per capita (current US$) `)) + 
  geom_point() +
  geom_smooth(method = "lm")

regression <- lm(`Tax revenue (% of GDP) ` ~ `Ease of doing business rank (1=most business-friendly regulations) `, tidy_data)

ggplot(tidy_data, aes(y = `Tax revenue (% of GDP) `, x = `Ease of doing business rank (1=most business-friendly regulations) `)) + 
  geom_point() +
  geom_smooth(method = "lm")


no_c <- tidy_data[,-2]

reg <- lm(`GDP per capita (current US$) ` ~ ., no_c)
summary(reg)
library(stargazer)

stargazer(reg, type = "html", out = "table.html")
