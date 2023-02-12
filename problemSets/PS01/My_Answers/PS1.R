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
setwd("~/Documents/GitHub/StatsII_Spring2023/problemSets/PS01/template")


#####################
# Problem 1
#####################

set.seed(2023)
#generate random data 
data <- rcauchy(1000, location = 0, scale = 1)

#ks function with reference to normal distribution
# pnorm gives the cdf of the standard normal distribution.  

ks_test <- function(data){
  n <- length(data)
  edf <- ecdf(data)
  empircalCDF <- edf(data)
  test_stat <- max(abs(empircalCDF - pnorm(data)))
  p_value <- 1 - pnorm(test_stat * sqrt(n))
  return(c(ks_statistic = test_stat, p_value = p_value))
}

output <- ks_test(data)
print(output)


#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

linear.lik <- function(theta, y, X) {
  n <- nrow(X)
  k <- ncol(X)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - X%*%beta
  logl <- -0.5 * n * log(2 * pi) - 0.5 * n * log(sigma2) - 
    ((t(e) %*% e) / (2 * sigma2))
  return(-logl)
}
#fit the model with the bfgs method
model_fit <- optim(par = c(1, 1, 1), fn = linear.lik, hessian = TRUE, y = data$y, 
             X = cbind(1, data$x), method = "BFGS")
#extract beta-hat
beta_hat <- model_fit$par[1:2]
print(beta_hat)


summary(ols_lm <- lm(y ~ x, data))




