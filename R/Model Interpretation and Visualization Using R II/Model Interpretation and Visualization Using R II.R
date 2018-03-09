# Model Interpretation and Visualization Using R II
# Log Transformations and Interactions

## Load Required Packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse",
              "rio",
              "margins")

ipak(packages)

## Set Working Directory

setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Model Interpretation and Visualization Using R II")

## Load data

MIVdata01 <- import("./Data/MIVdata01.dta")

## Estimate regression model predicting a respondent's income
## based on their log transformed age.

model01 <- lm(realrinc ~ log(age),
              data = MIVdata01)

summary(model01)

## Calculate marginal effects (semi-elasticities)

margins(model01)

margins(model01,
        at = list(age = 18))

margins(model01,
        change = "dydx",
        variable = "age")

margins(model01,
        change = "sd",
        variable = "age")

margins(model01,
        change = "minmax",
        variable = "age")

margins(model01,
        change = "iqr",
        variable = "age")

margins(model01,
        change = c(18,
                   30))

## Estimate regression model predicting a respondent's log
## transformed income based on their age.

model02 <- lm(log(realrinc) ~ age,
              data = MIVdata01)

summary(model02)

## Calculate marginal effects (semi-elasticities)

margins(model02)

margins(model02,
        at = list(age = 18))

margins(model02,
        change = "dydx",
        variable = "age")

margins(model02,
        change = "sd",
        variable = "age")

margins(model02,
        change = "minmax",
        variable = "age")

margins(model02,
        change = "iqr",
        variable = "age")

margins(model02,
        change = c(18,
                   30))

## Estimate regression model predicting a respondent's log
## transformed income based on their log transformed age.

model03 <- lm(log(realrinc) ~ log(age),
              data = MIVdata01)

summary(model03)

## Calculate marginal effects (semi-elasticities)

margins(model03)

margins(model03,
        at = list(age = 18))

margins(model03,
        change = "dydx",
        variable = "age")

margins(model03,
        change = "sd",
        variable = "age")

margins(model03,
        change = "minmax",
        variable = "age")

margins(model03,
        change = "iqr",
        variable = "age")

margins(model03,
        change = c(18,
                   30))
