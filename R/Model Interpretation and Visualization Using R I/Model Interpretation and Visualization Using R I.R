# Model Interpretation and Visualization Using R I
# Introduction t margins package

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

setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Model Interpretation and Visualization Using R I")

## Load data

MIVdata01 <- import("./Data/MIVdata01.dta")

## Estimate regression model predicting a respondent's income
## based on their age and gender.

model01 <- lm(realrinc ~ age + as.factor(female),
              data = MIVdata01)

## Calculate marginal effects using margins

### Specifically Age Variable

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

### Specifically Female Variable

margins(model01,
        change = "dydx",
        variable = "female")

margins(model01,
        change = "sd",
        variable = "female")

margins(model01,
        change = "minmax",
        variable = "female")

margins(model01,
        change = "iqr",
        variable = "female")

margins(model01,
        change = c(0,
                   1),
        variable = "female")
