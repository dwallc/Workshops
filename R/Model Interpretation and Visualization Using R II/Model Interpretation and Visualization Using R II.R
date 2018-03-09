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

## Logarithm Transformations

### Estimate regression model predicting a respondent's income
### based on their log transformed age.

model01 <- lm(realrinc ~ log(age),
              data = MIVdata01)

summary(model01)

### Calculate marginal effects (semi-elasticities)

summary(margins(model01))

summary(margins(model01,
                at = list(age = 18)))

summary(margins(model01,
                change = "dydx",
                variable = "age"))

summary(margins(model01,
                change = "sd",
                variable = "age"))

summary(margins(model01,
                change = "minmax",
                variable = "age"))

summary(margins(model01,
                change = "iqr",
                variable = "age"))

summary(margins(model01,
                change = c(18,
                           30),
                variable = "age"))

### Estimate regression model predicting a respondent's log
### transformed income based on their age.

model02 <- lm(log(realrinc) ~ age,
              data = MIVdata01)

summary(model02)

### Calculate marginal effects (semi-elasticities)

summary(margins(model02))

summary(margins(model02,
                at = list(age = 18)))

summary(margins(model02,
                change = "dydx",
                variable = "age"))

summary(margins(model02,
                change = "sd",
                variable = "age"))

summary(margins(model02,
                change = "minmax",
                variable = "age"))

summary(margins(model02,
                change = "iqr",
                variable = "age"))

summary(margins(model02,
                change = c(18,
                           30),
                variable = "age"))

### Estimate regression model predicting a respondent's log
### transformed income based on their log transformed age.

model03 <- lm(log(realrinc) ~ log(age),
              data = MIVdata01)

summary(model03)

### Calculate marginal effects (elasticities)

summary(margins(model03))

summary(margins(model03,
                at = list(age = 18)))

summary(margins(model03,
                change = "dydx",
                variable = "age"))

summary(margins(model03,
                change = "sd",
                variable = "age"))

summary(margins(model03,
                change = "minmax",
                variable = "age"))

summary(margins(model03,
                change = "iqr",
                variable = "age"))

summary(margins(model03,
                change = c(18,
                           30),
                variable = "age"))

## Quadratic Term

model04 <- lm(realrinc ~ age + I(age ^ 2) + as.factor(female),
              data = MIVdata02)

summary(model04)

summary(margins(model04))

## Interactions

### Categorical-Categorical Interactions

model05 <- lm(realrinc ~ age + as.factor(female)*as.factor(marital),
              data = MIVdata02)

summary(model05)

summary(margins(model05))

### Categorical-Continuous Interactions

model06 <- lm(realrinc ~ age*as.factor(marital),
              data = MIVdata02)

summary(model06)

summary(margins(model06))

summary(margins(model06,
                at = list(age = 18)))

### Continuous-Continuous Interactions

model07 <- lm(realrinc ~ age*maeduc,
              data = MIVdata02)

summary(model07)

summary(margins(model07))

summary(margins(model07,
                at = list(age = 18)))

summary(margins(model07,
                at = list(maeduc = 8)))
