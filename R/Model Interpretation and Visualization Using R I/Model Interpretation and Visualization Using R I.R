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

model01 <- lm(realrinc ~ age + female,
              data = MIVdata01)

summary(model01)

## Calculate marginal effects using margins

### Average Marginal Effects

summary(margins(model01))

### Plotting Average Marginal Effects

plot(margins(model01))

### Marginal Effects at Specific Values

#### Single Value

summary(margins(model01,
                at = list(age = 18)))

#### Multiple Values

summary(margins(model01,
                at = list(age = fivenum(MIVdata01$age))))

summary(margins(model01,
                at = list(female = unique(MIVdata01$female))))

## Plot Predictions and Marginal Effects

### Age Variable

cplot(model01,
      x = "age",
      what = "prediction",
      main = "Predicted Income, Given Age")

cplot(model01,
      x = "age",
      what = "effect",
      main = "Average Marginal Effect of Age")

### Female Variable

cplot(model01,
      x = "female",
      what = "prediction",
      main = "Predicted Income, Given Gender")

cplot(model01,
      x = "female",
      what = "effect",
      main = "Average Marginal Effect of Gender")

### Calculate Discrete Changes

#### Age Variable

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

#### Female Variable

summary(margins(model01,
                change = "dydx",
                variable = "female"))

summary(margins(model01,
                change = "sd",
                variable = "female"))

summary(margins(model01,
                change = "minmax",
                variable = "female"))

summary(margins(model01,
                change = "iqr",
                variable = "female"))

summary(margins(model01,
                change = c(0,
                           1),
                variable = "female"))
