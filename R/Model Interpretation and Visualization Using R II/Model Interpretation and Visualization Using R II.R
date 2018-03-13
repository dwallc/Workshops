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

MIVdata01 <- import("./Data/MIVdata01.dta") %>%
             mutate(age_ln = log(age),
                    realrinc_ln = log(realrinc))

## Logarithm Transformations

### Estimate regression model predicting a respondent's income
### based on their log transformed age.

model01a <- lm(realrinc ~ log(age),
               data = MIVdata01)

model01b <- lm(realrinc ~ age_ln,
               data = MIVdata01)

summary(model01a)

summary(model01b)

### Calculate marginal effects (semi-elasticities)

summary(margins(model01a))

summary(margins(model01b))

summary(margins(model01a,
                at = list(age = mean(MIVdata01$age,
                                     na.rm = TRUE)))) # Keeps age on original scale

summary(margins(model01b,
                at = list(age_ln = mean(MIVdata01$age_ln,
                                        na.rm = TRUE)))) # Treats age on logarithmic scale

summary(margins(model01b,
                at = list(age_ln = fivenum(MIVdata01$age_ln))))

cplot(model01b,
      x = "age_ln",
      what = "prediction",
      main = "Predicted Income, Given ln(Age)")

cplot(model01b,
      x = "age_ln",
      what = "effect",
      main = "Average Marginal Effect of ln(Age)")

summary(margins(model01b,
                change = "dydx",
                variable = "age_ln"))

summary(margins(model01b,
                change = "sd",
                variable = "age_ln"))

summary(margins(model01b,
                change = "minmax",
                variable = "age_ln"))

summary(margins(model01b,
                change = "iqr",
                variable = "age_ln"))

summary(margins(model01b,
                change = c(1,
                           3),
                variable = "age_ln"))

### Estimate regression model predicting a respondent's log
### transformed income based on their age.

model02 <- lm(realrinc_ln ~ age,
              data = MIVdata01)

summary(model02)

### Calculate marginal effects (semi-elasticities)

summary(margins(model02))

summary(margins(model02,
                at = list(age = 18)))

summary(margins(model02,
                at = list(age = fivenum(MIVdata01$age))))

cplot(model02,
      x = "age",
      what = "prediction",
      main = "Predicted Income, Given Age")

cplot(model02,
      x = "age",
      what = "effect",
      main = "Average Marginal Effect of Age")

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

model03 <- lm(realrinc_ln ~ age_ln,
              data = MIVdata01)

summary(model03)

### Calculate marginal effects (elasticities)

summary(margins(model03))

summary(margins(model03,
                at = list(age_ln = fivenum(MIVdata01$age_ln))))

cplot(model03,
      x = "age_ln",
      what = "prediction",
      main = "Predicted Income, Given ln(Age)")

cplot(model03,
      x = "age_ln",
      what = "effect",
      main = "Average Marginal Effect of ln(Age)")

summary(margins(model03,
                change = "dydx",
                variable = "age_ln"))

summary(margins(model03,
                change = "sd",
                variable = "age_ln"))

summary(margins(model03,
                change = "minmax",
                variable = "age_ln"))

summary(margins(model03,
                change = "iqr",
                variable = "age_ln"))

summary(margins(model03,
                change = c(1,
                           3),
                variable = "age_ln"))

## Quadratic Term

model04 <- lm(realrinc ~ age + I(age ^ 2) + female,
              data = MIVdata01)

summary(model04)

summary(margins(model04))

cplot(model04,
      x = "age",
      what = "prediction",
      main = "Predicted Income, Given Age")

cplot(model04,
      x = "age",
      what = "effect",
      main = "Average Marginal Effect of Age")

cplot(model04,
      x = "female",
      what = "prediction",
      main = "Predicted Income, Given Gender")

cplot(model04,
      x = "female",
      what = "effect",
      main = "Average Marginal Effect of Gender")

## Interactions

### Categorical-Categorical Interactions

MIVdata01[["female"]] <- factor(MIVdata01[["female"]])

MIVdata01[["marital"]] <- factor(MIVdata01[["marital"]])

model05 <- lm(realrinc ~ age + female*marital,
              data = MIVdata01)

summary(model05)

summary(margins(model05))

cplot(model05,
      x = "female",
      what = "prediction",
      main = "Predicted Income, Given Gender")

cplot(model05,
      x = "marital",
      what = "prediction",
      main = "Predicted Income, Given Marital Status")

cplot(model05,
      x = "female",
      what = "effect",
      main = "Average Marginal Effect of Gender") # Values not Calculated

cplot(model05,
      x = "marital",
      what = "effect",
      main = "Average Marginal Effect of Marital Status") # Currently not supported

### Categorical-Continuous Interactions

model06 <- lm(realrinc ~ age*marital,
              data = MIVdata01)

summary(model06)

summary(margins(model06))

summary(margins(model06,
                at = list(age = 18)))

summary(margins(model06,
                at = list(age = fivenum(MIVdata01$age))))

cplot(model06,
      x = "age",
      what = "prediction",
      main = "Predicted Income, Given Age")

cplot(model06,
      x = "marital",
      what = "prediction",
      main = "Predicted Income, Given Marital Status")

cplot(model06,
      x = "age",
      what = "effect",
      main = "Average Marginal Effect of Age") # Effect of Age for Married Respondents

cplot(model06,
      x = "marital",
      what = "effect",
      dx = "age",
      main = "Average Marginal Effect of Age")

cplot(model06,
      x = "age",
      what = "effect",
      dx = "marital",
      main = "Average Marginal Effect of Marital Status") # Currently not supported

### Continuous-Continuous Interactions

model07 <- lm(realrinc ~ age*maeduc,
              data = MIVdata01)

summary(model07)

summary(margins(model07))

summary(margins(model07,
                at = list(age = 18)))

summary(margins(model07,
                at = list(maeduc = 8)))

summary(margins(model07,
                at = list(age = fivenum(MIVdata01$age))))

summary(margins(model07,
                at = list(maeduc = fivenum(MIVdata01$maeduc))))

cplot(model07,
      x = "age",
      what = "prediction",
      main = "Predicted Income, Given Age")

cplot(model07,
      x = "maeduc",
      what = "prediction",
      main = "Predicted Income, Given Maternal Education")

cplot(model07,
      x = "age",
      what = "effect",
      main = "Average Marginal Effect of Age") # Effect of age when maeduc = 0

cplot(model07,
      x = "maeduc",
      what = "effect",
      dx = "age",
      main = "Average Marginal Effect of Age")

cplot(model07,
      x = "maeduc",
      what = "effect",
      main = "Average Marginal Effect of Maternal Education") # Effect of maeduc when age = 0

cplot(model07,
      x = "age",
      what = "effect",
      dx = "maeduc",
      main = "Average Marginal Effect of Maternal Education")
