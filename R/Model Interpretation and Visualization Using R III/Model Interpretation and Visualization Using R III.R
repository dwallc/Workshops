# Model Interpretation and Visualization Using R III
# Binomial Models

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

setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Model Interpretation and Visualization Using R III")

## Load data

MIVdata02 <- import("./Data/MIVdata02.dta")

## Estimate binomial regression model

MIVdata02[["hsgrad"]] <- factor(MIVdata02[["hsgrad"]])

model01 <- glm(fepres ~ children + hsgrad,
               family = "binomial",
               data = MIVdata02)

summary(model01)

## Calculate Average marginal effects

summary(margins(model01))

summary(margins(model01,
                at = list(children = 0)))

summary(margins(model01,
                at = list(children = 8)))

## Plotting Average Marginal Effects

plot(margins(model01))

## Plotting Predicted Probabilities and Marginal Effects

cplot(model01,
      x = "children",
      what = "prediction",
      main = "Predicted Probability of Supporting a Female President, Given Number of Children")

cplot(model01,
      x = "hsgrad",
      what = "prediction",
      main = "Predicted Probability of Supporting a Female President, Given High School Graduation")

cplot(model01,
      x = "children",
      what = "effect",
      main = "Average Marginal Effect of Number of Children")

cplot(model01,
      x = "hsgrad",
      what = "effect",
      main = "Average Marginal Effect of High School Graduation") # Does not work

## Calculate Discrete Changes

#### children Variable

summary(margins(model01,
                change = "dydx"))

summary(margins(model01,
                change = "sd"))

summary(margins(model01,
                change = "minmax"))

summary(margins(model01,
                change = "iqr"))

summary(margins(model01,
                change = c(1,
                           2)))

summary(margins(model01,
                change = c(3,
                           4))) # Change in marginal effect differs going from 3 -> 4 as opposed to 1 -> 2
