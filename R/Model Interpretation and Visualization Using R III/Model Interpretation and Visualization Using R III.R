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

## EStimate binomial regression model

model01 <- glm(fepres ~ children + as.factor(hsgrad),
               family = "binomial",
               data = MIVdata02)

summary(model01)

## Calculate marginal effects

summary(margins(model01))

summary(margins(model01,
                at = list(children = 0)))

summary(margins(model01,
                at = list(children = 8)))
