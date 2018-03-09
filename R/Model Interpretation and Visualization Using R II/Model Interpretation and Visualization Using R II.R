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

setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/Stata/Model Interpretation and Visualization Using Stata II")

## Load data

MIVdata01 <- import("./Data/MIVdata01.dta")
