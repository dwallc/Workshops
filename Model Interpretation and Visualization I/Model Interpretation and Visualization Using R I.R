#########################################################################################
#       Name:   Model Interpretation and Visualization Using R I.R                      #
#       Date:   October 24, 2018                                                        #
#       Author: Desmond D. Wallace                                                      #
#       Purpose:        Create tables and plots to report regression results, and       #
#                               introduce margins project.                              #
#       Input Files:    Data\MIVdata.dta                                                #
#       Output Files:   Tables\MIVmodel01.txt,                                          #
#                       Tables\MIVmodel01.tex,                                          #
#                       Graphs\MIVcoefplot01.png,                                       #
#                       Graphs\MIVcoefplot01b.png                                       #
#########################################################################################


# Load Required Packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("rio",
              "texreg",
              "dotwhisker",
              "margins")

## rio - A Swiss-Army Knife for Data I/O
## texreg - Convert regression output to LaTeX or HTML tables
## dotwhisker - Dot-and-Whisker Plots of Regression Results
## margins - Marginal Effects Estimation

ipak(packages)

# Set Working Directory

#setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/Model Interpretation and Visualization I")

# Import Dataset

MIVdata <- import("./Data/MIVdata.dta")

# Estimate Regression Models

## Estimate an OLS regression models predicting a respondent's
## income based on their age and gender.

MIVdata[["female"]] <- factor(MIVdata[["female"]]) # Treat female as a factor variable

ols01 <- lm(realrinc ~ age + female,
            data = MIVdata)

summary(ols01)

## Estimate probit and logit regression models predicting
## whether a respondent is willing to vote for a female
## president based on their number of children and whether
## respondent graduated from high school.

MIVdata[["hsgrad"]] <- factor(MIVdata[["hsgrad"]])

probit01 <- glm(fepres ~ children + hsgrad,
                family = binomial(link = "probit"),
                data = MIVdata)

summary(probit01)

logit01 <- glm(fepres ~ children + hsgrad,
               family = "binomial",
               data = MIVdata)

summary(logit01)


# Part I - Regression Tables
