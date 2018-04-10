#################################################################################
#       Name: Multilevel Modeling using Stata I.R                               #
#       Date: March 23, 2018                                                    #
#       Author:	Desmond D. Wallace                                              #
#       Purpose:	Estimating basic random slope and random coefficient    #
#                               multilevel models via the 'multilevel' package. #
#       Input Files:	Data\HSB All.xlsx,                                      #
#                       productivity.dta                                        #
#################################################################################

# Set working directory

setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/Multilevel Modeling I")


# Load required packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("tidyverse",
              "rio",
              "lme4",
              "sjstats")

ipak(packages)

## Example I - SES

### First, import the HSB All.xlsx dataset

dataHSB <- import("./Data/HSB All R.xlsx")

### Estimate Null Random Intercept Model

modelHSB <- lmer(ses ~ 1 + (1|idGrp),
                 data = dataHSB,
                 REML = FALSE)

summary(modelHSB)

icc(modelHSB)

### Calculate GrandMean and Group-specific Means

dataHSB <- dataHSB %>%
           mutate(GrandMean = predict(modelHSB,
                                      re.form = NA),
                  idGrpMean = predict(modelHSB),
                  idGrpEffect = idGrpMean - GrandMean)

## Example II - GSP

### First, import the HSB All.xlsx dataset

dataProd <- import("http://www.stata-press.com/data/r12/productivity.dta")

### Estimate Null Random Intercept Model

modelProd <- lmer(gsp ~ 1 + (1|state) + (1|region),
                  data = dataProd,
                  REML = FALSE)

summary(modelProd)

icc(modelProd)

### Calculate GrandMean and Group-specific Means

dataProd <- dataProd %>%
            mutate(GrandMean = predict(modelProd,
                                       re.form = NA),
                   RegionMean = predict(modelProd,
                                        re.form = ~(1|region)),
                   StateMean = predict(modelProd),
                   RegionEffect = RegionMean - GrandMean,
                   StateEffect = StateMean - GrandMean - RegionEffect)
