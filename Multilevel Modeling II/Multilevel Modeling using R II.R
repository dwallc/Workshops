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
              "sjstats",
              "sjPlot")

ipak(packages)

## Example I - Math Achievement

### Import the HSB All.xlsx dataset

dataHSB <- import("./Data/HSB All R.xlsx")

### Visually explore the 'mathach' variable

#### Plot #1 - idGrp = 1224

plotHSB1 <- ggplot(data = dataHSB[dataHSB$idGrp == 1224, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB1

#### Plot #2 - idGrp = 1288

plotHSB2 <- ggplot(data = dataHSB[dataHSB$idGrp == 1288, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB2

#### Plot #3 - idGrp = 1296

plotHSB3 <- ggplot(data = dataHSB[dataHSB$idGrp == 1296, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB3

### Estimate the null model with only individual-level variation

modelHSB1 <- lm(mathach ~ 1,
                data = dataHSB) # Using ols

summary(modelHSB1)

### Calculate and plot the grand mean

dataHSB <- dataHSB %>%
           mutate(GrandMean = predict(modelHSB1))

#### Plot #4 - idGrp = 1224

plotHSB4 <- ggplot(data = dataHSB[dataHSB$idGrp == 1224, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1224]))

plotHSB4

#### Plot #5 - idGrp = 1288

plotHSB5 <- ggplot(data = dataHSB[dataHSB$idGrp == 1288, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1288]))

plotHSB5

#### Plot #6 - idGrp = 1296

plotHSB6 <- ggplot(data = dataHSB[dataHSB$idGrp == 1296, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1296]))

plotHSB6

### Estimate the model with individual- and group-level variation

modelHSB2 <- lmer(mathach ~ 1 + (1|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB2)

icc(modelHSB2)

### Create data object featuring group-specific effects

grpEffHSB <- ranef(modelHSB2)

grpEffHSB <- rownames_to_column(grpEffHSB[["idGrp"]],
                                var = "idGrp") %>%
             mutate(idGrp = as.numeric(idGrp)) %>%
             rename(idGrpEffect = `(Intercept)`)

### Calculate and plot the group means

dataHSB <- dataHSB %>%
           mutate(GrandMean2 = predict(modelHSB,
                                       re.form = NA),
                  idGrpMean = predict(modelHSB2),
                  idGrpEffect2 = idGrpMean - GrandMean2) %>%
           inner_join(grpEffHSB,
                      by = "idGrp")

#### Plot #7 - idGrp = 1224

plotHSB7 <- ggplot(data = dataHSB[dataHSB$idGrp == 1224, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1224])) +
            geom_hline(aes(yintercept = dataHSB$idGrpMean[dataHSB$idGrp == 1224]),
                       colour = "blue",
                       linetype = "dashed")

plotHSB7

#### Plot #8 - idGrp = 1288

plotHSB8 <- ggplot(data = dataHSB[dataHSB$idGrp == 1288, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1288])) +
            geom_hline(aes(yintercept = dataHSB$idGrpMean[dataHSB$idGrp == 1288]),
                       colour = "blue",
                       linetype = "dashed")

plotHSB8

#### Plot #9 - idGrp = 1296

plotHSB9 <- ggplot(data = dataHSB[dataHSB$idGrp == 1296, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1296])) +
            geom_hline(aes(yintercept = dataHSB$idGrpMean[dataHSB$idGrp == 1296]),
                       colour = "blue",
                       linetype = "dashed")

plotHSB9


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
