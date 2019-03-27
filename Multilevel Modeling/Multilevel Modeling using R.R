###############################################################################
#       Name: Multilevel Modeling using R.R                                   #
#       Date: March 27, 2019                                                  #
#       Author:	Desmond D. Wallace                                            #
#       Purpose:        Estimating  multilevel models via the 'lme4' package. #
#       Input Files:	Data\HSB All.xlsx                                     #
###############################################################################


# Set working directory

#setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/Multilevel Modeling")


# Load required packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("gridExtra",
              "tidyverse",
              "rio",
              "lme4",
              "sjstats")

ipak(packages)


# Import the HSB All.xlsx dataset

dataHSB <- import("./Data/HSB All R.xlsx") %>%
           mutate(sizeScaled = size / 100)


# Visually explore the 'mathach' variable

## Plot #1 - idGrp = 1224

plotHSB1 <- ggplot(data = dataHSB[dataHSB$idGrp == 1224, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

windows()

plotHSB1

## Plot #2 - idGrp = 1288

plotHSB2 <- ggplot(data = dataHSB[dataHSB$idGrp == 1288, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB2

## Plot #3 - idGrp = 1296

plotHSB3 <- ggplot(data = dataHSB[dataHSB$idGrp == 1296, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB3

## Place individual plots on same page

grid.arrange(plotHSB1,
             plotHSB2,
             plotHSB3)


# Example I: Null Model (Pooled OLS)

## Estimate the null model with only individual-level variation

modelHSB1 <- lm(mathach ~ 1,
                data = dataHSB) # Using ols

summary(modelHSB1)

## Calculate and plot the grand mean

dataHSB <- dataHSB %>%
           mutate(GrandMean = predict(modelHSB1))

### Plot #4 - idGrp = 1224

plotHSB4 <- ggplot(data = dataHSB[dataHSB$idGrp == 1224, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1224]))

### Plot #5 - idGrp = 1288

plotHSB5 <- ggplot(data = dataHSB[dataHSB$idGrp == 1288, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1288]))

### Plot #6 - idGrp = 1296

plotHSB6 <- ggplot(data = dataHSB[dataHSB$idGrp == 1296, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1296]))

### Place individual plots on same page

grid.arrange(plotHSB4,
             plotHSB5,
             plotHSB6)


# Example II: Random Intercept Model (Null Model)

## Estimate the model with individual- and group-level variation

modelHSB2 <- lmer(mathach ~ 1 + (1|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB2)

icc(modelHSB2)

## Create data object featuring group-specific effects

fixEffHSB <- fixef(modelHSB2)

grpEffHSB <- ranef(modelHSB2)

grpEffHSB <- rownames_to_column(grpEffHSB[["idGrp"]],
                                var = "idGrp") %>%
             mutate(idGrp = as.numeric(idGrp)) %>%
             rename(idGrpEffect = `(Intercept)`)

### Calculate and plot the group means

dataHSB <- dataHSB %>%
           select(-GrandMean) %>%
           mutate(GrandMean = predict(modelHSB2,
                                      re.form = NA),
                  idGrpMean = predict(modelHSB2)) %>%
           inner_join(grpEffHSB,
                      by = "idGrp") %>%
           mutate(idGrpMeanCheck = fixEffHSB[1] +
                          idGrpEffect)

#### Plot #7 - idGrp = 1224

plotHSB7 <- ggplot(data = dataHSB[dataHSB$idGrp == 1224, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1224])) +
            geom_hline(aes(yintercept = dataHSB$idGrpMean[dataHSB$idGrp == 1224]),
                       colour = "blue",
                       linetype = "dashed")

#### Plot #8 - idGrp = 1288

plotHSB8 <- ggplot(data = dataHSB[dataHSB$idGrp == 1288, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1288])) +
            geom_hline(aes(yintercept = dataHSB$idGrpMean[dataHSB$idGrp == 1288]),
                       colour = "blue",
                       linetype = "dashed")

#### Plot #9 - idGrp = 1296

plotHSB9 <- ggplot(data = dataHSB[dataHSB$idGrp == 1296, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point() +
            geom_hline(aes(yintercept = dataHSB$GrandMean[dataHSB$idGrp == 1296])) +
            geom_hline(aes(yintercept = dataHSB$idGrpMean[dataHSB$idGrp == 1296]),
                       colour = "blue",
                       linetype = "dashed")

### Place individual plots on same page

grid.arrange(plotHSB7,
             plotHSB8,
             plotHSB9)


# Example III: Random Intercept Model (Level-1 Covariates)

## Estimate a random intercept model with level-1 variables

modelHSB3 <- lmer(mathach ~ ses + as.factor(minority) + as.factor(female) +
                          (1|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB3)

icc(modelHSB3)

## Create data object featuring group-specific effects

fixEffHSB <- fixef(modelHSB3)

grpEffHSB <- ranef(modelHSB3)

grpEffHSB <- rownames_to_column(grpEffHSB[["idGrp"]],
                                var = "idGrp") %>%
             transmute(idGrp = as.numeric(idGrp),
                       idGrpEffect = `(Intercept)`)

## Calculate the group means

dataHSB <- dataHSB %>%
           select(-idGrpMean,
                  -idGrpEffect,
                  -idGrpMeanCheck) %>%
           mutate(predMathAch = predict(modelHSB3)) %>%
           inner_join(grpEffHSB,
                      by = "idGrp") %>%
           mutate(predMathAchCheck = fixEffHSB[1] +
                          idGrpEffect +
                          fixEffHSB[2]*ses +
                          fixEffHSB[3]*minority +
                          fixEffHSB[4]*female)

## Plot fixed effects depending on group levels

plotHSB10 <- ggplot(dataHSB[dataHSB$minority == 0 & dataHSB$female == 0, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
            geom_line() +
            ggtitle("White Males") +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "none")

plotHSB11 <- ggplot(dataHSB[dataHSB$minority == 0 & dataHSB$female == 1, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("White Females") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

plotHSB12 <- ggplot(dataHSB[dataHSB$minority == 1 & dataHSB$female == 0, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("Minority Males") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

plotHSB13 <- ggplot(dataHSB[dataHSB$minority == 1 & dataHSB$female == 1, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("Minority Females") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

### Place individual plots on same page

grid.arrange(plotHSB10,
             plotHSB11,
             plotHSB12,
             plotHSB13)


# Example IV: Random Slope Model (Level-1 Covariates)

## Estimate a random slope model with level-1 variables

modelHSB4 <- lmer(mathach ~ ses + as.factor(minority) + as.factor(female) +
                          (1 + ses|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB4)

## Create data object featuring group-specific effects

fixEffHSB <- fixef(modelHSB4)

grpEffHSB <- ranef(modelHSB4)

grpEffHSB <- rownames_to_column(grpEffHSB[["idGrp"]]) %>%
             transmute(idGrp = as.numeric(rowname),
                       idGrpEffect = `(Intercept)`,
                       sesEffect = ses)

## Calculate the group means and slopes

dataHSB <- dataHSB %>%
           select(-predMathAch,
                  -idGrpEffect,
                  -predMathAchCheck) %>%
           mutate(predMathAch = predict(modelHSB4)) %>%
           inner_join(grpEffHSB,
                      by = "idGrp") %>%
           mutate(predMathAchCheck = fixEffHSB[1] +
                          idGrpEffect +
                          (fixEffHSB[2] +
                          sesEffect)*ses +
                          fixEffHSB[3]*minority +
                          fixEffHSB[4]*female)

## Plot fixed effects depending on group levels

plotHSB14 <- ggplot(dataHSB[dataHSB$minority == 0 & dataHSB$female == 0, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("White Males") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

plotHSB15 <- ggplot(dataHSB[dataHSB$minority == 0 & dataHSB$female == 1, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("White Females") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

plotHSB16 <- ggplot(dataHSB[dataHSB$minority == 1 & dataHSB$female == 0, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("Minority Males") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

plotHSB17 <- ggplot(dataHSB[dataHSB$minority == 1 & dataHSB$female == 1, ],
                    aes(ses,
                        predMathAch,
                        colour = factor(idGrp))) +
             geom_line() +
             ggtitle("Minority Females") +
             theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "none")

### Place individual plots on same page

grid.arrange(plotHSB14,
             plotHSB15,
             plotHSB16,
             plotHSB17)


# Example V: Random Slope Model (Level-1 and Level-2 Covariates)

## Estimate a random slope model with level-1 and level-2 variables

modelHSB5 <- lmer(mathach ~ ses*sizeScaled + minority + female +
                          sector + (1 + ses|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB5)
