#################################################################################
#       Name: Multilevel Modeling using Stata II.R                              #
#       Date: April 13, 2018                                                    #
#       Author:	Desmond D. Wallace                                              #
#       Purpose:	Estimating basic random intercept and random slope      #
#                               multilevel models via the 'lme4' package.       #
#       Input Files:	Data\HSB All.xlsx                                       #
#################################################################################

# Set working directory

setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/Multilevel Modeling II")


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

## Math Achievement

### Import the HSB All.xlsx dataset

dataHSB <- import("./Data/HSB All R.xlsx")

### Visually explore the 'mathach' variable

#### Plot #1 - idGrp = 2305

plotHSB1 <- ggplot(data = dataHSB[dataHSB$idGrp == 2305, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB1

#### Plot #2 - idGrp = 4523

plotHSB2 <- ggplot(data = dataHSB[dataHSB$idGrp == 4523, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB2

#### Plot #3 - idGrp = 6816

plotHSB3 <- ggplot(data = dataHSB[dataHSB$idGrp == 6816, ],
                   aes(x = idInd,
                       y = mathach)) +
            geom_point()

plotHSB3

### Random Intercept Model

#### Estimate a random intercept model with level-1 variables

modelHSB1 <- lmer(mathach ~ ses + as.factor(minority) + as.factor(female) +
                          (1|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB1)

icc(modelHSB1)

#### Create data object featuring group-specific effects

grpEffHSB <- ranef(modelHSB1)

grpEffHSB <- rownames_to_column(grpEffHSB[["idGrp"]],
                                var = "idGrp") %>%
             transmute(idGrp = as.numeric(idGrp),
                       idGrpEffect = `(Intercept)`)

#### Calculate and plot the group means

dataHSB <- dataHSB %>%
           mutate(GrandMean = predict(modelHSB1,
                                      re.form = NA),
                  predMathAch = predict(modelHSB1),
                  idGrpEffect2 = predMathAch - GrandMean) %>%
           inner_join(grpEffHSB,
                      by = "idGrp")

#### Plot fixed effects depending on group levels

sjp.lmer(modelHSB1,
         type = "ri.slope",
         vars = "ses",
         emph.grp = c(22,
                      74,
                      108),
         facet.grid = FALSE,
         show.legend = FALSE)

### Random Slope Models

#### Estimate a random slope model with level-1 variables

modelHSB2 <- lmer(mathach ~ ses + as.factor(minority) + as.factor(female) +
                          (ses|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB2)

#### Create data object featuring group-specific effects

grpEffHSB2 <- ranef(modelHSB2)

grpEffHSB2 <- rownames_to_column(grpEffHSB2[["idGrp"]]) %>%
              transmute(idGrp = as.numeric(rowname),
                        idGrpEffect = `(Intercept)`,
                        sesEffect = ses)

#### Calculate and plot the group means

dataHSB <- dataHSB %>%
           select(-GrandMean,
                  -predMathAch,
                  -idGrpEffect2,
                  -idGrpEffect) %>%
           mutate(GrandMean = predict(modelHSB2,
                                      re.form = NA),
                  predMathAch = predict(modelHSB2)) %>%
           inner_join(grpEffHSB2,
                      by = "idGrp")

#### Plot fixed effects depending on group levels

sjp.lmer(modelHSB2,
         type = "rs.ri",
         vars = "ses",
         sample.n = c(22,
                      74,
                      108),
         facet.grid = FALSE,
         p.kr = FALSE,
         show.legend = TRUE)

sjp.lmer(modelHSB2,
         type = "rs.ri",
         vars = "ses",
         sample.n = 10,
         facet.grid = FALSE,
         p.kr = FALSE,
         show.legend = TRUE)

### Random Slope Models II

#### Estimate a random slope model with level-1 and level-2 variables

modelHSB3 <- lmer(mathach ~ ses*size + minority + female +
                          sector + (ses|idGrp),
                  data = dataHSB,
                  REML = FALSE)

summary(modelHSB3)

#### Create data object featuring group-specific effects

grpEffHSB3 <- ranef(modelHSB3)

grpEffHSB3 <- rownames_to_column(grpEffHSB3[["idGrp"]]) %>%
              transmute(idGrp = as.numeric(rowname),
                        idGrpEffect = `(Intercept)`,
                        sesEffect = ses)

#### Calculate and plot the group means

dataHSB <- dataHSB %>%
           select(-GrandMean,
                  -predMathAch,
                  -idGrpEffect) %>%
           mutate(GrandMean = predict(modelHSB3,
                                      re.form = NA),
                  predMathAch = predict(modelHSB2)) %>%
           inner_join(grpEffHSB3,
                      by = "idGrp")

#### Plot fixed effects depending on group levels

##### Interaction Plots

sjp.int(modelHSB3,
        type = "eff",
        int.term = "ses*size",
        mdrt.values = "meansd",
        facet.grid = FALSE,
        show.ci = TRUE,
        show.legend = TRUE)

sjp.int(modelHSB3,
        type = "eff",
        int.term = "ses*size",
        swap.pred = TRUE,
        mdrt.values = "meansd",
        facet.grid = FALSE,
        show.ci = TRUE,
        show.legend = TRUE)

sjp.lmer(modelHSB3,
         type = "rs.ri",
         vars = "ses",
         sample.n = c(22,
                      74,
                      108),
         facet.grid = FALSE,
         p.kr = FALSE,
         show.legend = TRUE)

sjp.lmer(modelHSB3,
         type = "rs.ri",
         vars = "ses",
         sample.n = 10,
         facet.grid = FALSE,
         p.kr = FALSE,
         show.legend = TRUE)
