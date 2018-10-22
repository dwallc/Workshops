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
#                       Graphs\MIVcoefplot02.png,                                       #
#                       Graphs\MIVcoefplot02b.png                                       #
#########################################################################################


# Load Required Packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("rio",
              "dplyr",
              "broom",
              "texreg",
              "dotwhisker",
              "margins")

## rio - A Swiss-Army Knife for Data I/O
## dplyr - A Grammar of Data Manipulation
## broom - Convert Statistical Analysis Objects into Tidy Data Frames
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

MIVdata[["female"]] <- factor(MIVdata[["female"]],
                              labels = c("Male",
                                         "Female")) # Treat female as a factor variable

ols01 <- lm(realrinc ~ age + female,
            data = MIVdata)

summary(ols01)

## Estimate probit and logit regression models predicting
## whether a respondent is willing to vote for a female
## president based on their number of children and whether
## respondent graduated from high school.

MIVdata[["hsgrad"]] <- factor(MIVdata[["hsgrad"]],
                              labels = c("No",
                                         "Yes"))

probit01 <- glm(fepres ~ children + hsgrad,
                family = binomial(link = "probit"),
                data = MIVdata)

summary(probit01)

logit01 <- glm(fepres ~ children + hsgrad,
               family = "binomial",
               data = MIVdata)

summary(logit01)


# Part I - Regression Tables

## Create a folder called "Tables" if it does not exist

if (file.exists("./Tables")) {
        print("Sub-directory ./Tables already exists!")
} else {
        dir.create("./Tables")
        print("Sub-directory ./Tables created!")
}

screenreg(list(ols01,
               probit01,
               logit01),
          custom.model.names = c("OLS",
                                 "Probit",
                                 "Logit"),
          custom.coef.names = c("Constant",
                                "Age",
                                "Gender",
                                "Number of Children",
                                "High School Grad."),
          reorder.coef = c(2,
                           3,
                           4,
                           5,
                           1),
          stars = 0.05,
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          include.rmse = FALSE,
          include.loglik = FALSE,
          include.deviance = FALSE,
          custom.gof.names = c("N",
                               NA,
                               NA))

screenreg(list(ols01,
               probit01,
               logit01),
          file = "./Tables/MIVmodel01.txt",
          custom.model.names = c("OLS",
                                 "Probit",
                                 "Logit"),
          custom.coef.names = c("Constant",
                                "Age",
                                "Gender",
                                "Number of Children",
                                "High School Grad."),
          reorder.coef = c(2,
                           3,
                           4,
                           5,
                           1),
          stars = 0.05,
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          include.rmse = FALSE,
          include.loglik = FALSE,
          include.deviance = FALSE,
          custom.gof.names = c("N",
                               NA,
                               NA))

texreg(list(ols01,
            probit01,
            logit01),
       file = "./Tables/MIVmodel01.tex",
       custom.model.names = c("OLS",
                              "Probit",
                              "Logit"),
       custom.coef.names = c("Constant",
                             "Age",
                             "Gender",
                             "Number of Children",
                             "High School Grad."),
       reorder.coef = c(2,
                        3,
                        4,
                        5,
                        1),
       stars = 0.05,
       include.rsquared = FALSE,
       include.adjrs = FALSE,
       include.rmse = FALSE,
       include.loglik = FALSE,
       include.deviance = FALSE,
       custom.gof.names = c("N",
                            NA,
                            NA),
       caption = NULL,
       booktabs = TRUE,
       dcolumn = TRUE)

### For more information on texreg: https://cran.r-project.org/web/packages/texreg/


# Part II - Coefficient Plots

## Create a folder called "Graphs" if it does not exist

if (file.exists("./Graphs")) {
        print("Sub-directory ./Graphs already exists!")
} else {
        dir.create("./Graphs")
        print("Sub-directory ./Graphs created!")
}

## OLS Models

dwplot(ols01,
       show_intercept = TRUE) %>%
        relabel_predictors(c("(Intercept)" = "Intercept",
                             age = "Age",
                             female1 = "Gender")) +
        geom_vline(xintercept = 0,
                   colour = "red") +
        xlab("Coefficient Estimate") +
        ggtitle("OLS Model Results") +
        theme(plot.title = element_text(hjust = 0.5))

ggsave("./Graphs/MIVcoefplot01.png")

dwplot(ols01,
       show_intercept = FALSE) %>%
        relabel_predictors(c(age = "Age",
                             female1 = "Gender")) +
        geom_vline(xintercept = 0,
                   colour = "red") +
        xlab("Coefficient Estimate") +
        ggtitle("OLS Model Results") +
        theme(plot.title = element_text(hjust = 0.5))

ggsave("./Graphs/MIVcoefplot01b.png")

## Binary Regression (BRM) Models

### Probit and Logit Results on Same Plot

dwplot(list(probit01,
            logit01),
       show_intercept = TRUE) %>%
        relabel_predictors(c("(Intercept)" = "Intercept",
                             children = "Number of Children",
                             hsgrad1 = "High School Grad")) +
        geom_vline(xintercept = 0,
                   colour = "red") +
        xlab("Coefficient Estimate") +
        ggtitle("BRM Model Results") +
        scale_color_discrete(labels = c("Probit",
                                        "Logit")) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "right",
              legend.justification = c(0,
                                       0),
              legend.background = element_rect(colour = "grey80"),
              legend.title = element_blank())

ggsave("./Graphs/MIVcoefplot02.png")

### Probit and Logit Results Side-by-Side

brm01 <- bind_rows(tidy(probit01) %>%
                           mutate(model = "Probit"),
                   tidy(logit01) %>%
                           mutate(model = "Logit"))

dwplot(brm01,
       show_intercept = TRUE) %>%
        relabel_predictors(c("(Intercept)" = "Intercept",
                             children = "Number of Children",
                             hsgrad1 = "High School Grad")) +
        facet_grid(. ~ model) +
        geom_vline(xintercept = 0,
                   colour = "red") +
        xlab("Coefficient Estimate") +
        ggtitle("BRM Model Results") +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")

ggsave("./Graphs/MIVcoefplot02b.png")

### For more information on dotwhisker: https://cran.r-project.org/web/packages/dotwhisker/


# Part III - Predicted (Fitted) Values

## Part A - OLS Model

### Printing predicted values for specific values of age

print(cplot(ols01,
            x = "age",
            xvals = c(mean(ols01[["model"]]$age)),
            what = "prediction",
            draw = FALSE)) # Default is to set female = "No

print(cplot(ols01,
            x = "age",
            xvals = c(mean(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == "No",],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(mean(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == "Yes",],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(min(ols01[["model"]]$age),
                      mean(ols01[["model"]]$age),
                      max(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == "No",],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(min(ols01[["model"]]$age),
                      mean(ols01[["model"]]$age),
                      max(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == "Yes",],
            what = "prediction",
            draw = FALSE))

### Plotting predicted values for age

#### Separate Plots

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == "No",],
      xlab = "Age",
      ylab = "Fitted Values",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == "Yes",],
      xlab = "Age",
      ylab = "Fitted Values",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

#### Single Plot

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == "No",],
      col = "blue",
      xlab = "Age",
      ylab = "Fitted Values",
      ylim = c(0,
               50000),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == "Yes",],
      col = "red",
      rug = FALSE,
      draw = "add")

### Printing predicted values for specific values of female

print(cplot(ols01,
            x = "female",
            what = "prediction",
            draw = FALSE)) # Default is to set age = mean(ols01$model$age)

print(cplot(ols01,
            x = "female",
            what = "prediction",
            data = ols01[["model"]][ols01[["model"]]$age == min(ols01[["model"]]$age),],
            draw = FALSE))

print(cplot(ols01,
            x = "female",
            what = "prediction",
            data = ols01[["model"]][ols01[["model"]]$age == median(ols01[["model"]]$age),],
            draw = FALSE))

print(cplot(ols01,
            x = "female",
            what = "prediction",
            data = ols01[["model"]][ols01[["model"]]$age == max(ols01[["model"]]$age),],
            draw = FALSE))

### Plotting predicted values for specific values of female

cplot(ols01,
      x = "female",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$age == min(ols01[["model"]]$age),],
      col = "blue",
      xlab = "Gender",
      ylab = "Fitted Values",
      ylim = c(0,
               50000),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01,
      x = "female",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$age == median(ols01[["model"]]$age),],
      col = "black",
      rug = FALSE,
      draw = "add")

cplot(ols01,
      x = "female",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$age == max(ols01[["model"]]$age),],
      col = "red",
      rug = FALSE,
      draw = "add")

## Part B - BRM Models

### Printing predicted values for specific values of children

print(cplot(logit01,
            x = "children",
            xvals = c(mean(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == "No",],
            what = "prediction",
            draw = FALSE)) # Default is to set hsgrad = "Yes"

print(cplot(logit01,
            x = "children",
            xvals = c(mean(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == "Yes",],
            what = "prediction",
            draw = FALSE))

print(cplot(logit01,
            x = "children",
            xvals = c(min(logit01[["model"]]$children),
                      mean(logit01[["model"]]$children),
                      max(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == "No",],
            what = "prediction",
            draw = FALSE))

print(cplot(logit01,
            x = "children",
            xvals = c(min(logit01[["model"]]$children),
                      mean(logit01[["model"]]$children),
                      max(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == "Yes",],
            what = "prediction",
            draw = FALSE))

### Plotting predicted values for children

#### Separate Plots

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == "No",],
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == "Yes",],
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

#### Single Plot

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == "No",],
      col = "blue",
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      ylim = c(0.6,
               1),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == "Yes",],
      col = "red",
      rug = FALSE,
      draw = "add")

### Printing predicted values for specific values of hsgrad

print(cplot(logit01,
            x = "hsgrad",
            what = "prediction",
            draw = FALSE)) # Default is to set age = mean(ols01$model$age)

print(cplot(logit01,
            x = "hsgrad",
            what = "prediction",
            data = logit01[["model"]][logit01[["model"]]$children == min(logit01[["model"]]$children),],
            draw = FALSE))

print(cplot(logit01,
            x = "hsgrad",
            what = "prediction",
            data = logit01[["model"]][logit01[["model"]]$children == median(logit01[["model"]]$children),],
            draw = FALSE))

print(cplot(logit01,
            x = "hsgrad",
            what = "prediction",
            data = logit01[["model"]][logit01[["model"]]$children == max(logit01[["model"]]$children),],
            draw = FALSE))

### Plotting predicted values for specific values of female

cplot(logit01,
      x = "hsgrad",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$children == min(logit01[["model"]]$children),],
      col = "blue",
      xlab = "High School Grad.",
      ylab = "Predicted Probability",
      ylim = c(0.6,
               1),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01,
      x = "hsgrad",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$children == median(logit01[["model"]]$children),],
      col = "black",
      rug = FALSE,
      draw = "add")

cplot(logit01,
      x = "hsgrad",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$children == max(logit01[["model"]]$children),],
      col = "red",
      rug = FALSE,
      draw = "add") # Notice that the probabilities are plotted in the wrong location!


# Part IV - Marginal Effects

## Part A - OLS Model

### Marginal Change

### Discrete Change



















## Part B - BRM Models
















