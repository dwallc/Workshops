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

### Estimate treating female as continuous

ols01 <- lm(realrinc ~ age + female,
            data = MIVdata)

summary(ols01)

### Estimate treating female as a factor

MIVdata[["female"]] <- factor(MIVdata[["female"]],
                              labels = c("Male",
                                         "Female")) # Treat female as a factor variable

ols01Factor <- lm(realrinc ~ age + female,
                  data = MIVdata)

summary(ols01Factor)

## Estimate probit and logit regression models predicting
## whether a respondent is willing to vote for a female
## president based on their number of children and whether
## respondent graduated from high school.

### EStimate treating hsgrad as continuous

probit01 <- glm(fepres ~ children + hsgrad,
                family = binomial(link = "probit"),
                data = MIVdata)

summary(probit01)

logit01 <- glm(fepres ~ children + hsgrad,
               family = "binomial",
               data = MIVdata)

summary(logit01)

### EStimate treating hsgrad as a factor

MIVdata[["hsgrad"]] <- factor(MIVdata[["hsgrad"]],
                              labels = c("No",
                                         "Yes"))

probit01Factor <- glm(fepres ~ children + hsgrad,
                      family = binomial(link = "probit"),
                      data = MIVdata)

summary(probit01Factor)

logit01Factor <- glm(fepres ~ children + hsgrad,
                     family = "binomial",
                     data = MIVdata)

summary(logit01Factor)


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
windows() # quartz() if using Mac
dwplot(ols01,
       show_intercept = TRUE) %>%
        relabel_predictors(c("(Intercept)" = "Intercept",
                             age = "Age",
                             female = "Gender")) +
        geom_vline(xintercept = 0,
                   colour = "black") +
        xlab("Coefficient Estimate") +
        ggtitle("OLS Model Results") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

ggsave("./Graphs/MIVcoefplot01.png")

dwplot(ols01,
       show_intercept = FALSE) %>%
        relabel_predictors(c(age = "Age",
                             female = "Gender")) +
        geom_vline(xintercept = 0,
                   colour = "black") +
        xlab("Coefficient Estimate") +
        ggtitle("OLS Model Results") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5))

ggsave("./Graphs/MIVcoefplot01b.png")

## Binary Regression (BRM) Models

### Probit and Logit Results on Same Plot

dwplot(list(probit01,
            logit01),
       show_intercept = TRUE) %>%
        relabel_predictors(c("(Intercept)" = "Intercept",
                             children = "Number of Children",
                             hsgrad = "High School Grad")) +
        geom_vline(xintercept = 0,
                   colour = "black") +
        xlab("Coefficient Estimate") +
        ggtitle("BRM Model Results") +
        theme_bw() +
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
                             hsgrad = "High School Grad")) +
        facet_grid(. ~ model) +
        geom_vline(xintercept = 0,
                   colour = "black") +
        xlab("Coefficient Estimate") +
        ggtitle("BRM Model Results") +
        theme_bw() +
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
            data = ols01[["model"]],
            what = "prediction",
            draw = FALSE)) # Default is to set female = mean(ols01[["model"]]$female)

print(cplot(ols01,
            x = "age",
            xvals = c(mean(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == 0,],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(mean(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == 1,],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(min(ols01[["model"]]$age),
                      mean(ols01[["model"]]$age),
                      max(ols01[["model"]]$age)),
            data = ols01[["model"]],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(min(ols01[["model"]]$age),
                      mean(ols01[["model"]]$age),
                      max(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == 0,],
            what = "prediction",
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(min(ols01[["model"]]$age),
                      mean(ols01[["model"]]$age),
                      max(ols01[["model"]]$age)),
            data = ols01[["model"]][ols01[["model"]]$female == 1,],
            what = "prediction",
            draw = FALSE))

### Plotting predicted values for age

#### Separate Plots

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]],
      xlab = "Age",
      ylab = "Fitted Values",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == 0,],
      xlab = "Age",
      ylab = "Fitted Values",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == 1,],
      xlab = "Age",
      ylab = "Fitted Values",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

#### Single Plot

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]],
      col = "black",
      xlab = "Age",
      ylab = "Fitted Values",
      ylim = c(0,
               50000),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == 0,],
      col = "blue",
      rug = FALSE,
      draw = "add")

cplot(ols01,
      x = "age",
      what = "prediction",
      data = ols01[["model"]][ols01[["model"]]$female == 1,],
      col = "red",
      rug = FALSE,
      draw = "add")

### Printing predicted values for specific values of female

print(cplot(ols01,
            x = "female",
            xvals = c(min(ols01[["model"]]$female),
                      mean(ols01[["model"]]$female),
                      max(ols01[["model"]]$female)),
            what = "prediction",
            data = ols01[["model"]],
            draw = FALSE)) # Default is to set age = mean(ols01[["model"]]$age)

print(cplot(ols01,
            x = "female",
            xvals = c(min(ols01[["model"]]$female),
                      mean(ols01[["model"]]$female),
                      max(ols01[["model"]]$female)),
            what = "prediction",
            data = ols01[["model"]][ols01[["model"]]$age == min(ols01[["model"]]$age),],
            draw = FALSE))

print(cplot(ols01,
            x = "female",
            xvals = c(min(ols01[["model"]]$female),
                      mean(ols01[["model"]]$female),
                      max(ols01[["model"]]$female)),
            what = "prediction",
            data = ols01[["model"]][ols01[["model"]]$age == median(ols01[["model"]]$age),],
            draw = FALSE))

print(cplot(ols01,
            x = "female",
            xvals = c(min(ols01[["model"]]$female),
                      mean(ols01[["model"]]$female),
                      max(ols01[["model"]]$female)),
            what = "prediction",
            data = ols01[["model"]][ols01[["model"]]$age == max(ols01[["model"]]$age),],
            draw = FALSE))

### Plotting predicted values for specific values of female

cplot(ols01Factor,
      x = "female",
      what = "prediction",
      data = ols01Factor[["model"]][ols01Factor[["model"]]$age == min(ols01Factor[["model"]]$age),],
      col = "blue",
      xlab = "Gender",
      ylab = "Fitted Values",
      ylim = c(0,
               50000),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(ols01Factor,
      x = "female",
      what = "prediction",
      data = ols01Factor[["model"]][ols01Factor[["model"]]$age == median(ols01Factor[["model"]]$age),],
      col = "black",
      rug = FALSE,
      draw = "add")

cplot(ols01Factor,
      x = "female",
      what = "prediction",
      data = ols01Factor[["model"]][ols01Factor[["model"]]$age == max(ols01Factor[["model"]]$age),],
      col = "red",
      rug = FALSE,
      draw = "add")

## Part B - BRM Models

### Printing predicted values for specific values of children

print(cplot(logit01,
            x = "children",
            xvals = c(mean(logit01[["model"]]$children)),
            data = logit01[["model"]],
            what = "prediction",
            draw = FALSE)) # Default is to set hsgrad = mean(logit01[["model"]]$hsgrad)

print(cplot(logit01,
            x = "children",
            xvals = c(mean(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == 0,],
            what = "prediction",
            draw = FALSE))

print(cplot(logit01,
            x = "children",
            xvals = c(mean(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == 1,],
            what = "prediction",
            draw = FALSE))

print(cplot(logit01,
            x = "children",
            xvals = c(min(logit01[["model"]]$children),
                      mean(logit01[["model"]]$children),
                      max(logit01[["model"]]$children)),
            data = logit01[["model"]],
            what = "prediction",
            draw = FALSE))

print(cplot(logit01,
            x = "children",
            xvals = c(min(logit01[["model"]]$children),
                      mean(logit01[["model"]]$children),
                      max(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == 0,],
            what = "prediction",
            draw = FALSE))

print(cplot(logit01,
            x = "children",
            xvals = c(min(logit01[["model"]]$children),
                      mean(logit01[["model"]]$children),
                      max(logit01[["model"]]$children)),
            data = logit01[["model"]][logit01[["model"]]$hsgrad == 1,],
            what = "prediction",
            draw = FALSE))

### Plotting predicted values for children

#### Separate Plots

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]],
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == 0,],
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == 1,],
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

#### Single Plot

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]],
      col = "black",
      xlab = "Number of Children",
      ylab = "Predicted Probability",
      ylim = c(0.6,
               1),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == 0,],
      col = "blue",
      rug = FALSE,
      draw = "add")

cplot(logit01,
      x = "children",
      what = "prediction",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == 1,],
      col = "red",
      rug = FALSE,
      draw = "add")

### Printing predicted values for specific values of hsgrad

print(cplot(logit01,
            x = "hsgrad",
            xvals = c(min(logit01[["model"]]$hsgrad),
                      mean(logit01[["model"]]$hsgrad),
                      max(logit01[["model"]]$hsgrad)),
            what = "prediction",
            data = logit01[["model"]],
            draw = FALSE)) # Default is to set children = mean(logit01[["model"]]$children)

print(cplot(logit01,
            x = "hsgrad",
            what = "prediction",
            xvals = c(min(logit01[["model"]]$hsgrad),
                      mean(logit01[["model"]]$hsgrad),
                      max(logit01[["model"]]$hsgrad)),
            data = logit01[["model"]][logit01[["model"]]$children == min(logit01[["model"]]$children),],
            draw = FALSE))

print(cplot(logit01,
            x = "hsgrad",
            xvals = c(min(logit01[["model"]]$hsgrad),
                      mean(logit01[["model"]]$hsgrad),
                      max(logit01[["model"]]$hsgrad)),
            what = "prediction",
            data = logit01[["model"]][logit01[["model"]]$children == median(logit01[["model"]]$children),],
            draw = FALSE))

print(cplot(logit01,
            x = "hsgrad",
            xvals = c(min(logit01[["model"]]$hsgrad),
                      mean(logit01[["model"]]$hsgrad),
                      max(logit01[["model"]]$hsgrad)),
            what = "prediction",
            data = logit01[["model"]][logit01[["model"]]$children == max(logit01[["model"]]$children),],
            draw = FALSE))

### Plotting predicted values for specific values of female

cplot(logit01Factor,
      x = "hsgrad",
      what = "prediction",
      data = logit01Factor[["model"]][logit01Factor[["model"]]$children == min(logit01Factor[["model"]]$children),],
      col = "blue",
      xlab = "High School Grad.",
      ylab = "Predicted Probability",
      ylim = c(0.6,
               1),
      main = "Adjusted Predictions with 95% CIs",
      rug = FALSE)

cplot(logit01Factor,
      x = "hsgrad",
      what = "prediction",
      data = logit01Factor[["model"]][logit01Factor[["model"]]$children == median(logit01Factor[["model"]]$children),],
      col = "black",
      rug = FALSE,
      draw = "add")

cplot(logit01Factor,
      x = "hsgrad",
      what = "prediction",
      data = logit01Factor[["model"]][logit01Factor[["model"]]$children == max(logit01Factor[["model"]]$children),],
      col = "red",
      rug = FALSE,
      draw = "add") # Notice that the probabilities are plotted in the wrong location!


# Part IV - Marginal Effects

## Part A - OLS Model

### Marginal Change

#### Printing marginal changes for specific values of age

print(cplot(ols01,
            x = "age",
            xvals = c(mean(ols01[["model"]]$age)),
            what = "effect",
            data = ols01[["model"]],
            draw = FALSE))

print(cplot(ols01,
            x = "age",
            xvals = c(min(ols01[["model"]]$age),
                      mean(ols01[["model"]]$age),
                      max(ols01[["model"]]$age)),
            what = "effect",
            data = ols01[["model"]],
            draw = FALSE))

#### Printing marginal changes for specific values of female

print(cplot(ols01,
            x = "female",
            xvals = c(0,
                      1),
            what = "effect",
            data = ols01[["model"]],
            draw = FALSE)) # Default is to set age = mean(ols01[["model"]]$age)

print(cplot(ols01,
            x = "female",
            xvals = c(0,
                      1),
            what = "effect",
            data = ols01[["model"]][ols01[["model"]]$age == min(ols01[["model"]]$age),],
            draw = FALSE))

print(cplot(ols01,
            x = "female",
            xvals = c(0,
                      1),
            what = "effect",
            data = ols01[["model"]][ols01[["model"]]$age == median(ols01[["model"]]$age),],
            draw = FALSE))

print(cplot(ols01,
            x = "female",
            xvals = c(0,
                      1),
            what = "effect",
            data = ols01[["model"]][ols01[["model"]]$age == max(ols01[["model"]]$age),],
            draw = FALSE))

#### Plotting marginal changes for age

cplot(ols01,
      x = "age",
      what = "effect",
      data = ols01[["model"]],
      xlab = "Age",
      ylab = "Effects on Linear Prediction",
      main = "Conditional Marginal Effects with 95% CIs",
      rug = FALSE)

#### Plotting marginal changes for female

cplot(ols01,
      x = "female",
      xvals = c(0,
                1),
      what = "effect",
      data = ols01[["model"]],
      xlab = "Gender",
      ylab = "Effects on Linear Prediction",
      main = "Conditional Marginal Effects with 95% CIs",
      rug = FALSE)

### Discrete Change

#### Numerical approximation of the derivative

summary(margins(ols01Factor,
                change = "dydx"))

plot(margins(ols01Factor,
             change = "dydx"),
     main = "OLS Average Marginal Effects (AMEs) - Numerical Derivative (dydx)",
     ylab = "AME")

summary(margins(ols01Factor,
                change = "dydx",
                at = list(age = c(25,
                                  75))))

summary(margins(ols01Factor,
                change = "dydx",
                at = list(female = c("Male",
                                     "Female"))))

summary(margins(ols01Factor,
                change = "dydx",
                at = list(age = c(25,
                                  75),
                          female = c("Male",
                                     "Female"))))

#### Moving from min(x) to max(x)

summary(margins(ols01Factor,
                change = "minmax"))

plot(margins(ols01Factor,
             change = "minmax"),
     main = "OLS Average Marginal Effects (AMEs) - Minimum to Maximum (MinMax)",
     ylab = "AME")

#### Moving from the 1st quartile to 3rd quartile of x

summary(margins(ols01Factor,
                change = "iqr"))

plot(margins(ols01Factor,
             change = "iqr"),
     main = "OLS Average Marginal Effects (AMEs) - Inter-quartile Range (IQR)",
     ylab = "AME")

#### Moving from mean(x) - sd(x) to mean(x) + sd(x)

summary(margins(ols01Factor,
                change = "sd"))

plot(margins(ols01Factor,
             change = "sd"),
     main = "OLS Average Marginal Effects (AMEs) - 1 Standard Deviation (SD)",
     ylab = "AME")

#### Moving from specified values

##### Going from age 30 to age 31

summary(margins(ols01Factor,
                change = c(30,
                           31)))

plot(margins(ols01Factor,
             change = c(30,
                        31)),
     main = "OLS Average Marginal Effects (AMEs) - Specific Change (Age 30 to Age 31)",
     ylab = "AME")

summary(margins(ols01Factor,
                change = c(30,
                           31),
                at = list(age = c(25,
                                  75)))) # Ignoring specified at values for age

summary(margins(ols01Factor,
                change = c(30,
                           31),
                at = list(female = c("Male",
                                     "Female"))))

summary(margins(ols01Factor,
                change = c(30,
                           31),
                at = list(age = c(25,
                                  75),
                          female = c("Male",
                                     "Female"))))

##### Going from age 50 to age 51

summary(margins(ols01Factor,
                change = c(50,
                           51)))

plot(margins(ols01Factor,
             change = c(50,
                        51)),
     main = "OLS Average Marginal Effects (AMEs) - Specific Change (Age 50 to Age 51)",
     ylab = "AME")

summary(margins(ols01Factor,
                change = c(50,
                           51),
                at = list(age = c(25,
                                  75)))) # Ignoring specified at values for age

summary(margins(ols01Factor,
                change = c(50,
                           51),
                at = list(female = c("Male",
                                     "Female"))))

summary(margins(ols01Factor,
                change = c(50,
                           51),
                at = list(age = c(25,
                                  75),
                          female = c("Male",
                                     "Female"))))

## Part B - BRM Model

### Average Conditional Marginal Change

#### Printing and plotting marginal changes for specific values of children

print(cplot(logit01,
            x = "children",
            xvals = c(unique(logit01[["model"]]$children)),
            what = "effect",
            data = logit01[["model"]],
            draw = FALSE))

cplot(logit01,
      x = "children",
      xvals = c(unique(logit01[["model"]]$children)),
      what = "effect",
      data = logit01[["model"]],
      xlab = "Number of Children",
      ylab = "Effects on Pr(Vote for Female President)",
      main = "Average Conditional Marginal Effects with 95% CIs",
      rug = FALSE)

#### Printing and plotting marginal changes for specific values of hsgrad

print(cplot(logit01,
            x = "hsgrad",
            xvals = c(unique(logit01[["model"]]$hsgrad)),
            what = "effect",
            data = logit01[["model"]],
            draw = FALSE))

cplot(logit01,
      x = "hsgrad",
      xvals = c(unique(logit01[["model"]]$hsgrad)),
      what = "effect",
      data = logit01[["model"]],
      xlab = "High School Grad.",
      ylab = "Effects on Pr(Vote for Female President)",
      main = "Average Conditional Marginal Effects with 95% CIs",
      rug = FALSE)

### Conditional Marginal Change

#### Printing and plotting marginal changes for specific values of children

print(cplot(logit01,
            x = "children",
            xvals = c(unique(logit01[["model"]]$children)),
            what = "effect",
            data = logit01[["model"]][logit01[["model"]]$hsgrad == 0,],
            draw = FALSE))

cplot(logit01,
      x = "children",
      xvals = c(unique(logit01[["model"]]$children)),
      what = "effect",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == 0,],
      col = "blue",
      xlab = "Number of Children",
      ylab = "Effects on Pr(Vote for Female President)",
      ylim = c(-0.03,
               0),
      main = "Conditional Marginal Effects with 95% CIs",
      rug = FALSE)

print(cplot(logit01,
            x = "children",
            xvals = c(unique(logit01[["model"]]$children)),
            what = "effect",
            data = logit01[["model"]][logit01[["model"]]$hsgrad == 1,],
            draw = FALSE))

cplot(logit01,
      x = "children",
      xvals = c(unique(logit01[["model"]]$children)),
      what = "effect",
      data = logit01[["model"]][logit01[["model"]]$hsgrad == 1,],
      col = "red",
      rug = FALSE,
      draw = "add")

#### Printing and plotting marginal changes for specific values of hsgrad

for (child in sort(unique(logit01[["model"]]$children))) {
        print(cplot(logit01,
                    x = "hsgrad",
                    xvals = c(unique(logit01[["model"]]$hsgrad)),
                    what = "effect",
                    data = logit01[["model"]][logit01[["model"]]$children == child,],
                    draw = FALSE))
}

cplot(logit01,
      x = "hsgrad",
      xvals = c(unique(logit01[["model"]]$hsgrad)),
      what = "effect",
      data = logit01[["model"]][logit01[["model"]]$children == 0,],
      col = "blue",
      xlab = "High School Grad.",
      ylab = "Effects on Pr(Vote for Female President)",
      ylim = c(0.05,
               0.25),
      main = "Conditional Marginal Effects with 95% CIs",
      rug = FALSE)

for (child in sort(unique(logit01[["model"]]$children))[2:9]) {
        cplot(logit01,
              x = "hsgrad",
              xvals = c(unique(logit01[["model"]]$hsgrad)),
              what = "effect",
              data = logit01[["model"]][logit01[["model"]]$children == child,],
              col = "blue",
              rug = FALSE,
              draw = "add")
}

### Average Discrete Change

#### Numerical approximation of the derivative

summary(margins(logit01Factor,
                change = "dydx"))

plot(margins(logit01Factor,
             change = "dydx"),
     main = "Logit Average Marginal Effects (AMEs) - Numerical Derivative (dydx)",
     ylab = "AME")

#### Moving from min(x) to max(x)

summary(margins(logit01Factor,
                change = "minmax"))

plot(margins(logit01Factor,
             change = "minmax"),
     main = "Logit Average Marginal Effects (AMEs) - Minimum to Maximum (MinMax)",
     ylab = "AME")

#### Moving from the 1st quartile to 3rd quartile of x

summary(margins(logit01Factor,
                change = "iqr"))

plot(margins(logit01Factor,
             change = "iqr"),
     main = "Logit Average Marginal Effects (AMEs) - Inter-quartile Range (IQR)",
     ylab = "AME")

#### Moving from mean(x) - sd(x) to mean(x) + sd(x)

summary(margins(logit01Factor,
                change = "sd"))

plot(margins(logit01Factor,
             change = "sd"),
     main = "Logit Average Marginal Effects (AMEs) - 1 Standard Deviation (SD)",
     ylab = "AME")

#### Moving from specified values

##### Going from 2 children to 3 children

summary(margins(logit01Factor,
                change = c(2,
                           3)))

plot(margins(logit01Factor,
             change = c(2,
                        3)),
     main = "Logit Average Marginal Effects (AMEs) - Specific Change (Age 30 to Age 31)",
     ylab = "AME")

##### Going from 6 children to 7 children

summary(margins(logit01Factor,
                change = c(6,
                           7)))

plot(margins(logit01Factor,
             change = c(6,
                        7)),
     main = "Logit Average Marginal Effects (AMEs) - Specific Change (Age 50 to Age 51)",
     ylab = "AME")

### Average Conditional Discrete Change

#### Numerical approximation of the derivative

summary(margins(logit01Factor,
                change = "dydx",
                at = list(children = sort(unique(logit01Factor[["model"]]$children)))))

#### Moving from specified values

##### Going from 2 children to 3 children

summary(margins(logit01Factor,
                change = c(2,
                           3),
                at = list(children = sort(unique(logit01Factor[["model"]]$children)))))

##### Going from 6 children to 7 children

summary(margins(logit01Factor,
                change = c(6,
                           7),
                at = list(children = sort(unique(logit01Factor[["model"]]$children)))))

### Conditional Discrete Change

#### Numerical approximation of the derivative

summary(margins(logit01Factor,
                change = "dydx",
                at = list(children = sort(unique(logit01Factor[["model"]]$children)),
                          hsgrad = sort(unique(logit01Factor[["model"]]$hsgrad)))))

#### Moving from specified values

##### Going from 2 children to 3 children

summary(margins(logit01Factor,
                change = c(2,
                           3),
                at = list(children = sort(unique(logit01Factor[["model"]]$children)),
                          hsgrad = sort(unique(logit01Factor[["model"]]$hsgrad)))))

##### Going from 6 children to 7 children

summary(margins(logit01Factor,
                change = c(6,
                           7),
                at = list(children = sort(unique(logit01Factor[["model"]]$children)),
                          hsgrad = sort(unique(logit01Factor[["model"]]$hsgrad)))))
