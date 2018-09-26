#################################################################
#       Name:   Basic Data Visualization Using R (lattice).R    #
#       Date:   September 26, 2018                              #
#       Author: Desmond D. Wallace                              #
#       Purpose:        Create basic plots via lattice plotting #
#                       system.                                 #
#################################################################


# Install and load needed packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("datasets",
              "lattice")

## datasets - Base R datasets
## lattice - Lattice Graphics

ipak(packages)


# Set Working Directory

## setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Basic Data Visualization Using R")


# Load airquality dataset

## airquality - Daily air quality measurements in New York, May to September 1973.

exData <- airquality

str(exData) ## str - Compactly Display the Structure of an Arbitrary R Object

View(airquality)


# Create Graphs

## Bar Graph

barchart(Day ~ factor(Month),
         data = exData,
         horizontal = FALSE,
         main = "Number of Days in Each Month (May Through September)",
         sub = "Source: airquality dataset",
         xlab = "Months",
         ylab = "Number of Days",
         col = rainbow(5))

barchart(Day ~ factor(Month),
         data = exData,
         horizontal = FALSE,
         main = "Number of Days in Each Month (May Through September)",
         sub = "Source: airquality dataset",
         xlab = "Months",
         ylab = "Number of Days",
         col = c("red",
                 "yellow",
                 "green",
                 "blue",
                 "purple"))

## Box Plot

### Create boxplots for each month's recorded temperatures

bwplot(Temp ~ factor(Month),
       data = exData,
       main = "Temperature for May - September '73",
       sub = "Source: airquality dataset",
       xlab = "Months",
       ylab = "Temperature",
       col = "green",
       horizontal = FALSE)

## Histogram

### Create a histogram of Temperature frequencies

histogram(~ Temp,
          data = exData,
          main = "Histogram of Temperature Occurances",
          sub = "Source: airquality dataset",
          xlab = "Temperature",
          ylab = "Frequency",
          col = "gray",
          type = "count")

### Create a histogram of Temperature frequencies for the month of May

histogram(~ Temp | factor(Month),
          data = exData,
          main = "Histogram of Temperature Occurances",
          sub = "Source: airquality dataset",
          xlab = "Temperature",
          ylab = "Frequency",
          col = "gray",
          type = "count",
          layout = c(2,
                     3))

## Kernel Density

### Create a kernel density plot for observed Wind

densityplot(~ Wind,
            data = exData,
            main = "Kernel Density of Observed Wind",
            col = "blue",
            lwd = 3,
            plot.points = FALSE)

### Create a kernel density plot for observed Wind for each month

densityplot(~ Wind | factor(Month),
            data = exData,
            main = "Kernel Density of Observed Wind",
            col = "blue",
            lwd = 3,
            plot.points = FALSE,
            layout = c(2,
                       3))

## Scatterplot

### Create a scatterplot plotting the relationship between Wind and Temperature

xyplot(Wind ~ Temp,
       data = exData,
       main = "Relationship between Temperature and Wind",
       xlab = "Temperature",
       pch = 19,
       col = "blue")


### Add a line of best fit

xyplot(Wind ~ Temp,
       data = exData,
       main = "Relationship between Temperature and Wind",
       xlab = "Temperature",
       pch = 19,
       col = "blue",
       panel = function(x,
                        y,
                        ...) {
               panel.xyplot(x,
                            y,
                            ...)
               panel.lmline(x,
                            y,
                            ...)
       })

### Distinguish scatterplot by Month

xyplot(Wind ~ Temp | factor(Month),
       data = exData,
       main = "Relationship between Temperature and Wind",
       xlab = "Temperature",
       pch = 19,
       col = "blue",
       panel = function(x,
                        y,
                        ...) {
               panel.xyplot(x,
                            y,
                            ...)
               panel.lmline(x,
                            y,
                            ...)
       },
       layout = c(2,
                  3))
