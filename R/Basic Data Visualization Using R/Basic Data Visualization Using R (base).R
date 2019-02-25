#################################################################
#       Name:   Basic Data Visualization Using R (base).R       #
#       Date:   September 26, 2018                              #
#       Author: Desmond D. Wallace                              #
#       Purpose:        Create basic plots via base plotting    #
#                       system.                                 #
#################################################################


# Install and load needed packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("datasets")

## datasets - Base R datasets

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

### Create an object containing number of days for each month (frequency count)

exDataMonthsCounts <- table(exData$Month)

### Create basic plot using "artist palette" approach (Add different plot layers separately)

# png("./bar.png") # Saving plot to specified file

barplot(exDataMonthsCounts,
        col = rainbow(5))

title(main = "Number of Days in Each Month (May Through September)",
      sub = "Source: airquality dataset",
      xlab = "Months",
      ylab = "Number of Days") # Add annotation layer

legend(x = 7,
       y = 30,
       legend = names(exDataMonthsCounts),
       fill = rainbow(5),
       col = rainbow(5)) # Add legend layer

# dev.off() # Turns graphics device off

### Create same basic plot specifying aadditional layers within function

windows() # Opens a graphics device window

barplot(exDataMonthsCounts,
        main = "Number of Days in Each Month (May Through September)",
        sub = "Source: airquality dataset",
        xlab = "Months",
        ylab = "Number of Days",
        col = rainbow(5),
        legend = names(exDataMonthsCounts))

### Create horizontal version

barplot(exDataMonthsCounts,
        main = "Number of Days in Each Month (May Through September)",
        sub = "Source: airquality dataset",
        xlab = "Number of Days",
        ylab = "Months",
        col = rainbow(5),
        legend = names(exDataMonthsCounts),
        horiz = TRUE)

## Box Plot

### Create boxplot for all recorded temperatures

boxplot(exData$Temp,
        main = "Temperature for May - September '73",
        sub = "Source: airquality dataset",
        ylab = "Temperature",
        col = "blue")

### Create boxplots for each month's recorded temperatures

boxplot(Temp ~ Month,
        data = exData,
        main = "Temperature for May - September '73",
        sub = "Source: airquality dataset",
        xlab = "Months",
        ylab = "Temperature",
        col = "green")

## Histogram

### Create a histogram of Temperature frequencies

hist(exData$Temp,
     freq = TRUE,
     main = "Histogram of Temperature Occurances",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "gray")

### Create a histogram of Temperature frequencies for each month

par(mfrow = c(3,
              2)) # Add additional plots by row

hist(exData$Temp[exData$Month == 5],
     freq = TRUE,
     main = "Histogram of Temperature Occurances in May",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "red")

hist(exData$Temp[exData$Month == 6],
     freq = TRUE,
     main = "Histogram of Temperature Occurances in June",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "yellow")

hist(exData$Temp[exData$Month == 7],
     freq = TRUE,
     main = "Histogram of Temperature Occurances in July",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "green")

hist(exData$Temp[exData$Month == 8],
     freq = TRUE,
     main = "Histogram of Temperature Occurances in August",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "blue")

hist(exData$Temp[exData$Month == 9],
     freq = TRUE,
     main = "Histogram of Temperature Occurances in September",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "purple")

hist(exData$Temp,
     freq = TRUE,
     main = "Histogram of Temperature Occurances",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Frequency",
     col = "gray")

## Kernel Density

### Create a kernel density plot for observed Wind

par(mfrow = c(1,
              1))

plot(density(exData$Wind),
     main = "Kernel Density of Observed Wind",
     col = "black",
     lwd = 3,
     zero.line = FALSE)

### Create a kernel density plot for observed Wind for each month

par(mfrow = c(3,
              2))

plot(density(exData$Wind[exData$Month == 5]),
     main = "Kernel Density of Observed Wind in May",
     col = "red",
     lwd = 3,
     zero.line = FALSE)

plot(density(exData$Wind[exData$Month == 6]),
     main = "Kernel Density of Observed Wind in June",
     col = "yellow",
     lwd = 3,
     zero.line = FALSE)

plot(density(exData$Wind[exData$Month == 7]),
     main = "Kernel Density of Observed Wind in July",
     col = "green",
     lwd = 3,
     zero.line = FALSE)

plot(density(exData$Wind[exData$Month == 8]),
     main = "Kernel Density of Observed Wind in August",
     col = "blue",
     lwd = 3,
     zero.line = FALSE)

plot(density(exData$Wind[exData$Month == 9]),
     main = "Kernel Density of Observed Wind in September",
     col = "purple",
     lwd = 3,
     zero.line = FALSE)

plot(density(exData$Wind),
     main = "Kernel Density of Observed Wind",
     col = "black",
     lwd = 3,
     zero.line = FALSE)

## Scatterplot

### Create a scatterplot plotting the relationship between Wind and Temperature

plot(exData$Temp,
     exData$Wind,
     main = "Relationship between Temperature and Wind",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Wind",
     pch = 19)

### Add a line of best fit

abline(lm(Wind ~ Temp,
          data = exData),
       col = "blue")

### Distinguish scatterplot by Month

plot(exData$Temp,
     exData$Wind,
     main = "Relationship between Temperature and Wind",
     sub = "Source: airquality dataset",
     xlab = "Temperature",
     ylab = "Wind",
     pch = 19,
     col = exData$Month)

legend(x = 60,
       y = 8,
       legend = unique(exData$Month),
       col = unique(exData$Month),
       fill = unique(exData$Month))

abline(lm(Wind ~ Temp,
          data = exData),
       col = "blue",
       lwd = 4)

abline(lm(Wind ~ Temp,
          data = exData,
          subset = Month == 5),
       col = "cyan",
       lwd = 2,
       lty = 2)

abline(lm(Wind ~ Temp,
          data = exData,
          subset = Month == 6),
       col = "magenta",
       lwd = 2,
       lty = 2)

abline(lm(Wind ~ Temp,
          data = exData,
          subset = Month == 7),
       col = "yellow",
       lwd = 2,
       lty = 2)

abline(lm(Wind ~ Temp,
          data = exData,
          subset = Month == 8),
       col = "gray",
       lwd = 2,
       lty = 2)

abline(lm(Wind ~ Temp,
          data = exData,
          subset = Month == 9),
       col = "black",
       lwd = 2,
       lty = 2)
