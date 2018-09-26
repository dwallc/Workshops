#################################################################
#       Name:   Basic Data Visualization Using R (ggplot).R     #
#       Date:   September 26, 2018                              #
#       Author: Desmond D. Wallace                              #
#       Purpose:        Create basic plots via ggplor plotting  #
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
              "tidyverse") # Will load ggplot2 package automatically

## datasets - Base R datasets
## lattice - Create Elegant Data Visualisations Using the Grammar of Graphics

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

### Using qplot() (Quick plot)

qplot(factor(Month),
      data = exData,
      geom = "bar",
      main = "Number of Days in Each Month (May Through September)",
      xlab = "Months",
      ylab = "Number of Days",
      fill = factor(Month)) # Create a quick plot

### Using ggplot() (Create a new ggplot)

ggplot(data = exData,
       mapping = aes(x = factor(Month),
                     fill = factor(Month))) +
        geom_bar(stat = "count") +
        xlab("Months") +
        ylab("Number of Days") +
        ggtitle("Number of Days in Each Month (May Through September)") # Equivalent to qplot() above

## Center plot title and change legend title

ggplot(data = exData,
       mapping = aes(x = factor(Month),
                     fill = factor(Month))) +
        geom_bar(stat = "count") +
        xlab("Months") +
        ylab("Number of Days") +
        ggtitle("Number of Days in Each Month (May Through September)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_discrete(name = "Months")

# ggsave("./bar2.png") # Automatically save last created plot

## Box Plot

### Create boxplots for each month's recorded temperatures

ggplot(data = exData,
       mapping = aes(x = factor(Month),
                     y = Temp)) +
        geom_boxplot(fill = "green") +
        xlab("Months") +
        ylab("Temperature") +
        ggtitle("Temperature for May - September '73") +
        theme(plot.title = element_text(hjust = 0.5))

## Histogram

### Create a histogram of Temperature frequencies

ggplot(data = exData,
       mapping = aes(x = Temp)) +
        geom_histogram(bins = 9,
                       fill = "gray",
                       colour = "black") +
        xlab("Temperature") +
        ylab("Frequency") +
        ggtitle("Histogram of Temperature Occurances") +
        theme(plot.title = element_text(hjust = 0.5))

### Create a histogram of Temperature frequencies for the month of May

#### Same Graph

ggplot(data = exData,
       mapping = aes(x = Temp,
                     fill = factor(Month))) +
        geom_histogram(position = "identity",
                       bins = 9,
                       colour = "black",
                       alpha = 0.4) +
        xlab("Temperature") +
        ylab("Frequency") +
        ggtitle("Histogram of Temperature Occurances") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_discrete(name = "Months")

#### Multiple Graphs

ggplot(data = exData,
       mapping = aes(x = Temp,
                     fill = factor(Month))) +
        geom_histogram(bins = 9,
                       colour = "black") +
        facet_grid(. ~ factor(Month)) +
        xlab("Temperature") +
        ylab("Frequency") +
        ggtitle("Histogram of Temperature Occurances") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_discrete(name = "Months")

## Kernel Density

### Create a kernel density plot for observed Wind

ggplot(data = exData,
       mapping = aes(x = Wind)) +
        geom_density(size = 1.5) +
        xlab("Wind") +
        ylab("Density") +
        ggtitle("Kernel Density of Observed Wind") +
        theme(plot.title = element_text(hjust = 0.5)) # Includes horizontal line at y = 0

ggplot(data = exData,
       mapping = aes(x = Wind)) +
        geom_line(stat = "density",
                  size = 1.5) +
        xlab("Wind") +
        ylab("Density") +
        ggtitle("Kernel Density of Observed Wind") +
        theme(plot.title = element_text(hjust = 0.5)) # Does not include horizontal line at y = 0

### Create a kernel density plot for observed Wind for each month

#### Same Graph

ggplot(data = exData,
       mapping = aes(x = Wind,
                     colour = factor(Month))) +
        geom_line(stat = "density",
                  size = 1.5) +
        xlab("Wind") +
        ylab("Density") +
        ggtitle("Kernel Density of Observed Wind") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_discrete(name = "Months")

#### Multiple Graphs

ggplot(data = exData,
       mapping = aes(x = Wind,
                     colour = factor(Month))) +
        geom_line(stat = "density",
                  size = 1.5) +
        facet_grid(. ~ factor(Month)) +
        xlab("Wind") +
        ylab("Density") +
        ggtitle("Kernel Density of Observed Wind") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_discrete(name = "Months")

## Scatterplot

### Create a scatterplot plotting the relationship between Wind and Temperature

ggplot(data = exData,
       mapping = aes(x = Temp,
                     y = Wind)) +
        geom_point(colour = "blue",
                   size = 3) +
        xlab("Temperature") +
        ylab("Wind") +
        ggtitle("Relationship between Temperature and Wind") +
        theme(plot.title = element_text(hjust = 0.5))


### Add a line of best fit

ggplot(data = exData,
       mapping = aes(x = Temp,
                     y = Wind)) +
        geom_point(colour = "blue",
                   size = 3) +
        geom_smooth(method = lm,
                    colour = "black",
                    size = 1.5) +
        xlab("Temperature") +
        ylab("Wind") +
        ggtitle("Relationship between Temperature and Wind") +
        theme(plot.title = element_text(hjust = 0.5))

### Distinguish scatterplot by Month

#### Same Graph

ggplot(data = exData,
       mapping = aes(x = Temp,
                     y = Wind,
                     colour = factor(Month))) +
        geom_point(size = 3) +
        geom_smooth(method = lm,
                    size = 1.5,
                    linetype = 2,
                    se = FALSE) +
        xlab("Temperature") +
        ylab("Wind") +
        ggtitle("Relationship between Temperature and Wind") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_discrete(name = "Months")

#### Multiple Graphs

ggplot(data = exData,
       mapping = aes(x = Temp,
                     y = Wind,
                     colour = factor(Month))) +
        geom_point(size = 3) +
        geom_smooth(method = lm,
                    size = 1.5,
                    linetype = 2,
                    se = FALSE) +
        facet_grid(. ~ factor(Month)) +
        xlab("Temperature") +
        ylab("Wind") +
        ggtitle("Relationship between Temperature and Wind") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_color_discrete(name = "Months")
