#################################################################
#       Name:   Basic Network Analysis Using R.R                #
#       Date:   October 10, 2018                                #
#       Author: Desmond D. Wallace                              #
#       Purpose:        Create basic plots and and calculate    #
#                       basic statistics of network data..      #
#################################################################


# Install and load needed packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("NetData",
              "igraph")

## NetData - Data for Social Network Analysis labs
## igraph - A library and R package for network analysis

ipak(packages)


# Set Working Directory

## setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Basic Network Analysis Using R")


# Create network data

## Undirected Tree

### Undirected - Does tie exist between node_i and node_j (i->j/i<-j)?
### Tree - Type of Graph

exTree <- make_tree(40,
                    children = 3,
                    mode = "undirected") # Create a regular tree graph.

windows()
plot(exTree,
     vertex.size = 10,
     vertex.label = NA)

### Because igraph utilizes the base plotting engine, resulting
### plot can be annotated

title(main = "Example Network I",
      sub = "40 Nodes, Tree Graph, Undirected")

## Directed Ring

### Directed - Does tie exist between node_i and node_j (i->j or i<-j)?
### Ring - Type of graph

exRing <- make_ring(40,
                    directed = TRUE)

plot(exRing,
     vertex.size = 10,
     vertex.label = NA)

title(main = "Example Network II",
      sub = "40 Nodes, Ring Graph, Directed")

## Erdos-Renyi Random Graph Model

### Directed, No Loops

exERnl <- sample_gnm(n = 100,
                     m = 40,
                     directed = TRUE)

plot(exERnl,
     vertex.size = 6,
     vertex.label = NA)

title(main = "Example Network III",
      sub = "100 Nodes, 40 Ties, Erdos-Renyi Random Graph Model, Directed, No Loops")

### Undirected, Loops

exERl <- sample_gnm(n = 100,
                    m = 40,
                    loops = TRUE)

plot(exERl,
     vertex.size = 6,
     vertex.label = NA)

title(main = "Example Network IV",
      sub = "100 Nodes, 40 Ties, Erdos-Renyi Random Graph Model, Undirected, Loops")

# Load kracknets dataset

data(kracknets,
     package = "NetData")

## Type ?NetData::krack_full_data_frame for description of dataset.

str(krack_full_data_frame)

View(krack_full_data_frame)


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
        xlab = "Months",
        ylab = "Number of Days",
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
