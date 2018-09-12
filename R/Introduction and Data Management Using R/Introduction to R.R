#########################################################################################
#       Name:   Introduction to R.R                                                     #
#       Date:   September 12, 2018                                                      #
#       Author: Desmond D. Wallace                                                      #
#       Purpose:        Introduction and basic data managmement for first time users.   #
#########################################################################################


# Installation

## Programs

### Install R

#### https://cran.r-project.org/

### Install RStudio

#### https://www.rstudio.com/

## Packages

### CRAN Packages

#### install.packages('package name')

### GitHub

#### devtools::install_github

### Load packages

#### library('package name')

#### require('package name') (Used when loading package(s) within other functions)


# First-time Use

## Working Directory

getwd() # Current working directory

## setwd('file path') # Change working directory

## Can also change working directory via file menus (Session > Set Working Directory > Choose Directory...)

## Can also change working directory via keyboard shortcut (Ctrl+Shift+H)


# Creating Objects

## Matrix

A <- matrix(-8:7,
            nrow = 4,
            ncol = 4)

A # View object in R console

A[2,
  3] # Report single maxtrix element

View(A) # View object in RStudio

View(A[2,
       3])

## Vector

### Class Numeric

c(1,
  2,
  3) # Integer

c(1.5,
  -2.34,
  NA) # Double

# Class Factor
factor(c(1,
         2,
         2,
         3),
       labels = c("Apple",
                  "Pear",
                  "Orange"))

# Class Character

c("R is hard.",
  "But I can learn.")

# Class POSIXlt (Time)

as.POSIXlt("2018-9-12 13:30:00") # "%Y-%m-%d %H:%M:%S"

# Class Logical

c(TRUE,
  TRUE,
  FALSE)

# Class Data Frame

exData <- mtcars # Example dataset already installed

View(exData)

View(mtcars$mpg)

# Class Array

array(-9:9,
      dim = c(3,
              3,
              2))

# Class List

exList <- list(c(1:5),
               letters[1:5])

exList

exList[[2]]
