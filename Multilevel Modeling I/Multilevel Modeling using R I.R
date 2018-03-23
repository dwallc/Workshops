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
              "multilevel")

ipak(packages)
