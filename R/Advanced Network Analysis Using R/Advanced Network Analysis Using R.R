#########################################################
#       Name:   Advanced Network Analysis Using R.R     #
#       Date:   May 1, 2019                             #
#       Author: Desmond D. Wallace                      #
#       Purpose:        Conduct basic hypothesis        #
#                       testing with network data.      #
#########################################################


# Install and load needed packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg))
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

packages <- c("NetData",
              "network",
              "sna")

## NetData - Data for Social Network Analysis labs
## network - Classes for Relational Data
## sna - Tools for Social Network Analysis

ipak(packages)


# Set Working Directory

## setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Basic Network Analysis Using R")


# Load kracknets dataset

data(kracknets,
     package = "NetData")

## Type ?NetData::krack_full_data_frame for description of dataset.

str(krack_full_data_frame)

View(krack_full_data_frame)

## Create a network graph objects from the data frame

### Advice Network

netAdvice <- as.network.matrix(advice_data_frame[advice_data_frame[["advice_tie"]] == 1,],
                               matrix.type = "edgelist",
                               directed = TRUE)

summary(netAdvice) # reports basic properties and attributes of igraph object

### Friendship Network

netFriendship <- as.network.matrix(friendship_data_frame[friendship_data_frame[["friendship_tie"]] == 1,],
                                   matrix.type = "edgelist",
                                   directed = TRUE)

summary(netFriendship)

### Reports Network

netReports <- as.network.matrix(reports_to_data_frame[reports_to_data_frame[["reports_to_tie"]] == 1,],
                                matrix.type = "edgelist",
                                directed = TRUE)

summary(netReports)


# Calculate Network Statistics

## Centrality

### Degree - Number of adjacent ties for a node

degree(netAdvice,
       gmode = "digraph",
       cmode = "indegree")

degree(netAdvice,
       gmode = "digraph",
       cmode = "outdegree")

degree(netAdvice,
       gmode = "digraph",
       cmode = "freeman") # total degree (indegree + outdegree)

#### Normalized degree

degree(netAdvice,
       gmode = "digraph",
       cmode = "indegree",
       rescale = TRUE)

degree(netAdvice,
       gmode = "digraph",
       cmode = "outdegree",
       rescale = TRUE)

degree(netAdvice,
       gmode = "digraph",
       cmode = "freeman",
       rescale = TRUE)

### Betweenness - The number of geodesics (shortest paths) going through a node or a tie

betweenness(netAdvice,
            gmode = "digraph",
            cmode = "directed",
            rescale = TRUE)

betweenness(netFriendship,
            gmode = "digraph",
            cmode = "directed",
            rescale = TRUE)

betweenness(netReports,
            gmode = "digraph",
            cmode = "directed",
            rescale = TRUE)

### Density - The ratio of the number of edges and the number of possible edges

network.density(netAdvice)

network.density(netFriendship)

network.density(netReports)


# Univariate hypothesis testing

## Question: Are the network statistics we observed above different from what
## would be expected if the ties were randomly assigned?
