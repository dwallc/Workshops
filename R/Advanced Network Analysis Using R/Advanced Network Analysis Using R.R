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

packages <- c("rio",
              "network",
              "sna")

## NetData - Data for Social Network Analysis labs
## network - Classes for Relational Data
## sna - Tools for Social Network Analysis

ipak(packages)


# Set Working Directory

## setwd("E:/Desmond's Files/Cloud Storage/Dropbox/GitHub/Workshops/R/Advanced Network Analysis Using R")


# Load adjacency matrices into memory

## Advice

adjAdvice <- import("./Data/advice.csv",
                    header = TRUE,
                    select = 2:22)

## Friends

adjFriends <- import("./Data/friends.csv",
                     header = TRUE,
                     select = 2:22)

## Reports

adjReports <- import("./Data/reports.csv",
                     header = TRUE,
                     select = 2:22)

## Create a network graph objects from the data frame

### Advice Network

netAdvice <- as.network.matrix(adjAdvice,
                               matrix.type = "adjacency",
                               directed = TRUE)

summary(netAdvice) # reports basic properties and attributes of igraph object

### Friendship Network

netFriends <- as.network.matrix(adjFriends,
                                matrix.type = "adjacency",
                                directed = TRUE)

summary(netFriends)

### Reports Network

netReports <- as.network.matrix(adjReports,
                                matrix.type = "adjacency",
                                directed = TRUE)

summary(netReports)


# Calculate Network Statistics

## Degree Centrality

# NOTE: Important not to load igraph and sna packages together
# What happens when igraph and sna are loaded together?

library(igraph)


## Density - The ratio of the number of edges and the number of possible edges

gden(netAdvice,
     mode = "digraph")

gden(netFriends,
     mode = "digraph")

gden(netReports,
     mode = "digraph")

## Transitivity - Situation where actor i is tied to actor j, actor j is tied
## to actor k, and actor i is likewise tied to actor k. In other words, "a
## friend of my friend is also my friend."

gtrans(netAdvice,
       mode = "digraph",
       measure = "weak")

gtrans(netFriends,
       mode = "digraph",
       measure = "weak")

gtrans(netReports,
       mode = "digraph",
       measure = "weak")

## Reciprocity - proportion of dyads which are symmetric

grecip(netAdvice,
       measure = "dyadic")

grecip(netFriends,
       measure = "dyadic")

grecip(netReports,
       measure = "dyadic")


# Univariate hypothesis testing

## Question: Are the network statistics we observed above different from what
## would be expected if the ties were randomly assigned?

## Univariate Conditional Uniform Graph Tests

### Density

#### Conditioning on network size

adviceCugTestDenSize <- cug.test(netAdvice,
                                 gden,
                                 mode = "digraph",
                                 cmode = "size",
                                 reps = 1000)

print.cug.test(adviceCugTestDenSize)

windows()
plot.cug.test(adviceCugTestDenSize)

#### Conditioning on number of edges



#### Conditioning on distribution of dyads



### Transitivity



### Reciprocity




# Quadratic Assignment Procedure (QAP) Hypothesis Tests

## Advice and Friends networks

qapAdviceFriends <- qaptest(list(netAdvice,
                                 netFriends),
                             gcor,
                             g1 = 1,
                             g2 = 2,
                             reps = 1000)

summary.qaptest(qapAdviceFriends)

plot.qaptest(qapAdviceFriends)

## Advice and Reports nwtworks



## Friends and Reports networks




# Network Regression

## Null Hypothesis: Classical (tests based on classical asymptotics)

modClass <- netlogit(netAdvice,
                     list(netFriends,
                          netReports),
                     nullhyp = "classical",
                     reps = 1000)

summary(modClass)

## Null Hypothesis: Cugden (conditional uniform graph test, controlling for order and density)



## Null Hypothesis: Qap (QAP permutation test)


