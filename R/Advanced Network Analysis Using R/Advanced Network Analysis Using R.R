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

adviceCugTestDen <- cug.test(netAdvice,
                             gden,
                             mode = "digraph",
                             cmode = "edges",
                             reps = 1000)

print.cug.test(adviceCugTestDen)

plot.cug.test(adviceCugTestDen)

friendshipCugTestDen <- cug.test(netFriendship,
                                 gden,
                                 mode = "digraph",
                                 cmode = "edges",
                                 reps = 1000)

print.cug.test(friendshipCugTestDen)

plot.cug.test(friendshipCugTestDen)

reportsCugTestDen <- cug.test(netReports,
                              gden,
                              mode = "digraph",
                              cmode = "edges",
                              reps = 1000)

print.cug.test(reportsCugTestDen)

plot.cug.test(reportsCugTestDen)

### Transitivity

adviceCugTestTrans <- cug.test(netAdvice,
                               gtrans,
                               mode = "digraph",
                               cmode = "edges",
                               reps = 1000)

print.cug.test(adviceCugTestTrans)

plot.cug.test(adviceCugTestTrans)

friendshipCugTestTrans <- cug.test(netFriendship,
                                   gtrans,
                                   mode = "digraph",
                                   cmode = "edges",
                                   reps = 1000)

print.cug.test(friendshipCugTestTrans)

plot.cug.test(friendshipCugTestTrans)

reportsCugTestTrans <- cug.test(netReports,
                                gtrans,
                                mode = "digraph",
                                cmode = "edges",
                                reps = 1000)

print.cug.test(reportsCugTestTrans)

plot.cug.test(reportsCugTestTrans)

### Reciprocity

adviceCugTestRecip <- cug.test(netAdvice,
                               grecip,
                               mode = "digraph",
                               cmode = "edges",
                               reps = 1000)

print.cug.test(adviceCugTestRecip)

plot.cug.test(adviceCugTestRecip)

friendshipCugTestRecip <- cug.test(netFriendship,
                                   grecip,
                                   mode = "digraph",
                                   cmode = "edges",
                                   reps = 1000)

print.cug.test(friendshipCugTestRecip)

plot.cug.test(friendshipCugTestRecip)

reportsCugTestRecip <- cug.test(netReports,
                                grecip,
                                mode = "digraph",
                                cmode = "edges",
                                reps = 1000)

print.cug.test(reportsCugTestRecip)

plot.cug.test(reportsCugTestRecip)


# Quadratic Assignment Procedure (QAP) Hypothesis Tests

qapFriendsReports <- qaptest(list(netFriendship,
                                  netReports),
                             gcor,
                             g1 = 1,
                             g2 = 2,
                             reps = 1000)

summary.qaptest(qapFriendsReports)

plot.qaptest(qapFriendsReports)


# Network Regression

