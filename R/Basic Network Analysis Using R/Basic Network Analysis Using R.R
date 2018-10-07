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

## Create a igraph graph object from the data frame

krackFull <- graph.data.frame(krack_full_data_frame)

summary(krackFull) # reports basic properties and attributes of igraph object

## Print out the edge sequence for 'advice' network

E(krackFull)$advice_tie

## Simplify the networks via removing loops

krackFull <- simplify(krackFull,
                      remove.multiple = FALSE,
                      remove.loops = TRUE)

## Plot the network

### Assign different colors for each set of network ties

plot(krackFull)

tieTypeColors = c(rgb(1,0,0,.5), # Advice Network
                  rgb(0,0,1,.5), # Friendship Network
                  rgb(0,0,0,.5)) # Report Network

E(krackFull)$color[E(krackFull)$advice_tie == 1] <- tieTypeColors[1]

E(krackFull)$color[E(krackFull)$friendship_tie == 1 ] <- tieTypeColors[2]

E(krackFull)$color[E(krackFull)$reports_to_tie == 1] <- tieTypeColors[3]

E(krackFull)$arrow.size <- 0.5

plot(krackFull,
     vertex.color = "red",
     vertex.label = NA,
     edge.arrow.size = 0.5)

legend(x = 1.5,
       y = 1.25,
       legend = c('Advice',
                  'Friendship',
                  'Reports To'),
       col = tieTypeColors,
       lty = 1,
       cex = 1)

title(main = "Krackhardt's Data on Ties among High-Tech Workers",
      sub = "Advice, Friendship, and Reports To Networks")


# Calculate Network Statistics

## Centrality

### Degree - Number of adjacent ties for a node

degree(graph_from_data_frame(advice_data_frame),
       mode = "in")

degree(graph_from_data_frame(advice_data_frame),
       mode = "out")

#### According to the results above, every node is connected to every
#### other node, which is incorrect.

#### Remedy #1 - Use subset() to reduce edgelist to just existing ties

degree(graph_from_data_frame(subset(advice_data_frame,
                                    (advice_tie == 1))),
       mode = "in")

degree(graph_from_data_frame(subset(advice_data_frame,
                                    (advice_tie == 1))),
       mode = "out")

##### Removes isolates from the network

#### Remedy #2 - Use delete.edges() to remove non-existing ties

adviceAll <- graph.data.frame(advice_data_frame)

adviceThin <- delete.edges(adviceAll,
                           E(adviceAll)[get.edge.attribute(adviceAll,
                                                           name = "advice_tie") == 0])

degree(adviceThin,
       mode = "in")

degree(adviceThin,
       mode = "out")

##### Removes ties that equal 0, but keeps all nodes

### Betweenness - The number of geodesics (shortest paths) going through a node or a tie

betweenness(graph.data.frame(subset(friendship_data_frame,
                                    (friendship_tie > 0))),
            directed = TRUE)

betweenness(graph.data.frame(subset(friendship_data_frame,
                                    (friendship_tie > 0))),
            directed = FALSE)

betweenness(graph.data.frame(subset(friendship_data_frame,
                                    (friendship_tie > 0))),
            directed = TRUE,
            normalized = TRUE)

#### betweenness() calculates node betweenness

### Density - The ratio of the number of edges and the number of possible edges

edge_density(adviceThin)

edge_density(graph.data.frame(subset(friendship_data_frame,
                                     (friendship_tie != 0))))
