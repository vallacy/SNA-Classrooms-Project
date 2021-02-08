##########################################
## SNA in Stats Classrooms Project Code ##
########### ERGMs for 5 Labs #############
######## Created by Valerie Ryan #########
######### Last Updated 2/8/2021 ##########
##########################################
#rm(list=ls())

# load packages
library(igraph)
library(network)
library(ergm)

# set working directory
setwd("C:/Users/Your Directory")

###########
## Lab 1 ##
###########

## Importing Data

# import adjacency matrix
lab1 = read.csv("Lab 1.csv", header = TRUE, row.names = 1, check.names = FALSE)

# manipulate data so it is a matrix, graph, and edgelist
m_1 = as.matrix(lab1)
g_1 = graph.adjacency(m_1, mode = "directed", weighted = NULL)
e.list_1 = get.edgelist(g_1, names = TRUE)

# calculate descriptive network characteristics
transitivity(g_1)
edge_density(simplify(g_1), loops = FALSE)
mean(betweenness(g_1))
mean(degree(g_1))
assortativity.degree(g_1, directed = TRUE)

# import attribute data
demo_1 = read.csv("Lab 1 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# add attribute data to graph object
g_demo1 = graph.data.frame(e.list_1, directed = "TRUE", vertices = demo_1)

# prep data for analysis
A1 = get.adjacency(g_1)
lab.1 = network::as.network(as.matrix(A1), directed = TRUE)

lab1.demo = get.data.frame(g_demo1, what = "vertices")
network::set.vertex.attribute(lab.1, "Grit", lab1.demo$Grit_total)
network::set.vertex.attribute(lab.1, "BRS", lab1.demo$BRS_total)

## ERGM

# difference in scores model
lab1.model1 = ergm(lab.1 ~ edges + mutual                     
                    + diff("Grit", pow=1, dir="t-h", sign.action="identity")
                    + diff("BRS", pow=1, dir="t-h", sign.action="identity"))

summary(lab1.model1)

## Calculate Odds Ratios

# convert coefficients to OR with 95% CI
exp(summary(lab1.model1)$coefficients["edges",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab1.model1)$coefficients["edges",2])
exp(summary(lab1.model1)$coefficients["mutual",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab1.model1)$coefficients["mutual",2])
exp(summary(lab1.model1)$coefficients["diff.t-h.Grit",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab1.model1)$coefficients["diff.t-h.Grit",2])
exp(summary(lab1.model1)$coefficients["diff.t-h.BRS",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab1.model1)$coefficients["diff.t-h.BRS",2])

## Plot network graph

# compute node degree
deg <- degree(g_1, mode = "all")

# set node size based on degree
V(g_1)$size <- deg*3

# default labels are node ids, set them to NA
V(g_1)$label <- NA

# change default arrow size and set edge color to be gray
E(g_1)$arrow.size <- .1
E(g_1)$edge.color <- "gray80"

# set layout of graph using Fruchterman-Reingold layout algorithm
l <- layout_with_fr(g_1)

# rescale graph
l <- norm_coords(l, ymin = -0.95, ymax = 0.95, xmin = -0.95, xmax = 0.95)

# plot the network graph
plot(g_1, rescale = FALSE, layout = l*1.5, vertex.color = "black")

###########
## Lab 2 ##
###########

## Importing Data

# import adjacency matrix
lab2 = read.csv("Lab 2.csv", header = TRUE, row.names = 1, check.names = FALSE)

# manipulate data so it is a matrix, graph, and edgelist
m_2 = as.matrix(lab2)
g_2 = graph.adjacency(m_2, mode = "directed", weighted = NULL)
e.list_2 = get.edgelist(g_2, names = TRUE)

# calculate descriptive network characteristics
transitivity(g_2)
edge_density(simplify(g_2), loops = FALSE)
mean(betweenness(g_2))
mean(degree(g_2))
assortativity.degree(g_2, directed = TRUE)

# import attribute data
demo_2 = read.csv("Lab 2 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# add attribute data to graph object
g_demo2 = graph.data.frame(e.list_2, directed = "TRUE", vertices = demo_2)

# prep data for analysis
A2 = get.adjacency(g_2)
lab.2 = network::as.network(as.matrix(A2), directed = TRUE)

lab2.demo = get.data.frame(g_demo2, what = "vertices")
network::set.vertex.attribute(lab.2, "Grit", lab2.demo$Grit_total)
network::set.vertex.attribute(lab.2, "BRS", lab2.demo$BRS_total)

## ERGM

# difference in scores model
lab2.model1 = ergm(lab.2 ~ edges + mutual                    
                    + diff("Grit", pow=1, dir="t-h", sign.action="identity")
                    + diff("BRS", pow=1, dir="t-h", sign.action="identity"))

summary(lab2.model1)

## Calculate Odds Ratios

# convert coefficients to OR with 95% CI
exp(summary(lab2.model1)$coefficients["edges",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab2.model1)$coefficients["edges",2])
exp(summary(lab2.model1)$coefficients["mutual",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab2.model1)$coefficients["mutual",2])
exp(summary(lab2.model1)$coefficients["diff.t-h.Grit",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab2.model1)$coefficients["diff.t-h.Grit",2])
exp(summary(lab2.model1)$coefficients["diff.t-h.BRS",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab2.model1)$coefficients["diff.t-h.BRS",2])

## Plot network graph

# compute node degree
deg <- degree(g_2, mode = "all")

# set node size based on degree
V(g_2)$size <- deg*3

# default labels are node ids, set them to NA
V(g_2)$label <- NA

# change default arrow size and set edge color to be gray
E(g_2)$arrow.size <- .1
E(g_2)$edge.color <- "gray80"

# set layout of graph using Fruchterman-Reingold layout algorithm
l <- layout_with_fr(g_2)

# rescale graph
l <- norm_coords(l, ymin = -0.95, ymax = 0.95, xmin = -0.95, xmax = 0.95)

# plot the network graph
plot(g_2, rescale = FALSE, layout = l*1.5, vertex.color = "black")

###########
## Lab 3 ##
###########

## Importing Data

# import adjacency matrix
lab3 = read.csv("Lab 3.csv", header = TRUE, row.names = 1, check.names = FALSE)

# manipulate data so it is a matrix, graph, and edgelist
m_3 = as.matrix(lab3)
g_3 = graph.adjacency(m_3, mode = "directed", weighted = NULL)
e.list_3 = get.edgelist(g_3, names = TRUE)

# calculate descriptive network characteristics
transitivity(g_3)
edge_density(simplify(g_3), loops = FALSE)
mean(betweenness(g_3))
mean(degree(g_3))
assortativity.degree(g_3, directed = TRUE)

# import attribute data
demo_3 = read.csv("Lab 3 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# add attribute data to graph object
g_demo3 = graph.data.frame(e.list_3, directed = "TRUE", vertices = demo_3)

# prep data for analysis
A3 = get.adjacency(g_3)
lab.3 = network::as.network(as.matrix(A3), directed = TRUE)

lab3.demo = get.data.frame(g_demo3, what = "vertices")
network::set.vertex.attribute(lab.3, "Grit", lab3.demo$Grit_total)
network::set.vertex.attribute(lab.3, "BRS", lab3.demo$BRS_total)

## ERGMs

# difference in scores model
lab3.model1 = ergm(lab.3 ~ edges + mutual                    
                    + diff("Grit", pow=1, dir="t-h", sign.action="identity")
                    + diff("BRS", pow=1, dir="t-h", sign.action="identity"))

summary(lab3.model1)

## Calculate Odds Ratios

# convert coefficients to OR with 95% CI
exp(summary(lab3.model1)$coefficients["edges",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab3.model1)$coefficients["edges",2])
exp(summary(lab3.model1)$coefficients["mutual",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab3.model1)$coefficients["mutual",2])
exp(summary(lab3.model1)$coefficients["diff.t-h.Grit",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab3.model1)$coefficients["diff.t-h.Grit",2])
exp(summary(lab3.model1)$coefficients["diff.t-h.BRS",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab3.model1)$coefficients["diff.t-h.BRS",2])

## Plot network graph

# compute node degree
deg <- degree(g_3, mode = "all")

# set node size based on degree
V(g_3)$size <- deg*3

# default labels are node ids, set them to NA
V(g_3)$label <- NA

# change default arrow size and set edge color to be gray
E(g_3)$arrow.size <- .1
E(g_3)$edge.color <- "gray80"

# set layout of graph using Fruchterman-Reingold layout algorithm
l <- layout_with_fr(g_3)

# rescale graph
l <- norm_coords(l, ymin = -0.95, ymax = 0.95, xmin = -0.95, xmax = 0.95)

# plot the network graph
plot(g_3, rescale = FALSE, layout = l*1.5, vertex.color = "black")

###########
## Lab 4 ##
###########

## Importing Data

# import adjacency matrix
lab4 = read.csv("Lab 4.csv", header = TRUE, row.names = 1, check.names = FALSE)

# manipulate data so it is a matrix, graph, and edgelist
m_4 = as.matrix(lab4)
g_4 = graph.adjacency(m_4, mode = "directed", weighted = NULL)
e.list_4 = get.edgelist(g_4, names = TRUE)

# calculate descriptive network characteristics
transitivity(g_4)
edge_density(simplify(g_4), loops = FALSE)
mean(betweenness(g_4))
mean(degree(g_4))
assortativity.degree(g_4, directed = TRUE)

# import attribute data
demo_4 = read.csv("Lab 4 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# add attribute data to graph object
g_demo4 = graph.data.frame(e.list_4, directed = "TRUE", vertices = demo_4)

# prep data for analysis
A4 = get.adjacency(g_4)
lab.4 = network::as.network(as.matrix(A4), directed = TRUE)

lab4.demo = get.data.frame(g_demo4, what = "vertices")
network::set.vertex.attribute(lab.4, "Grit", lab4.demo$Grit_total)
network::set.vertex.attribute(lab.4, "BRS", lab4.demo$BRS_total)

## ERGM

# difference in scores model
lab4.model1 = ergm(lab.4 ~ edges + mutual                    
                    + diff("Grit", pow=1, dir="t-h", sign.action="identity")
                    + diff("BRS", pow=1, dir="t-h", sign.action="identity"))

summary(lab4.model1)

## Calculate Odds Ratios

# convert coefficients to OR with 95% CI
exp(summary(lab4.model1)$coefficients["edges",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab4.model1)$coefficients["edges",2])
exp(summary(lab4.model1)$coefficients["mutual",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab4.model1)$coefficients["mutual",2])
exp(summary(lab4.model1)$coefficients["diff.t-h.Grit",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab4.model1)$coefficients["diff.t-h.Grit",2])
exp(summary(lab4.model1)$coefficients["diff.t-h.BRS",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab4.model1)$coefficients["diff.t-h.BRS",2])

## Plot network graph

# compute node degree
deg <- degree(g_4, mode = "all")

# set node size based on degree
V(g_4)$size <- deg*3

# default labels are node ids, set them to NA
V(g_4)$label <- NA

# change default arrow size and set edge color to be gray
E(g_4)$arrow.size <- .1
E(g_4)$edge.color <- "gray80"

# set layout of graph using Fruchterman-Reingold layout algorithm
l <- layout_with_fr(g_4)

# rescale graph
l <- norm_coords(l, ymin = -0.95, ymax = 0.95, xmin = -0.95, xmax = 0.95)

# plot the network graph
plot(g_4, rescale = FALSE, layout = l*1.5, vertex.color = "black")

###########
## Lab 5 ##
###########

## Importing Data

# import adjacency matrix
lab5 = read.csv("Lab 5.csv", header = TRUE, row.names = 1, check.names = FALSE)

# manipulate data so it is a matrix, graph, and edgelist
m_5 = as.matrix(lab5)
g_5 = graph.adjacency(m_5, mode = "directed", weighted = NULL)
e.list_5 = get.edgelist(g_5, names = TRUE)

# calculate descriptive network characteristics
transitivity(g_5)
edge_density(simplify(g_5), loops = FALSE)
mean(betweenness(g_5))
mean(degree(g_5))
assortativity.degree(g_5, directed = TRUE)

# import attribute data
demo_5 = read.csv("Lab 5 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# add attribute data to graph object
g_demo5 = graph.data.frame(e.list_5, directed = "TRUE", vertices = demo_5)

# prep data for analysis
A5 = get.adjacency(g_5)
lab.5 = network::as.network(as.matrix(A5), directed = TRUE)

lab5.demo = get.data.frame(g_demo5, what = "vertices")
network::set.vertex.attribute(lab.5, "Grit", lab5.demo$Grit_total)
network::set.vertex.attribute(lab.5, "BRS", lab5.demo$BRS_total)

## ERGM

# difference in scores model
lab5.model1 = ergm(lab.5 ~ edges + mutual                    
                    + diff("Grit", pow=1, dir="t-h", sign.action="identity")
                    + diff("BRS", pow=1, dir="t-h", sign.action="identity"))

summary(lab5.model1)

## Calculate Odds Ratios

# convert coefficients to OR with 95% CI
exp(summary(lab5.model1)$coefficients["edges",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab5.model1)$coefficients["edges",2])
exp(summary(lab5.model1)$coefficients["mutual",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab5.model1)$coefficients["mutual",2])
exp(summary(lab5.model1)$coefficients["diff.t-h.Grit",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab5.model1)$coefficients["diff.t-h.Grit",2])
exp(summary(lab5.model1)$coefficients["diff.t-h.BRS",1] + 
      +   qnorm(c(0.025,0.5,0.975)) * summary(lab5.model1)$coefficients["diff.t-h.BRS",2])

## Plot network graph

# compute node degree
deg <- degree(g_5, mode = "all")

# set node size based on degree
V(g_5)$size <- deg*3

# default labels are node ids, set them to NA
V(g_5)$label <- NA

# change default arrow size and set edge color to be gray
E(g_5)$arrow.size <- .1
E(g_5)$edge.color <- "gray80"

# set layout of graph using Fruchterman-Reingold layout algorithm
l <- layout_with_fr(g_5)

# rescale graph
l <- norm_coords(l, ymin = -0.95, ymax = 0.95, xmin = -0.95, xmax = 0.95)

# plot the network graph
plot(g_5, rescale = FALSE, layout = l*1.5, vertex.color = "black")