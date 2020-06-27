##########################################
## SNA in Stats Classrooms Project Code ##
######### ERGMs at time point 3 ##########
######## Created by Valerie Ryan #########
######### Last Updated 6/27/2020 #########
##########################################
#rm(list=ls())

#load packages
library(igraph)
library(network)
library(ergm)

#set working directory
setwd("C:/Users/Your Directory")

##################
## Lab 1 Time 3 ##
##################

####################
## Importing Data ##
####################

#import adjacency matrix
lab1.t3 = read.csv("lab 1 time 3 mini.csv", header=TRUE, row.names = 1, check.names = FALSE)

#manipulate data so it is a matrix, graph, and edgelist
m_1_t3 = as.matrix(lab1.t3)
g_1_t3 = graph.adjacency(m_1_t3, mode = "directed", weighted = NULL)
e.list_1_t3 <- get.edgelist(g_1_t3, names = TRUE)

#calculate descriptive network characteristics
transitivity(g_1_t3)
edge_density(simplify(g_1_t3), loops=FALSE)
mean(betweenness(g_1_t3))
mean(degree(g_1_t3))
assortativity.degree(g_1_t3,directed=T)

#import attribute data
t3 = read.csv("lab 1 time 3 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#add attribute data to graph object
g_t3_demo = graph.data.frame(e.list_1_t3, directed = "TRUE", vertices = t3)

#prep data for analysis
A3 <- get.adjacency(g_1_t3)
lab1.t3 <- network::as.network(as.matrix(A3), directed=TRUE)

t3.demo <- get.data.frame(g_t3_demo, what = "vertices")
network::set.vertex.attribute(lab1.t3, "PSS", t3.demo$PSS_total)
network::set.vertex.attribute(lab1.t3, "DASS_depress", t3.demo$DASS_depress)
network::set.vertex.attribute(lab1.t3, "DASS_anx", t3.demo$DASS_anxiety)
network::set.vertex.attribute(lab1.t3, "DASS_tot", t3.demo$DASS_total)
network::set.vertex.attribute(lab1.t3, "Grit", t3.demo$Grit_total)
network::set.vertex.attribute(lab1.t3, "BRS", t3.demo$BRS_total)

###########
## ERGMs ##
###########

#small difference in scores model
lab1.model1 <- ergm(lab1.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + smalldiff("PSS", cutoff=11)
                    + smalldiff("DASS_depress", cutoff=5)  
                    + smalldiff("DASS_anx", cutoff=5)
                    + smalldiff("Grit", cutoff=8)
                    + smalldiff("BRS", cutoff=6))

summary(lab1.model1)

#absolute difference in scores model
lab1.model2 <- ergm(lab1.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + diff("PSS", pow=1, dir="t-h", sign.action="abs")
                    + diff("DASS_depress", pow=1, dir="t-h", sign.action="abs")  
                    + diff("DASS_anx", pow=1, dir="t-h", sign.action="abs")
                    + diff("Grit", pow=1, dir="t-h", sign.action="abs")
                    + diff("BRS", pow=1, dir="t-h", sign.action="abs"))

summary(lab1.model2)

##################
## Lab 2 Time 3 ##
##################

####################
## Importing Data ##
####################

#import adjacency matrix
lab2.t3 = read.csv("lab 2 time 3 mini.csv", header=TRUE, row.names = 1, check.names = FALSE)

#manipulate data so it is a matrix, graph, and edgelist
m_2_t3 = as.matrix(lab2.t3)
g_2_t3 = graph.adjacency(m_2_t3, mode = "directed", weighted = NULL)
e.list_2_t3 <- get.edgelist(g_2_t3, names = TRUE)

#calculate descriptive network characteristics
transitivity(g_2_t3)
edge_density(simplify(g_2_t3), loops=FALSE)
mean(betweenness(g_2_t3))
mean(degree(g_2_t3))
assortativity.degree(g_2_t3,directed=T)

#import attribute data
t3 = read.csv("lab 2 time 3 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#add attribute data to graph object
g_t3_demo = graph.data.frame(e.list_2_t3, directed = "TRUE", vertices = t3)

#prep data for analysis
A3 <- get.adjacency(g_2_t3)
lab2.t3 <- network::as.network(as.matrix(A3), directed=TRUE)

t3.demo <- get.data.frame(g_t3_demo, what = "vertices")
network::set.vertex.attribute(lab2.t3, "PSS", t3.demo$PSS_total)
network::set.vertex.attribute(lab2.t3, "DASS_depress", t3.demo$DASS_depress)
network::set.vertex.attribute(lab2.t3, "DASS_anx", t3.demo$DASS_anxiety)
network::set.vertex.attribute(lab2.t3, "DASS_tot", t3.demo$DASS_total)
network::set.vertex.attribute(lab2.t3, "Grit", t3.demo$Grit_total)
network::set.vertex.attribute(lab2.t3, "BRS", t3.demo$BRS_total)

###########
## ERGMs ##
###########

#small difference in scores model
lab2.model1 <- ergm(lab2.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + smalldiff("PSS", cutoff=11)
                    + smalldiff("DASS_depress", cutoff=5)  
                    + smalldiff("DASS_anx", cutoff=5)
                    + smalldiff("Grit", cutoff=8)
                    + smalldiff("BRS", cutoff=6))

summary(lab2.model1)

#absolute difference in scores model
lab2.model2 <- ergm(lab2.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + diff("PSS", pow=1, dir="t-h", sign.action="abs")
                    + diff("DASS_depress", pow=1, dir="t-h", sign.action="abs")  
                    + diff("DASS_anx", pow=1, dir="t-h", sign.action="abs")
                    + diff("Grit", pow=1, dir="t-h", sign.action="abs")
                    + diff("BRS", pow=1, dir="t-h", sign.action="abs"))

summary(lab2.model2)

##################
## Lab 3 Time 3 ##
##################

####################
## Importing Data ##
####################

#import adjacency matrix
lab3.t3 = read.csv("lab 3 time 3 mini.csv", header=TRUE, row.names = 1, check.names = FALSE)

#manipulate data so it is a matrix, graph, and edgelist
m_3_t3 = as.matrix(lab3.t3)
g_3_t3 = graph.adjacency(m_3_t3, mode = "directed", weighted = NULL)
e.list_3_t3 <- get.edgelist(g_3_t3, names = TRUE)

#calculate descriptive network characteristics
transitivity(g_3_t3)
edge_density(simplify(g_3_t3), loops=FALSE)
mean(betweenness(g_3_t3))
mean(degree(g_3_t3))
assortativity.degree(g_3_t3,directed=T)

#import attribute data
t3 = read.csv("lab 3 time 3 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#add attribute data to graph object
g_t3_demo = graph.data.frame(e.list_3_t3, directed = "TRUE", vertices = t3)

#prep data for analysis
A3 <- get.adjacency(g_3_t3)
lab3.t3 <- network::as.network(as.matrix(A3), directed=TRUE)

t3.demo <- get.data.frame(g_t3_demo, what = "vertices")
network::set.vertex.attribute(lab3.t3, "PSS", t3.demo$PSS_total)
network::set.vertex.attribute(lab3.t3, "DASS_depress", t3.demo$DASS_depress)
network::set.vertex.attribute(lab3.t3, "DASS_anx", t3.demo$DASS_anxiety)
network::set.vertex.attribute(lab3.t3, "DASS_tot", t3.demo$DASS_total)
network::set.vertex.attribute(lab3.t3, "Grit", t3.demo$Grit_total)
network::set.vertex.attribute(lab3.t3, "BRS", t3.demo$BRS_total)

###########
## ERGMs ##
###########

#small difference in scores model
lab3.model1 <- ergm(lab3.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + smalldiff("PSS", cutoff=11)
                    + smalldiff("DASS_depress", cutoff=5)  
                    + smalldiff("DASS_anx", cutoff=5)
                    + smalldiff("Grit", cutoff=8)
                    + smalldiff("BRS", cutoff=6))

summary(lab3.model1)

#absolute difference in scores model
lab3.model2 <- ergm(lab3.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + diff("PSS", pow=1, dir="t-h", sign.action="abs")
                    + diff("DASS_depress", pow=1, dir="t-h", sign.action="abs")  
                    + diff("DASS_anx", pow=1, dir="t-h", sign.action="abs")
                    + diff("Grit", pow=1, dir="t-h", sign.action="abs")
                    + diff("BRS", pow=1, dir="t-h", sign.action="abs"))

summary(lab3.model2)

##################
## Lab 4 Time 3 ##
##################

####################
## Importing Data ##
####################

#import adjacency matrix
lab4.t3 = read.csv("lab 4 time 3 mini.csv", header=TRUE, row.names = 1, check.names = FALSE)

#manipulate data so it is a matrix, graph, and edgelist
m_4_t3 = as.matrix(lab4.t3)
g_4_t3 = graph.adjacency(m_4_t3, mode = "directed", weighted = NULL)
e.list_4_t3 <- get.edgelist(g_4_t3, names = TRUE)

#calculate descriptive network characteristics
transitivity(g_4_t3)
edge_density(simplify(g_4_t3), loops=FALSE)
mean(betweenness(g_4_t3))
mean(degree(g_4_t3))
assortativity.degree(g_4_t3,directed=T)

#import attribute data
t3 = read.csv("lab 4 time 3 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#add attribute data to graph object
g_t3_demo = graph.data.frame(e.list_4_t3, directed = "TRUE", vertices = t3)

#prep data for analysis
A3 <- get.adjacency(g_4_t3)
lab4.t3 <- network::as.network(as.matrix(A3), directed=TRUE)

t3.demo <- get.data.frame(g_t3_demo, what = "vertices")
network::set.vertex.attribute(lab4.t3, "PSS", t3.demo$PSS_total)
network::set.vertex.attribute(lab4.t3, "DASS_depress", t3.demo$DASS_depress)
network::set.vertex.attribute(lab4.t3, "DASS_anx", t3.demo$DASS_anxiety)
network::set.vertex.attribute(lab4.t3, "DASS_tot", t3.demo$DASS_total)
network::set.vertex.attribute(lab4.t3, "Grit", t3.demo$Grit_total)
network::set.vertex.attribute(lab4.t3, "BRS", t3.demo$BRS_total)

###########
## ERGMs ##
###########

#small difference in scores model
lab4.model1 <- ergm(lab4.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + smalldiff("PSS", cutoff=11)
                    + smalldiff("DASS_depress", cutoff=5)  
                    + smalldiff("DASS_anx", cutoff=5)
                    + smalldiff("Grit", cutoff=8)
                    + smalldiff("BRS", cutoff=6))

summary(lab4.model1)

#absolute difference in scores model
lab4.model2 <- ergm(lab4.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + diff("PSS", pow=1, dir="t-h", sign.action="abs")
                    + diff("DASS_depress", pow=1, dir="t-h", sign.action="abs")  
                    + diff("DASS_anx", pow=1, dir="t-h", sign.action="abs")
                    + diff("Grit", pow=1, dir="t-h", sign.action="abs")
                    + diff("BRS", pow=1, dir="t-h", sign.action="abs"))

summary(lab4.model2)

##################
## Lab 5 Time 3 ##
##################

####################
## Importing Data ##
####################

#import adjacency matrix
lab5.t3 = read.csv("lab 5 time 3 mini.csv", header=TRUE, row.names = 1, check.names = FALSE)

#manipulate data so it is a matrix, graph, and edgelist
m_5_t3 = as.matrix(lab5.t3)
g_5_t3 = graph.adjacency(m_5_t3, mode = "directed", weighted = NULL)
e.list_5_t3 <- get.edgelist(g_5_t3, names = TRUE)

#calculate descriptive network characteristics
transitivity(g_5_t3)
edge_density(simplify(g_5_t3), loops=FALSE)
mean(betweenness(g_5_t3))
mean(degree(g_5_t3))
assortativity.degree(g_5_t3,directed=T)

#import attribute data
t3 = read.csv("lab 5 time 3 demo.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

#add attribute data to graph object
g_t3_demo = graph.data.frame(e.list_5_t3, directed = "TRUE", vertices = t3)

#prep data for analysis
A3 <- get.adjacency(g_5_t3)
lab5.t3 <- network::as.network(as.matrix(A3), directed=TRUE)

t3.demo <- get.data.frame(g_t3_demo, what = "vertices")
network::set.vertex.attribute(lab5.t3, "PSS", t3.demo$PSS_total)
network::set.vertex.attribute(lab5.t3, "DASS_depress", t3.demo$DASS_depress)
network::set.vertex.attribute(lab5.t3, "DASS_anx", t3.demo$DASS_anxiety)
network::set.vertex.attribute(lab5.t3, "DASS_tot", t3.demo$DASS_total)
network::set.vertex.attribute(lab5.t3, "Grit", t3.demo$Grit_total)
network::set.vertex.attribute(lab5.t3, "BRS", t3.demo$BRS_total)

###########
## ERGMs ##
###########

#small difference in scores model
lab5.model1 <- ergm(lab5.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + smalldiff("PSS", cutoff=11)
                    + smalldiff("DASS_depress", cutoff=5)  
                    + smalldiff("DASS_anx", cutoff=5)
                    + smalldiff("Grit", cutoff=8)
                    + smalldiff("BRS", cutoff=6))

summary(lab5.model1)

#absolute difference in scores model
lab5.model2 <- ergm(lab5.t3 ~ edges + mutual #+ gwodegree(decay=1, fixed=TRUE) 
                    + diff("PSS", pow=1, dir="t-h", sign.action="identity")
                    + diff("DASS_depress", pow=1, dir="t-h", sign.action="identity")  
                    + diff("DASS_anx", pow=1, dir="t-h", sign.action="identity")
                    + diff("Grit", pow=1, dir="t-h", sign.action="identity")
                    + diff("BRS", pow=1, dir="t-h", sign.action="identity"))

summary(lab5.model2)

##############################
## Models that did not work ##
##############################

#main effect of a covariate for out-degree model
lab1.model3 <- ergm(lab5.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE)
                    + nodeocov("PSS") 
                    + nodeocov("DASS_depress")
                    + nodeocov("DASS_anx")
                    + nodeocov("Grit")
                    + nodeocov("BRS"))

summary(lab1.model3)

#main effect of a covariate for in-degree model
lab1.model4 <- ergm(lab1.t3 ~ edges 
                    + nodeicov("PSS") 
                    + nodeicov("DASS_depress")
                    + nodeicov("DASS_anx")
                    + nodeicov("Grit")
                    + nodeicov("BRS"))

summary(lab1.model4)

#homophily model
lab1.model5 <- ergm(lab1.t3 ~ edges 
                    + nodematch("PSS") 
                    + nodematch("DASS_depress")
                    + nodematch("DASS_anx")
                    + nodematch("Grit")
                    + nodematch("BRS"))

summary(lab1.model5)
