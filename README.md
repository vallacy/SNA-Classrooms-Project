# Social Network Analysis (SNA) in Introductory Statistics Classrooms Project

## Project Description
I lead a team of researchers to collect and analyze data on students' connections with one another in introductory statistics classes to see whether classroom connectedness within the lab section of statistics courses was associated with students' levels of stress, anxiety, depression, grit, and/or resilience.

We collected data at three time points across the semester. The first time point was a few weeks after the beginning of the semester. The second time point was around mid-terms. The third time point was collected at the end of the semester. Students were given a link to complete the survey via Google Forms and completed it on their own time.

## Measures

We created a social network survey, asking students 1) if they knew each of the other students in their lab section and 2) how they knew that person. We also had students complete the Perceived Stress Scale (PSS), the depression and anxiety subscales of the Depression, Anxiety, and Stress Scales (DASS-21), the Short Grit Scale (GRIT-S), and the Brief Resilience Scale (BRS).

## Analyses

I used exponential random graph models (ERGMs) to investigate the association between students' connectedness and their levels of stress, anxiety, depression, grit, and/or resilience. Networks were fairly sparse and ERGMs were only used on data collected at the final time point, when networks tended to be the most connected.

Example code for an ERGM looking at a small difference in scores on the measures:
~~~ R
lab1.model1 <- ergm(lab1.t3 ~ edges + mutual + gwodegree(decay=1, fixed=TRUE) 
                    + smalldiff("PSS", cutoff=11)
                    + smalldiff("DASS_depress", cutoff=5)  
                    + smalldiff("DASS_anx", cutoff=5)
                    + smalldiff("Grit", cutoff=8)
                    + smalldiff("BRS", cutoff=6))

summary(lab1.model1)
~~~

## Results
We found that students were not very well-connected within their lab sections and these sparse networks resulted in a lack of association between students' connections and their levels of stress, anxiety, depression, grit, and/or resilience. A few labs were more well-connected and findings from those labs suggest that students may connect with other students who have similar levels of resilience and/or dissimilar levels of grit.

Below is a plot of the student network in lab 2. Colors indicate a student's score on the grit scale: red indicates a low score, blue indicates a high score, and purple is an in-between score. Node size is based on degree, i.e., how many connections a student has in the lab. The more connections, the larger the node.

![network graph of student's connections in lab 2](Grit_plot.jpeg)

Code to create above plot:
~~~ R
########################
## Grit Network Graph ##
########################

#load library
library(RColorBrewer)

#generate color gradient starting at red and ending at blue
colfunc <- colorRampPalette(c("red", "blue"))
colfunc(18)

#plot gradient to check colors
plot(rep(1,18),col=colfunc(18),pch=19,cex=3)

#assign generated colors to object 'colors'
colors <- c("#FF0000", "#F0000F", "#E1001E", "#D2002C", "#C3003C", 
            "#B3004B", "#A50059", "#960069", "#870078", "#780087", 
            "#690096", "#5900A5", "#4B00B3", "#3C00C3", "#2D00D2",
            "#1E00E1", "#0F00F0", "#0000FF")

#color the nodes based on level of Grit using generated color gradient
V(net.l2.t3)$color <- colors[V(net.l2.t3)$Grit]

#compute node degree
deg <- degree(net.l2.t3, mode="all")

#set node size based on degree
V(net.l2.t3)$size <- deg*3

#default labels are node ids, set them to NA
V(net.l2.t3)$label <- NA

#change default arrow size and set edge color to be gray
E(net.l2.t3)$arrow.size <- .2
E(net.l2.t3)$edge.color <- "gray80"

#set layout of graph using Fruchterman-Reingold layout algorithm
l <- layout_with_fr(net.l2.t3)

#rescale graph
l <- norm_coords(l, ymin=-0.95, ymax=0.95, xmin=-0.95, xmax=0.95)

#plot the network graph
plot(net.l2.t3, rescale=FALSE, layout=l*1.5)
~~~
