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

![network graph of student's connections in lab 2](Grit_plot.jpeg)
