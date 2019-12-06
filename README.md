# Patching-Home-Assignment
### Alexis De Los Reyes
### Home Assignment Patching 



####### PCA
# set working directory 

# load packages
library(psych)
library(lsr)
library(reshape2)
source("GraphPlot.R")
library(tidyverse)

# load data
# reshape data
PAQ_Alexis2 <-dcast(PAQ_Alexis, id~var, value.var = "value")


# delete id column
PAQ_Alexis2$id <- NULL
PAQ_Alexis2$age <- NULL
PAQ_Alexis2$sex <- NULL

#find any NA values
View(PAQ_Alexis2)
summary(PAQ_Alexis2)
describe(PAQ_Alexis2)
#missing variable = Q6_inferior

#impose missing value
PAQ_Alexis_NA = PAQ_Alexis2
PAQ_Alexis_NA$Q6_inferior[is.na(PAQ_Alexis_NA$Q6_inferior)]= mean(PAQ_Alexis_NA$Q6_inferior, na.rm = TRUE)
describe(PAQ_Alexis_NA)

#review cleaned data
summary(PAQ_Alexis_NA)
str(PAQ_Alexis_NA)
describe(PAQ_Alexis_NA)


#cor and cov
data_pcacor<-princomp(PAQ_Alexis_NA, cor=TRUE)
summary(data_pcacor, loadings=TRUE)

#proportion of variance = Comp 1, Comp 2, Comp 3, cumulative proportion of 0.78


# plot scree plot
openGraph()
screeplot(data_pcacor)


# plot biplot
openGraph()
biplot(data_pcacor)







########### Multi-dimensional scaling 
#load data
# N rows = 12, p columns = 12 

#load packages
library(MASS)
library(smacof)

Nation.distance <- sim2diss(Nations, method = 9)

dist <- dist(Nation.distance, method = "euclidian", diag = T)

Nations1 <- isoMDS(dist)

Nations.mds = isoMDS(dist) #Kruskal's Non-Metric Multidimensional Scaling 
Nations.mds$points
Nations.mds$stress #20.01, model fit in two dimensions 
#reference what you deem fair (Kruskal, 1964)
# above 20 is poor and 10-20 is fair

print(Nations.mds)
print(Nation.distance)

#plotting 

x1 = Nations.mds$points[,1]
y1 = Nations.mds$points[,2]
windows()
plot(x1, y1, xlab = "Coordinate 1", ylab = "Coordinate 2", type = "n")
text(x1, y1, labels = colnames(Nations), cex = 0.8)


#shepards plot, goodness of fit 
shepard.nations = Shepard(dist, Nations.mds$points)
windows()
plot(shepard.nations, pch = 20, xlab = "Dissimilarity", ylab = "Distance", xlim = range(shepard.nations$x), ylim = range(shepard.nations$x))
lines(shepard.nations$x, shepard.nations$yf, type = "s")







##### Power Analysis 

#load package

#try 1
library(pwr)
p.out <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50), sig.level = 0.05, power = 0.90, alternative = "greater")
openGraph()
plot(p.out)

#power sequence 
n <-seq(10,110,1)
power.out <- pwr.p.test(h = 0.5, n= n, sig.level = 0.05)
data.frame(n, power = sprintf("%.2f%%", power.out$power * 100))

#try 2
df(40,3,36,ncp = 0.125, log = FALSE)
pf(40,3,36, ncp = 0.125, lower.tail = TRUE, log.p = FALSE)
qf(.95, 3,36, ncp = 0.125, lower.tail = TRUE, log.p = FALSE)
rf(40,3,36, ncp = 0.125)
