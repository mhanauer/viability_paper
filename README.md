#Create Data Pathway
setwd("S:/Indiana Research & Evaluation/CCPE/Concept Mapping/Data/Participant Sorting Sheets/Formatted Participant Sorting Sheets")
#Import Participant Matricies
Sub1 = read.csv("Sub1.csv", header = FALSE)
Sub2 = read.csv("Sub2.csv", header = FALSE)
Sub3 = read.csv("Sub3.csv", header = FALSE)
Sub4 = read.csv("Sub4.csv", header = FALSE)
Sub5 = read.csv("Sub5.csv", header = FALSE)
Sub6 = read.csv("Sub6.csv", header = FALSE)
Sub7 = read.csv("Sub7.csv", header = FALSE)
Sub8 = read.csv("Sub8.csv", header = FALSE)
Sub9 = read.csv("Sub9.csv", header = FALSE)
Sub10 = read.csv("Sub10.csv", header = FALSE)
Sub11 = read.csv("Sub11.csv", header = FALSE)
Sub12 = read.csv("Sub12.csv", header = FALSE)
Sub13 = read.csv("Sub13.csv", header = FALSE)
Sub14 = read.csv("Sub14.csv", header = FALSE)
Sub15 = read.csv("Sub15_2.csv", header = FALSE)
Sub16 = read.csv("Sub16.csv", header = FALSE)
Sub17 = read.csv("Sub17.csv", header = FALSE)
Sub18 = read.csv("Sub18.csv", header = FALSE)
Sub19 = read.csv("Sub19.csv", header = FALSE)
Sub20 = read.csv("Sub20.csv", header = FALSE)
Sub21 = read.csv("Sub21.csv", header = FALSE)
Sub22 = read.csv("Sub22.csv", header = FALSE)
Sub24 = read.csv("Sub24.csv", header = FALSE)

#Create Aggregate Matrix
Sub.Aggregate = Sub1 + Sub2 + Sub3 + Sub4 + Sub5 + Sub6 + Sub7 + Sub8 + Sub9 + Sub10 + Sub11 + Sub12 + Sub13 + Sub14 + Sub15 + Sub16 + Sub17 + Sub18 + Sub19 + Sub20 + Sub21 + Sub22 + Sub24
head(Sub.Aggregate)

#Export New Dataset
write.csv(Sub.Aggregate, "Sub_Aggregate_Data.csv")

#Import New Dataset (Excluded column with participant Number)
Sub.Aggregate = read.csv("Sub_Aggregate_Data1.csv", header = TRUE)

#Install and Load Packages
library(factoextra)
library(MASS)
library(cluster)
library(ggpubr)
library(tidyverse)

#Identify Appropriate Method for Cluster Creation
hcWard = agnes(Sub.Aggregate, diss = FALSE, stand = FALSE, method = "ward")
hcC = agnes(Sub.Aggregate, diss = FALSE, stand = FALSE, method = "complete")
hcWeighted = agnes(Sub.Aggregate, diss = FALSE, stand = FALSE, method = "weighted")

hcWard$ac
hcC$ac
hcWeighted$ac

#Identify Appropriate Number of CLusters
fviz_nbclust(Sub.Aggregate, FUN = hcut, method = "wss")

fviz_nbclust(Sub.Aggregate, FUN = hcut, method = "silhouette")

gap_stat = clusGap(Sub.Aggregate, FUN = hcut, K.max = 10, B = 100)
fviz_gap_stat(gap_stat)

#Create 8 Cluster Divisions
hcTree = cutree(hcWard, k=8)
Sub.Aggregate %>%
  mutate(cluster = hcTree)

#Create Dataset with Cluster Designation
head(Sub.Aggregate)
Sub.AggregateTest = data.frame(Sub.Aggregate, hcTree)
head(Sub.AggregateTest)

#Create 8 Cluster Map
library(prettyR)
library(Hmisc)
library(pastecs)
library(tidyr)
library(readr)
library(ggrepel)

ClusterMap = fviz_cluster(list(data = Sub.Aggregate, cluster = hcTree), main = "Clusters of Barriers to Program Viability Validity", repel = TRUE, ellipse.alpha = 0.05, axes = c(1,2), geom = "point", xlab = FALSE, ylab = FALSE, show.clust.cent = F) +
  labs(fill = "Cluster", shape = "Cluster", color = "Cluster")
ClusterMap

ClusterMap2 = ClusterMap + 
  geom_text_repel(label = row.names(Sub.AggregateTest), point.padding = 0.5, size = 3.5) +
  theme_light() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ClusterMap2


describe(hcTree)


