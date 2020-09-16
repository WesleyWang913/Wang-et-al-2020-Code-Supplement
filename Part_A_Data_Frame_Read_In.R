#Add in the directories
#-----
library(Boruta) #randomForest system
library(broom) #To derive p-value of linear model
library(factoextra) #For ggplots
library(MASS) #LDA
library(caret) #Packages for machine learning
library(randomForest)
library(e1071)
library(doParallel)
library(rminer) #Derive importance in learning algorithms
library(sp)  #get the data from the polygon
library(rgeos)
library(car) #fit ellipse to poincare
library(ggplot2)
library(ggpubr)
library(ggcorrplot)


#Load all mice to generate data frames
#----- 
#P1 Control Baseline
setwd("/P1 Baseline Directory")
data.files = list.files(pattern = "*.csv")
for (i in 1:length(data.files)) {
  name <-paste("P1DataControl", i, sep="")
  assign(name, read.csv(data.files[i])[c("f", "NTVb", "NPIFb", "NPEFb", "Ti", "Te", "Tr")])
}


#P21 Control Baseline
setwd/P21 Baseline Directory ")
data.files = list.files(pattern = "*.csv")
for (i in 1:length(data.files)) {
  name <-paste("P21DataControl", i, sep="")
  assign(name, read.csv(data.files[i])[c("f", "NTVb", "NPIFb", "NPEFb", "Ti", "Te", "Tr")])
}

#P21 Control Hypercapnic
setwd/P21 Hyper Directory ")
data.files = list.files(pattern = "*.csv")
for (i in 1:length(data.files)) {
  name <-paste("P21DataControlHyper", i, sep="")
  assign(name, read.csv(data.files[i])[c("f", "NTVb", "NPIFb", "NPEFb", "Ti", "Te", "Tr")])
}

#P56 Control Baseline
setwd("/P56 Baseline Directory ")
data.files = list.files(pattern = "*.csv")
for (i in 1:length(data.files)) {
  name <-paste("P56DataControl", i, sep="")
  assign(name, read.csv(data.files[i])[c("f", "NTVb", "NPIFb", "NPEFb", "Ti", "Te", "Tr")])
}

#P56 Control Hypercapnic
setwd("/P56 Hyper Directory")
data.files = list.files(pattern = "*.csv")
for (i in 1:length(data.files)) {
  name <-paste("P56DataControlHyper", i, sep="")
  assign(name, read.csv(data.files[i])[c("f", "NTVb", "NPIFb", "NPEFb", "Ti", "Te", "Tr")])
}

