data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE, fileEncoding="UTF-8-BOM") #choose RMforChap1.csv
attach(data)

library(ggplot2)
library(tidyverse)
library(dplyr)