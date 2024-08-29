#------------------------------------------------------------------------------
# Name date
#------------------------------------------------------------------------------

setwd("~/R/Base_R")

install.packages('tidyverse')
install.packages('ggpubr')
install.packages('patchwork')
install.packages('cowplot')
install.packages('viridis')
install.packages('usethis')
#install.packages('PerformaceAnalytics')
#install.packages('lme4')

library('tidyverse')
library('ggpubr')
library('patchwork')
library('cowplot')
library('viridis')
#library('PerformanceAnalytics')
#library('lme4')

#------------------------------------------------------------------------------

#To mutate the data set at once without having to use multiple commands
data <- read.csv("filename.csv", header=T, sep=",") %>%
  mutate_if(is.character,as.factor) %>%
  mutate_at(c('A', 'B'), as.factor)

#To create a subset of the data that contain only "x" from the column A
retval <- subset(data, A=="x")

#Shows all the options within one factor
levels(data$A)

#------------------------------------------------------------------------------

#Visualisation

#Plots
ggplot(data=data, aes(x=as.factor(A), y=B)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + xlab("xxx") +
  geom_point()
 #geom_bar()