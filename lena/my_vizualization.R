# Install devtools package, alluvial and waffle.

# install.packages("devtools")
# devtools::install_github("konradsemsch/aider")
# install.packages("alluvial")
# install.packages("waffle")

# Installing libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(waffle)
library(extrafont)


## Load dataframe ##

# df <- readRDS("data/student-full.rds")

# Data preprovessing

# df$Dalc <- as.factor(df$Dalc)      
# df$Dalc <- mapvalues(df$Dalc, from = 1:5, 
#                     to = c("Very Low", "Low", "Medium", "High", "Very High"))

# df$Walc <- as.factor(df$Walc)      
#df$Walc <- mapvalues(df$Walc, from = 1:5, 
#                     to = c("Very Low", "Low", "Medium", "High", "Very High"))


ggplot2::ggplot(df, aes(x=sex, y=Dalc)) + geom_jitter() 
ggplot2::ggplot(df, aes(x=sex, y=Dalc, group=sex)) + geom_boxplot()
# ggplot2::ggplot(df, aes(x=sex, y=Dalc)) + facet_grid(~ sex)

ggplot(df, aes(x=sex, fill=as.factor(Dalc))) + geom_bar()


