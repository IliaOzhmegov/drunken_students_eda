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

drunk_students_df <- read.csv("DrunkStudentsFullDF.csv")
drunk_students_math <- read.csv("student-mat.csv")

plot(drunk_students_df$sex, drunk_students_df$Dalc.math)
plot(drunk_students_math$sex, drunk_students_math$Dalc)

daily.alc.math <- as.factor(drunk_students_df$Dalc.math)

plot(daily.alc.math)

# Data preprovessing

drunk_students_df$Dalc.math <- as.factor(drunk_students_df$Dalc.math)      
drunk_students_df$Dalc.math <- mapvalues(drunk_students_df$Dalc.math, 
                              from = 1:5, 
                              to = c("Very Low", "Low", "Medium", "High", "Very High"))

drunk_students_df$Walc.math <- as.factor(drunk_students_df$Walc.math)      
drunk_students_df$Walc.math <- mapvalues(drunk_students_df$Walc.math, 
                              from = 1:5, 
                              to = c("Very Low", "Low", "Medium", "High", "Very High"))

ggplot2::ggplot(drunk_students_df, aes(x=sex, y=Dalc.math)) + geom_jitter() 
ggplot2::ggplot(drunk_students_df, aes(x=sex, y=Dalc.math, group=sex)) + geom_boxplot()
ggplot2::ggplot(drunk_students_df, aes(x=sex, y=Dalc.math)) + facet_grid(~ sex)

ggplot(drunk_students_df, aes(x=sex, fill=as.factor(Dalc.math))) + geom_bar()




