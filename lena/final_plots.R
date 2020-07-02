# Loading libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(alluvial)
library(waffle)
library(extrafont)
library(tidyverse)
library(ggmosaic)

# Load the data frame.
df <- readRDS("data/student-full.rds")

# Jitter plot: Dalc, Age, sex -- reversed
full_df %>% 
ggplot(aes(x=age, y=Dalc, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Age")+
  ylab("Alcohol consumption")+
  ggtitle("Weekday alcohol consumption per Age and Gender")

# Jitter plot: Walc, Age, sex -- reversed
full_df %>% 
ggplot(aes(x=age, y=Walc, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Age")+
  ylab("Alcohol consumption")+
  ggtitle("Weekend alcohol consumption per Age and Gender")


# Percentage Address bar plot
# Dalc
df1 <- full_df %>% 
  group_by(address, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
df1 <- ggplot(df1, aes(x=factor(Dalc), y = perc*100, fill=address))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Weekday alcohol consumption depending on the Area")
df1


#Walc
df2 <- full_df %>% 
  group_by(address, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
df2 <- ggplot(df2, aes(x=factor(Walc), y = perc*100, fill=address))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Weekend alcohol consumption depending on the Area")
df2

# Arrange plots
# Side-by-side
grid.arrange(df1, df2, ncol=2)


## Bar plot. % of students of different fam. sizes and alc. consumption.
## Daily alc. consumption
df3 <- full_df %>% 
  group_by(famsize, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
df3 <- ggplot(df3, aes(x=factor(Dalc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Weekday alcohol consumption per students' family size")
df3

# Bar plot. % of students of different fam. sizes and alc. consumption.
## Weekend alc. consumption
df4 <- full_df %>% 
  group_by(famsize, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
df4 <- ggplot(df4, aes(x=factor(Walc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Weekend alcohol consumption per students' family size")
df4

# Arrange plots
# Side-by-side
grid.arrange(df3, df4, ncol=2)


## Free time:

df5 <- full_df %>% 
  group_by(freetime, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
df5 <- ggplot(df5, aes(x=factor(freetime), y = count, fill=Dalc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Greens")+
  coord_flip()+
  theme_bw()+
  xlab("Free time")+
  ylab("Number of students")+
  ggtitle("Weekday alcohol consumption depending on students' free time")
df5

df6 <- full_df %>% 
  group_by(freetime, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
df6 <- ggplot(df6, aes(x=factor(freetime), y = count, fill=Walc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Greens")+
  coord_flip()+
  theme_bw()+
  xlab("Free time")+
  ylab("Number of students")+
  ggtitle("Weekend alcohol consumption depending on students' free time")
df6



# Go out
df7 <- full_df %>% 
  group_by(goout, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
df7 <- ggplot(df7, aes(x=factor(goout), y = count, fill=Dalc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Oranges")+
  coord_flip()+
  theme_bw()+
  xlab("Go out time")+
  ylab("Number of students")+
  ggtitle("Weekday alcohol consumption depending students' go out time")
df7

df8 <- full_df %>% 
  group_by(goout, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
df8 <- ggplot(df8, aes(x=factor(goout), y = count, fill=Walc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Oranges")+
  coord_flip()+
  theme_bw()+
  xlab("Go out time")+
  ylab("Number of students")+
  ggtitle("Weekend alcohol consumption depending students' go out time")
df8

# Arrange plots

grid.arrange(df5, df6, df7, df8, nrow=2, ncol=2)


#### Other dot plot: Final grade, gender, Dalc/Walc ###
# Dalc
full_df %>% 
  ggplot(aes(x=Dalc %>% as.factor(),  y=G3, fill=gender)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               position = "dodge",
               dotsize = 0.3, binwidth = 0.5)+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Final Grade")+
  ggtitle("Grades by gender and Weekday alcohol consumption")


# Walc
full_df %>% 
  ggplot(aes(x=Walc %>% as.factor(),  y=G3, fill=gender)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               position = "dodge",
               dotsize = 0.3, binwidth = 0.5)+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Final Grade")+
  ggtitle("Grades by gender and Weekend alcohol consumption")



