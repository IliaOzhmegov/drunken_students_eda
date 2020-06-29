# Install devtools package, alluvial and waffle.

# install.packages("devtools")
# devtools::install_github("konradsemsch/aider")
# install.packages("alluvial")
# install.packages("waffle")
# install.packages("plotly")
# install.packages("ggplotly")


# Installing libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(waffle)
library(extrafont)
library(plotly)



## Load dataframe ##

df <- readRDS("data/student-full.rds")

# Use this ones:

plot1 <- ggplot(df, aes(x=sex, y=Dalc, 
                        text=paste("Gender count: \nmales ", table(sex)[2], "\nfemales", table(sex)[1]))) + 
                        geom_jitter(stat = "identity", aes(color = Dalc))
plot1
ggplotly(plot1, tooltip = c("x", "y", "text"))


ggplot(df, aes(x=sex, y=Walc)) + geom_jitter(aes(color = Walc))
# ggplot(df, aes(x=sex, y=Dalc)) + geom_jitter(aes(color = Dalc))

ggplot2::ggplot(df, aes(x=sex, y=Dalc, group=sex)) + geom_boxplot()
# ggplot2::ggplot(df, aes(x=sex, y=Dalc)) + facet_grid(~ sex)

ggplot(df, aes(x=sex, fill=as.factor(Dalc))) + geom_bar()
# reorder factors
reversed.Dalc <- fct_rev(df$Dalc)
ggplot(df, aes(x=sex, fill = reversed.Dalc)) + geom_bar()
# or
## TODO: disp. as % of gender ##
ggplot(df, aes(x=sex, fill = reversed.Dalc), position = position_stack(reverse = TRUE)) + geom_bar()

## Alc. consumption and Area.
# Using jitter
ggplot(df, aes(x=Dalc, y=address, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Area where student lives")+
  ggtitle("Workday alcohol consumption per Area and Gender")

table(df$address)

# Bar plot
ggplot(df, aes(x=Dalc, fill=address))+
  geom_bar(position = "dodge")+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Area where student lives")+
  ggtitle("Workday alcohol consumption per Area and Gender")

prop_u <- (sum(table(df$address)[2])/sum(table(df$address)))*100
round(prop_u, 1)

prop_r <- (sum(table(df$address)[1])/sum(table(df$address)))*100
round(prop_r, 1)

# Percentage Address bar plot
# Use this one!
df1 <- df %>% 
  group_by(address, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ggplot(df1, aes(x=factor(Dalc), y = perc*100, fill=address))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Workday alcohol consumption depending on the Area")
df1

# Family size bar plot
ggplot(df, aes(x=Dalc, fill=famsize))+
  geom_bar(position = "dodge")+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Count of students")+
  ggtitle("Workday alcohol consumption depending on students' family size")



# Family size jitter
ggplot(df, aes(x=Dalc, y=famsize, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Count of students")+
  ggtitle("Workday alcohol consumption depending on students' family size")

## Bar plot
# Improve this one (% vise)
ggplot(df, aes(x=famsize, fill=Dalc))+
  geom_bar(position = "dodge", width = 0.5)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Family size")+
  ylab("Count of students")+
  ggtitle("Workday alcohol consumption depending on students' family size")

## Bar plot. % of students of different fam. sizes and alc. consumption.
## Daily alc. consumption
df2 <- df %>% 
  group_by(famsize, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ggplot(df2, aes(x=factor(Dalc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Workday alcohol consumption depending students' family size")
df2

# Bar plot. % of students of different fam. sizes and alc. consumption.
## Weekend alc. consumption
df3 <- df %>% 
  group_by(famsize, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ggplot(df3, aes(x=factor(Walc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Weekend alcohol consumption depending students' family size")


## look at: matlab color scheme

