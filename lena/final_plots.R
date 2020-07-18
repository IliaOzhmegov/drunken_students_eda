# Loading libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(extrafont)
library(tidyverse)
library(ggmosaic)

# Load the data frame.
# df <- readRDS("data/student-full.rds")

## Saving plots

source("common/load_functions.R")

# Jitter plot: Dalc, Age, sex -- reversed

p1 <- full_df %>% 
ggplot(aes(x=age, y=Dalc, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#F8766D", "#00BFC4"), name="Gender", labels=c("Female", "Male"))+
  theme_bw()+
  xlab("Age")+
  ylab("Alcohol consumption")+
  ggtitle("Weekday alcohol consumption per Age and Gender")+
  theme(plot.title = element_text(hjust = 0.5))
p1

save_plot(path="lena/pics/png/1.png", plot=p1)

# Jitter plot: Walc, Age, sex -- reversed
p2 <- full_df %>% 
ggplot(aes(x=age, y=Walc, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#F8766D", "#00BFC4"), name="Gender", labels=c("Female", "Male"))+
  theme_bw()+
  xlab("Age")+
  ylab("Alcohol consumption")+
  ggtitle("Weekend alcohol consumption per Age and Gender")+
  theme(plot.title = element_text(hjust = 0.5))
p2

save_plot(path="lena/pics/png/2.png", plot=p2)

# Percentage Address bar plot
# Dalc
p3 <- full_df %>% 
  group_by(address, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>% 
  ggplot(aes(x=factor(Dalc), y = perc*100, fill=address))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), labels = c('Rural', 'Urban'))+
  labs(fill = "Area")+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Weekday alcohol consumption depending on the Area")+
  theme(legend.position = c(0.9, 0.9), legend.box = "vertical") +
  theme(plot.title = element_text(hjust = 0.5))
p3

# Walc
p4 <- full_df %>% 
  group_by(address, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>% 
  ggplot(aes(x=factor(Walc), y = perc*100, fill=address))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), labels = c('Rural', 'Urban'))+
  labs(fill = "Area")+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Weekend alcohol consumption depending on the Area")+
  theme(legend.position = c(0.9, 0.9), legend.box = "vertical") +
  theme(plot.title = element_text(hjust = 0.5))

p4
# Arrange plots side-by-side

p<-grid.arrange(p3, p4, ncol=2)

save_plot(path="lena/pics/png/3.png", plot=p)


## Bar plot. % of students of different fam. sizes and alc. consumption.
## Daily alc. consumption
p5 <- full_df %>% 
  group_by(famsize, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>% 
  ggplot(aes(x=factor(Dalc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), 
                    labels = c('> 3', '≤ 3'))+
  labs(fill = "Family size")+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Weekday alcohol consumption per students' family size")+
  theme(legend.position = c(0.82, 0.9), legend.box = "vertical") +
  theme(plot.title = element_text(hjust = 0.5))
p5

# Bar plot. % of students of different fam. sizes and alc. consumption.
## Weekend alc. consumption
p6 <- full_df %>% 
  group_by(famsize, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>% 
  ggplot(aes(x=factor(Walc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), 
                    labels = c('> 3', '≤ 3'))+
  labs(fill = "Family size")+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Weekend alcohol consumption per students' family size")+
  theme(legend.position = c(0.82, 0.9), legend.box = "vertical") +
  theme(plot.title = element_text(hjust = 0.5))
p6

# Arrange plots
# Side-by-side
p <- grid.arrange(p5, p6, ncol=2)

save_plot(path="lena/pics/png/4.png", plot=p)



## Overall plot: Free time/ Go out time vs Salc

# research over facet_grid

p7 <- full_df %>% 
  ggplot(aes(y=freetime, x=Salc, color=Salc, group=freetime)) +
  geom_count() + 
  scale_size(range = c(3, 15)) +
  scale_color_manual(values=colours_9, name="Total \nalcohol \nconsumption") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing.x=unit(0.5, "lines")) +
  facet_grid(. ~ goout, switch="both") +
  labs(x = "Goout",
       size = "")+
  xlab("Go Out Time")+
  ylab("Free Time")+
  ggtitle("Alcohol consumption depending on students' free time and go out time")+
  theme(plot.title = element_text(hjust = 0.5))

p7

save_plot(path="lena/pics/png/5.png", plot=p7)



## Free time:

p8 <- full_df %>% 
  group_by(freetime, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count) %>% 
  ggplot(aes(x=factor(freetime), y = count, fill=Dalc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Greens")+
  coord_flip()+
  theme_bw()+
  xlab("Free time")+
  ylab("Number of students")+
  labs(fill = "Alcohol \nConsumption")+
  ggtitle("Weekday alcohol consumption \ndepending on students' free time") +
  theme(legend.position = c(0.9, 0.3), 
        legend.box = "vertical", 
        legend.key.size = unit(0.2, "cm")) 
p8

p9 <- full_df %>% 
  group_by(freetime, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count) %>% 
  ggplot(aes(x=factor(freetime), y = count, fill=Walc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Greens")+
  coord_flip()+
  theme_bw()+
  xlab("Free time")+
  ylab("Number of students")+
  labs(fill = "Alcohol \nConsumption")+
  ggtitle("Weekend alcohol consumption \ndepending on students' free time") +
  theme(legend.position="none") 
p9



# Go out time
p10 <- full_df %>% 
  group_by(goout, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count) %>% 
  ggplot(aes(x=factor(goout), y = count, fill=Dalc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Oranges")+
  coord_flip()+
  theme_bw()+
  xlab("Go out time")+
  ylab("Number of students")+
  labs(fill = "Alcohol \nConsumption")+
  ggtitle("Weekday alcohol consumption \ndepending students' go out time") +
  theme(legend.position = c(0.9, 0.3), 
        legend.box = "vertical",
        legend.key.size = unit(0.2, "cm")) 
p10

p11 <- full_df %>% 
  group_by(goout, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count) %>% 
  ggplot(aes(x=factor(goout), y = count, fill=Walc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_fill_brewer(palette="Oranges")+
  coord_flip()+
  theme_bw()+
  xlab("Go out time")+
  ylab("Number of students")+
  labs(fill = "Alcohol \nConsumption")+
  ggtitle("Weekend alcohol consumption \ndepending students' go out time") +
  theme(legend.position="none") 
p11

# Arrange plots

p <- grid.arrange(p8, p9, p10, p11, nrow=2, ncol=2)

save_plot(path="lena/pics/png/6.png", plot=p, w=13, h=7.8)


#### Other dot plot: Final grade, gender, Dalc/Walc ###
# Dalc
p12 <- full_df %>% 
  ggplot(aes(x=Dalc %>% as.factor(),  y=G3, fill=gender)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               position = "dodge",
               dotsize = 0.3, binwidth = 0.5)+
  theme_bw()+
  labs(fill = "Gender")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), labels = c('Female', 'Male'))+
  xlab("Alcohol consumption")+
  ylab("Final Grade")+
  ggtitle("Grades by gender and weekday alcohol consumption")+
  theme(plot.title = element_text(hjust = 0.5))
p12


# Walc
p13 <- full_df %>% 
  ggplot(aes(x=Walc %>% as.factor(),  y=G3, fill=gender)) +
  geom_dotplot(binaxis = "y", 
               stackdir = "center", 
               position = "dodge",
               dotsize = 0.3, binwidth = 0.5)+
  theme_bw()+
  labs(fill = "Gender")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4"), labels = c('Female', 'Male'))+
  xlab("Alcohol consumption")+
  ylab("Final Grade")+
  ggtitle("Grades by gender and weekend alcohol consumption")+
  theme(plot.title = element_text(hjust = 0.5))
p13

### 





