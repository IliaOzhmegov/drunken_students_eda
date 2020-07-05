# Install devtools package, alluvial and waffle.

# install.packages("devtools")
# devtools::install_github("konradsemsch/aider")
# install.packages("alluvial")
# install.packages("waffle")
# install.packages("plotly")
# install.packages("ggplotly")
# install.packages("ggmosaic")

# Loading libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(alluvial)
library(waffle)
library(extrafont)
library(plotly)
library(tidyverse)
library(ggmosaic)


## Load dataframe ##

df <- readRDS("data/student-full.rds")

# Use this ones:

plot1 <- ggplot(df, aes(x=gender, y=Dalc, 
                        text=paste("Gender count: \n -males", table(gender)[2], "\n -females", table(gender)[1]))) + 
                        geom_jitter(stat = "identity", aes(color = Dalc))
plot1
# plot1 + geom_text()
ggplotly(plot1, tooltip = c("x", "y", "text"))

ggplot2::ggplot(df, aes(x=gender, y=Dalc, label= "sex")) + 
  geom_jitter(stat = "identity", aes(color = Dalc)) ##???

# Walc
ggplot(df, aes(x=gender, y=Walc)) + geom_jitter(aes(color = Walc))
# ggplot(df, aes(x=sex, y=Dalc)) + geom_jitter(aes(color = Dalc))

## Dot plot: Age, Dalc/Walc
ggplot(df, aes(x=Dalc, y=age)) + geom_count()+scale_size(range=c(1, 10))
ggplot(df, aes(x=Walc, y=age)) + geom_count()+scale_size(range=c(1, 10))


ggplot(df, aes(x=Dalc, y=age)) + 
  geom_count(aes(color = ..n.., size = ..n..))+
  scale_size(range=c(1, 20))+
  guides(color = 'legend')

## Dot plot: Dalc/Walc, Gender ## GOOD ##
ggplot(df, aes(x=gender, y=Dalc)) + 
  geom_count(aes(color = ..n.., size = ..n..))+
  scale_size(range=c(5, 25))+
  guides(color = 'legend')

## Dot plot: Dalc/Walc, Gender + Grade (G3)  #### Do side by side by gender"
ggplot(df, aes(x=Dalc, y=G3)) + 
  geom_count(aes(color = age, size = ..n..), position = "identity")+
  scale_size(range=c(1, 12))+
  guides(color = 'legend')



# Jitter plot: Dalc, Age, sex
ggplot(df, aes(x=Dalc, y=age, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Age")+
  ggtitle("Workday alcohol consumption per Age and Gender")

# Jitter plot: Dalc, Age, sex -- reversed
ggplot(df, aes(x=age, y=Dalc, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Age")+
  ggtitle("Weekday alcohol consumption per Age and Gender")

# Jitter plot: Walc, Age, sex -- reversed
ggplot(df, aes(x=age, y=Walc, color=gender))+
  geom_jitter()+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Age")+
  ggtitle("Weekend alcohol consumption per Age and Gender")




## Dot plot: Age, Dalc/Walc, Gender
ggplot(df, aes(x=Dalc, y=age)) + geom_point()+scale_size_area()

## Continuous distribution??:
ggplot(df, aes(x=Dalc, y=age))+geom_hex()

## Tile distribution??:
ggplot(df, aes(x=Dalc, y=age))+geom_raster(aes(fill=gender))


ggplot2::ggplot(df, aes(x=gender, y=Dalc, group=gender)) + geom_boxplot()
# ggplot2::ggplot(df, aes(x=sex, y=Dalc)) + facet_grid(~ sex)

ggplot(df, aes(x=gender, fill=as.factor(Dalc))) + geom_bar()
# reorder factors
reversed.Dalc <- fct_rev(df$Dalc)
reversed.Walc <- fct_rev(df$Walc)
ggplot(df, aes(x=gender, fill = reversed.Dalc)) + geom_bar()
ggplot(df, aes(x=gender, fill = reversed.Walc)) + geom_bar()
# or
## TODO: disp. as % of gender ##
ggplot(df, aes(x=gender, fill = Dalc), position = position_stack(reverse = TRUE)) + geom_bar()
ggplot(df, aes(x=gender, fill = Walc), position = position_stack(reverse = TRUE)) + geom_bar()



## Bar plot 
df %>% 
  count(gender, Dalc) %>% 
  ggplot(aes(x = gender, y = n, fill = Dalc))+
  geom_col(position = "dodge")


## Alc. consumption and Area.
# Using jitter
ggplot(df, aes(x=Dalc, y=address, color=gender))+
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
  xlab("Alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Workday alcohol consumption depending on the Area")
df1

df1_W <- df %>% 
  group_by(address, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ggplot(df1_W, aes(x=factor(Walc), y = perc*100, fill=address))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Alcohol consumption")+
  ylab("Population of students per Area, %")+
  ggtitle("Weekend alcohol consumption depending on the Area")
df1_W

# Arrange plots

grid.arrange(df1, df1_W, nrow=2)

# Family size bar plot
ggplot(df, aes(x=Dalc, fill=famsize))+
  geom_bar(position = "dodge")+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Count of students")+
  ggtitle("Workday alcohol consumption depending on students' family size")



# Family size jitter
ggplot(df, aes(x=Dalc, y=famsize, color=gender))+
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
plot_df2<- ggplot(df2, aes(x=factor(Dalc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Workday alcohol consumption depending students' family size")
plot_df2

# Bar plot. % of students of different fam. sizes and alc. consumption.
## Weekend alc. consumption
df3 <- df %>% 
  group_by(famsize, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
plot_df3 <- ggplot(df3, aes(x=factor(Walc), y = perc*100, fill=famsize))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("Population of students per family size, %")+
  ggtitle("Weekend alcohol consumption depending students' family size")
plot_df3

# Arrange plots

grid.arrange(plot_df2, plot_df3, ncol=2)




## Free time:
df4 <- df %>% 
  group_by(freetime, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
plot_df4 <- ggplot(df4, aes(x=factor(freetime), y = count, fill=Walc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  coord_flip()+
  theme_bw()+
  xlab("Free time")+
  ylab("Number of students")+
  ggtitle("Weekend alcohol consumption depending students' freee time")
plot_df4

df5 <- df %>% 
  group_by(freetime, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
plot_df5 <- ggplot(df5, aes(x=factor(freetime), y = count, fill=Dalc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  coord_flip()+
  theme_bw()+
  xlab("Free time")+
  ylab("Number of students")+
  ggtitle("Weekday alcohol consumption depending students' freee time")
plot_df5


# Go out
df6 <- df %>% 
  group_by(goout, Dalc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
plot_df6 <- ggplot(df6, aes(x=factor(goout), y = count, fill=Dalc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  coord_flip()+
  theme_bw()+
  xlab("Go out time")+
  ylab("Number of students")+
  ggtitle("Weekday alcohol consumption depending students go out time")
plot_df6

df7 <- df %>% 
  group_by(goout, Walc) %>% 
  summarise(count=n()) %>% 
  mutate(count=count)
plot_df7 <- ggplot(df7, aes(x=factor(goout), y = count, fill=Walc))+
  geom_bar(stat = "identity", position = "dodge")+
  ylim(0,100)+
  scale_colour_manual(values=c("#ff7f50", "#468499"))+
  coord_flip()+
  theme_bw()+
  xlab("Go out time")+
  ylab("Number of students")+
  ggtitle("Weekend alcohol consumption depending students go out time")
plot_df7

# Arrange plots

grid.arrange(plot_df5, plot_df4, plot_df6, plot_df7, nrow=2, ncol=2)




# Jitter plot: Free Time, G3, Dalc
ggplot(df, aes(x=freetime, y=G3, color=Dalc))+
  geom_jitter()+
  #scale_colour_manual(values=c("#ff7f50", "#468499"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Free Time")+
  ggtitle("Grades depending on free time \nand Workday alcohol consumption")

#### Other dot plot: Final grade, gender, Dalc/Walc ###

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





### Mosaic plot ###

ggplot(df) + 
  geom_mosaic(aes(product(x=gender, y=Dalc), fill=Dalc))
  #geom_mosaic(aes(x = product(sex), fill=Dalc))
?geom_mosaic

mosaic1 <- ggplot(df) + 
  geom_mosaic(aes(product(x=gender, y=Dalc), fill=gender),
  divider=mosaic("v"), na.rm=TRUE, offset=0.01)+
  facet_grid(Dalc~.) +
  #facet_grid(cols = vars(gender))+
  scale_x_productlist("Gender", expand = c(0,0))+
  scale_y_productlist("Workday Alcohol Consumtion")+
  labs(x="divider='vbar'", y="", title = "Weekday Alcohol Consumtion")+
  theme(plot.title = element_text(hjust = 0.5))
mosaic1

mosaic2 <- ggplot(df) + 
  geom_mosaic(aes(product(x=gender, y=Walc), fill=gender),
  divider=mosaic("v"), na.rm=TRUE, offset=0.01)+
  facet_grid(Walc~.) +
  #facet_grid(cols = vars(gender))+
  scale_x_productlist("Gender", expand = c(0,0))+
  scale_y_productlist("Alcohol Consumtion")+
  labs(x="divider='vbar'", y="", title = "Weekend Alcohol Consumtion")+
  theme(plot.title = element_text(hjust = 0.5))
mosaic2

# Arrange plots

grid.arrange(mosaic1, mosaic2, ncol=2)

ggplot(df) + 
  geom_mosaic(aes(product(x=G3_d, Dalc), fill=gender),
              divider=mosaic("v"), na.rm=TRUE, offset=0.01)+
  facet_grid(Dalc~.) +
  #facet_grid(gender~.) +
  #facet_grid(cols = vars(gender))+
  scale_x_productlist("Final Grade", expand = c(0,0))+
  scale_y_productlist("Alcohol Consumtion")+
  labs(x="divider='vbar'", y="", title = "Weekend Alcohol Consumtion")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df) + 
  geom_mosaic(aes(product(x=higher, Dalc), fill=gender),
              divider=mosaic("v"), na.rm=TRUE, offset=0.01)+
  facet_grid(Dalc~.) +
  #facet_grid(vars(Dalc), vars(higher))+
  #facet_grid(gender~.) +
  #facet_grid(cols = vars(gender))+
  scale_x_productlist("Want Higher Education: Yes/No", expand = c(0,0))+
  scale_y_productlist("Alcohol Consumtion")+
  labs(x="divider='vbar'", y="", title = "Weekend Alcohol Consumtion")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Higher")


## look at: matlab color scheme

full_df %>% 
  ggplot(aes(y=freetime, x=Salc, color=Salc, group=freetime)) +
  geom_count() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing.x=unit(0.5, "lines")) +
  theme_bw()+
  scale_size(range=c(1, 10))+
  facet_grid(. ~ goout) +
  labs(x = "Goout",
       size = "")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Go Out Time")+
  ylab("Free Time")+
  ggtitle("Alcohol consumption depending on students' free time and go out time")

