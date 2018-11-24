
#load libraries
library(ggplot2)
library(dplyr)

#Load data for analysis
viz <- read.csv("spend_data.csv", stringsAsFactors = FALSE)
View(viz)

#Explore data 
class(viz) 
dim(viz) 
str(viz) 
summary(viz) 
head(viz)

#Grouping columns
Dis <- viz %>%
  group_by(District) %>%
  summarise(Total = n())
#Reordering columns
new_dis <- transform(Dis,
                     District = reorder(District, Total))

#plotting a bar graph
ggplot(data = new_dis, aes(x = District, y = Total,
                           fill=District,
                           label=Total))+
  geom_bar(stat = "identity"
  )+
  geom_text(hjust = 0.8, vjust = 0,color = "black",
            fontface = "bold",size = 5)
coord_flip()+
  ggtitle(paste("Bar graph of districts"))+
  labs(x = "District", y = "Number of Count",
       caption = "Source of Data: selfmade") + theme(legend.position = 'none')

#plotting a scatter plot
ggplot(viz, aes(x = Commercial, y = Profit, colour = "blue")) +
  geom_point(shape = 10, size = 2, show.legend = FALSE) + geom_line()

#plotting a box plot
ggplot(viz, aes(x = District, y = Profit,)) + geom_boxplot(outlier.colour="red", outlier.shape=8,
                                                           outlier.size=4)

#plotting piechart

ggplot(data = viz, aes(x = "Profit",y= "District", fill=District)) +
  geom_bar(width = 30, stat = 'identity') +coord_polar('y',start = 20)

# Commercial Distribution
ggplot(viz, aes(x=Commercial, colour=District, fill=District)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(Commercial),  colour=District),linetype="dashed",color="grey", size=1)+
  xlab("Ammount on Commercial") +
  ylab("Density")+
  ggtitle(paste("Distribution on commercial"))

# Box plot

ggplot(viz, aes(District, Profit, fill=District)) +
  geom_boxplot()+
  scale_y_continuous("Profit 'UGX' ", breaks= seq(0,1000, by=500))+
  labs(title = "Box plot of Profits", x = "District")

# Scatter

ggplot(viz, aes(x = Commercial, y = Profit))+
  xlab("Ammount")+
  ylab("Profit") +
  geom_point(aes(color = District,shape=District))+
  geom_smooth(method='lm')+
  ggtitle("Profit vs Ammount")
