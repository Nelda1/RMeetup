
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
ggplot(viz, aes(x = District, y = Profit,)) + geom_boxplot()

#plotting piechart

ggplot(data = viz, aes(x = "Profit",y= "District", fill=District)) +
  geom_bar(width = 30, stat = 'identity') +coord_polar('y',start = 20)


