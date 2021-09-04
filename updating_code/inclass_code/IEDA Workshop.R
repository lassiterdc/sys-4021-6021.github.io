#Introduction to Exploratory Data Analysis in R
#TAs: Angel Vela, Daniel Lassiter, Katherine Korngiebel
#Exercises and code are based from the book R for Data Science Chapter 3-5 https://r4ds.had.co.nz/index.html#welcome


#Before starting a new script,check to see if you need to clear your environment

#Load the ggplot2 and dplyr libraries
#if package is not installed you can use install.packages("package_name")
#Or go to Tools -> Install Packages... and type in the package name
#ggplot2 and dplyr is part of the tidyverse package
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)


#load the mpg data from ggplot2
mpg <- ggplot2::mpg

#***********************************
#
# 	getting to know your dataset
#
#***********************************

#Run ?mpg on the console
#Display the top rows of the dataset
head(mpg)

#Display its structure
str(mpg)

#Display the names of the columns
colnames(mpg)

#How many rows does mpg have?
count(mpg)

#How many columns does mpg have?
length(colnames(mpg))

#***********************************
#
# 	NA Values
#
#***********************************

#base r
sapply(mpg,function(x) sum(is.na(x)))

#tidyverse
mpg %>% summarise_all(list(~sum(is.na(.x))))

#***********************************
#
# 	Summary Statistics and Information
#
#***********************************

#Summary stats for year
summary(mpg$year)

#Summary stats for cylinder
summary(mpg$cyl)

# How many entries does each car manufacturer have?
#tidyverse
# %>% is called the pipe operator
mpg %>%  group_by(manufacturer) %>% 
  summarise(entries = n()) %>% 
  arrange(desc(entries))

#***********************************
#
# 	Data visualization
#
#***********************************
#Following from Chapter 3 of the book
#hwy vs. cyl scatterplot
ggplot(mpg,aes(x = hwy, y = cyl))+geom_point()

#Color the dots purple
ggplot(mpg,aes(x = hwy, y = cyl))+geom_point(color ="purple" )

#scatter plot displ vs. hwy by by manufacturer
ggplot(mpg) + 
  geom_point(mapping = aes(x= displ, y =hwy)) +
  facet_wrap(~ manufacturer, nrow = 5)

#bar plot with number of entries per manufacturer
ggplot(mpg, aes(x = manufacturer)) + geom_bar(color = "blue",fill = "orange")

#use built-in themes https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(mpg, aes(x = manufacturer)) + geom_bar()+theme_minimal()
#use built-in themes https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(mpg, aes(x = manufacturer)) + geom_bar()+theme_gray()

#***********************************
#
# 	Data transformation
#
#***********************************
#Following from Chapter 5 of the book
#let's install and load the nycflights13 data
library(nycflights13)

#flights data
flights <- nycflights13::flights

#New year flights using filter
newYear <- flights %>% filter(month == 12, day == 31)

#Flights departing from JFK or LGA on newYear
newYear.JFK.LAX <- flights %>% filter(month == 12 & day ==31 & origin %in% c ("JFK","LGA"))

#new dataframe select arr_delay, and carrier, arrange by delay time in descending order
delay <- flights %>% select(arr_delay,carrier) %>% arrange(desc(arr_delay))

#Create a new column with arrival delayed time in hours displaying 2 decimal digits
delay <- delay %>% mutate(arr_delayHrs = round(arr_delay/60,digits = 2))

View(delay)
