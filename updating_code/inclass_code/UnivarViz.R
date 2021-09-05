#********************************************************************************
#             
#					Univariate Graphics
#
#********************************************************************************

#***************************
# 0.1 installing and loading the library for the visualization
#***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session


#***************************
# 0.2 installing and loading the library for this session
#***************************
# First install packages (only needs to be done once), and next load the packages
#install.packages('readr')
library(readr) # for loading data
#install.packages('dplyr')
library(dplyr) # for manipulating tabular data
#install.packages('ggplot2')
library(ggplot2) # for plotting


#***************************
# 0.3 loading data
#***************************
# define filepath to dataset exported in 'ProcessingTrainData.R'
train_data <- "data/totacts.csv"

totacts <- read_csv(train_data)


#***********************************
#
# 	1. Visualization
#
#***********************************


#***************************
# 1.1 Histogram
#***************************

# These examples are for year 2011 
# Histograms are both in default way and also with ggplot2 package

# for 2011
acts <- filter(totacts, YEAR==2011)

ggplot(acts, aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Different bin widths

# Bin FD
ggplot(acts, aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.FD(acts$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Bin Scott
ggplot(acts, aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = nclass.scott(acts$ACCDMG)) + 
  ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Bin 20
ggplot(acts, aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 20) + ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

# Bin 2
ggplot(acts, aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 2) + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))


# other years
acts <-  totacts %>% filter(YEAR %in% c(2001,2004,2008,2011))
ggplot(as.data.frame(acts), aes(x=ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)


# Accident damage 2009-2020

# filter the selected years
acts = totacts %>% filter(YEAR %in% c(2009:2020))
# histogram of accident damage based on each year
ggplot(as.data.frame(acts), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)

# histogram of accident damage based on each year with bins = 10
ggplot(as.data.frame(acts), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue", bins = 10) + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  facet_wrap(~YEAR)


# Damage in constant scales
acts = totacts %>% filter(YEAR %in% c(2001:2011))
# histogram of accident damage based on each year
ggplot(as.data.frame(acts), aes(ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2009-2020") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  xlim(c(0,1.7e7)) + ylim(c(0,1000)) + 
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~YEAR)


#***************************
# 1.2 Boxplots of ACCDMG
#***************************

## accident damagem for 2011
acts <- filter(totacts, YEAR==2011)

ggplot(acts, aes(x=ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggplot(acts, aes(x=ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = "*", outlier.size = 5) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip() 

ggplot(acts, aes(x=ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = 4, outlier.size = 2) + ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

## accident damagem for 2006
acts <- filter(totacts, YEAR==2006)

ggplot(acts, aes(x=ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2006") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

#4 plots on a single graph
acts = totacts %>% filter(YEAR %in% c(2001,2004,2008,2011))
# box plot of accident damage based on each year
ggplot(acts, aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  facet_wrap(~YEAR)

# box plot of accident damage based on each year
ggplot(acts, aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  facet_wrap(~YEAR)


# box plot of accident damage based on each year
#With a constant scale
ggplot(acts, aes(ACCDMG)) + 
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  xlim(c(0,1.7e7)) +
  facet_wrap(~YEAR)


# box plot of accident damage based on each year
acts = totacts %>% filter(YEAR %in% c(2009:2019))
#With a constant scale
ggplot(acts, aes(ACCDMG)) +  
  geom_boxplot(fill= "steelblue") + 
  ggtitle("Total Accident Damage in the selected years") + 
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  coord_flip() + 
  facet_wrap(~YEAR)


#change the color for the outlier
# for 2011
acts <- filter(totacts, YEAR==2011)

ggplot(acts, aes(x=ACCDMG)) +
  geom_boxplot(col= "steelblue",outlier.shape=1) + 
  ggtitle("Total Accident Damage in 2011") +
  labs(x = "Dollars ($)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip()

#***************************
# 1.3 QQ plot of ACCDMG
#***************************

#Does our data come from a Guassian distribution
ggplot(acts, aes(sample=ACCDMG)) + 
  stat_qq() + 
  stat_qq_line() +
  ggtitle("Total Accident Damage") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(acts, aes(sample=TEMP)) +
  stat_qq() + 
  stat_qq_line() + 
  ggtitle("Accident Temperature") + 
  theme(plot.title = element_text(hjust = 0.5))


#***************************
# 1.4 Density plot of ACCDMG
#***************************

d <- density(acts$ACCDMG,
             kernel = "gaussian")
plot(d,  main = "gaussian kernel", col = 'red')

#density plot on top of histogram
h <- hist(acts$ACCDMG, breaks = "FD", plot = FALSE)

ggplot(data=acts, aes(ACCDMG)) + 
  geom_histogram(aes(y =..density..),
                 breaks=h$breaks,
                 col="green",
                 fill="green",
                 alpha=.2) +
  geom_density(fill="red", color=1, alpha=0.4) +
  labs(title="Density and Histogram for ACCDMG", x="ACCDMG", y="Count") +
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))

#***************************
# 1.5 Bar plot of Accident Types
#***************************

# Each number represents a different type of accident as given in the data dictionary, 
#for e.g. 1-Derailment, 2-Head on Collision
ggplot(acts, aes(TYPE)) +
  geom_bar(stat="count", width=0.7, fill="steelblue", col = 'black') +
  labs(title="Bar plot of Accident Type", x="accident type")+
  scale_fill_brewer(palette="Paired")