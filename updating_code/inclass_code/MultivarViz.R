#******************************************************
#
#					    
#			 		Multivariate Viz
#
#******************************************************


#***************************
# 0.1 installing and loading the library for the visualization
#***************************
sessionInfo() #information about the version of your R and packages that are loaded in this session


#***************************
# 0.2 installing and loading the library for this session
#***************************
# First install packages (only needs to be done once), and next load the packages
#install.packages('ggplot2')
library(ggplot2)
#install.packages('dplyr')
library(dplyr)
#install.packages('psych')
library(psych)
#install.packages('lattice')
library(lattice)
#install.packages('readr')
library(readr)

#**********************************************************
# 1. Reading in data
#**********************************************************

# from UnivarViz.R
train_data <- "data/totacts.csv"
plts_dir <- "plots/"
totacts <- read_csv(train_data)


#*************************************************
#
#		2. More Data Cleaning
#
#*************************************************

# Variable names
names(totacts)
colnames(totacts)

# View the data types
str(totacts)


#***********************************
#
# 	3. Visualization
#
#***********************************

#***************************
# 3.1 bar plot
#***************************
# Use barplot() to graph this
ggplot(totacts, aes(TYPE)) + 
  geom_bar()

# Add color, a title, and change the text size and rotate text
ggplot(totacts, aes(TYPE)) + 
  geom_bar(fill= "steelblue")+ 
  ggtitle("Accident Frequency by Type") +
  labs(x = "Type of Accident")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))

# Use barplot() to graph frequencies corresponding to different types of trains
ggplot(totacts, aes(TYPEQ)) + 
  geom_bar() +
  labs(x = "Train Type")+
  theme(axis.text.x = element_text(size = 8,  angle = 45))

# Look at histograms of TEMP with different breaks
# Breaks with gap of 10
ggplot(totacts, aes(x=TEMP)) + 
  geom_histogram(breaks=seq(-50, 120, by=10)) + 
  labs(x = "Temperature", y = "Frequency") 

# Breaks with gap of 15
ggplot(totacts, aes(x=TEMP)) + 
  geom_histogram(breaks=seq(-50, 120, by=15)) + 
  labs(x = "Temperature", y = "Frequency") 

# Breaks with gap of 30
ggplot(totacts, aes(x=TEMP)) +
  geom_histogram(breaks=seq(-50, 120, by=30)) + 
  labs(x = "Temperature", y = "Frequency") 

# Change the color and title
ggplot(totacts, aes(x=TEMP)) +
  geom_histogram(fill= "steelblue",breaks=seq(-50, 120, by=15)) + 
  ggtitle("Temperature Frequency") +
  labs(x = "Temperature)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Now do the summary for totacts, but remove all narratives
# and any other variables you won't use for this project
cbind(1:ncol(totacts), names(totacts))
tmp = totacts[,c(122:136)]
new.df <- totacts[,-c(122:136)]


#***************************
# 3.1 scatter plot
#***************************

# Scatter plots
df <- totacts %>% group_by(YEAR) %>% summarize(damage = sum(ACCDMG))

ggplot(df, aes(YEAR, damage)) + 
  geom_line() + 
  geom_point()

# without panel functions for 2020
acts <- totacts %>% filter(YEAR==2020)
pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = acts)

# with panel function- a little more detail
pairs.panels(select(acts, TRKDMG, EQPDMG, ACCDMG, TOTINJ, TOTKLD))

# Do this for all accidents
pairs.panels(select(totacts, TRKDMG, EQPDMG, ACCDMG, TOTINJ, TOTKLD))

# Save as png to avoid problems in the document - save to 'plots' directory
png(paste0(plts_dir, "metrics.png"))
pairs.panels(select(totacts, TRKDMG, EQPDMG, ACCDMG, TOTINJ, TOTKLD))
dev.off()


#***************************
# 3.2 Trellis Categorical Plots
#***************************
# Plotting damage per year
ggplot(data = totacts, aes(x = as.factor(YEAR), y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(x = "Year", y = "Damage ($)")

# Which accident has the most damage?
worst_dmg <- totacts %>% filter(ACCDMG == max(ACCDMG))

# what type of accident had the most damage?
worst_dmg %>% select(TYPE)

# Find out the worst accident for total killed and injured
worst_inj <- totacts %>% filter(TOTINJ == max(TOTINJ))
worst_inj %>% select(TOTINJ)

# What's the narrative of the worst accident for total injured?
worst_inj %>% select(starts_with("NARR"))
worst_inj %>% select(ACCDMG)

# Find the worst accidents in 2018.  What happened?
acts <- totacts %>% filter(YEAR == 2018)
#damage
acts %>% filter(ACCDMG == max(ACCDMG))
#injury
acts %>% filter(TOTINJ == max(TOTINJ))
#killed
acts %>% filter(TOTKLD == max(TOTKLD))

#Find the accidents with the most injuries in 2018.  What happened in these accidents?  
#Explore the news to understand underlying causes and what could have been done to prevent these accidents.
acts %>% arrange(desc(TOTINJ)) %>% relocate(TOTINJ) %>% head(5)

#Find the accident with the most damage in 2018.  What do you notice about the accidents with the most damage and those with the most injuries?
acts %>% filter(ACCDMG == max(ACCDMG))
acts %>% arrange(desc(ACCDMG)) %>% relocate(ACCDMG) %>% head(5)

# Plotting accident cause vs. damage
ggplot(data = totacts, aes(x = Cause, y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(y = "Damage ($)", x = "Accident Cause")

# Plot scaled (log) accident damage
bwplot(Cause~ log(ACCDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = totacts)

# Plot cause vs. scaled (log) accident damage
ggplot(data = totacts, aes(x = Cause, y = log(ACCDMG+1))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Log(Accident Damage)") +
  labs(y = "log(Damage ($))", x = "Accident Cause")

# Plot cause vs. no. killed 
ggplot(data = totacts, aes(x = Cause, y = TOTKLD)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Box Plots of Total Killed")+
  labs(y = "Total Killed", x = "Accident Cause")

# Plot cause vs. injured
ggplot(data = totacts, aes(x = Cause, y = TOTINJ)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Total Injured") +
  labs(y = "Total Injured", x = "Accident Cause")


# X-Y plots conditioned on cause for both killed and injured
qplot(ACCDMG, TOTKLD, data = totacts) + 
  facet_wrap(~Cause, scales = "free") +
  ggtitle("Damage vs. Killed Conditioned on Cause") + 
  labs(x =  "Total Accident Damage", y  = "Total Killed") +
  theme(plot.title = element_text(hjust = 0.5))