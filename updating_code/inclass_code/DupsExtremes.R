
#******************************************************
#
#  				             Session 3
#				Duplicates, Categorial Variable Relationships &
#				            Extreme values
#
#******************************************************

##############
# Get the data
##############

# Set working directory (change to your path)

traindir <- "D:/GoogleDrive/Julie_SYS4021/2020/Data/Trains"
sourcedir <-"D:/GoogleDrive/Julie_SYS4021/2020/R Code"

setwd(sourcedir)

# Source AccidentInput

source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2019

# the combined data frame

totacts <- combine.data(acts)

dim(totacts)

#***********************************************************
#
#			Setup Categorical Variables
#
#***********************************************************


# Accident type
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))


# Type of train
totacts$TYPEQ <- as.numeric(totacts$TYPEQ)


# Now convert to factor
totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))


# Accident cause

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor
totacts$Cause <- factor(totacts$Cause)

#***********************************************************
#
#			Extreme Points
#
#***********************************************************

library(ggplot2)

ggplot(as.data.frame(totacts$ACCDMG), aes(x=totacts$ACCDMG)) + geom_histogram()

#Look also at TOTKLD and TOTINJ


# Get the values in the box plot for ACCDMG
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

# find only the extremes - the points above the upper whisker
names(ggplot_build(dmgbox)$data[[1]]) # what variables are associated with the boxplot?

# ymax is the upper whisker - anything above that is an outlier
upper <- ggplot_build(dmgbox)$data[[1]]$ymax

# create a new data frame with only the outliers
xdmg <- totacts[totacts$ACCDMG > upper,]

# how many outliers are there
nrow(xdmg)

# What proportion of accidents are extreme?

nrow(xdmg)/nrow(totacts)

# Proportion of costs

sum(as.numeric(totacts$ACCDMG[which(totacts$ACCDMG > ggplot_build(dmgbox)$data[[1]]$ymax)]))/sum(as.numeric(totacts$ACCDMG))


# Look at the graphs of these extreme accidents

#histogram
ggplot(as.data.frame(xdmg$ACCDMG), aes(xdmg$ACCDMG)) + geom_histogram()

# boxplot
xdmgbox <- ggplot(as.data.frame(xdmg$ACCDMG), aes(x=xdmg$ACCDMG)) + 
  geom_boxplot() + ggtitle("Accidents with Extreme Damage") +
  labs(x = "Cost ($)") + theme(plot.title = element_text(hjust = 0.5))

names(ggplot_build(xdmgbox)$data[[1]]) # note boxplot variables now have x instead of y in front

# time series
df <- data.frame(year=2001:2019,damages=tapply(xdmg$ACCDMG, as.factor(xdmg$YEAR), sum))
ggplot(data=df, aes(x=year, y=damages)) + geom_line() + geom_point()


# also plot number of accidents per year.

ggplot(as.data.frame(table(totacts$YEAR)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")

# Frequency of accident types
# compare with the totacts plot
ggplot(as.data.frame(table(xdmg$TYPE)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")


# SPM of metrics & train variables

library(GGally)
ggpairs(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD")])

source("SPM_Panel.R")
uva.pairs(xdmg[,c("ACCDMG", "TOTKLD", "TOTINJ", "TRNSPD")])

# Categorical variables

# Cause

ggplot(data = xdmg, aes(x = Cause, y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage by Cause") +
  labs(x = "Damage ($)", y = "Accident Cause")

# Repeat for Type of accident






#Use of jitter
bwplot(as.factor(YEAR)~jitter(ACCDMG, amount = 2.5e5), data = xdmg, main = "Box Plots of Extreme Accident Damage by Year (with jitter)", xlab = "Damage ($)", ylab = "Year")

ggplot(data = xdmg, aes(x = as.factor(YEAR), y = jitter(ACCDMG, amount = 2.5e5))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Extreme Accident Damage by Year (with jitter)") +
  labs(x = "Damage ($)", y = "Year")


par(mfrow = c(1,2))
boxplot(jitter(xdmg$ACCDMG, amount = 2.5e5), col = "steelblue", main = "Extreme Accident Damage with Jitter")
boxplot(xdmg$ACCDMG, col = "steelblue", main = "Extreme Accident Damage without Jitter")
par(mfrow = c(1,1))

p <- ggplot(as.data.frame(jitter(xdmg$ACCDMG, amount = 2.5e5), aes(x=jitter(xdmg$ACCDMG, amount = 2.5e5)))) + 
  geom_boxplot(col= "steelblue") + ggtitle("Extreme Accident Damage with Jitter") +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

q <- ggplot(as.data.frame(xdmg$ACCDMG), aes(x=xdmg$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Extreme Accident Damage without Jitter") +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() 

ggarrange(p, q, nrow = 1, ncol = 2)
# Conditioning on categorical variables

# on Cause

xyplot(ACCDMG~TRNSPD | Cause, main = "Extreme Damage vs. Train Speed Conditioned on Cause", xlab = "Train Speed", ylab = "Total Accident Damage", data = xdmg)

qplot(ACCDMG, TRNSPD, data = xdmg) + facet_wrap(~ Cause, scales = "free") +
  ggtitle("Extreme Damage vs. Train Speed Conditioned on Cause") + 
  labs(x = "Total Accident Damage", y = "Total Accident Damage") +
  theme(plot.title = element_text(hjust = 0.5))

# on type of accident



# Repeat the above extreme point analysis but use TOTINJ + TOTKLD
# But wait until we do more cleaning

#***********************************************************
#
#			Heatmaps for categorical variabels
#
#***********************************************************

table(xdmg$Cause, xdmg$TYPE)

heatmap(table(xdmg$Cause, xdmg$TYPE), Rowv = NA, Colv = NA)

# With legend (optional)


install.packages("gplots", dependencies = T)

library(gplots)

heatmap.2(table(xdmg$Cause, xdmg$TYPE), Rowv = F, Colv = F)

source("http://www.phaget4.org/R/myImagePlot.R")

myImagePlot(table(xdmg$Cause, xdmg$TYPE), title = "No. of Accidents by Cause and Type of Accident")


#***********************************************************
#
#			Data Cleaning
#
#***********************************************************

# Let's look at the most extreme cost accidents. Are there any of particular interest?

which(xdmg$ACCDMG > 15e6)

# Duplicates?

# The max

which(xdmg$ACCDMG == max(xdmg$ACCDMG))

# Look at the narrative

as.matrix(names(xdmg))

xdmg[which(xdmg$ACCDMG == max(xdmg$ACCDMG)), 122:136]

# Are there other duplicates?

duplicated(xdmg[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])


# What about longitude and latitude?

# what about incident number?

which(xdmg$INCDTNO == "110058")

xdmg[which(xdmg$INCDTNO == "110058"),  c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]

duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

# Not duplicated

!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]))


#remove the duplicates

xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

dim(xdmg)
dim(xdmgnd)

# number of duplicates

nrow(xdmg) - nrow(xdmgnd)

