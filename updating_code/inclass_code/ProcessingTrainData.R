#********************************************************************************
#             
#					Processing Train Data
#
#********************************************************************************

#***************************
# 0.1 installing and loading the library for this session
#***************************

# First install packages (only needs to be done once), and next load the packages
#install.packages('readr')
library(readr) # for loading data
#install.packages('dplyr')
library(dplyr) # for manipulating tabular data
#install.packages('ggplot2')

#**********************************************************
# 1. Reading in data
#**********************************************************

#***************************************
# 1.1 Define data and source directories
#***************************************

# if your data and source directories are in your project directory:
datadir <- "data/" # export location of processed train data
traindir <- "data/TrainData/" # location of raw train data

# if your data and source directories are above your project directory,
# use '../' to direct R up one directory
# datadir <- "../data/"
# traindir <- "../data/TrainData/" 


#***************************
# 1.2 loading the data
#***************************

# Read in the accident files one at at time
# for the first questions in the in-class assignment we will 
# only use data from 2020
accident_data_20 <- read_csv(paste0(traindir, "RailAccidents20.csv"))


#***************************
# 1.3 information about the selected year accident data
#***************************

head(accident_data_20) # the first 6 rows (observations) of data
dim(accident_data_20) #number of rows (observation) and number of columns (variables)
summary(accident_data_20) #summary of each column (variable)
str(accident_data_20) #type of each column (variable) of data

# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
str(accident_data_20[,c("ACCDMG", "TOTKLD", "CARS", "STATION")])

colnames(accident_data_20) # number of columns (variable)
class(accident_data_20$TOTKLD) #type of one specific variable 
var(accident_data_20$TOTKLD) #variance of one specific variable (total killed) - quantitative
mean(accident_data_20$TOTKLD) #mean of one specific variable (total killed) - quantitative
table(accident_data_20$TOTKLD) #frequency of each value in the selected variable

class(accident_data_20$STATION) #type of one specific variable 
levels(accident_data_20$STATION) #differnt values of categorical variable (station) - qualitative
table(accident_data_20$STATION) #frequency of each value in the selected variable


# You can round your answer using the round() function
print(mean(accident_data_20$TOTKLD))
print(round(mean(accident_data_20$TOTKLD)))

#***************************
# 1.4 loading all years data
#***************************

# You will need to read in all 20 years of the data 
# You will put the data into a data structure called a list
# You must have ALL and ONLY the rail accident data
# files in one directory.
my.files <- list.files(traindir, full.names = TRUE)

# loading multiple files in a directory using lapply
acts <- lapply(my.files, read_csv, show_col_types=FALSE)

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.
acts[[1]]$YEAR #it shows 1 --> 2001
acts[[5]]$YEAR #it shows 5 --> 2005

# Before we put all the data into one data frame
# we must clean the data
##################################################
#
#	2. Data Tidying and Exporting
#
##################################################

#***************************
# 2.1 columns in all years
#***************************
# Are the number of columns different from year to year?
ncol(acts[[1]])
ncol(acts[[8]])
ncol(acts[[7]])

#***************************
# 2.1 combine all years of data after selecting common columns
#***************************
# Now combine the data frames for all 20 years

# totacts <- bind_rows(acts)

# The line above fails because the variable TYPEQ is recognized as a double
# in one tibble and a character in another. This is useful to know!
# It turns out this happens with a bunch of other variables too,
# so we re-import the data and manually assign the datatype for those columns.
acts <- lapply(my.files, read_csv, show_col_types=FALSE,
               col_types=list(TYPEQ = col_integer(),
                              TYPE = col_integer(),
                              IYR = col_integer(),
                              IYR2 = col_integer(),
                              IYR3 = col_integer(),
                              TYPRR = col_character(),
                              HIGHSPD = col_double(),
                              YEAR = col_integer()))

# Now we'll try again and select only columns for which all years have data\
# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

totacts <- bind_rows(acts) %>% select(comvar)

# Format the year column from a 2-digit year to a 4-digit year and
# convert to numeric datatype
totacts <- totacts %>% mutate(YEAR = YEAR + 2000)

# Make the Type columns more readable
## TYPE
type_labs <- c("Derailment", "HeadOn", "Rearend", "Side", "Raking",
               "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction",
               "Explosive", "Fire","Other","SeeNarrative")

totacts <- totacts %>% mutate(TYPE = factor(TYPE, labels = type_labs))

## TYPEQ
typeq_labs <- c("Freight", "Passenger", "Commuter", "Work",
                "Single", "CutofCars", "Yard", "Light", "Maint")

totacts <- totacts %>% mutate(TYPEQ = factor(TYPEQ, labels = typeq_labs))

# Create a new variable called 'Cause' that uses labels for cause.
totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "(M) Miscellaneous"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "(T) Track, Roadbed and Structures"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "(S) Signal and Communication"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "(H) Human Factors"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "(E) Mechanical and Electrical Failures"

#***************************
# 2.2 Inspect the combined data
#***************************

#check the number of rows and columns in this merged dataset
dim(totacts)

# the total accident should be combination of all data
rows = 0
for(acc_year in acts){
  print(dim(acc_year))
  rows = rows + dim(acc_year)[1]
  print(rows)
}
# the following numbers should be the same
print(rows) #totoal number of observations
dim(totacts)[1] #total number of observations

# How many entries (observations) does each year of accident have?
totacts %>% group_by(YEAR) %>% 
  summarise(entries = n()) %>% 
  arrange(desc(entries))

#***************************
# 2.3 Export data
#***************************
# Now we'll write this to a csv in our data directory so we don't have to
# re-process the data every time
write_csv(totacts,paste0(datadir, "totacts.csv"))
