#***************************************************************
#
#      In-Class Grid Stability Classification Contest
#   		   
#***************************************************************

#***************************************************************
#
#  Read in the data
#
#***************************************************************

# Set working directory
sourcedir <- "D:/GoogleDrive/Julie_SYS4021/2020/R Code"
datadir <- "D:/GoogleDrive/Julie_SYS4021/2020/Data/GridStability"
setwd(sourcedir)

# Read in the data
GS.train <- read.csv(paste(datadir,"/GridStability_train.csv", sep=""), sep = ",", header = T)
GS.test <- read.csv(paste(datadir,"/GridStability_test.csv", sep=""), sep = ",", header = T)

# Source potentially useful functions
source("pc.glm.R")
source("PCAplots.R")
source("FactorPlots.R")
source("ROC.R")
source("TestSet.R")


#*************************************************************************************
#
# Build the best GLM you can to classify tumors as malignant
# (1) or benign (0) and write classifications to file
#
#*************************************************************************************

# Build model to training set:
GS.glm <- glm(stabf~., GS.train, family = binomial)

# Predict probabilities on testing set:
GS.pred <- predict(GS.glm, type = "response", newdata = GS.test)

# Convert probabilities to 0/1 classification based on threshold T
T <- 0.5
GS.classifications <- matrix(0,nrow=length(GS.pred),ncol=2)
GS.classifications[,1] <- c(1:length(GS.pred))
GS.classifications[which(GS.pred > T),2] <- 1

# Write predictions to file
colnames(GS.classifications) = c("Id","Predicted")
write.csv(GS.classifications, paste(datadir,"/sampleSubmission.csv", sep=""), row.names=FALSE)
