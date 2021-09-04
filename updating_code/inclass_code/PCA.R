

#******************************************************
#
#					Session 5
#				
#				Principal Components
#
#******************************************************


#***********************************************************
#
#			Install and load the necessary libraries
#
#***********************************************************

# Load ggplot2 libraries
library(ggplot2)
library(ggpubr)
library(GGally)

# Install and load devtools and ggbiplot
install.packages("devtools")
library(devtools) # for ggbiplot
install_github("vqv/ggbiplot")
library(ggbiplot)

# load data.table to convert data frame index to column for ggplot barplot
library(data.table)

#***********************************************************
#
#			Load and format the data
#
#***********************************************************

traindir <- "D:/GoogleDrive/Julie_SYS4021/2020/Data/Trains"
sourcedir <-"D:/GoogleDrive/Julie_SYS4021/2020/R Code"

# Source AccidentInput
setwd(sourcedir)
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2019

# the combined data frame

totacts <- combine.data(acts)

#***********************************************************
#
#			Get the extreme accident data
#
#***********************************************************

# For ACCDMG
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()
dmgbox

# find only those above the upper whisker
upper <- ggplot_build(dmgbox)$data[[1]]$ymax
xdmg <- totacts[totacts$ACCDMG > upper,]

# For Casualties (TOTINJ + TOTKLD)

xdmg$Casualties <- xdmg$TOTINJ + xdmg$TOTKLD

# Investigate extremes among the extremes
which(xdmg$ACCDMG > 15e6)

# What happened in the first of these?


# Is this likely to happen again? May want to remove.
xdmg <- xdmg[-186,]


#***********************************************************
#
#			Remove duplicates
#
#***********************************************************

# Remove duplicates
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

#Reset rownames (observation #s) for sequential numbering- otherwise they will remain the #s from totacts

rownames(xdmgnd) <- NULL

#***********************************************************
#
#		Principal Components Analysis
#
#***********************************************************
?princomp


# Principal Components of metrics for extreme accidents

xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")])

#***********************************************************
#
#		Biplot
#
#***********************************************************

# View the data in the first 2 PCs

biplot(xdmgnd.pca, main="Biplot of Extreme Accident Metrics")

# If you've installed ggbiplot

ggbiplot(xdmgnd.pca, varname.size = 5, labels=row(xdmgnd)[,1], main="Biplot of Extreme Accident Metrics")

# Remove outliers in components 1 and 2

xdmgnd.pca <- princomp(xdmgnd[-c(5900,5337,5901,5330),c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")])

# View the first 2 PCs without ouliers - do they change your conclusions?

biplot(xdmgnd.pca)
ggbiplot(xdmgnd.pca, varname.size = 5, labels=row(xdmgnd[-c(5900,5337,5901,5330),])[,1], main="Biplot of Extreme Accident Metrics")

#***********************************************************
#
#		Scree plot and cumulative variance
#
#***********************************************************

screeplot(xdmgnd.pca, main = "Variance of each PC")

# If you've installed ggbiplot
ggscreeplot(xdmgnd.pca)

# Cumulative variance

source("PCAplots.R")

cumsum <- cumplot(xdmgnd.pca, col = "blue")

ggplot(data=cumsum, aes(x=Component,y=Proportion))  + geom_bar(stat="identity") + 
  ggtitle("Cumulative Variance in the PCs")

#***********************************************************
#
#		Loadings plots
#
#***********************************************************

# Loadings in the first 2 PCs

barplot(xdmgnd.pca$loadings[,1], main='PC1 Loadings')
barplot(xdmgnd.pca$loadings[,2], main='PC2 Loadings')

# using ggplot

df = as.data.frame(xdmgnd.pca$loadings[,1:6]) # get first 6 columns which are the 6 PCs
setDT(df, keep.rownames=TRUE)[]
names(df)[names(df) == "rn"] <- "Variable"
PC1 <- ggplot(data=df, aes(x=Variable,y=Comp.1)) + geom_bar(stat="identity") + ggtitle("PC1 Loadings")
PC2 <- ggplot(data=df, aes(x=Variable,y=Comp.2)) + geom_bar(stat="identity") + ggtitle("PC2 Loadings")
ggarrange(PC1, PC2, ncol=2, nrow=1)


#***********************************************************
#
#		Principal Components with the Correlation Matrix	
#
#***********************************************************


# Principal Components with the Correlation Matrix

xdmgnd.pca.corr <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")], cor = T)


#***********************************************************
#
#		Biplot Comparison
#
#***********************************************************

# View data in the first 2 PCs

par(mfrow=c(1,2))
biplot(xdmgnd.pca, main="Biplot with Covariance Matrix")
biplot(xdmgnd.pca.corr, main="Biplot with Correlation Matrix")
par(mfrow=c(1,1))

# If you've install ggbiplot
cov_biplot <- ggbiplot(xdmgnd.pca, varname.size = 5, labels=row(xdmgnd[-c(5900,5337,5901,5330),])[,1], main="Biplot with Covariance Matrix")
corr_biplot <- ggbiplot(xdmgnd.pca.corr, varname.size = 5, labels=row(xdmgnd)[,1], main="Biplot with Correlation Matrix")
ggarrange(cov_biplot, corr_biplot, ncol=2, nrow=1)


#***********************************************************
#
#		Scree plot and cumulative variance comparison
#
#***********************************************************

par(mfrow=c(1,2))
screeplot(xdmgnd.pca, main = "Variance of each PC using Covariance Matrix")
screeplot(xdmgnd.pca.corr, main = "Variance of each PC using Correlation Matrix")
par(mfrow=c(1,2))

cov_scree <- ggscreeplot(xdmgnd.pca)
corr_scree <- ggscreeplot(xdmgnd.pca.corr)
ggarrange(cov_scree, corr_scree, ncol=2, nrow=1)

# Cumulative variance

par(mfrow=c(1,2))
cov_cumsum <- cumplot(xdmgnd.pca, col = "blue")
corr_cumsum <- cumplot(xdmgnd.pca.corr, col = "blue")
par(mfrow=c(1,2))

# with ggplot

cov_cumsum_plot <- ggplot(data=cov_cumsum, aes(x=Component,y=Proportion))  + geom_bar(stat="identity") + 
  ggtitle("Covariance Matrix")
corr_cumsum_plot <- ggplot(data=corr_cumsum, aes(x=Component,y=Proportion))  + geom_bar(stat="identity") + 
  ggtitle("Correlation Matrix")
ggarrange(cov_cumsum_plot, corr_cumsum_plot, ncol=2, nrow=1)

#***********************************************************
#
#		Loadings plots comparison
#
#***********************************************************

par(mfrow=c(2,2))
barplot(xdmgnd.pca$loadings[,1], main='PC1 Loadings with Covariance Matrix')
barplot(xdmgnd.pca.corr$loadings[,1], main='PC1 Loadings with Correlation Matrix')
barplot(xdmgnd.pca$loadings[,2], main='PC2 Loadings with Covariance Matrix')
barplot(xdmgnd.pca.corr$loadings[,2], main='PC2 Loadings with Correlation Matrix')
par(mfrow=c(1,1))

# with ggplot
df_cov = as.data.frame(xdmgnd.pca$loadings[,1:6]) # get first 6 columns which are the 6 PCs
setDT(df_cov, keep.rownames=TRUE)[]
names(df_cov)[names(df_cov) == "rn"] <- "Variable"
PC1 <- ggplot(data=df_cov, aes(x=Variable,y=Comp.1)) + geom_bar(stat="identity") + ggtitle("PC1 Loadings with Covariance Matrix")
PC2 <- ggplot(data=df_cov, aes(x=Variable,y=Comp.2)) + geom_bar(stat="identity") + ggtitle("PC2 Loadings with Covariance Matrix")

df_corr = as.data.frame(xdmgnd.pca.corr$loadings[,1:6]) # get first 6 columns which are the 6 PCs
setDT(df_corr, keep.rownames=TRUE)[]
names(df_corr)[names(df_corr) == "rn"] <- "Variable"
PC1_corr <- ggplot(data=df_corr, aes(x=Variable,y=Comp.1)) + geom_bar(stat="identity") + ggtitle("PC1 Loadings with Correlation Matrix")
PC2_corr <- ggplot(data=df_corr, aes(x=Variable,y=Comp.2)) + geom_bar(stat="identity") + ggtitle("PC2 Loadings with Correlation Matrix")

ggarrange(PC1, PC1_corr, PC2, PC2_corr, ncol=2, nrow=2)


#***********************************************************
#
#		Possible predictors of damage	
#
#***********************************************************

# SPM
source("SPM_Panel.R")
uva.pairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])
ggpairs(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")])

# PCA

pred.pca <- princomp(xdmgnd[,c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

biplot(pred.pca)

ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd)[,1])

# Remove outlier

pred.pca <- princomp(xdmgnd[-c(5337),c("ACCDMG", "TRNSPD", "CARS", "TIMEHR", "TEMP")], cor = T )

biplot(pred.pca)

ggbiplot(pred.pca, varname.size = 5, labels=row(xdmgnd[-c(5337),])[,1])
