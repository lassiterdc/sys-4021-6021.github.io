

#************************************************************
#
#				SPAM Filter 2 
#			Generalized Linear Models of SPAM 
#
#
#************************************************************


#*****************************
#
# Load the data & source files
#
#*****************************

sourcedir <- "D:/GoogleDrive/Julie_SYS4021/2020/R Code"
datadir <- "D:/GoogleDrive/Julie_SYS4021/2020/Data/Spam"

setwd(sourcedir)
spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

library(ggfortify)
library(ggResidpanel)

#*****************************
#
# GLM
#
#*****************************

# Use the glm() function to obtain the glm for the spam filter.

spam.glm.main <- glm(V58~., data = spam, family = binomial)

# notice the warning message!

# Use summary to evaluate the model

summary(spam.glm.main)

# What is the contribution of variable 51 to the model?
# notice that we add one to the variable number (51)
# to account for the intercept

(exp(spam.glm.main$coefficients[52])-1)*100

# Explain this result.

spam[1,]
predict(spam.glm.main, newdata = spam[1,])
exp(predict(spam.glm.main, newdata = spam[1,]))
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:50, 52:57)], V51 = 1)))


# Can we use an F test for model utility?
#Perform a model utility test for the glm
# Model Utility Test using Chi2 statistic

# What do you conclude?

spam.null <- glm(V58~1, data = spam, family = binomial)
anova(spam.null, spam.glm.main, test = "Chi")



# Create a model with just the capital letters, 
# V55 - V58  as predictors

spam.cap <- glm(V58~., data = spam[,55:58], family = binomial)

# How does this model do on the model utility test?

anova(spam.null, spam.cap, test = "Chi")

# Use the likelihood ratio or partial chi square test to compare the full main effects model with the capital letters model

anova(spam.cap, spam.glm.main, test = "Chi")


# Can we use the t-test for individual coefficients?
# Use the full main effect model and test the coefficient on 
# V57

spam.no57 <- update(spam.glm.main, .~.-V57, data = spam)

anova(spam.no57, spam.glm.main, test = "Chi")


# What is the contribution of variable 57 to the model?
# notice that we add one to the variable number (57)
# to account for the intercept

(exp(spam.glm.main$coefficients[58])-1)*100

# Explain this result.
spam[1,]
predict(spam.glm.main, newdata = spam[1,])
exp(predict(spam.glm.main, newdata = spam[1,]))
exp(predict(spam.glm.main, newdata = data.frame(spam[1,c(1:56)], V57 = 279)))


# compare the drop1 chi square test to the approximate Gaussian test in summary.
# This will take some time.

library(MASS)

drop1(spam.glm.main, response~., test = "Chi", data = spam)

#  Compare the step model with capital letter predictors to the capital letter model

step.cap <- step(spam.cap, data = spam, family = binomial)


# Run stepwise for the complete model
# Do this at home when you have time.
# Compare a stepwise model that starts with the full main effects model to the main effects model.

# **********************************************************
# Repeat the above analysis with log transformed predictors
#***********************************************************


#*****************************
#
# GLM with Interactions
#
#*****************************

# Compare a main effects model with all variables to this same model that also includes
# the interaction terms between V5, V6, V7. 
# Do the comparison with a partial likelihood test.
# (note: do not do a complete interacton model! Unless you have time.)
# Which model do we choose?

spam.glm <- glm(V58~., data = spam, family = binomial)

spam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = spam, family = binomial)

anova(spam.glm, spam.glm2, test = "Chi")

# Now compare a main effects model with log transformed predictors 
# to this same model that also includes
# the interaction terms between the log transformed variables V5, V6, V7. 
# Use an offset of 0.1
# Do this comparison with a partial likelihood test.
# (note: Again do not do a complete interacton model! Unless you have time.) 
# Which model do you choose?

Lspam <- log(spam[,-58] +.1)

Lspam$V58 <- spam[,58]

Lspam.glm <- glm(V58~., data = Lspam, family = binomial)


Lspam.glm2 <- glm(V58~. + (V5+V6+V7)^2, data = Lspam, family = binomial)

anova(Lspam.glm, Lspam.glm2, test = "Chi")


#**************************************************
#
# 		Evaluate performance with AIC
#
#**************************************************

# Compare the AIC for the 4 models you developed.
# Which model would you choose?

AIC(spam.glm)

AIC(spam.glm2)

AIC(Lspam.glm)

AIC(Lspam.glm2)

# Compare the BIC for the 4 models you developed.
# Which model would you choose?

BIC(spam.glm)

BIC(spam.glm2)

BIC(Lspam.glm)

BIC(Lspam.glm2)

#*****************************
#
# Diagnostic plots
#
#*****************************

# Get diagnostic plots and comment

autoplot(spam.glm2)

autoplot(Lspam.glm2)

resid_compare(list(spam.glm2, Lspam.glm2))

# only Cook's distance diagnostics

autoplot(spam.glm2, which = 4, nrow=1, ncol=1)

autoplot(Lspam.glm2, which = 4, nrow=1, ncol=1)

resid_compare(list(spam.glm, Lspam.glm), plots="cookd")


# Influential points
# Find the most influential points
# in both spam.glm and Lspam.glm

which(hatvalues(spam.glm2) > 0.8)

which(hatvalues(Lspam.glm2) > 0.8)

spam[which(hatvalues(spam.glm2) > 0.8),]
