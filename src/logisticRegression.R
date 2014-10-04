#
# Very Small Logistic Regression Example
# The data set is the cases of severe Pneumoconiosis in coal miners from a Biometrics paper from 1959.
# Credit: Much of this has been taken from the video tutorial
# http://www.youtube.com/watch?v=xEllScuasns
#

library("RCurl")

myData <- getURL('https://raw.githubusercontent.com/john-hawkins/MLWorkBook/master/data/CoalMiners.csv')

pneu <- read.csv(text= myData)

# Attach the data so that columns are available as variables in the workspace
attach(pneu)

# Build a new data structure of the 
CW <- cbind(Cases, Miners-Cases)

# Build the Logistic Regression Model
# Using the Generalized Linear Model function
logrm <- glm(CW~Years, family=binomial)

# If you output the results of that model you will see the parameters of the linear model
# and some test statistics. You can use those test statistics to determine whether the model
# is better than a NULL model (with just an intercept) with statistical significance.

# You first need to look for the residual degrees of freedom in the model, in this example it
# is 6. You then use that to get the critical value for a 95% confidence as follows

qchisq(0.95, 6)

# Which results in the critical value 12.59159
# Look in the results of you regression and you will see the Residual Deviance value of 6.051
# As it is lower than the critical value :
# We do not reject the hypothesis that our model is better than the null model


# How do we get predictions from the model ?
# We plug in new independant variables:
newYears <- rbind(11, 19, 45, 59, 63, 69)

# Make them into a data frame
pd <-data.frame(newYears)
# Then rename the column to be the same as the original data set 
# (It is very annoying that you need to do this!)
colnames(pd) <- "Years"
# Now you can use the predict function to plug in your new values
 preds <- predict(logrm, pd, type="response")
# NOTE: If you omit the type, then you will get the results of the linear model, i.e. the Log Odds.

# Alternatively, you can use the paramters of the linear model 
#  and construct a function that output the predictions for you

linearModel <- function(x) { 
	return ( logrm$coefficients["(Intercept)"] + logrm$coefficients["Years"] * x )
}

predictor <- function(x) {
	return ( 1/(1 + exp( - linearModel(x) ) ) )
}

# Now you can apply your predictor function directly to new values ( or vectors thereof)
# simply apply that function directly to new data
results <- predictor(newYears)


# --- Finally: Lets Visualize what the model is doing ---
#  FIRST plot what the model looks like
plot(Years, fitted.values(logrm), xlim=c(0, 70), ylim=c(0, 1), main="Coal Miners & Pneumoconiosis", xlab="Years in Mines", ylab="probability" )
#  THEN plot what the original data looks like
points(Years, Cases/Miners, col="green", pch=16 )
#  FINALLY plot your predictions
points(newYears, predictor(newYears), col="red", pch=18 )



