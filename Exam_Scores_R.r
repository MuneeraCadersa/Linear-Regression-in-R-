#load in the data set
exam=read.csv("Exam_Scores_R.csv",header=T)

#summarize the data set
summary(exam)

#make a scatterplot of exam scores versus hours studied
plot(exam$Hours.Studying,exam$Test.score,main="Scatterplot of Test Scores",ylab="Score",xlab="Hours Studied")

#check correlation
cor(exam$Hours.Studying,exam$Test.score)
cor.test(exam$Hours.Studying,exam$Test.score)

#make a scatterplot of exam scores versus hours studied WITH a trendline
plot(exam$Hours.Studying,exam$Test.score,main="Scatterplot of Test Scores",ylab="Score",xlab="Hours Studied")
abline(lm(exam$Test.score~exam$Hours.Studying), col="red")

#build linear regression model
model=lm(exam$Test.score~exam$Hours.Studying)

#get the regression line
coefficients(model)

#add residuals and predicted exam scores to our dataset
errors=residuals(model)
predicted_values=fitted.values(model)
exam$Predicted_Values=predicted_values
exam$errors=errors

#make a scatterplot of exam scores versus hours studied WITH the average line showing
plot(exam$Hours.Studying,exam$Test.score,main="Scatterplot of Test Scores",ylab="Score",xlab="Hours Studied")
abline(h=78.04, col="blue")

#finding confidence intervals for the slope and intercept
confint(model)

#look at histogram of residuals
hist(exam$errors)

#create Normal Probability Plot
qqnorm(rstandard(model))
