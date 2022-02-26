library(Rcpp)
library(readxl)
library(caret)
library(e1071)
library(MASS)

bbtrain <- read_excel("BBBC-Train.xlsx",sheet = 1)
bbtest <- read_excel("BBBC-Test.xlsx", sheet = 1)

# remove observation column
bbtrain[1] <- NULL 
bbtest[1] <- NULL

str(bbtrain)
anyNA(bbtrain)
anyNA(bbtest)

plot(bbtrain$Choice)


# SVM
bbtrain2 <- bbtrain # COPY DATA
bbtrain2[["Choice"]] = factor(bbtrain2[["Choice"]])
str(bbtrain2)

form1 = Choice ~ .

# Tuning takes a while to finish
tuned = tune.svm(form1, 
                 data = bbtrain2, 
                 gamma= seq(.01, .1,  by = .01), 
                 cost = seq(.1, 1, by = .1))

# create best model
bestsvm <- tuned$best.model
summary(bestsvm)

# make predictions from model and test it
svmpredict = predict(bestsvm, bbtest, type = "response")
caret::confusionMatrix(svmpredict,as.factor(bbtest$Choice))


## LINEAR REGRESSION
bbtrain2 <- bbtrain # reset data
lm <- lm(Choice ~ ., data=bbtrain2)
summary(lm)
# find best model
stepLM <- stepAIC(lm, direction="both")
stepLM$anova
# new model
bestlm <- lm(Choice ~ . - First_purchase, data=bbtrain2)
summary(bestlm)

# create predictions and test it/ probably doing this wrong
prediction <- predict(bestlm, newdata = bbtest)
prediction
bbtrain2$predprob = predict.lm(bestlm)
bbtrain2$predc = ifelse(bbtrain2$predprob >= 0.5,1,0)
bbtrain2$predc
caret::confusionMatrix(as.factor(bbtrain$Choice), as.factor(bbtrain2$predc))


# LOGIT
bbtrain2 <- bbtrain # reset data
bbtrain2$Gender = as.factor(bbtrain$Gender)
str(bbtrain2)
m1 <- glm(formula = Choice ~ . - First_purchase, data = bbtrain2, family = binomial)
summary(m1)

# Test predictions
bbtrain2$predprob = predict.glm(m1, newdata = bbtrain2, type = "response")
bbtrain2$predc = ifelse(bbtrain2$predprob >= 0.5,1,0)
caret::confusionMatrix(as.factor(bbtrain$Choice), as.factor(bbtrain2$predc))


