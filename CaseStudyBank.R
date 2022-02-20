library(caret)
library(lattice)
library(ggplot2)
library(gam)
library(car)
library(ggplot)
library(ROCR)


data = read.csv("bank-additional.csv", sep = ";")
summary(data)
str(data)

bank = data

bank$y <- as.factor(bank$y)
summary(bank)

# Majority Null for pdays, duration should be removed
bank$pdays <- NULL 
bank$duration <- NULL


# omitting null/unknowns
bank2 <- na.omit(bank)
bank2[bank2 == "unknown"] <- NA
bank2 <- na.omit(bank2)
str(bank2)


levels(as.factor(bank2$default))
bank2$default = as.factor(bank2$default)
summary(bank2)

# Splitting data train and test
splitBank = sort(sample(nrow(bank2), nrow(bank2)*.75))
Btrain <- bank2[splitBank,]
Btest <- bank2[-splitBank,]

# AIC STUFF
form_2 = as.formula(paste0('y ~ .'))
form_2
set.seed(1234)
objControl <- trainControl(method = "none",
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = TRUE)

Btrain[Btrain == "blue-collar"] <- "bluecollar"
Btrain[Btrain == "self-employed"] <- "selfemployed"

aicmodel <- train(form_2, data = Btrain,
               method = 'glmStepAIC',
               trControl = objControl,
               metric = "ROC",
               direction = 'forward')

summary(aicmodel)
g1 = glm(formula = y ~ ., data = Btrain, family = binomial)
summary(g1)
car::vif(g1)
#multicollinerty square last column > 10
g2 = glm(formula = y ~ age + job + marital + education + default + housing + loan + contact + month + day_of_week + campaign + previous + poutcome + cons.conf.idx, data = Btrain, family = binomial) 
summary(g2)
car::vif(g2)
# model 3 from AIC
g3 = glm(formula = y ~ nr.employed + poutcome + month + contact + cons.conf.idx + campaign, data = Btrain, family = binomial)
car::vif(g3)

# Probability for 1st model
Btrain$PredProb = predict.glm(g2, newdata = Btrain, type = "response")
Btrain$Predy = ifelse(Btrain$PredProb >= 0.5,"yes","no")
caret::confusionMatrix(Btrain$y, as.factor(Btrain$Predy))

# ROC CURVE MODEL 1
#lgPredObj <- prediction(Btrain$PredProb, Btrain$y)
#lgPerfObj <- performance(lgPredObj, "tpr", "fpr")
#plot(lgPerfObj, main = "ROC Curve", col = 2, lwd = 2)
#abline(a = 0, b = 1, lwd = 2, lty = 3, col = "black")


find_p_cutoff <- function(actual_value, positive_class_name, negitive_class_name, pred_probability, p_01=1, p_10=1){
  # Initialising Variables
  msclaf_cost <- c()
  youden_index <- c()
  cutoff <- c()
  P00 <- c() #correct classification of negative as negative (Sensitivity)
  P01 <- c() #misclassification of negative class to positive class (actual is 0, predicted 1)
  P10 <- c() #misclassification of positive class to negative class (actual 1 predicted 0)
  P11 <- c() #correct classification of positive as positive (Specificity)
  
  costs = matrix(c(0, p_01, p_10, 0), ncol = 2)
  
  for (i in 1:100) {
    predList <- as.factor(ifelse(pred_probability >= i/100, positive_class_name, negitive_class_name))
    tbl <- table(predList, actual_value)
    
    # Classifying actual no as yes
    P00[i] <- tbl[1]/(tbl[1] + tbl[2])
    
    P01[i] <- tbl[2]/(tbl[1] + tbl[2])
    
    # Classifying actual yes as no
    P10[i] <- tbl[3]/(tbl[3] + tbl[4])
    
    P11[i] <- tbl[4]/(tbl[3] + tbl[4])
    
    cutoff[i] <- i/100
    msclaf_cost[i] <- P10[i] * costs[3] + P01[i] * costs[2]
    youden_index[i] <- P11[i] + P00[i] - 1
  }
  df.cost.table <- as.data.frame(cbind(cutoff, P10, P01, P11, P00, youden_index, msclaf_cost))
  cat(paste0('The ideal cutoff for:\n Yodens Index approach : ', which.max(df.cost.table$youden_index)/100))
  cat(paste0('\n Cost based approach : ', which.min(df.cost.table$msclaf_cost)/100))
  ggplot(df.cost.table, aes(x = cutoff)) +
    geom_line(aes(y = youden_index, color = 'yoden index')) +
    geom_line(aes(y = msclaf_cost, color = 'misclassification cost'))+
    labs(x = 'Cutoff p value', y='Index',  title = 'Cutoff p value',fill = 'Plot') +
    theme_minimal()+ theme(legend.position="bottom")
}

# CUTOFF MODEL
find_p_cutoff(actual_value = Btrain$y, positive_class_name = 'yes', negitive_class_name = 'no', pred_probability = Btrain$PredProb, p_01 =3, p_10 = 1)
Btrain$Predy = ifelse(Btrain$PredProb >= 0.14,"yes","no")
caret::confusionMatrix(Btrain$y, as.factor(Btrain$Predy))

#### AIC MODEL
Btrain$PredProb3 = predict.glm(g3, newdata = Btrain, type = "response")
Btrain$Predy3 = ifelse(Btrain$PredProb3 >= 0.5,"yes","no")
caret::confusionMatrix(Btrain$y, as.factor(Btrain$Predy3))


#Model 3 Cutoff
find_p_cutoff(actual_value = Btrain$y, positive_class_name = 'yes', negitive_class_name = 'no', pred_probability = Btrain$PredProb3, p_01 =3, p_10 = 1)
Btrain$Predy3 = ifelse(Btrain$PredProb3 >= 0.13,"yes","no")
caret::confusionMatrix(Btrain$y, as.factor(Btrain$Predy3))
