library(caret)
library(tidyverse)
library(magrittr)
library(olsrr)
library(car)
library(corrplot)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(moments)
library(bestNormalize) # normalization 
library(MASS)
library(psych) 
library(mvnTest) # perform multivariate normality test
library(tree) # perform regression and decision tree
library(randomForest) # perform random forest
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(kmed)
library(klaR)
library(e1071)
library(gridExtra)
library(ggalt)
#install.packages("ROCR")
library(ROCR)
library(MVN)

df <- get(data("heart", package = "kmed"))

# Dependent variable is decreased to two levels: 0 for healthy, 1 for heart disease
df %<>% mutate(class = ifelse(df$class == 0, 0,1)) 
df2 <- df

# required transformations

str(df)

df$sex <- as.numeric(df$sex)
df$sex <- as.factor(df$sex)
df$fbs <- as.numeric(df$fbs)
df$fbs <- as.factor(df$fbs)
df$exang <- as.numeric(df$exang)
df$exang <- as.factor(df$exang)
df$ca <- as.factor(df$ca)
df$class <- as.factor(df$class)


# after transformation
str(df)


sum(is.na(df))
# there is no na in the dataset

##############################
### Descriptive Statistics ###
##############################

summary(df)


# When we examine the descriptive statistics of the numerical values in the data set:

# The mean of the age variable was found to be lower than the median. This shows that the variable is skewed to the left. Considering the difference between the first quartile and the minimum value, it was thought that there might be extreme values.
# The mean of the Trestbps variable was found to be slightly larger than the median. This shows that the variable is skewed to the right. When the quartiles and min max variables are examined, it is thought that there may be outlier observations.
# The mean of the chol variable was found to be larger than the median. This shows that the variable is skewed to the right. When the min-max values are examined with cartillary, it is thought that there may be outlier observations.
# The median of the thalach variable was found to be larger than the mean. This indicates that the variable is skewed to the left. When the difference between the min-max values with the quartiles is examined, it is thought that there may be outlier observations.
# Boxplots will be used for outlier observations and histogram graphs will be used to have a general information about the distributions.


# When the descriptive statistics of the categorical values in the data set are examined:

# When the sex variable is analyzed, it is determined that the majority of the observations in the data are male.
# When the cp variable was examined, it was determined that the majority had asymptomatic chest pain.
# When the fbs variable was analyzed, it was determined that the majority of the observations had blood glucose less than 120mg/dl.
# When the restecg variable was analyzed, it was found that the observations had normal to possible electrocardiographic results and very few had abnormal results.
# When the  exang variable was analyzed, it was found that the majority of observations did not have angina.
# When the slope variable was analyzed, it was found that the slope of the exercise ST segment was flat in the majority of observations.
# When the ca variable was examined, it was determined that the majority of the observations took the value 0.
# When the thal variable was analyzed, it was observed that the majority of the observations received normal and reversable defect levels.
# When the dependent variable class was analyzed, it was found that 160 people did not have a heart attack and 137 people had a heart attack. 

##########################
### Data Visualization ###
##########################

par(mfrow = c(1,5), bty = "n")

boxplot(df$age, col = "goldenrod1", main = "Age", border = "firebrick3")
boxplot(df$trestbps, col = "goldenrod1" ,main = "Trestbps", border = "firebrick3")
boxplot(df$chol, col = "goldenrod1", main = "Chol", border = "firebrick3")
boxplot(df$thalach, col = "goldenrod1", main = "Thalach", border = "firebrick3")
boxplot(df$oldpeak, col = "goldenrod1", main = "Oldpeak", border = "firebrick3")

# When the box plots of numerical variables are analyzed:

# There is no outlier observation in the age variable. Left skewness is again noteworthy. Its range is quite high.
# When the Trestbps variable is analyzed, it is found that there are many outlier observations. 
# When the Chol variable is analyzed, 5 outlier observations are detected.
# When the Thalach variable was analyzed, 1 outlier observation was detected.
# 4 outlier observations were observed in the Oldpeak variable. 



indexes = sapply(df, is.numeric)
indexes["class"] = TRUE
df[,indexes]%>%
  gather(-class, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = class, color = class)) +
  geom_boxplot() +
  facet_wrap(~ var, scales = "free")+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.85),legend.position="none",
        panel.background = element_rect(fill = "white"))+
  theme(strip.background =element_rect(fill="goldenrod1"))+
  theme(strip.text = element_text(colour = "firebrick3"))

# When the box plots of the numerical variables according to the levels of the dependent variable are analyzed:

# Observations who did not have a heart attack were found to be in a wider range.
# It was found that the average age of observations who had a heart attack was higher than those who did not.
# Interestingly, there is no noticeable change for the variable containing cholesterol information according to the levels of the class variable.
# It is also interesting to note that the individual with maximum cholesterol did not have a heart attack. 
# When the oldpeak variable, which contains information on ST depression caused by exercise compared to rest, was examined, it was found that individuals who had a heart attack had higher values.
# When the thalach variable, which includes the maximum heart rate reached, was examined, it was found that individuals who did not have a heart attack reached a higher heart rate. While it was found that the observations who had a heart attack had a wider range, it was also found that they had lower values.
# When the variable trestbps, which includes resting blood pressure information, is analyzed, there is no difference between the averages of those who had a heart attack and those who did not. However, it can be said that those who had a heart attack had slightly higher values. 


################################
### Train - Test Separation ###
##############################

smp_size <- floor(0.70 * nrow(df)) 
set.seed(2021900444) 
train_ind <- sample(nrow(df), size = smp_size, replace = FALSE)
train <- df[train_ind, ]
test <- df[-train_ind, ]

#################################
### Classification Algorithms ###
#################################

###########################
### Classification Tree ###
###########################

####################
### Tree Package ###
####################

treeclass <- tree(class~. , train )
summary(treeclass ) 

# Tree was created with a total of 18 terminal nodes.
# Residual mean deviance is 0.4224.
# Error rate is as low as 0.09.
dev.off()

plot(treeclass )
text(treeclass ,pretty =0)

# The root node is identified as if cp is 1,2 and 3.
# age less than 66.5 was identified as one of the terminal nodes. However, it is noteworthy that there is no change for both nodes
# The other terminal nodes have a similar situation. It is clear that the tree needs to be pruned.
# Age lower than 55.5 was identified as one of the internal nodes.
# Most of the terminal nodes have the same values. This emphasizes the need for pruning.

set.seed(3)
cv.treeclass <- cv.tree(treeclass ,FUN=prune.misclass )
plot(cv.treeclass$size ,cv.treeclass$dev ,type="b")


# When both graphs are analyzed, there is a significant decrease in deviance at size 4. 
# However, although the same is true for 5,6,7,8, an increase is observed at 10. After 10, the decrease occurred again.
# For this reason, it was decided to make two prunings for both the first decrease of 4 and the second decrease of 12.

### First Pruning

prune.treeclass1 <- prune.misclass (treeclass,best=4)
summary(prune.treeclass1)

# When building the model with only four nodes after pruning, the residual mean deviance was found to be 0.82. It seems to have shown an increase.
# Error rate is 0.1594. There also seems to be an increase for the misclassification error rate.

dev.off()
plot(prune.treeclass1 )
text(prune.treeclass1 ,pretty =0)

# It is not a very efficient tree since there are only four terminal nodes. 
# The tree was built using only cp, cave, and thalach variables.


### Second Pruning

prune.treeclass2 <- prune.misclass (treeclass,best=12)
summary(prune.treeclass2)

# When building the model with 12 nodes after pruning, the residual mean deviance was found to be 0.5. It seems to show an increase compared to the first tree. However, it is a lower value than the first pruned tree.
# The error rate was 0.09179. The same value for the misclassification error rate as the unpruned tree.

dev.off()
plot(prune.treeclass2 )
text(prune.treeclass2 ,pretty =0)

# The tree was created using the variables cp, age, thalach, testbps, sex, ca, exang, chol and restecg.
# The terminal nodes do not show splits of the same class. For this reason, it can be said that it is a more accurate distinction than the unpruned tree. 

### Predictions before pruning ###

### Train

classtree.pred <- predict(treeclass ,train ,type="class")

a<-caret::confusionMatrix(classtree.pred, train$class)  
a$overall[1]
# Accuracy Rate : 0.9082
# Sensitivity : 0.8981
# Specificity : 0.9192

ctpredictions <- data.frame()
ctpredictions[1,1] <- "Before Prune CT"
ctpredictions[1,2] <- "Train"
ctpredictions[1,3] <- a$overall[1]
ctpredictions[1,4] <- a$byClass[1]
ctpredictions[1,5] <- a$byClass[2]


### Test

classtree.predtest <- predict(treeclass, test, type = "class")

a <- caret::confusionMatrix(classtree.predtest, test$class)

# Accuracy Rate : 0.6889
# Sensitivity : 0.7308
# Specificity : 0.6316

ctpredictions[2,1] <- "Budama Öncesi CT"
ctpredictions[2,2] <- "Test"
ctpredictions[2,3] <- a$overall[1]
ctpredictions[2,4] <- a$byClass[1]
ctpredictions[2,5] <- a$byClass[2]


### Predictions of first pruning ###

### Train

prunedtree.pred1 <- predict(prune.treeclass1 ,train ,type="class")

a <- caret::confusionMatrix(prunedtree.pred1, train$class)

# Accuracy Rate : 0.8406
# Sensitivity : 0.9259
# Specificity : 0.7475

ctpredictions[3,1] <- "İlk Budama CT"
ctpredictions[3,2] <- "Train"
ctpredictions[3,3] <- a$overall[1]
ctpredictions[3,4] <- a$byClass[1]
ctpredictions[3,5] <- a$byClass[2]


### Test

prunedtree.predtest1 <- predict(prune.treeclass1, test, type = "class")

a <- caret::confusionMatrix(prunedtree.predtest1, test$class)

# Accuracy Rate : 0.711
# Sensitivity : 0.8654
# Specificity : 0.5

ctpredictions[4,1] <- "İlk Budama CT"
ctpredictions[4,2] <- "Test"
ctpredictions[4,3] <- a$overall[1]
ctpredictions[4,4] <- a$byClass[1]
ctpredictions[4,5] <- a$byClass[2]


### Predictions of second pruning ###

### Train

prunedtree.pred2 <- predict(prune.treeclass2 ,train ,type="class")

a <- caret::confusionMatrix(prunedtree.pred2, train$class)

# Accuracy Rate : 0.9082
# Sensitivity : 0.9074
# Specificity : 0.9091

ctpredictions[5,1] <- "İkinci Budama CT"
ctpredictions[5,2] <- "Train"
ctpredictions[5,3] <- a$overall[1]
ctpredictions[5,4] <- a$byClass[1]
ctpredictions[5,5] <- a$byClass[2]

### Test

prunedtree.predtest2 <- predict(prune.treeclass2, test, type = "class")

a <- caret::confusionMatrix(prunedtree.predtest2, test$class)

# Accuracy Rate : 0.7111
# Sensitivity : 0.7692
# Specificity : 0.6316

ctpredictions[6,1] <- "İkinci Budama CT"
ctpredictions[6,2] <- "Test"
ctpredictions[6,3] <- a$overall[1]
ctpredictions[6,4] <- a$byClass[1]
ctpredictions[6,5] <- a$byClass[2]

########################################################### rpart package ############################################################

# this function also performs cross validation and automatically generates the pruned tree with the fewest errors.
treeclass2 <- rpart(class~., data = train, method = "class")

treeclass2$variable.importance

# When ranking the importance of variables, the most important variable is cp followed by thalach, thal, exang variables.

treeclass2$numresp
# the tree was constructed using four independent variables.

rpart.plot(treeclass2)

# when we examine the tree, it is noteworthy that cp is 1,2,3 again as the root node.
# internal nodes draw attention as the cases where ca is equal to zero, thalach is greater than 146.
# there are 7 terminal nodes in total. each assignment is shown in different colors. the shade of the colors indicates the amount of observations it contains.



### Predictions ###

preds <- data.frame()

preds[1,1] <- "ClassTree"
preds[1,2] <- "Train"

### Train

prunedtree.pred3 <- predict(treeclass2 ,train ,type="class")

a <- caret::confusionMatrix(prunedtree.pred3, train$class)

# Accuracy Rate : 0.8744
# Sensitivity : 0.9444
# Specificity : 0.7980

preds[1,3] <- 0.8744 
preds[1,4] <- 0.9444
preds[1,5] <- 0.7980

ctpredictions[7,1] <- "rpart CT"
ctpredictions[7,2] <- "Train"
ctpredictions[7,3] <- a$overall[1]
ctpredictions[7,4] <- a$byClass[1]
ctpredictions[7,5] <- a$byClass[2]

### Test

prunedtree.predtest3 <- predict(treeclass2, test, type = "class")

a <- caret::confusionMatrix(prunedtree.predtest3, test$class)

# Accuracy Rate : .78
# Sensitivity : 0.9038
# Specificity : 0.6316



ctpredictions[8,1] <- "rpart CT"
ctpredictions[8,2] <- "Test"
ctpredictions[8,3] <- a$overall[1]
ctpredictions[8,4] <- a$byClass[1]
ctpredictions[8,5] <- a$byClass[2]

names(ctpredictions) <- c("Algoritma", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )



ctpredictions %>% 
  ggplot(aes(x= Accuracy_Rate, y= reorder(Algoritma, -Accuracy_Rate))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Accuracy Rate") +
  ylab("Algoritma") 

ctpredictions %>% 
  ggplot(aes(x= Sensivity, y= reorder(Algoritma, -Sensivity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Sensitivity") +
  ylab("Algoritma")


ctpredictions %>% 
  ggplot(aes(x= Specificity, y= reorder(Algoritma, -Specificity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Specificity") +
  ylab("Algoritma")

# Ultimately, sensitivity values will be important for us. Because predicting someone with heart disease as having heart disease is of vital importance.
# For this reason, when choosing between algorithms, the tree built with rpart, which gives a high sensitivity value, stands out.
# The fact that the distinction between train and test is also less has led to this choice.
# When plotting the ROC curve, the classification tree built with the rpart package will be used.

##############################################################################################################################################

###############
### Bagging ###
###############


### Random Forest Package ###

set.seed(125) 
bag <- randomForest(class~. , data=train, mtry=13,importance=TRUE)

bag

# The model was built using a total of 500 trees.
# 13 variables were used in each split.
# The OOB error rate was found to be 18.84%.

bag$importance
varImpPlot(bag)

# important variables according to the meandecreaseaccuracy value of the variable proline when the graph indicating the importance of the variables is analyzed:
# ca, cp,, thalach, oldpeak, thal
# important variables by gini value indicating node purity:
# cp, thalach, ca, oldpeak, thal, age

### ipred package ###

# Use nbagg to control how many iterations are included in the model 
# coob = TRUE indicates to use the OOB error rate. 
# 10-fold cross validation with control argument is implemented inside the function

bag2 <- bagging(
  formula = class ~ .,
  data = train,
  nbagg = 500,  
  coob = TRUE,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10))


bag2$err

# OOB Missclassification error rate is 0.1642 

VI <- data.frame(var=names(train[,-14]), imp=varImp(bag2))

VI_plot <- VI[order(VI$Overall, decreasing=F),]

barplot(VI_plot$Overall,
        names.arg=rownames(VI_plot),
        horiz=T,
        col="goldenrod1",
        xlab="Variable Importance",
        las = 2)

# When we examined the graph expressing the importance of the variables, a different graph was encountered from the previous package.
# While the ca and cp variables appeared to be the most important variables in the other package, it was noticed that the thalach variable was more important this time.
# It can be said that thalach is followed by cp, ca, thal, old peak, exang and age variables.

bagpred <- data.frame()

### RF package preds ###

### Train

baggintrain <- predict(bag ,train ,type="class")

a <- caret::confusionMatrix(baggintrain, train$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1


### Test

baggintest <- predict(bag, test, type = "class")

b <- caret::confusionMatrix(baggintest, test$class)

# Accuracy Rate : 0.8
# Sensitivity : 0.9038
# Specificity : 0.6579

bagpred <- data.frame()
bagpred[1,1] <- "bagmodel1"
bagpred[1,2] <- "train"
bagpred[2,2] <- "test"
bagpred[2,1] <- "bagmodel1"
bagpred[1,3] <- a$overall[1]
bagpred[2,3] <- b$overall[1]
bagpred[1,4] <- a$byClass[1]
bagpred[2,4] <- b$byClass[1]
bagpred[1,5] <- a$byClass[2]
bagpred[2,5] <- b$byClass[2]


### ipred package preds

### Train

baggintrain1 <- predict(bag2 ,train ,type="class")

a <- caret::confusionMatrix(baggintrain1, train$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1

### Test

baggintest1 <- predict(bag2, test, type = "class")

b <- caret::confusionMatrix(baggintest1, test$class)

# Accuracy Rate : 0.81
# Sensitivity : 0.9231
# Specificity : 0.6579


bagpred[3,1] <- "ipredmodel"
bagpred[3,2] <- "train"
bagpred[4,2] <- "test"
bagpred[4,1] <- "ipredmodel"
bagpred[3,3] <- a$overall[1]
bagpred[4,3] <- b$overall[1]
bagpred[3,4] <- a$byClass[1]
bagpred[4,4] <- b$byClass[1]
bagpred[3,5] <- a$byClass[2]
bagpred[4,5] <- b$byClass[2]



names(bagpred) <- c("Algoritma", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )

bagpred %>% 
  ggplot(aes(x= Accuracy_Rate, y= reorder(Algoritma, -Accuracy_Rate))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Accuracy Rate") +
  ylab("Algoritma") 

bagpred %>% 
  ggplot(aes(x= Sensivity, y= reorder(Algoritma, -Sensivity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Sensitivity") +
  ylab("Algoritma")

bagpred %>% 
  ggplot(aes(x= Specificity, y= reorder(Algoritma, -Specificity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Specificity") +
  ylab("Algoritma")


##############################################################################################################################################

#####################
### Random Forest ###
#####################

sqrt(13)

rf <- randomForest(class~. ,data=train, mtry=4,importance=TRUE)

rf$confusion

# The error was 0.14 for class zero and 0.18 for class one. A total of 34 observations were misclassified.


rf 
# OOB estimate error rate was 16.43%. This can be said to be a lot.
# 4 variables were tried in each decomposition.
# A total of 500 trees were built.

which.max(rf$err.rate[,1]) 


rf$importance
varImpPlot(rf)

# When we look at the importance of the variables;

# When mean decrease accuracy is analyzed, the order of ca, followed by cp, thalach, thal, oldpeak stands out the most.
# When we look at the Gini values, the order of thalach, cp, ca stands out
# It would not be wrong to say that it is quite parallel to Bagging.

###################
### Grid Search ###
###################

# A graph is drawn to decide the range of the number of trees in the grid search.
plot(rf)

hyper_grid <- expand.grid(
  mtry = c(3, 4, 5, 6),
  nodesize = c(1, 3, 5, 10), 
  numtrees = c(200, 220,300,330),
  rmse = NA                                               
)

# fitting
for (i in 1:nrow(hyper_grid)) {
  fit <- randomForest(class~. ,
                      data=train, 
                      mtry=hyper_grid$mtry[i],
                      nodesize = hyper_grid$nodesize[i],
                      ntree = hyper_grid$numtrees[i],
                      importance=TRUE)
  hyper_grid$rmse[i] <- mean(fit$confusion[,3])
}

# assessing top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  head(10)

# Thus, the model with the best parameters should be as follows.

rf2 <- randomForest(class~. ,data=train, mtry=4,importance=TRUE, nodesize = 10, ntree= 220)

rf2$confusion

# The error was 0.10 for class zero and 0.18 for class one. A total of 29 observations were misclassified.

rf2

# OOB estimate error rate was 14.01%. This can be said to be a lot.
# 4 variables were tried in each decomposition.
# A total of 220 trees were built.


rf2$importance
varImpPlot(rf2)

# When we look at the importance of the variables;

# When mean decrease accuracy is analyzed, the order of ca, followed by cp, thalach, thal, oldpeak stands out the most.
# When Gini values are analyzed, the order of cp, ca, thalach, thal, oldpeak stands out
# Observed change in gini importance rank compared to the initial random forest model.


### first models predictions ###

rfpred <- data.frame()
### Train

ranfortrain <- predict(rf ,train ,type="class")

a <- caret::confusionMatrix(ranfortrain, train$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1

### Test

ranfortest <- predict(rf, test, type = "class")

b <- caret::confusionMatrix(ranfortest, test$class)

# Accuracy Rate : 0.8
# Sensitivity : 0.9038
# Specificity : 0.6579

rfpred <- data.frame()
rfpred[1,1] <- "rfmodel1"
rfpred[2,1] <- "rfmodel1"
rfpred[1,2] <- "train"
rfpred[2,2] <- "test"
rfpred[1,3] <- a$overall[1]
rfpred[2,3] <- b$overall[1]
rfpred[1,4] <- a$byClass[1]
rfpred[2,4] <- b$byClass[1]
rfpred[1,5] <- a$byClass[2]
rfpred[2,5] <- b$byClass[2]

### preds after gs ###

### Train

ranfortrain1 <- predict(rf2 ,train ,type="class")

a <- caret::confusionMatrix(ranfortrain1, train$class)

# Accuracy Rate : 0.94
# Sensitivity : 0.9722
# Specificity : 0.9091



### Test

ranfortest1 <- predict(rf2, test, type = "class")

b <- caret::confusionMatrix(ranfortest1, test$class)

# Accuracy Rate : 0.7778
# Sensitivity : 0.9038
# Specificity : 0.6053

rfpred[3,1] <- "rfmodel2"
rfpred[4,1] <- "rfmodel2"
rfpred[3,2] <- "train"
rfpred[4,2] <- "test"
rfpred[3,3] <- a$overall[1]
rfpred[4,3] <- b$overall[1]
rfpred[3,4] <- a$byClass[1]
rfpred[4,4] <- b$byClass[1]
rfpred[3,5] <- a$byClass[2]
rfpred[4,5] <- b$byClass[2]



names(rfpred) <- c("Algoritma", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )


rfpred %>% 
  ggplot(aes(x= Accuracy_Rate, y= reorder(Algoritma, -Accuracy_Rate))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Accuracy Rate") +
  ylab("Algoritma")


rfpred %>% 
  ggplot(aes(x= Sensivity, y= reorder(Algoritma, -Sensivity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Sensitivity") +
  ylab("Algoritma")

rfpred %>% 
  ggplot(aes(x= Specificity, y= reorder(Algoritma, -Specificity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Specificity") +
  ylab("Algoritma")


##############################################################################################################################################

###########################
### Logistic Regression ###
###########################

logmodel1 <- glm(class ~ age + sex + cp + trestbps + chol +
                   fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = train, family = binomial)

summary(logmodel1)

# models statistical significance

#𝐻 0 : 𝛽 1 = 𝛽 2 = ⋯ = 𝛽 𝑘 = 0
# 𝐻 1 : En azından bir 𝛽 𝑗 ≠ 0

# G= Null deviance-Residual Deviance
286.57 - 118.63

1-pchisq(286.57 - 118.63,206-186) 

# Since this p-value is less than .05, we can reject the null hypothesis. 
### In other words, we have sufficient statistical evidence to say that the independent variables are effective in explaining the dependent variable. 


###
# coefficient comment

# Change in prediction value when we increase the value of the independent variable by one unit to determine the log(odds), the exp function is first applied to both sides of the log(odds) formula.

# Coefficient interpretation of significant variables:


exp(-3.279e-02) # 1 unit increase in age variable changes odds ratio by 0.9677418 times.
exp(1.497e+00) # a one unit increase in sex1 variable changes odds ratio by 4.468264 times
exp(2.293e+00) # A one unit increase in the variable cp2 changes the odds ratio by a factor of 9.904607
exp(1.143e+00) # 1 unit increase in variable cp3 changes odds ratio by 3.136163 times.
exp(3.281e+00) # 1 unit increase in variable cp4 changes odds ratio by 26.60236 times.
exp(2.477e-02) # a one unit increase in the variable trestbps changes the odds ratio by 1.025079 times
exp(5.457e-03) # A one unit increase in the variable chol changes the odds ratio by a factor of 1.005472
exp(-4.559e-02) # 1 unit increase in variable fbs1 changes odds ratio by 0.9554336 times.
exp(1.306e+01) # 1 unit increase in variable restecg1 changes odds ratio by 469770.7 times.
exp(5.368e-02) # 1 unit increase in restecg2 variable changes odds ratio by 1.055147 times.
exp(-3.774e-02) # a one unit increase in the variable thalach changes the odds ratio by 0.9629633 times
exp(6.948e-0) # 1 unit increase in exang1 variable changes odds ratio by 1041.066 times.
exp(3.551e-01) # 1 unit increase in oldpeak variable changes odds ratio by 1.426323 times.
exp(1.350e+00) # 1 unit increase in slope2 variable changes odds ratio by 3.857426 times.
exp(1.005e+00) # 1 unit increase in slope3 variable changes odds ratio by 2.731907 times.
exp(2.688e+00) # 1 unit increase in ca1 variable changes odds ratio by 14.70224 times.
exp(4.369e+00) # one unit increase in ca2 variable changes odds ratio 78.96463 times
exp(2.833e+00) # 1 unit increase in ca3 variable changes odds ratio by 16.99637 times.
exp(-8.000e-01) # 1 unit increase in thal6 variable changes odds ratio by 0.449329 times.
exp(9.626e-01) # 1 unit increase in thal7 variable changes odds ratio by 2.618496 times.

###  Confidence Interval for Coefficients

confint.default(logmodel1)

# Since the confidence interval for the β coefficient does not include the zero value, the null hypothesis Ho is rejected and the following coefficients are statistically significant.

# sex1, cp4, thalach, slope2, ca

# Since the confidence interval for the β coefficient contains zero value, the null hypothesis Ho cannot be rejected and the following coefficients are not statistically significant.

# age, cp2, cp3, trestbps, chol, fbs1, restecg, exang, oldpeak, slope3, thal


### Confidence Interval for odds 

odds.confint <- exp(confint.default(logmodel1))
odds.confint

# Since the confidence interval for the odds ratio value does not include the value 1, the null hypothesis Ho is rejected and the following coefficients are statistically significant:

# age, sex, cp, trestbps, chol, fbs, restecg2, exang, oldpeak, slope, ca, thal

# The interpretation of the significant variables according to the confidence interval for the odds value would be as follows: 

cat("Increasing Age by one unit increases the odds of having a heart attack by a factor between ", odds.confint[2,1], " and ", odds.confint[2,2], "times the odds of having a heart attack with 95% confidence when Age is one unit lower" )
cat("Increasing Sex by one unit increases the odds of having a heart attack at 95% confidence by a factor between ", odds.confint[3,1], " and ", odds.confint[3,2], "times that of Sex by one unit lower" )
cat("Increasing cp2 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[4,1], " and ", odds.confint[4,2], "times that of cp2 being one unit lower" )
cat("Increasing cp3 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[5,1], " and ", odds.confint[5,2], "times that of cp3 being one unit lower" )
cat("Increasing cp4 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[6,1], " and ", odds.confint[6,2], "times that of cp4 being one unit lower" )
cat("Increasing trestbps by one unit increases the odds of having a heart attack at 95% confidence by a factor between ", odds.confint[7,1], " and ", odds.confint[7,2], "times the odds of having a heart attack by one unit less" )
cat("Increasing chol by one unit increases the odds of having a heart attack at 95% confidence between ", odds.confint[8,1], " and ", odds.confint[8,2], "times the odds of having a heart attack by one unit less" )
cat("Increasing fbs1 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[9,1], " and ", odds.confint[9,2], "times the odds of having a heart attack by one unit less" )
cat("Increasing restecg2 by one unit increases the odds of having a heart attack at 95% confidence by a multiple of ", odds.confint[11,1], " to ", odds.confint[11,2], "times that of restecg2 by one unit lower" )
cat("Increasing exang1 by one unit increases the odds of having a heart attack at 95% confidence by a factor between ", odds.confint[12,1], " and ", odds.confint[12,2], "times that of exang1 by one unit lower" )
cat("Increasing oldpeak by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[13,1], " and ", odds.confint[13,2], "times the odds of having a heart attack by one unit lower than oldpeak" )
cat("Increasing slope2 by one unit increases the odds of having a heart attack at 95% confidence by a multiple of ", odds.confint[14,1], " to ", odds.confint[14,2], "times that of slope2 by one unit lower" )
cat("Increasing slope3 by one unit increases the odds of having a heart attack at 95% confidence by a multiple of ", odds.confint[15,1], " to ", odds.confint[15,2], "times that of slope3 by one unit lower" )
cat("Increasing ca1 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[16,1], " and ", odds.confint[16,2], "times that of ca1 being one unit lower" )
cat("Increasing ca2 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[17,1], " and ", odds.confint[17,2], "times that of ca2 being one unit lower" )
cat("Increasing ca3 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[18,1], " and ", odds.confint[18,2], "times that of ca3 being one unit lower" )
cat("Increasing thal6 by one unit increases the odds of having a heart attack at 95% confidence by a factor between ", odds.confint[19,1], " and ", odds.confint[19,2], "times that of decreasing thal6 by one unit" )
cat("Increasing thal7 by one unit increases the odds of having a heart attack with 95% confidence by a factor between ", odds.confint[20,1], " and ", odds.confint[20,2], "times that of decreasing thal7 by one unit" )


## Logistic Regression Diagnosis

### Outliers 

outlierTest(logmodel1)
# There is no outlier according to benforrini

### Leverages
hvalues <- influence(logmodel1)$hat
r_si <- pearson.res.chd/(sqrt(1-hvalues))

# The following observations were determined as leverage points:
which(abs(r_si) > 2)


influencePlot(logmodel1)
# Observations taken in the big blue circle stand out as effective observations. 
# There seem to be quite a lot of them.




lgpred <- data.frame()

### preds with median

ppred <- fitted(logmodel1)
summary(ppred)
# İlk olarak threshold değeri medyan değeri kabul edilecek tahminlerde bulunulacaktır.
threshold <- 0.356234
ppred[ppred > threshold] <- 1
ppred[ppred < threshold] <- 0
ppred <- as.factor(ppred)
a <- caret::confusionMatrix(ppred, train$class)
# Accuracy Rate : 0.87
# Sensitivity : 0.86
# Specificity : 0.8889
### Test
testpred <- predict(logmodel1, newdata = test)
testpred[testpred > threshold] <- 1
testpred[testpred < threshold] <- 0
testpred <- as.factor(testpred)
b <- caret::confusionMatrix(testpred, test$class)
# Accuracy Rate : 0.83
# Sensitivity : 0.9038
# Specificity : 0.7368

lgpred <- data.frame()
lgpred[1,1] <- "lrmedyan"
lgpred[2,1] <- "lrmedyan"
lgpred[1,2] <- "train"
lgpred[2,2] <- "test"
lgpred[1,3] <- a$overall[1]
lgpred[2,3] <- b$overall[1]
lgpred[1,4] <- a$byClass[1]
lgpred[2,4] <- b$byClass[1]
lgpred[1,5] <- a$byClass[2]
lgpred[2,5] <- b$byClass[2]


### Means preds

ppred <- fitted(logmodel1)

threshold <- 0.4688995
ppred[ppred > threshold] <- 1
ppred[ppred < threshold] <- 0
ppred <- as.factor(ppred)
a <- caret::confusionMatrix(ppred, train$class)


# Accuracy Rate : 0.88
# Sensitivity : 0.91
# Specificity : 0.8586


### Test
testpred1 <- predict(logmodel1, newdata = test)
testpred1[testpred1 > threshold] <- 1
testpred1[testpred1 < threshold] <- 0
testpred1 <- as.factor(testpred1)
b <- caret::confusionMatrix(testpred1, test$class)


# Accuracy Rate : 0.83
# Sensitivity : 0.9038
# Specificity : 0.7368


lgpred[3,1] <- "lrmean"
lgpred[4,1] <- "lrmean"
lgpred[3,2] <- "train"
lgpred[4,2] <- "test"
lgpred[3,3] <- a$overall[1]
lgpred[4,3] <- b$overall[1]
lgpred[3,4] <- a$byClass[1]
lgpred[4,4] <- b$byClass[1]
lgpred[3,5] <- a$byClass[2]
lgpred[4,5] <- b$byClass[2]


### Threshold = 0.6 preds

ppred <- fitted(logmodel1)
summary(ppred)
threshold <- 0.6
ppred[ppred > threshold] <- 1
ppred[ppred < threshold] <- 0
ppred <- as.factor(ppred)
a <- caret::confusionMatrix(ppred, train$class)
# Accuracy Rate : 0.88
# Sensitivity : 0.9537
# Specificity : 0.8182



### Test
testpred1 <- predict(logmodel1, newdata = test)
testpred1[testpred1 > threshold] <- 1
testpred1[testpred1 < threshold] <- 0
testpred1 <- as.factor(testpred1)
b <- caret::confusionMatrix(testpred1, test$class)
# Accuracy Rate : 0.83
# Sensitivity : 0.9231
# Specificity : 0.7105

lgpred[5,1] <- "lr0.6"
lgpred[6,1] <- "lr0.6"
lgpred[5,2] <- "train"
lgpred[6,2] <- "test"
lgpred[5,3] <- a$overall[1]
lgpred[6,3] <- b$overall[1]
lgpred[5,4] <- a$byClass[1]
lgpred[6,4] <- b$byClass[1]
lgpred[5,5] <- a$byClass[2]
lgpred[6,5] <- b$byClass[2]


names(lgpred) <- c("Algorithm", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )
# Accuracy Rate, sensitivity and specificity values should be as high as possible.
# However, as a result of various experiments, it has been observed that as the sensitivity value increases, the specificity value decreases.
# Considering this dataset and the levels we want to classify, it was concluded that the accuracy of predicting heart attack prediction of heart attack survivors is more important.
# For this reason, the last threshold, which is the highest according to the sensitivity result, is preferred as the threshold we will choose.

lgpred %>% 
  ggplot(aes(x= Accuracy_Rate, y= reorder(Algoritma, -Accuracy_Rate))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Accuracy Rate") +
  ylab("Algoritma")

lgpred %>% 
  ggplot(aes(x= Sensivity, y= reorder(Algoritma, -Sensivity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Sensitivity") +
  ylab("Algoritma")

lgpred %>% 
  ggplot(aes(x= Specificity, y= reorder(Algoritma, -Specificity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Specificity") +
  ylab("Algoritma")



############################
### Varsayım Kontrolleri ###
############################


vif(logmodel1)
# no  multicolinerity.

# Sample size:

# The number of observations we need to have if we want a success like 0.90:

10*13/0.90

# Since we have more than 144 observations, we can say that this assumption is also met.


##############################################################################################################################################

####################################
### Linear Discriminant Analysis ###
####################################

# Since only numeric variables can be used in Linear Discriminant Analysis, a data set consisting of numeric data will be created and so on.

trainda <- train[,c(1,4,5,8,10,14)]
testda <- test[,c(1,4,5,8,10,14)]

pairs.panels(trainda[1:5],
             gap=0,
             bg=c("goldenrod1","firebrick4")[train$class],
             pch=21)

# when the pairplot graph is examined:

# some pairs of variables seem to have separated in spite of the errors. these pairs of variables can be listed as follows:
# especially in the graphs examining the relationship of the variable thalach with other variables, the divergences seem to be more obvious.
# Likewise, the divergences in the oldpeak variable also seem to be clearly realized.
# the separations in the chol, trestbps and age variables appear to be less pronounced.
# histograms of the variables are close to normal despite the skewness, but the histogram of the oldpeak variable in particular is highly skewed.
# When the correlations are analyzed, the correlation of -0.35 between thalach and age stands out as the highest correlation.
# The correlation between oldpeak and thalach is second with -0.32.

desc <- describeBy(trainda[1:5], trainda[,6]) # looking for each type is required for score calculation
desc

# When descriptive statistics by class are analyzed:

# the thalach variable showed more separation between classes.
# The oldpeak variable also seems to provide separation, although not as much as the thalach variable.


model_lda<-lda(class~.,data=trainda)
model_lda

# When the output of the linear discriminant analysis model is analyzed:

# There is only one linear discrimination.
# The probability of the observations belonging to the zero class is 0.52 and the probability of belonging to the first group is 0.47.
# The ratios are very close to each other.


predict_1<-predict(model_lda,trainda)
hist_lda1<-ldahist(data=prediction_1$x[,1],g=trainda$class) 

# Comparative histograms showing how it should be separated according to the first function: 
# There is an approximate overlap between the values -1 and 1. 
# This may indicate possible mis-sorting.

# When the probabilities of the observations being included in the groups are examined, it is found that some observations have close probability values. 
# Some of these observations can be listed as follows.

tahmin_1$posterior[13,]
tahmin_1$posterior[14,]
tahmin_1$posterior[52,]


partimat(class~., data=trainda,method="lda") 

# When the Partition Graph is analyzed: 

# When observations are shown in red color, it indicates mislabeling. 
# The graph with the least mislabeling is seen in the graph showing the relationship and separation between oldpeak and thalach.
# When the graphs between trestbps and chol and age and trestbps are examined, it is noteworthy that the separation is not done well.



### Preds

### Train

ldatrain <- predict(model_lda ,trainda)
caret::confusionMatrix(ldatrain$class, trainda$class)

# Accuracy Rate : 0.7488
# Sensitivity : 0.7963
# Specificity : 0.6970

### Test

ldatest <- predict(model_lda ,testda)
caret::confusionMatrix(ldatest$class, testda$class)

# Accuracy Rate : 0.7
# Sensitivity : 0.8269
# Specificity : 0.5263

preds[13,1] <- "lda"
preds[13,2] <- 0.7488
preds[13,3] <- 0.7
preds[13,4] <- 0.79
preds[13,5] <- 0.8269
preds[13,6] <- 0.6970
preds[13,7] <- 0.5263

##############################################################################################################################################

#######################################
### Quadratic Discriminant Analysis ###
#######################################

set.seed(2021900444)
train_indices <- sample(2, size=nrow(df), replace = TRUE, prob=c(0.7,0.3))
trainn <- df[train_indices==1, ]
qdatest <- df[train_indices==2, ]

trainn <- trainn[,c(1,4,5,8,10,14)]
qdatest <- qdatest[,c(1,4,5,8,10,14)]


model_qda <- qda(class~. , data=trainn) 

model_qda

# When the Quadratic Discriminant Analysis output is analyzed:

# The probability of placing the observations in the zero class is 0.53 and the probability of placing them in the first class is 0.46.
# Such a low difference between the probabilities calls into question the reliability of the model.
# When we look at the averages of the zero and first classes on the basis of variables, we see that the variables trestbps, chol, thalach, oldpeak are more clearly differentiated,
# It is observed that there is not a good decomposition on the basis of other variables. 

partimat(class~., data= trainn, method="qda")

# When the Partition Graph for QDA is analyzed: 

# When observations are shown in red color, it indicates mislabeling. 
# The least mislabeling is seen in the graphs showing the relationship and separation between oldpeak and age and oldpeak and trestbps.
# When the graphs between trestbps and chol and age and trestbps are analyzed, it is noteworthy that the separation is not done well. The same results were obtained in LDA in this respect.


### preds
### Train

trainn <- predict(model_qda ,trainn)
caret::confusionMatrix(trainn$class, trainn$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1

### Test

qdatest <- predict(model_qda ,qdatest)
caret::confusionMatrix(qdatest$class, qdatest$class)

# Accuracy Rate : 1
# Sensitivity : 1
# Specificity : 1

preds[14,1] <- "qda"
preds[14,2] <- 1.000001
preds[14,3] <- 1
preds[14,4] <- 1
preds[14,5] <- 1
preds[14,6] <- 1
preds[14,7] <- 1


########################
### Assumption Check ###
########################

### Multivariate Normality 
dfmvn <- df[,c(1,4,5,8,10,14)]
sifir <- df[df$class==0,c(1,4,5,8,10,14)]
sifir <- sifir[,-6]

bir <- df[df$class==1, c(1,4,5,8,10,14)]
bir <- bir[,-6]

# Henze - Zirkler Test

# Ho: Data come from multiple normal distributions.
# Ha: Data does not come from a multiple normal distribution. 


result <- mvn(data = dfmvn, subset = "class", mvnTest = "hz")
result$multivariateNormality

# According to Henze - Zinkler normality test, at 0.05 level of significance, Ho hypothesis that the data come from multiple normal distributions can be rejected.


# Mardia Test 

# Ho: Data come from multiple normal distributions.
# Ha: Data does not come from a multiple normal distribution. 


resultmardia <- mvn(data = dfmvn, subset = "class", mvnTest = "mardia")
resultmardia$multivariateNormality

# According to the Mardia multivariate normality test, at a significance level of 0.05, the null hypothesis Ho that the data come from a multiple normal distribution can be rejected.

# Royston Test

# Ho: Data come from multiple normal distributions.
# Ha: Data does not come from a multiple normal distribution.

resultroyston <- mvn(data = dfmvn, subset = "class", mvnTest = "royston")
resultroyston$multivariateNormality

### According to the Royston multivariate normality test, at a significance level of 0.05, the null hypothesis Ho that the data come from a multiple normal distribution can be rejected.


### Variance Homogeneity Test

### Levene Test

# Ho :	σ21=σ22=…=σ2k
# Ha :	σ2i≠σ2j    for at least one pair (i,j).

library(car)
leveneTest(df$age ~ as.factor(df$class), df) 

# According to Levene's homogeneity of variance test, at 0.05 level of significance, the null hypothesis that there is homogeneity of variance by class for the variable Ho Age can be rejected. In other words, the variance of the Age variable is not homogeneous.

leveneTest(df$trestbps ~ as.factor(df$class), df) # homogeneous

# According to Levene's test for homogeneity of variance, the null hypothesis that there is homogeneity of variance by class for the variable Ho trestbps cannot be rejected at the 0.05 level of significance. In other words, the variance of the trestbps variable is homogeneous.

leveneTest(df$chol ~ as.factor(df$class), df)

# According to Levene's test for homogeneity of variance, the null hypothesis that there is homogeneity of variance by class for the variable Ho chol cannot be rejected at 0.05 level of significance. In other words, the variance of the chol variable is homogeneous.


leveneTest(df$thalach ~ as.factor(df$class), df)


# According to Levene's homogeneity of variance test, at 0.05 level of significance, the null hypothesis that there is homogeneity of variance by class for the variable Ho thalach can be rejected. In other words, the variance of the thalach variable is not homogeneous.


leveneTest(df$oldpeak ~ as.factor(df$class), df)

# According to Levene's test for homogeneity of variance, at the 0.05 level of significance, the null hypothesis that there is homogeneity of variance across classes for the Ho oldpeak variable can be rejected. In other words, the variance of oldpeak variable is not homogeneous.


plota <- list()
box_variables <- c("trestbps", "age", "chol", "oldpeak", "thalach")
for(i in box_variables) {
  plota[[i]] <- ggplot(df, 
                       aes_string(x = "class", 
                                  y = i, 
                                  col = "class", 
                                  fill = "class")) + 
    geom_boxplot(alpha = 0.2) + 
    theme(legend.position = "none", panel.background = element_rect(fill = "white")) + 
    scale_color_manual(values = c("goldenrod1", "firebrick3")) +
    scale_fill_manual(values = c("goldenrod1", "firebrick3"))
}

grid.arrange(plota$trestbps, plota$age,plota$chol, plota$oldpeak, plota$thalach,  ncol=3)

# When the graphs of numerical variables according to the classes of the dependent variable are analyzed:

# trestbps and chol variables have homogeneous variance.
# The variance of oldpeak, age and thalach variables are not homogeneous.
# These results are also in line with the levene test.

### BoxM Test

# Ho : Covariance matrices of the outcome variable are equal across all groups
# Ha : Covariance matrices of the outcome variable are different for at least one group


library(heplots)

boxm <- heplots::boxM(df[, c(1,4,5,8,10)], df$class) 
boxm 

# Since the p-value is less than 0.05, Ho's assumption that the covariance matrices of the independent variables are equal can be rejected at the 0.05 significance level.

dev.off()
plot(boxm)


##############################################################################################################################################

###############################
### Support Vector Machines ###
###############################

#############################
# Support Vector Classifier #
#############################

# The model will be tuned with direct cross-validation.

set.seed(2021900444)
tune.out <- tune(svm ,
                 class~.,
                 data=train,
                 kernel ="linear",
                 ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)

# When we tune with 10 fold cross validation:

# the cost parameter is set to 1. 
# the smaller the cost parameter, the less misclassifications are allowed and therefore the narrower the margin.
# for this model it turns out to be a very low and therefore narrow margin. 
# the lowest error is 0.1788095.

linearsvmbest <- tune.out$best.model
summary(linearsvmbest)

### Linear preds

### Train

lineartrain <- predict(linearsvmbest ,train)

a <- caret::confusionMatrix(lineartrain, train$class)

# Accuracy Rate : 0.8792
# Sensitivity : 0.9259
# Specificity : 0.8283

### Test

lineartest <- predict(linearsvmbest ,test)

b <- caret::confusionMatrix(lineartest, test$class)

# Accuracy Rate : 0.8444
# Sensitivity : 0.9231
# Specificity : 0.7368

svmpreds <- data.frame()
svmpreds[1,1] <- "linear"
svmpreds[2,1] <- "linear"
svmpreds[1,2] <- "train"
svmpreds[2,2] <- "test"
svmpreds[1,3] <- a$overall[1]
svmpreds[2,3] <- b$overall[1]
svmpreds[1,4] <- a$byClass[1]
svmpreds[2,4] <- b$byClass[1]
svmpreds[1,5] <- a$byClass[2]
svmpreds[2,5] <- b$byClass[2]
############################################
### Polynomial Support Vector Classifier ###
############################################

tune.out <- tune(svm,
                 class~.,
                 data = train,
                 kernel = "polynomial",
                 ranges = list(cost = c(0.00001, 0.0001, 0.001, 0.01, 0.1),
                               degree = c(1, 3, 4, 5, 7)))

summary(tune.out)

# When we tune with 10 fold cross validation:

# the cost parameter is chosen to be 0.1. 
# the smaller the cost parameter, the less misclassifications are allowed and therefore the narrower the margin.
# for this model it turns out to be quite low and therefore a narrow margin. 
# degree was found to be 1.
# the lowest error was 0.47.

polysvmbest <- tune.out$best.model

### Polynomial preds

### Train

polytrain <- predict(polysvmbest ,train)

a <- caret::confusionMatrix(polytrain, train$class)

# Accuracy Rate : 0.8019
# Sensitivity : 0.8796
# Specificity : 0.7172

### Test

polytest <- predict(polysvmbest ,test)

b <- caret::confusionMatrix(polytest, test$class)

# Accuracy Rate : 0.7333
# Sensitivity : 0.8846
# Specificity : 0.5263

svmpreds[3,1] <- "poly"
svmpreds[4,1] <- "poly"
svmpreds[3,2] <- "train"
svmpreds[4,2] <- "test"
svmpreds[3,3] <- a$overall[1]
svmpreds[4,3] <- b$overall[1]
svmpreds[3,4] <- a$byClass[1]
svmpreds[4,4] <- b$byClass[1]
svmpreds[3,5] <- a$byClass[2]
svmpreds[4,5] <- b$byClass[2]


##########################
# Support Vector Machine #
##########################

# The best model will be decided directly with 10 fold cross validation.

set.seed(2021900444)
tune.out <- tune(svm,
                 class~.,
                 data=train,
                 kernel ="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),
                             gamma=c(0.1,1,3,4,0.5) ))
summary(tune.out)

# When we tune the radial kernel with 10 fold cross validation:

# the cost parameter is set to 1. 
# the smaller the cost parameter, the less misclassifications are allowed and therefore the narrower the margin.
# for this model it turns out to be quite low and therefore a narrow margin.
# the gamma parameter was chosen to be the lowest, 0.1.
# this rather low value means that the flexibility of the hyperplane is rather low
# The lowest error is 0.21.


radialsvmbest <- tune.out$best.model

### Radial pred

### Train

radialtrain <- predict(radialsvmbest ,train)

a <- caret::confusionMatrix(radialtrain, train$class)

# Accuracy Rate : 0.8792
# Sensitivity : 0.9259
# Specificity : 0.8283

### Test

radialtest <- predict(radialsvmbest ,test)

b <- caret::confusionMatrix(radialtest, test$class)

# Accuracy Rate : 0.8222
# Sensitivity : 0.9231
# Specificity : 0.6842




svmpreds[5,1] <- "radial"
svmpreds[6,1] <- "radial"
svmpreds[5,2] <- "train"
svmpreds[6,2] <- "test"
svmpreds[5,3] <- a$overall[1]
svmpreds[6,3] <- b$overall[1]
svmpreds[5,4] <- a$byClass[1]
svmpreds[6,4] <- b$byClass[1]
svmpreds[5,5] <- a$byClass[2]
svmpreds[6,5] <- b$byClass[2]


names(svmpreds) <- c("Algoritma", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )



svmpreds %>% 
  ggplot(aes(x= Accuracy_Rate, y= reorder(Algoritma, -Accuracy_Rate))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Accuracy Rate") +
  ylab("Algoritma")



svmpreds %>% 
  ggplot(aes(x= Sensivity, y= reorder(Algoritma, -Sensivity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Sensitivity") +
  ylab("Algoritma")


svmpreds %>% 
  ggplot(aes(x= Specificity, y= reorder(Algoritma, -Specificity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Specificity") +
  ylab("Algoritma")



#################
### ROC Curve ###
#################

predct <- prediction(as.numeric(as.vector(prunedtree.predtest3)), test$class)
predbag <- prediction(as.numeric(as.vector(baggintest1)), test$class)
predrf <- prediction(as.numeric(as.vector(ranfortest1)), test$class)
predlr <- prediction(as.numeric(as.vector(testpred1)), test$class)
predlda <- prediction(as.numeric(as.vector(ldatest$class)), testda$class)
predqda <- prediction(as.numeric(as.vector(qdatest$class)), qdatest$class)
predsvm <- prediction(as.numeric(as.vector(radialtest)), test$class)

#####################
### AUC Değerleri ###
#####################

aucct <- performance( predct, measure= "auc" )
aucct <- aucct@y.values[[1]]

formatC(aucct, digits = 2)
aucbag <- performance(predbag, "auc")
aucbag <- aucbag@y.values[[1]]

aucrf <- performance(predrf, "auc")
aucrf <- aucrf@y.values[[1]]

auclr <- performance(predlr, "auc")
auclr <- auclr@y.values[[1]]

auclda <- performance(predlda, "auc")
auclda <- auclda@y.values[[1]]

aucqda <- performance(predqda, "auc")
aucqda <- aucqda@y.values[[1]]

aucsvm <- performance(predsvm, "auc")
aucsvm <- aucsvm@y.values[[1]]

#########################
### ROC Eğrileri İçin ###
#########################

perfct <- performance( predct, "tpr", "fpr" )
perfbag <- performance(predbag, "tpr", "fpr")
perfrf <- performance(predrf, "tpr", "fpr")
perflr <- performance(predlr, "tpr", "fpr")
perflda <- performance(predlda, "tpr", "fpr")
perfqda <- performance(predqda, "tpr", "fpr")
perfsvm <- performance(predsvm, "tpr", "fpr")

#################
### ROC Curve ###
#################

dev.off()
plot(perfct, col = "firebrick3", lwd = 2, lty = 1)
plot(perfbag, add = TRUE, col = "aquamarine3", lwd = 2, lty =2)
plot(perfrf, add=T, col = "chocolate3", lwd = 2, lty = 3)
plot(perflda, add = TRUE, col = "lightpink3", lwd = 2, lty =4)
plot(perfqda,add=T, col = "burlywood3", lwd = 2, lty =1)
plot(perflr, add = TRUE, col = "dodgerblue3", lwd = 2, lty=5)
plot(perfsvm, add=T, col = "darkorchid4", lwd = 2, lty=6)
legend("bottomright", title = "Algoritma - AUC Değeri", legend= c("ct - 0.77", "bag - 0.79", "rf - 0.75", "lda - 0.68", "qda - 1", "lr - 0.82", "svm - 0.8"), col = c( "firebrick3", "aquamarine3", "chocolate3","lightpink3","burlywood3","dodgerblue3","darkorchid4"), lty = c(1,2,3,4,1,5,6), lwd = 2)



##################
### Comparison ###
##################


preds <- data.frame()
preds[1,1] <- "ClassTree"
preds[2,1] <- "ClassTree"
preds[1,2] <- "train"
preds[2,2] <- "test"
preds[1,3] <- 0.866 
preds[2,3] <- 0.8295 
preds[1,4] <- 0.9099
preds[2,4] <- 0.9184
preds[1,5] <- 0.8163
preds[2,5] <- 0.7179

##############################

preds[3,1] <- "Bagging"
preds[4,1] <- "Bagging"
preds[3,2] <- "train"
preds[4,2] <- "test"
preds[3,3] <- 1.0000
preds[4,3] <- 0.7955
preds[3,4] <- 1.0000
preds[4,4] <- 0.8776
preds[3,5] <- 1.0000
preds[4,5] <- 0.6923


#################################

preds[5,1] <- "RanFor"
preds[6,1] <- "RanFor"
preds[5,2] <- "train"
preds[6,2] <- "test"
preds[5,3] <- 0.9378
preds[6,3] <- 0.8295
preds[5,4] <- 0.9459
preds[6,4] <- 0.8776
preds[5,5] <- 0.9256
preds[6,5] <- 0.7692

#################################

preds[7,1] <- "LogReg"
preds[8,1] <- "LogReg"
preds[7,2] <- "train"
preds[8,2] <- "test"
preds[7,3] <- 0.8947
preds[8,3] <- 0.7955
preds[7,4] <- 0.9459
preds[8,4] <- 0.9184
preds[7,5] <- 0.8367
preds[8,5] <- 0.6410

#################################

preds[9,1] <- "LDA"
preds[10,1] <- "LDA"
preds[9,2] <- "train"
preds[10,2] <- "test"
preds[9,3] <- 0.7081
preds[10,3] <- 0.75
preds[9,4] <- 0.7838
preds[10,4] <- 0.7939
preds[9,5] <- 0.6224
preds[10,5] <- 0.6923

#################################

preds[11,1] <- "QDA"
preds[12,1] <- "QDA"
preds[11,2] <- "train"
preds[12,2] <- "test"
preds[11,3] <- 0.756
preds[12,3] <- 0.7955
preds[11,4] <- 0.86
preds[12,4] <- 0.8571
preds[11,5] <- 0.6837
preds[12,5] <- 0.6923

#################################

preds[13,1] <- "SVM"
preds[14,1] <- "SVM"
preds[13,2] <- "train"
preds[14,2] <- "test"
preds[13,3] <- 0.7943
preds[14,3] <- 0.7841
preds[13,4] <- 0.8919
preds[14,4] <- 0.8571
preds[13,5] <- 0.6837
preds[14,5] <- 0.6923

###############################

names(preds) <- c("Algoritma", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )

preds

# Graphic information

# on the y-axis are the names of the algorithms used.
# The x-axis shows the accuracy rate of each algorithm.
# The yellow dot shows the accuracy rate of the model of that algorithm built with the train dataset and the red dot shows the accuracy rate of the model built with the test dataset.
# The length of the line between the two dots indicates how different the accuracy rate is between train and test, which can give us an idea about the overfit.

#####################
### Accuracy Rate ###
#####################


preds %>% 
  ggplot(aes(x= Accuracy_Rate, y= reorder(Algoritma, -Accuracy_Rate))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=4) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Accuracy Rate") +
  ylab("Algoritma")

# When the graph showing the Accuracy Rates of the models according to train and test data is analyzed:

# It is noticed that there is not much difference between the train and test accuracy ratios of LDA and no overfit problem is encountered. However, a lower success is observed compared to other algorithms. Since the assumptions are not met, it is thought that the model is not reliable.
# The difference between train and test accuracy ratios of Random Forest is at the exact border. However, it achieved a lower success compared to other algorithms.
# It is seen that the difference between train and test accuracy ratios of Classification Tree is not too much. Again, a lower success is noticed compared to other methods.
# When the difference between the train and test accuracy rates of Bagging is examined, it is thought that there may be an overfit problem. Although the accuracy rate is high, the overfit problem questions the reliability of the model.
# When the difference between the train and test accuracy rates of SVM is examined, there is no overfit problem. Accuracy Rates are also quite high. It can be said to be successful.
# When the difference between train and test accuracy ratios of Logistic Regression is examined, no overfit problem is observed. The rate is also quite high. It can be said to be successful.
# It was noticed that there is no difference between the train and test accuracy ratios of QDA. It is quite interesting that it achieves perfect success in both train and test. However, since the assumptions are not met, this model is not reliable.

###################
### Sensitivity ###
###################

preds %>% 
  ggplot(aes(x= Sensivity, y= reorder(Algoritma, -Sensivity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Sensitivity") +
  ylab("Algoritma")

# When the graph showing the Sensitivity of the models according to train and test data is examined:

# There is no overfit problem for any algorithm.
# It is noticed that there is no difference between the train and test sensitivities of QDA. It is quite interesting that it achieves perfect success in both train and test. However, since the assumptions are not met, this model is not reliable.
# Another remarkable detail is seen in the model built with the SVM algorithm. The sensitivity value in the test data set is higher than in the train set. This is exactly what we aimed for. It strengthens the success of the model.

# Sensitivity value is more important for us than other metrics. Considering the distinction we want to classify, the importance of sensitivity becomes clear.
# It is much more important to classify someone who has had a heart attack as having had a heart attack than to classify someone who has not had a heart attack as not having had a heart attack.
# Because this is a vital detail, it will be the main factor in our choice of model.

###################
### Specificity ###
###################

preds %>% 
  ggplot(aes(x= Specificity, y= reorder(Algoritma, -Specificity))) +
  geom_line(stat="identity") +
  geom_point(aes(color=TT), size=3) +
  theme(legend.position="top") +
  theme(panel.background = element_rect(fill="white"))+
  xlab("Specificity") +
  ylab("Algoritma")

# When the graph showing the Sensitivity of the models according to train and test data is examined:

# There is an overfit problem for Bagging and Random Forest.
# It is noticed that there is no difference between the train and test specificities of QDA. It is quite interesting that it achieves perfect success in both train and test. However, since the assumptions are not met, this model is not reliable.


######################
### The best Model ###
######################

# I want to compare the algorithms with the highest results on train and test data for the three metrics. 
# However, the models created with the LDA and QDA algorithms are excluded from this comparison because they do not meet the assumptions of these two algorithms.
# Algorithms that do not meet the assumptions will not be considered.


# Accuracy Rate:
# Train: Bagging, Random Forest
# Test: Logistic Regression, Support Vector Machine

# Sensitivity:
# Train: Bagging, Random Forest
# Test: SVM, Logistic Regression

# Specificity:
# Train: Bagging, Random Forest
# Test: Logistic Regression, Support Vector Machine

# AUC:
# Logictic Regression, Support Vector Machine


# In addition to the general summation, the model created with Classification Tree was selected as the model that gives the best results and stands out as the most suitable for this dataset, especially considering its success in the Sensitivity metric. When all metrics of the selected model in the test dataset are analyzed:

prunedtree.predtest3 <- predict(treeclass2, test, type = "class")

caret::confusionMatrix(prunedtree.predtest3, test$class)


# In the test dataset with a total number of 88 observations, 15 observations were misclassified.
# In addition to a correct prediction rate of 82.95%, a sensitivity value of 91.84% is noteworthy.
# The Specificity value of 71.79% reveals that the majority of the 15 misclassified observations were non-cardiac observations classified as heart patients.
# It can be easily said that the higher the success in sensitivity, the more valuable it is in classifying people with heart disease as heart patients, that is, the more success it has in this vital distinction.




       