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


# Kalp hastası, kalp hastası değil dönüşümü yapılmıştır.
df %<>% mutate(class = ifelse(df$class == 0, 0,1)) 
df2 <- df

# gerekli dönüşümler:

head(df)
str(df)
getwd()
df$sex <- as.numeric(df$sex)
df$sex <- as.factor(df$sex)
df$fbs <- as.numeric(df$fbs)
df$fbs <- as.factor(df$fbs)
df$exang <- as.numeric(df$exang)
df$exang <- as.factor(df$exang)
df$ca <- as.factor(df$ca)
df$class <- as.factor(df$class)


# gerekli dönüşümler sonrası veri setindeki değişkenlerin türleri
str(df)


sum(is.na(df))
# veri setinde hiç eksik değer bulunmamakta.

#################################
### Tanımlayıcı İstatistikler ###
#################################

summary(df)


# Veri setinde yer alan sayısal değerlerin tanımlayıcı istatistiklerini incelediğimizde:

# Age değişkeninin ortalamasının medyandan daha düşük olduğu saptanmıştır. Bu da değişkenin sola çarpık olduğunu göstermektedir. Birinci kartil ile minimum değer arasındaki farka bakıldığında uç değerlerin olabileceği düşünülmüştür.
# Trestbps değişkeninin ortalamasının medyanından az da olsa büyük olduğu saptanmıştır. Bu da değişkenin sağa çarpık olduğunu göstermektedir. Kartiller ile min max değişkenleri incelendiğinde ise aykırı gözlemlerin olabileceği düşünülmüştür.
# chol değişkeninin ortalamasının medyandan daha büyük olduğu saptanmıştır. Bu da değişkenin sağa çarpık olduğunu göstermektedir. Kartiller ile min-max değerleri incelendiğinde ise aykırı gözlemler olabileceği düşünülmektedir.
# thalach değişkeninin medyanının ortalamadan daha büyük olduğu görülmüştür. Bu da değişkenin sola çarpık olduğunu göstermektedir. Kartiller ile min-max değerleri arasındaki fark incelendiğinde ise aykırı gözlem olabileceği düşünülmektedir.
# Aykırı gözlemler için boxplot, dağılımlar ile ilgili genel bir bilgi sahibi olabilmek için ise histogram grafiklerine başvurulacaktır.


# Veri setinde yer alan kategorik değerlerin tanımlayıcı istatistikleri incelendiğinde:

# Sex değişkeni incelendiğinde ise veride yer alan gözlemlerin büyük bir çoğunluğunun erkek olduğu saptanmıştır.
# cp değişkeni incelendiğinde çoğunluğun asymptomatic türündeki göğüs ağrısı olduğu saptanmıştır.
# fbs değişkeni incelendiğinde gözlemlerin büyük bir çoğunluğunun 120mg/dl'den daha az kan şekeri olduğu saptanmıştır.
# restecg değişkeni incelendiğinde gözlemlerin normal ile olası elektrokardiografik sonuçları olduğu, çok az kişinin anormal olduğu saptanmıştır.
# exang değişkeni incelendiğinde gözlemlerin büyük bir çoğunluğunda anjin görülmediği saptanmıştır.
# slope değişkeni incelendiğinde gözlemlerin büyük bir çoğunluğunda egzersiz ST segmentinin eğiminin düz olduğu saptanmıştır.
# ca değişkeni incelendiğinde gözlemlerin büyük bir çoğunluğunun 0 değerini aldığı saptanmıştır.
# thal değişkeni incelendiğinde gözlemlerin büyük bir çoğunluğunun normal ve reversable defect seviyelerini aldığı gözlemlenmiştir.
# bağımlı değişken class incelendiğinde ise 160 kişinin kalp krizi geçirmediği, 137 kişinin ise kalp krizi geçirdiği saptanmıştır. 

### Görsel Analizler ###

par(mfrow = c(1,5), bty = "n")

boxplot(df$age, col = "goldenrod1", main = "Age", border = "firebrick3")
boxplot(df$trestbps, col = "goldenrod1" ,main = "Trestbps", border = "firebrick3")
boxplot(df$chol, col = "goldenrod1", main = "Chol", border = "firebrick3")
boxplot(df$thalach, col = "goldenrod1", main = "Thalach", border = "firebrick3")
boxplot(df$oldpeak, col = "goldenrod1", main = "Oldpeak", border = "firebrick3")

# Sayısal değişkenlerin kutu grafikleri incelendiğinde:

# Age değişkeninde herhangi bir aykırı gözlem görülmemektedir. Sola çarpıklık yine dikkat çekmektedir. Range'i ise oldukça yüksek görülmektedir.
# Trestbps değişkeni incelendiğinde birçok aykırı gözlem olduğu saptanmıştır. 
# Chol değişkeni incelendiğinde 5 adet aykırı gözlem saptanmıştır.
# Thalach değişkeni incelendiğinde 1 adet aykırı gözlem saptanmıştır.
# Oldpeak değişkeninde 4 adet aykırı gözlem dikkat çekmektedir. 


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

# Sayısal değişkenlerin bağımlı değişkenin seviyelerine göre kutu grafikleri incelendiğinde:

# Kalp krizi geçirmeyen gözlemlerin daha geniş bir rangede olduğu saptanmıştır.
# Kalp krizi geçiren gözlemlerin yaş ortalamasının geçirmeyenlere göre daha fazla olduğu saptanmıştır.
# İlginç bir şekilde kolesterol bilgisi içeren değişken için class değişkeninin seviyelerine göre fark edilir bir değişim bulunmamaktadır.
# Maximum kolesterole sahip bireyin kalp krizi geçirmemiş olması da ilginç olarak ifade edilebilir. 
# Dinlenmeye göre egzersizin neden olduğu ST depresyonu bilgisini içeren oldpeak değişkeni incelendiğinde kalp krizi geçiren bireylerin daha yüksek değerlerde olduğu saptanmıştır.
# Ulaşılan maksimum kalp atış hızı bilgisini içeren thalach değişkeni incelendiğinde kalp krizi geçirmeyen bireylerin daha yüksek kalp atışı hızına ulaştığı saptanmıştır. Kalp krizi geçiren gözlemlerin daha geniş bir aralıkta olduğu saptanırken daha düşük değerleri aldıkları da saptanmıştır.
# İstirahat halindeki kan basıncı bilgisini içeren trestbps değişkeni incelendiğinde ise kalp krizi geçirenler ile geçirmeyenlerin ortalamaları arasında bir fark görülmemektedir. Ancak kalp krizi geçirenlerin biraz daha yüksek değerler aldığı söylenebilir. 

############################
### Eğitim - Test Ayrımı ###
############################

smp_size <- floor(0.70 * nrow(df)) 
set.seed(2021900444) 
train_ind <- sample(nrow(df), size = smp_size, replace = FALSE)
train <- df[train_ind, ]
test <- df[-train_ind, ]

##############################################################
### Denetimli İstatistiksel Öğrenme Modellerinin Kurulması ###
##############################################################

###########################
### Sınıflandırma Ağacı ###
###########################


### Tree Paketi ile ###

treeclass <- tree(class~. , train )
summary(treeclass ) # error rate önemli

# Toplam 18 terminal node ile ağaç oluşturulmuş.
# Residual mean deviance 0.4224 olarak dikkat çekiyor.
# Error rate 0.09 gibi düşük sayılabilecek bir değer almış.
dev.off()

plot(treeclass )
text(treeclass ,pretty =0)

# Kök düğüm(root node) cp'nin 1,2 ve 3 olması olarak saptanmış.
# age'in 66.5'tan küçük olması son düğümlerden(terminal node) biri olarak saptanmış. Ancak her iki node için de değişiklik olmaması dikkat çekiyyor
# Diğer terminal node'larda da benzer bir durum var. Ağacın budanması gerektiği açıkça belli oluyor.
# Age'in 55.5'tenn düşük olması iç düğümlerden(internal node) biri olarak saptanmış.
# Terminal nodeların büyük bir çoğunluğunda aynı değerler dikkat çekiyor. Bu da prune etmenin gerekliliğini vurguluyor.

set.seed(3)
cv.treeclass <- cv.tree(treeclass ,FUN=prune.misclass )
plot(cv.treeclass$size ,cv.treeclass$dev ,type="b")


# Her iki grafik de incelendiğinde size'ın 4 olduğu noktada deviance'de belirgin azalmalar dikkat çekmekte. 
# Bununla birlikte 5,6,7,8 için de aynı durum geçerli olmasına rağmen, 10'da bir artış gözlemleniyor. 10'dan sonra düşüş tekrar gerçekleşmiş.
# Bu sebeple hem ilk düşüş olan 4 hem de ikinci düşüş olan 12 değerleri için iki adet budama yapılma kararı alınmıştır.

### Budama 1

prune.treeclass1 <- prune.misclass (treeclass,best=4)
summary(prune.treeclass1)

# Budama sonrası yalnızca dört node ile model kurulurken residual mean deviance'ının 0.82 olarak saptanmış. Bir artış göstermiş görünüyor.
# Error rate ise 0.1594 değerini almış. Misclassification error rate için de bir artış görünüyor.

dev.off()
plot(prune.treeclass1 )
text(prune.treeclass1 ,pretty =0)

# Yalnızca dört terminal node olduğu için çok verimli bir ağaç olduğu söylenemez. 
# Ağaç yalnızca cp, cave thalach değişkenleri kullanılarak oluşturulmuş.


### Budama 2

prune.treeclass2 <- prune.misclass (treeclass,best=12)
summary(prune.treeclass2)

# Budama sonrası 12 node ile model kurulurken residual mean deviance'ının 0.5 olarak saptanmış. İlk ağaca göre bir artış göstermiş görünüyor. Ancak ilk budanmış haline göre daha düşük bir değer.
# Error rate ise 0.09179 değerini almış. Misclassification error rate için budanmamış ağaç ile aynı değeri almış.

dev.off()
plot(prune.treeclass2 )
text(prune.treeclass2 ,pretty =0)

# Ağaç cp, age, thalach, testbps, sex, ca, exang, chol ve restecg değişkenleri kullanılarak oluşturulmuş.
# Terminal nodelarda aynı sınıftan ayrışmalar görünmüyor. Bu sebeple budanmamış ağaca göre daha doğru bir ayrım olduğu söylenebilir. 



### Budama Öncesi Tahminler ###

### Train

classtree.pred <- predict(treeclass ,train ,type="class")

a<-caret::confusionMatrix(classtree.pred, train$class)  
a$overall[1]
# Accuracy Rate : 0.9082
# Sensitivity : 0.8981
# Specificity : 0.9192

ctpredictions <- data.frame()
ctpredictions[1,1] <- "Budama Öncesi CT"
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


### İlk Budama Sonrası Tahminler ###

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


### İkinci Budama Sonrası Tahminler ###

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

########################################################### rpart paketi ile ############################################################

# bu paketin özelliği cross validationını da kendisinin yapıp en az hatanın olduğu budanmış ağacı otomatik olarak oluşturmasıdır.

treeclass2 <- rpart(class~., data = train, method = 'class')

treeclass2$variable.importance

# değişkenlerin önemi sıralandığında en önemli değişken cp olarak dikkat çekiyor.
# Ardından thalach, thal, exang değişkenleri geliyor.

treeclass2$numresp
# ağaç dört bağımsız değişken kullanılarak oluşturulmuş.

rpart.plot(treeclass2)

# ağacı incelediğimizde kök düğüm olarak yine cp'nin 1,2,3 olması dikkat çekiyor.
# internal node'lar ca'nın sıfıra eşit olması, thalach'ın 146'dan büyükeşit olması durumları olarak dikkat çekiyor.
# toplam 7 adet terminal node bulunuyor. her bir atamayı farklı renklere ayırararak göstermiş. renklerin tonu ise içerdiği gözlem miktarını işaret ediyor.



### Tahminler ###

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

# En nihayetinde bizim için önemli olan sensivity değerleri olacaktır. Çünkü kalp krizi geçirmiş birini kalp krizi geçirmiş olarak tahmin etmesi hayati bir önem taşımaktadır.
# Bu sebeple algoritmalar arasında seçim yaparken yüksek sensitivity değeri vermiş olan rpart ile oluşturulmuş ağaç öne çıkmaktadır.
# train ile test arasındaki ayrımın da daha az olması bu seçime sebep olmuştur.
# ROC curve çizdirilirken rpart paketi ile oluşturulan classification tree kullanılacaktır.

##############################################################################################################################################

###############
### Bagging ###
###############


### Random Forest Paketi ile ###
set.seed(125) 
bag <- randomForest(class~. , data=train, mtry=13,importance=TRUE)

bag

# Toplam 500 ağaç kullanılarak model kurulmuş.
# Her bir ayrımda 13 adet değişken kullanılmış.
# OOB hata oranı ise %18.84 olarak saptanmıştır.

bag$importance
varImpPlot(bag)

# değişkenlerin önemlerini işaret eden grafik incelendiğinde proline değişkeninin meandecreaseaccuracy değerine göre önemli değişkenler:
# ca, cp,, thalach, oldpeak, thal
# düğüm saflığını işaret eden gini değerine göre önemli değişkenler:
# cp, thalach, ca, oldpeak, thal, age


### ipred paketi ile ###

# Modele kaç iterasyonun dahil edileceğini kontrol etmek için nbagg kullanılır 
# coob = TRUE OOB hata oranını kullanmayı göstermektedir. 
# tr control argümani ile 10-fold cross validation fonksiyonun içinde uygulanır

bag2 <- bagging(
  formula = class ~ .,
  data = train,
  nbagg = 500,  
  coob = TRUE,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10))


bag2$err

# OOB Missclassification error rate 0.1642 olarak saptanmıştır.

VI <- data.frame(var=names(train[,-14]), imp=varImp(bag2))

VI_plot <- VI[order(VI$Overall, decreasing=F),]

barplot(VI_plot$Overall,
        names.arg=rownames(VI_plot),
        horiz=T,
        col="goldenrod1",
        xlab="Variable Importance",
        las = 2)
# Değişkenlerin önemini ifade eden grafiği incelediğimizde bir önceki paketten daha farklı bir grafikle karşılaşılmıştır.
# ca ve cp değişkenleri diğer pakette en önemli değişkenler olarak görünürken bu sefer thalach değişkeninin daha önemli olduğu fark edilmiştir.
# thalach değişkenini ise cp, ca, thal, old peak, exang ve age değişkenlerinin takip ettiği söylenebilir.

bagpred <- data.frame()

### Tree Paketi ile Oluşturulan Modelin Tahminleri ###

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


### ipred ile oluşturulan Modelin Tahminleri

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


# Classification Tree seçiminde uygulanan aynı mantık ile ROC curve için seçilen model ipred paketi ile oluşturulan paket olmuştur.

##############################################################################################################################################

#####################
### Random Forest ###
#####################

sqrt(13)

rf <- randomForest(class~. ,data=train, mtry=4,importance=TRUE)

rf$confusion

# Sıfırıncı sınıf için hata 0.14, birinci sınıf için ise 0.18 olarak saptanmış. Toplam 34 gözlem yanlış olarak sınıflandırılmış.


rf 
# OOB estimate error rate ise %16.43 çıkmış. Bunun çok olduğu söylenilebilir.
# her bir ayrışmada 4 değişken denenmiş.
# Toplam 500 ağaç kurulmuş.

which.max(rf$err.rate[,1]) 


rf$importance
varImpPlot(rf)

# Değişkenlerin önemlerine bakıldığında;

# Mean decrease accuracy incelendiğinde en çok ca, ardından cp,thalach,thal, oldpeak sıralaması dikkat çekmekte.
# Gini değerlerine bakıldığında ise thalach, cp, ca sıralaması dikkat çekiyor
# Baggin ile oldukça paralel olduğunu söylemek yanlış olmaz.

###################
### Grid Search ###
###################

# Grid search'te ağaç sayısı aralığına karar vermek için grafik çizdirilmiştir.
plot(rf)

hyper_grid <- expand.grid(
  mtry = c(3, 4, 5, 6),
  nodesize = c(1, 3, 5, 10), 
  numtrees = c(200, 220,300,330),
  rmse = NA                                               
)


for (i in 1:nrow(hyper_grid)) {
  fit <- randomForest(class~. ,
                      data=train, 
                      mtry=hyper_grid$mtry[i],
                      nodesize = hyper_grid$nodesize[i],
                      ntree = hyper_grid$numtrees[i],
                      importance=TRUE)
  hyper_grid$rmse[i] <- mean(fit$confusion[,3])
}

# assess top 10 models
hyper_grid %>%
  arrange(rmse) %>%
  head(10)

# Böylelikle en iyi parametrelerle kurulacak model aşağıdaki gibi olmalıdır.

rf2 <- randomForest(class~. ,data=train, mtry=4,importance=TRUE, nodesize = 10, ntree= 220)

rf2$confusion

# Sıfırıncı sınıf için hata 0.10, birinci sınıf için ise 0.18 olarak saptanmış. Toplam 29 gözlem yanlış olarak sınıflandırılmış.

rf2

# OOB estimate error rate ise %14.01 çıkmış. Bunun çok olduğu söylenilebilir.
# her bir ayrışmada 4 değişken denenmiş.
# Toplam 220 ağaç kurulmuş.


rf2$importance
varImpPlot(rf2)

# Değişkenlerin önemlerine bakıldığında;

# Mean decrease accuracy incelendiğinde en çok ca, ardından cp, thalach, thal, oldpeak sıralaması dikkat çekmekte.
# Gini değerlerine bakıldığında ise cp, ca, thalach, sıralaması dikkat çekiyor
# İlk random forest modeline kıyasla gini önem sırasında değişiklik gözlemlenmiş.


### İlk Model için Tahminler ###

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

### Grid Search Sonrası Tahminler ###

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

# Random forest modelleri arasında grid search sonrası seçilen model en iyi model olarak seçilmiştir.



##############################################################################################################################################

###########################
### Logistic Regression ###
###########################

logmodel1 <- glm(class ~ age + sex + cp + trestbps + chol +
                   fbs + restecg + thalach + exang + oldpeak + slope + ca + thal, data = train, family = binomial)

summary(logmodel1)

# modelin anlamlılığı

#𝐻 0 : 𝛽 1 = 𝛽 2 = ⋯ = 𝛽 𝑘 = 0
# 𝐻 1 : En azından bir 𝛽 𝑗 ≠ 0

# G= Null deviance-Residual Deviance
286.57 - 118.63

1-pchisq(286.57 - 118.63,206-186) 

# Bu p-değeri .05'ten küçük olduğu için sıfır hipotezini reddebiliriz. 
# Başka bir deyişle, bağımsız değişkenlerin bağımlı değişkeni açıklamada etkili olduğunu söyleyebilecek yeterli istatistiksel kanıtımız bulunmaktadır. 


###
# katsayı yorumu

# Bağımsız değişkenin değerini bir birim arttırdığımızda tahmin değerindeki değişikliği
# belirlemek için önce log(odds) formulünde her iki tarafa exp fonksiyonu uygulanır.

# Anlamlı değişkenlerin katsayı yorumu:


exp(-3.279e-02) # age değişkendeki 1 birimlik artış odds oranını 0.9677418 kat değiştirir.
exp(1.497e+00)  # sex1 değişkenindeki bir birimlik artış odds oranını 4.468264 kat değiştirir
exp(2.293e+00)  # cp2 değişkenindeki bir birimlik artış odds oranını 9.904607 kat değiştirir
exp(1.143e+00) # cp3 değişkenindeki 1 birimlik artış odds oranını 3.136163 kat değiştirir.
exp(3.281e+00) # cp4 değişkenindeki 1 birimlik artış odds oranını 26.60236 kat değiştirir.
exp(2.477e-02)  # trestbps değişkenindeki bir birimlik artış odds oranını 1.025079 kat değiştirir
exp(5.457e-03)  # chol değişkenindeki bir birimlik artış odds oranını 1.005472 kat değiştirir
exp(-4.559e-02) # fbs1 değişkenindeki 1 birimlik artış odds oranını 0.9554336 kat değiştirir.
exp(1.306e+01) # restecg1 değişkenindeki 1 birimlik artış odds oranını 469770.7 kat değiştirir.
exp(5.368e-02) # restecg2 değişkenindeki 1 birimlik artış odds oranını 1.055147 kat değiştirir.
exp(-3.774e-02)  # thalach değişkenindeki bir birimlik artış odds oranını 0.9629633 kat değiştirir
exp(6.948e-0) # exang1 değişkenindeki 1 birimlik artış odds oranını 1041.066 kat değiştirir.
exp(3.551e-01) # oldpeak değişkenindeki 1 birimlik artış odds oranını 1.426323 kat değiştirir.
exp(1.350e+00) # slope2 değişkenindeki 1 birimlik artış odds oranını 3.857426 kat değiştirir.
exp(1.005e+00) # slope3 değişkenindeki 1 birimlik artış odds oranını 2.731907 kat değiştirir.
exp(2.688e+00) # ca1 değişkenindeki 1 birimlik artış odds oranını 14.70224 kat değiştirir.
exp(4.369e+00)  # ca2 değişkenindeki bir birimlik artış odds oranını 78.96463 kat değiştirir
exp(2.833e+00) # ca3 değişkenindeki 1 birimlik artış odds oranını 16.99637 kat değiştirir.
exp(-8.000e-01) # thal6 değişkenindeki 1 birimlik artış odds oranını 0.449329 kat değiştirir.
exp(9.626e-01) # thal7 değişkenindeki 1 birimlik artış odds oranını 2.618496 kat değiştirir.

###  Katsayılar için güven aralığı tahmini

### HİPOTEZ TESTİ EKLENECEK #####

confint.default(logmodel1)

# β katsayısına ait güven aralığı sfır değerini içermediği için Ho hipotezi red edilerek aşağıdaki katsayıların istatistiksel olarak anlamlı olduğuna karar verilmiştir.

# sex1, cp4, thalach, slope2, ca

# β katsayısına ait güven aralığı sfır değerini içerdiği için Ho hipotezi red edilemeyerek aşağıdaki katsayıların istatistiksel olarak anlamlı olmadığına karar verilmiştir.

# age, cp2, cp3, trestbps, chol, fbs1, restecg, exang, oldpeak, slope3, thal


# odds değeri  için güven aralığı

odds.confint <- exp(confint.default(logmodel1))
odds.confint

# Odds oran değerine ait güven aralığı 1 değerini içermediği için Ho hipotezi red edilerek aşağıdaki katsayıların istatistiksel olarak anlamlı olduğuna karar verilmiştir:

# age, sex, cp, trestbps, chol, fbs, restecg2, exang, oldpeak, slope, ca, thal

# Anlamlı değişkenlerin odds değeri için güven aralığına göre yorumlanması aşağıdaki gibi olacaktır: 

cat("Age değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle Age değişkeninin bir birim düşük olana göre ", odds.confint[2,1], " ile ", odds.confint[2,2], "katı arasında değer alır"  )
cat("Sex değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle Sex değişkeninin bir birim düşük olana göre ", odds.confint[3,1], " ile ", odds.confint[3,2], "katı arasında değer alır"  )
cat("Cp2 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle cp2 değişkeninin bir birim düşük olana göre ", odds.confint[4,1], " ile ", odds.confint[4,2], "katı arasında değer alır"  )
cat("Cp3 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle cp3 değişkeninin bir birim düşük olana göre ", odds.confint[5,1], " ile ", odds.confint[5,2], "katı arasında değer alır"  )
cat("Cp4 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle cp4 değişkeninin bir birim düşük olana göre ", odds.confint[6,1], " ile ", odds.confint[6,2], "katı arasında değer alır"  )
cat("Trestbps değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle trestbps değişkeninin bir birim düşük olana göre ", odds.confint[7,1], " ile ", odds.confint[7,2], "katı arasında değer alır"  )
cat("Chol değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle choldeğişkeninin bir birim düşük olana göre ", odds.confint[8,1], " ile ", odds.confint[8,2], "katı arasında değer alır"  )
cat("fbs1 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle fbs1 değişkeninin bir birim düşük olana göre ", odds.confint[9,1], " ile ", odds.confint[9,2], "katı arasında değer alır"  )
cat("restecg2 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle restecg2 değişkeninin bir birim düşük olana göre ", odds.confint[11,1], " ile ", odds.confint[11,2], "katı arasında değer alır"  )
cat("exang1 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle exang1 değişkeninin bir birim düşük olana göre ", odds.confint[12,1], " ile ", odds.confint[12,2], "katı arasında değer alır"  )
cat("oldpeak değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle oldpeak değişkeninin bir birim düşük olana göre ", odds.confint[13,1], " ile ", odds.confint[13,2], "katı arasında değer alır"  )
cat("slope2 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle slope2 değişkeninin bir birim düşük olana göre ", odds.confint[14,1], " ile ", odds.confint[14,2], "katı arasında değer alır"  )
cat("slope3 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle slope3 değişkeninin bir birim düşük olana göre ", odds.confint[15,1], " ile ", odds.confint[15,2], "katı arasında değer alır"  )
cat("Ca1 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle ca1 değişkeninin bir birim düşük olana göre ", odds.confint[16,1], " ile ", odds.confint[16,2], "katı arasında değer alır"  )
cat("Ca2 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle ca2 değişkeninin bir birim düşük olana göre ", odds.confint[17,1], " ile ", odds.confint[17,2], "katı arasında değer alır"  )
cat("Ca3 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle ca3 değişkeninin bir birim düşük olana göre ", odds.confint[18,1], " ile ", odds.confint[18,2], "katı arasında değer alır"  )
cat("thal6 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle thal6 değişkeninin bir birim düşük olana göre ", odds.confint[19,1], " ile ", odds.confint[19,2], "katı arasında değer alır"  )
cat("thal7 değişkeninin bir birim arttırılması kalp krizi geçirme oddsunu %95 güvenle thal7 değişkeninin bir birim düşük olana göre ", odds.confint[20,1], " ile ", odds.confint[20,2], "katı arasında değer alır"  )


### Artıklar

outlierTest(logmodel1)
# Bonferroni'ye göre herhangi bir uç değer bulunmamakta.

### Kaldıraçlar 
hvalues <- influence(logmodel1)$hat
r_si <- pearson.res.chd/(sqrt(1-hvalues))

# Aşağıdaki gözlemler kaldıraç noktası olarak saptanmıştır:
which(abs(r_si) > 2)


# Etkin Gözlem Grafiği
influencePlot(logmodel1)
# Büyük mavi daire içerisinde alınan gözlemler etkin gözlem olarak dikkat çekmekte. 
# Oldukça fazla olduğu görünüyor.


# SPECIFICITY AND SENSITIVITY

#Duyarlılık:
# Gerçekte pozitif olanların içinde pozitif olarak teşhis(tahmin) edilenlerin oranı (TP/(TP+FN))

# Özgüllük:
# Gerçekte negatif olanların içinde negatif olarak tahmin edilenlerin oranı (TN/(TN+FN))

lgpred <- data.frame()

### Medyana göre tahminler

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


### Meane göre tahminler

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


### Threshold = 0.6'a göre tahminler

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


names(lgpred) <- c("Algoritma", "TT", "Accuracy_Rate", "Sensivity", "Specificity" )
# Accuracy Rate, sensitivity ve specificity değerlerinin mümkün olduğunca yüksek olması gerekiyor.
# Ancak çeşitli denemeler sonucunda sensitivity değeri arttıkça specificity değerinin azaldığı gözlemlenmiştir.
# Bu veri seti ve sınıflandırmak istediğimiz seviyeler düşünüldüğünde kalp krizi geçirenlerin kalp krizi geçirme tahmininin doğruluğunun daha önemli olduğu kanısına varılmıştır.
# Bu sebeple sensitivity sonucuna göre en yüksek olan son threshold, seçim yapacağımız threshold olarak tercih edilmiştir.

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


# Threshold'un 0.6 olarak seçildiği model en iyi model olarak seçilmiştir.

############################
### Varsayım Kontrolleri ###
############################


vif(logmodel1)
# herhangi bir bağlantı problemi görülmemektedir.



##############################################################################################################################################

####################################
### Linear Discriminant Analysis ###
####################################

# Linear Discriminant Analiz yalnızca numerik değişkenler kullanılabileceği için numerik verilerden oluşan bir veri seti oluşturulup öyle devam edilecektir.

trainda <- train[,c(1,4,5,8,10,14)]
testda <- test[,c(1,4,5,8,10,14)]

pairs.panels(trainda[1:5],
             gap=0,
             bg=c("goldenrod1","firebrick4")[train$class],
             pch=21)

# pairplot grafiği incelendiğinde:

# bazı değişken çiflerinin arasındaki ilişkide ayrışmanın hatalara rağmen gerçekleştiği görülmekte. bu değişken çiftleri aşağıdaki gibi sıralanabilir:
# özellikle thalach değişkeninin diğer değişkenlerle olan ilişkisini inceleyen grafiklerde ayrışmalar daha bariz bir şekilde gerçekleşmiş gibi görünüyor.
# aynı şekilde oldpeak değişkeninindeki ayrımlar da net bir şekilde gerçekleşmiş gibi görülmektedir.
# chol, trestbps ve age değişkenlerinde ayrımların ise daha az olduğu görülmektedir.
# değişkenlerin histogramlarına bakıldığında çarpıklıklara rağmen normale yakın görülmekteyken, özellikle oldpeak değişkeninin histogramında aşırı çarpıklık dikkat çekmektedir.
# korelasyonlar incelendiğinde ise thalach ile age arasındai -0.35'lik korelasyon en yüksek korelasyon olarak dikkat çekmekte.
# oldpeak ile thalach arasındaki korelasyon da -0.32 ile ikinci sırada yer almaktadır.

desc <- describeBy(trainda[1:5], trainda[,6]) # her bir tür için bakmak skor hesaplaması için gerekli
desc

# sınıflara göre tanımlayıcı istatistikler incelendiğinde:

# thalach değişkeninde sınıflar arası ayrımın daha fazla olduğu saptanmıştır.
# oldpeak değişkeni de thalach değişkeni kadar olmasa da ayrımı sağlamış gibi görünmekte.


model_lda<-lda(class~.,data=trainda)
model_lda

# Linear discriminant analysis modelinin çıktısı incelendiğinde:

# Yalnızca bir adet doğrusal ayrım olduğu saptanmıştır.
# Oranlar birbirine oldukça yakın çıkmıştır.


tahmin_1<-predict(model_lda,trainda)
hist_lda1<-ldahist(data=tahmin_1$x[,1],g=trainda$class) 

# Birinci fonksiyona göre nasıl ayrılması gerektiğini gösteren karşılaştırmalı histogramlar incelendiğinde: 
# Yaklaşık olarak -1 değeriyle 1 değerleri arasında örtüşme görülmekte. 
# Bu olası yanlış ayırmayı gösteriyor olabilir.

# Gözlemlerin gruplara dahil olma olasılıklarına bakıldığında bazı gözlemlerin birbirine yakın olasılık değerleri olduğu saptanmıştır. 
# Bu gözlemlerden bazıları aşağıdaki gibi sıralanabilir.

tahmin_1$posterior[13,]
tahmin_1$posterior[14,]
tahmin_1$posterior[52,]


partimat(class~., data=trainda,method="lda") 

# Partition Grafiği incelendiğinde: 

# Gözlemlerin kırmızı renk ile gösterilmesi yanlış etiketlemeyi ifade etmektedir. 
# Yanlış etiketlenmenin en az olduğu grafik oldpeak ile thalach arasındaki ilişkiyi ve ayrımı gösteren grafikte görülmekte.
# trestbps ile chol ve age ile trestbps arasındaki grafikler incelendiğinde ise ayrışmanın iyi bir şekilde yapılmadığı dikkat çekmektedir.




### Tahminler

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

# Quadratic Discriminant Analysis çıktısı incelendiğinde:

# Olasılıklar arasındaki farkın bu denli düşük olması modelin güvenilirliğini sorgulatmaktadır.
# Sıfır ve birinci sınıfların değişken bazında ortalamalarına bakıldığı zaman trestbps, chol, thalach, oldpeak değişkenlerinin daha net bir şekilde ayrıştığını,
# diğer değişkenler bazında ise iyi bir ayrışma olmadığı gözlemlenmiştir. 

partimat(class~., data= trainn, method="qda")

# QDA için Partition Grafiği incelendiğinde: 

# Gözlemlerin kırmızı renk ile gösterilmesi yanlış etiketlemeyi ifade etmektedir. 
# Yanlış etiketlenmenin en az olduğu grafik oldpeak ile age ve oldpeak ile trestbps arasındaki ilişkiyi ve ayrımı gösteren grafiklerde görülmekte.
# trestbps ile chol ve age ile trestbps arasındaki grafikler incelendiğinde ise ayrışmanın iyi bir şekilde yapılmadığı dikkat çekmektedir. LDA'da da bu açıdan aynı şekilde sonuçlara erişilmişti.


### Tahminler

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


############################
### Varsayım Kontrolleri ###
############################

### Multivariate Normallik Testleri 
dfmvn <- df[,c(1,4,5,8,10,14)]
sifir <- df[df$class==0,c(1,4,5,8,10,14)]
sifir <- sifir[,-6]

bir <- df[df$class==1, c(1,4,5,8,10,14)]
bir <- bir[,-6]

# Henze - Zirkler Testi

# Ho: Veri çoklu normal dağılımdan gelmektedir.
# Ha: Veri çoklu normal dağılımdan gelmemektedir. 


result <- mvn(data = dfmvn, subset = "class", mvnTest = "hz")
result$multivariateNormality

# Henze - Zinkler normallik testine göre 0.05 anlamlılık düzeyinde Ho verilerin çoklu normal dağılımdan geldiği hipotezi reddedilebilir.


# Mardia Testi 

# Ho: Veri çoklu normal dağılımdan gelmektedir.
# Ha: Veri çoklu normal dağılımdan gelmemektedir. 


resultmardia <- mvn(data = dfmvn, subset = "class", mvnTest = "mardia")
resultmardia$multivariateNormality

# Mardia çok değişkenli normallik testine göre 0.05 anlamlılık düzeyinde Ho verinin çoklu normal dağılımdan geldiği hipotezi reddedilebilir.

# Royston Testi

# Ho: Veri çoklu normal dağılımdan gelmektedir.
# Ha: Veri çoklu normal dağılımdan gelmemektedir.

resultroyston <- mvn(data = dfmvn, subset = "class", mvnTest = "royston")
resultroyston$multivariateNormality

# Royston çok değişkenli normallik testine göre 0.05 anlamlılık düzeyinde Ho verinin çoklu normal dağılımdan geldiği hipotezi reddedilebilir.


### Varyans Homojenliği Testi

### Levene Test

# Ho :	σ21=σ22=…=σ2k
# Ha :	σ2i≠σ2j    for at least one pair (i,j).

library(car)
leveneTest(df$age ~ as.factor(df$class), df) 

# Levene varyans homojenliği testine göre 0.05 anlamlılık düzeyinde Ho Age değişkeni için sınıflara göre varyans homojenliği olduğu hipotezini reddedilebilir. Yani Age değişkeninin varyansı homojen değildir.

leveneTest(df$trestbps ~ as.factor(df$class), df) # homojen

# Levene varyans homojenliği testine göre 0.05 anlamlılık düzeyinde Ho trestbps değişkeni için sınıflara göre varyans homojenliği olduğu hipotezini reddedilemez. Yani trestbps değişkeninin varyansı homojendir.

leveneTest(df$chol ~ as.factor(df$class), df)

# Levene varyans homojenliği testine göre 0.05 anlamlılık düzeyinde Ho chol değişkeni için sınıflara göre varyans homojenliği olduğu hipotezini reddedilemez. Yani chol değişkeninin varyansı homojendir.


leveneTest(df$thalach ~ as.factor(df$class), df)


# Levene varyans homojenliği testine göre 0.05 anlamlılık düzeyinde Ho thalach değişkeni için sınıflara göre varyans homojenliği olduğu hipotezini reddedilebilir. Yani thalach değişkeninin varyansı homojen değildir.


leveneTest(df$oldpeak ~ as.factor(df$class), df)

# Levene varyans homojenliği testine göre 0.05 anlamlılık düzeyinde Ho oldpeak değişkeni için sınıflara göre varyans homojenliği olduğu hipotezini reddedilebilir. Yani oldpeak değişkeninin varyansı homojen değildir.



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

# Sayısal değişkenlerin bağımlı değişkenin sınıflarına göre grafikleri incelendiğinde:

# trestbps ve chol değişkenlerin homojen varyanslı olduğu görülmektedir.
# oldpeak, age ve thalach değişkenlerinin varyansının homojen olmadığı görülmektedir.
# Bu sonuçlar da levene test ile paralellik göstermektedir.

### BoxM Testi

# Ho : Covariance matrices of the outcome variable are equal across all groups
# Ha : Covariance matrices of the outcome variable are different for at least one group


library(heplots)

boxm <- heplots::boxM(df[, c(1,4,5,8,10)], df$class) 
boxm 

# p-value 0.05'ten küçük çıktığı için 0.05 anlamlılık seviyesinde Ho bağımsız değişkenlerin kovaryans matrislerinin eşit olduğu varsayımı reddedilebilir.

dev.off()
plot(boxm)


##############################################################################################################################################

###############################
### Support Vector Machines ###
###############################

#############################
# Support Vector Classifier #
#############################

# Direkt cross-validation ile model tune edilecektir.

set.seed(2021900444)
tune.out <- tune(svm ,
                 class~.,
                 data=train,
                 kernel ="linear",
                 ranges=list(cost=c(0.1,1,10,100,1000)))
summary(tune.out)

# 10 fold cross validation ile tune işlemi yaptığımızda:

# cost parametresi 1 olarak seçilmiştir. 
# cost parametresi ne kadar küçük olursa yanlış sınıflandırmalara o kadar az izin verileceği, dolayısıyla marginin de o kadar dar olacağı anlamına gelmektedir.
# bu model için oldukça düşük ve dolayısıyla dar bir margin olduğu anlaşılmıştır. 
# en düşük hata olarak ise 0.1788095 göze çarpmaktadır.

linearsvmbest <- tune.out$best.model
summary(linearsvmbest)

### Linear Model Metrikleri

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

# 10 fold cross validation ile tune işlemi yaptığımızda:

# cost parametresi 0.1 olarak seçilmiştir. 
# cost parametresi ne kadar küçük olursa yanlış sınıflandırmalara o kadar az izin verileceği, dolayısıyla marginin de o kadar dar olacağı anlamına gelmektedir.
# bu model için oldukça düşük ve dolayısıyla dar bir margin olduğu anlaşılmıştır. 
# degree ise 1 olarak saptanmıştır.
# en düşük hata olarak ise 0.47 göze çarpmaktadır.

polysvmbest <- tune.out$best.model

### Polynomial Model Metrikleri

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

# Direkt 10 fold cross validation ile en iyi modele karar verilecektir.

set.seed(2021900444)
tune.out <- tune(svm,
                     class~.,
                     data=train,
                     kernel ="radial",
                     ranges=list(cost=c(0.1,1,10,100,1000),
                                 gamma=c(0.1,1,3,4,0.5) ))
summary(tune.out)

# Radial kernel için 10 fold cross validation ile tune işlemi yaptığımızda:

# cost parametresi 1 olarak seçilmiştir. 
# cost parametresi ne kadar küçük olursa yanlış sınıflandırmalara o kadar az izin verileceği, dolayısıyla marginin de o kadar dar olacağı anlamına gelmektedir.
# bu model için oldukça düşük ve dolayısıyla dar bir margin olduğu anlaşılmıştır.
# gamma parametresi en düşük olan 0.1 seçilmiştir.
# bu oldukça düşük olan değer hiperdüzlemin esnekliğinin oldukça düşük olduğu anlamına gelmektedir
# en düşük hata olarak ise 0.21 olarak göze çarpmaktadır.


radialsvmbest <- tune.out$best.model

### Radial Model Metrikleri

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



#############################################
### Model Metriklerinin Karşılaştırılması ###
#############################################

# Model metriklerini içeren bir veri seti oluşturulmuştur.
# Bu veri setindeki değerler kullanılarak ggplot ile accuracy rate, sensitivity, specificity değerleri için ayrı ayrı grafikler çizdirilecektir.

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

# Grafik Okuma Bilgisi:

# y ekseninde kullanılan algoritmaların isimleri yer almaktadır.
# x ekseninde ise her bir algoritmanın accuracy rate değeri yer almaktadır.
# Sarı nokta o algoritmanın train veri seti ile kurulan modelinin accuracy rate'ini, kırmızı nokta ise test veri seti ile kurulan modelin accuracy rate'ini göstermektedir.
# İki nokta arasındaki çizginin uzunluğu train ile test arasındaki accuracy rate'inin ne kadar farklı olduğunu işaret etmektedir. bu da overfit ile ilgili bir fikir sahibi olabilmemize olanak sağlayabilecektir.


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

# Modellerin train ve test verilerine göre Accuracy Rate'lerini gösteren grafik incelendiğinde:

# LDA'nın train ile test accuracy rateleri arasında çok fark olmadığı, overfit problemi ile karşılaşılmadığı fark edilmiştir. Ancak diğer algoritmalara kıyasla daha düşük bir başarı görülmektedir. Zaten varsayımlar sağlanmadığı için modelin güvenilirliği olmadığı düşünülmektedir.
# Random Forest'ın train ile test accuracy rateleri arasındaki fark tam sınırda görülmekte. Ancak diğer algoritmalara göre daha düşük bir başarı elde etmiş.
# Classification Tree'nin train ile test accuracy rateleri arasındaki farkın çok fazla olmadığı görülmektedir. Yine diğer yöntemlere kıyasla daha düşük bir başarı fark ediliyor.
# Bagging'in train ile test accuracy rateleri arasındaki fark incelendiğinde overfit problemi olabileceği düşünülmüştür. Accuracy rate yüksek olmasına rağmen overfit problemi modelin güvenilirliğini sorgulatmaktadır.
# SVM'in train ile test accuracy rateleri arasındaki fark incelendiğinde overfit problemi görülmemektedir. Accuracy Rateler de oldukça yüksek görülmektedir. Başarılı olduğu söylenebilir.
# Logistic Regression'ın train ile test accuracy rateleri arasındaki fark incelendiğinde overfit problemi görülmemektedir. Oran da oldukça yüksek çıkmıştır. Başarılı olduğu söylenebilir.
# QDA'nın train ile test accuracy rateleri arasında bir fark olmadığı fark edilmiştir. Hem trainde hem de testte kusursuz başarı sağlaması oldukça enterasan. Ancak zaten varsayımlar sağlanmadığı için bu modelin güvenililirliği yoktur.

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

# Modellerin train ve test verilerine göre Sensitivity'lerini gösteren grafik incelendiğinde:

# Hiçbir algoritma için bir overfit problemi görülmemektedir.
# QDA'nın train ile test sensitivityleri arasında bir fark olmadığı fark edilmiştir. Hem trainde hem de testte kusursuz başarı sağlaması oldukça enterasan. Ancak zaten varsayımlar sağlanmadığı için bu modelin güvenililirliği yoktur.
# Bir diğer dikkat çeken ayrıntı SVM algoritması ile oluşturulmuş modelde görülmektedir. Test veri setindeki sensitivity değeri train setindekine göre daha yüksek çıkmıştır. Bu tam olarak amaçladığımız bir şeydir. Modelin başarısını güçlendirmektedir.

# Sensitivity değeri bizim için diğer metriklere göre daha önemlidir. Sınıflandırmak istediğimiz ayrım düşünüldüğünde sensitivity'nin önemi ortaya çıkmaktadır.
# Kalp krizi geçiren birini kalp krizi geçirdi olarak sınıflandırmak, kalp krizi geçirmeyen birini kalp krizi geçirmediği olarak sınıflandırmaya kıyasla çok daha önemlidir.
# Çünkü bu hayati önem taşıyan bir ayrıntı olduğu için model seçimimizdeki ana faktör olacak.

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

# Modellerin train ve test verilerine göre Sensitivity'lerini gösteren grafik incelendiğinde:

# Bagging ve Random Forest için bir overfit problemi görülmektedir.
# QDA'nın train ile test specificityleri arasında bir fark olmadığı fark edilmiştir. Hem trainde hem de testte kusursuz başarı sağlaması oldukça enterasan. Ancak zaten varsayımlar sağlanmadığı için bu modelin güvenililirliği yoktur.


######################
### En Uygun Model ###
######################

# Üç metrik için train ve test verilerinde en yüksek sonuç veren algoritmalar karşılaştırılmak istenilmiştir. 
# Ancak LDA ve QDA algoritmalarıyla oluşturulan modeller bu iki algoritmanın varsayımlarını sağlamadığı için bu kıyaslamadan çıkarılmıştır.
# Varsayımları sağlamayan algoritmalar dikkate alınmayacaktır.


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

