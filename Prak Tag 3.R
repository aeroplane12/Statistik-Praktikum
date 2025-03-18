# Augabe 9
#a data einlesen
tab <- load("SPECTF.RData")
# dimension
dim(SPECTF)
#information
head(SPECTF)

#b 
#Spannweite mit sapply und histogramm 
spannweite <- sapply(SPECTF, max)-sapply(SPECTF, min)
summary(SPECTF)
png(file= "histogramm spannweite.png")
hist(spannweite)
dev.off()

#c 
#Varianz aller Parameter
varp <- sapply(SPECTF, var)
#Parameter mit der hochsten Varianz
vo <- SPECTF[order(varp, decreasing=TRUE)[1:6]]
# Boxplot
pdf=("boxplot varianz.pdf")
for(i in 1:6){
  boxplot(SPECTF[order(varp, decreasing=TRUE)[i]])
}
dev.off()

#Aufgabe10
#a
#Kreuzvalidierungsverfahren sind auf Resampling basierende Testverfahren der
#Statistik, die z. B. im Data-Mining die zuverlässige Bewertung von Maschinen
#gelernten Algorithmen erlauben. Es wird unterschieden zwischen der einfachen
#Kreuzvalidierung, der stratifizierten Kreuzvalidierung und 
#der Leave-One-Out-Kreuzvalidierung.
#b
library(caTools)
library(ggplot2)
library(lattice)
library(caret)

options(max.print = 999999)
SPECTF

set.seed(123)
fache = createFolds(SPECTF$X0, k=10 ,list=TRUE)
accuracy = numeric(length=10)

for(i in 1:10){
  train.data = SPECTF[-fache[[i]],]
  test.data = SPECTF[fache[[i]],]
  logistic_model=glm(X0~.,data=train.data,family="binomial")
  predicted = predict(logistic_model, newdata = test.data, type="response")
  predicted_values = ifelse(predicted > 0.5, 1, 0)
  accuracy[i]= mean(predicted_values == test.data$X0)
}

mean_accuracy = mean(accuracy)
mean_accuracy

#c
library(pROC)
citation("pROC")
library(pROC)
library(ROCR)
predicted_values
predicted_values1<-factor(c(1,0,1,1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
actual_values<-SPECTF[c(5,7,17,25,47,78,90,93,95,105,106,114,126,133,138,139,
155,160,171,201,213,217,221,232,233,242,248), 1]
actual_values
actual_values1<-factor(c( 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1))
example<-confusionMatrix(data=predicted_values1,reference=actual_values1)
example
#ROC CURVE
#Sensitivity (True Positive Rate): 0.50000         
#Specificity (True Negative Rate): 0.92000 
#False Positive Rate: 1- Specificity = 0.08000
#False Negative Rate: 1- Sensitivity = 0.50000
#Accuracy : 0.8889  
#Konfidenzintervall
#95% CI : (0.7084, 0.9765)

pred=prediction(predicted,actual_values1)
auc = performance(pred,"auc")
auc
auc@y.values[[1]]
#AUC Wert = 0.94

#d
roc=performance(pred,"tpr","fpr")
pdf(file="ROC SPECTF.pdf")
plot(roc,print.auc=TRUE,print.ci=TRUE)
dev.off()
