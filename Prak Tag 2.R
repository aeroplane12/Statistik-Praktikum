# Aufgabe 4
#a
#exponential-verteilte Zufallszahlen mit dem Parameter λ = 0.1.
X1<-rexp(100,0.1)
#exponential-verteilte Zufallszahlen mit dem Parameter λ = 0.1.
HX2<-rexp(100,0.1)
#alle Elemente von 20 subtrahieren.
X2 <- 20-HX2
#b
t.test(X1,X2)
#X1 und X2 besitzen unterschiedliche Mittelwert.
X2
#c
wilcox.test(X1,X2)
#X1 und X2 besitzen unterschiedliche Median.

#Aufgabe 5
#a
data=read.table("PISA.csv",header=TRUE,sep=';',dec=".")
attach(data)
#b
png(file="Boxplot PISA.png")
boxplot(R00,R06,M00,M06,S00,S06,names=c("R00","R06","M00","M06","S00","S06"),
main="Average Score",ylab="Score",col="orange",border="brown",notch=TRUE)
dev.off()
#Man kann schon merken, dass sich der PISA-Score zur Lese und Kompetenz 
#in der Mathematik verringert hat; 
#Im Gegensatz dazu hat sich der mittlere PISA-Score in den 
#Naturwissenschaften erhöht.
#c
#Also wir machen gepaarte t-Test  mit folgenden Alternativhypothesen:
t.test(R00, R06, paired=TRUE, alternative="greater")#p-Wert=0.0129
t.test(M00, M06, paired=TRUE, alternative="greater")#p-Wert=0.5032
t.test(S00, S06, paired=TRUE, alternative="less")#p-Wert=0.1024 
#Die Lesekompetenz hat sich signifikant verschlechtert,
#da p-Wert 0.01 < 0.05 ist. Die Mathekompetenz und Naturwissenschaftenkompetenz
#nicht signifikant verändert.

#Aufgabe 6
data=read.table("Hustensaft.csv",header=TRUE,sep=';',dec=".")
attach(data)
t.test(Kon, mu = 40, alternative="less")
# p-Wert = 0.05835, H0 kann nicht abgelehnt werden.
# Die Durchschnittskonzentration von Halsruhe in den entnommen Flaschen
# ist nicht signifikant von der Sollkonzentration von 40 g/Liter abweichend
# und begründet keinen Produktionsstopp.

#Aufgabe 7
#a
data=read.table("Suess.csv",header=TRUE,sep=';',dec=".")
attach(data)
sues<-data.frame(data)
sues
#b
png(file="coplot Geschmack.png")
coplot(Geschmack~Suesse|Feuchtigkeit,pch=c(5,18),rows=1,columns=3)
dev.off()
#In alle gegebene Feuchtigkeit sind, wenn mehr süße gibt, dann das Geschmack
#ist besser.
#c
model1=lm(Geschmack~Feuchtigkeit+Suesse)
#d
standard_res1<-residuals(model1)
final_data1<-cbind(Feuchtigkeit*Suesse,standard_res1)
final_data1[order(Feuchtigkeit*Suesse),]
png(file="plot model1.png")
plot(Feuchtigkeit*Suesse,model1$residuals,col="red",ylab="Residuen")
abline(0,0)
dev.off()
#Es ist ein Zusammenhang erkennbar. 
#e
model2=lm(Geschmack~Feuchtigkeit+Suesse+(Feuchtigkeit*Suesse))
#f
standard_res2<-residuals(model2)
final_data2<-cbind(Feuchtigkeit*Suesse,standard_res2)
final_data2[order(Feuchtigkeit*Suesse),]
png(file="plot model2.png")
plot(Feuchtigkeit*Suesse,model2$residuals,col="blue",ylab="Residuen")
abline(0,0)
dev.off()
#Es ist kein Zusammenhang erkennbar .
summary(model2)
#daraus folgt, dass der Interaktionsterm  signifikant ist.

#Aufgabe 8
#a
A=read.table("Air transport of passengers by country.csv",header=TRUE,sep=';',dec=".")
png(file="plot A Jahreszeit.png")
plot(A$TIME_PERIOD,A$OBS_VALUE,xlab="Jahreszeit",ylab="Passenger")
dev.off() 
B=read.table("Air transport of goods by country.csv",header=TRUE,sep=';',dec=".")
png(file="plot B Jahreszeit.png")
plot(B$TIME_PERIOD,B$OBS_VALUE,xlab="Jahreszeit",ylab="Goods")
dev.off()
#b
png(file="plot A und B.png")
plot(A$OBS_VALUE[1:length(B$OBS_VALUE)],B$OBS_VALUE, xlab="Passengers",ylab= "Goods")
dev.off()
cor.test(A$OBS_VALUE[1:length(B$OBS_VALUE)],B$OBS_VALUE)
cor.test(A$OBS_VALUE[1:length(B$OBS_VALUE)],B$OBS_VALUE)$p.value
#c
library(ggplot2)
library(tidyr)
merged_datensatz <- merge(A,B,by=c("geo","TIME_PERIOD"))
A_DE <- subset(A, geo == "DE")
B_DE <- subset(B, geo == "DE")
Laender_ohne_Deutschland <- unique(A$geo) [-which(unique(A$geo)=="DE")]
korrelationen <- list()

for(i in Laender_ohne_Deutschland){
   subset_land <- subset(merged_datensatz, geo == i)
   korrelationen[[i]] <- cor(subset_land$OBS_VALUE.x, subset_land$OBS_VALUE.y)
}

korrelation_df<-data.frame(Land = names(korrelationen), Korrelation = unlist(korrelationen))
korrelation_df[order(unlist(korrelationen)),]
korrelation_df[which.max(korrelation_df$Korrelation),"Land"]
korrelation_df[which.min(korrelation_df$Korrelation),"Land"]
png(file="correlationscoefficient.png")
ggplot(korrelation_df,aes(x=Land,y=Korrelation))+geom_bar(stat="identity")+
xlab("Land")+ylab("Korrelation")+ggtitle("Correlationcoefficient")
dev.off()
