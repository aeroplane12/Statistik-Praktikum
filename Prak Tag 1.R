#Aufgabe1
#a
data=read.table("oecdM.csv",header=TRUE,sep=';',dec=".")
attach(data)
#b
sapply(data[c(2:17)],mean,na.rm=TRUE)
sapply(data[c(2:17)],var,na.rm=TRUE)
#c
print(data)
grepl("Niederlande",data$X)
#Es gibt Niederlande in der Länderliste des Datensatze.
grepl("China",data$X)
#Es gibt keine China in der Länderliste des Datensatze.
#d
max(data$Alkohol,na.rm=TRUE)
#Maximale Prozentsatz ist 24.8.
which(data$Alkohol>=24.8)
#Dänemark ist das Land mit den meisten Jugendlichen mindestens
#zweimal betrunken.
#e
min(data$Säuglsterblichkeit,na.rm=TRUE)
#Minimale Prozentsatz ist 2.3
which(data$Säuglsterblichkeit<=2.3)
#Island ist das Land mit dem geringsten Säuglsterblichkeit.
#f
which(data$Bewegung<=mean(data$Bewegung,na.rm=TRUE))
#Länder mit der Prozentsatz an Jugendlichen, die sich regelmäßig 
#bewegen, kleiner als durchschnitt : Es gibt 16 Länder 
#Österreich,Belgien, Frankreich, Deutschland, Griechenland,
#Ungarn,Italien,Luxemburg,Mexiko,Norwegen, Polen, Portugal,
#Schweden, Schweitz, Türkei, VereinigtesKönigreich.

#Aufgabe 2
#a
#Anzahl die Ländern,die zu Europa gehören.
EU<-length(which(Geo=="E"))
EU
#Eu lander sind 21.
#Anzahl die Ländern, die zum Rest der Welt gehören.
RestWorld <-length(which(Geo=="R"))
RestWorld
# Rest der Wlet länder 9.
x<-c(21,9)
labels<-c("europe","restofworld")
piepercent<-paste(round(100*x/sum(x),1),"%",sep="")
pdf(file="Kuchendiagramm Länder.pdf")
pie(x,labels=piepercent, main="Land pie chart",col=c("green","blue"))
legend("topright",c("europe","restofworld"),cex=0.8,fill=c("green","blue"))
dev.off()
#b
png(file="Stripchart Lesen.png")
stripchart(data$Lesen~data$Geo,main="Different strip chart according to geo",
xlab="Geo",ylab="Reading Score",col="red",vertical=TRUE,pch=16)
dev.off()
#Das niedrigste Lesen Punkte ist nicht aus Europa. Das höchste Lesen Punkte
#ist aus Europa. Das Lesen Punkte außer Europa sind so miteinander 
#unterschiedlich.

#Aufgabe 3
#a
png(file="Boxplot Bildung.png")
boxplot(data$Bildung, main="Percentage of kids without basic equipment",
xlab="Bildung",ylab="Percentage",col="orange",border="brown",notch=TRUE)
dev.off()
#Türkei und Mexiko sind die zwei Ausreißer.
#b
quantile(data$Bildung)
#Das Ergebnis in 3b und das Bild in 3a übereinstimmen.
#c
png(file="Plot Bildung.png")
plot(sort(data$Bildung),pch=1,cex=1.5,col="red",type="l",lwd=2)
dev.off()
#d
#Es ist einen guten Trennpunkt zwischen Ländern mit "guter" und "schlechter" 
#Grundausstattung für Bildung,da für Werte, die  größer als das 75% Quantil sind,
#steigt die Kurve offensichtlich stark an.