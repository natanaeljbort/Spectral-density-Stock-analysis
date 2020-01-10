my.data <- read.table("C:/Users/JBMJA/Desktop/ASP/LTCUSD_2018.txt")
my.data2 = ts(my.data)
average <- mean(my.data2)
#volatility <- (my.data2-average)^2
#resta <- apply(my.data,2,diff)
aux1 <- my.data2[-1]
aux2 <- my.data2[1:length(my.data2)-1]
aux3 <- my.data2[2:length(my.data2)+1]
aux4 <- my.data2[-1]
suma <- 0.33333*(aux1+aux2+aux3)
volat <- (aux4-suma)**2

logtimes2 <- volat[1:10000]
print(logtimes2)
hist.data <- hist(logtimes2, breaks=40, plot=F)
hist.data$counts = log(1+hist.data$counts)
hist.data$xname<-"Volatitity"
histbreaks <- hist.data$breaks[-1]
corr <- acf(logtimes2,lag.max=2500)
corr2 <- acf(my.data2,lag.max = 10000)
correla <- corr2$acf
correlavol <- corr$acf

fit <- lm(hist.data$counts ~ histbreaks)
fitround <- round(coef(fit),2)

eq <- paste("y=",fitround[1],"+",fitround[2],"x")
print(fit$coefficients)
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
#hist.data$breaks = log(0.01+hist.data$breaks)
plot(hist.data)
#lines(histbreaks, fit$coefficients[1]+histbreaks*fit$coefficients[2])
#mtext(eq,3,line=-2).
plot(corr, ylim=c(0.001,1), log="y")





a <- spectrum(correlavol)
a$freq = log(a$freq)
a$spec = log(a$spec)
i<-0
bfreq<-0
bspec<-0
cont<-0
for (i in 1:length(a$freq)){
 if (a$freq[i]<(-4)){
   if (a$spec[i] >(-7.4)){ 
   bfreq[cont]=a$freq[i]
   bspec[cont]=a$spec[i]
   cont=cont+1
 }
 }
}


plot(bfreq,bspec, type = "p", pch=19, cex=0.1, xlab="log(Freq)", ylab="log(S)")

plot(a$freq,a$spec, type = "p", pch=19, cex=0.1, xlab="log(Freq)", ylab="log(S)")
fit2 <- lm(bspec ~ bfreq)
fitround2 <- round(coef(fit2),2)
eq2 <- paste("y=",fitround2[1],"+",fitround2[2],"x")
print(fit2$coefficients)
lines(bfreq, fit2$coefficients[1]+bfreq*fit2$coefficients[2])
