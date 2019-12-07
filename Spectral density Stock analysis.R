my.data <- read.table("C:/Users/natan/Desktop/Master/Stocastics/BTCUSD_h.txt")
my.data2 = ts(my.data)
average <- mean(my.data2)
#volatility <- (my.data2-average)^2
resta <- apply(my.data,2,diff)
aux1 <- my.data2[-1]
aux2 <- my.data2[1:length(my.data2)-1]
aux3 <- my.data2[2:length(my.data2)+1]
aux4 <- my.data2[-1]
suma <- 0.33333*(aux1+aux2+aux3)
volat <- (aux4-suma)*(aux4-suma)

logtimes2 <- volat[1:20000]
print(logtimes2)
hist.data <- hist(logtimes2, breaks=40, plot=F)
hist.data$counts = log(1+hist.data$counts)
histbreaks <- hist.data$breaks[-1]
corr <- acf(logtimes2,lag.max=100)
fit <- lm(hist.data$counts ~ histbreaks)
fitround <- round(coef(fit),2)
eq <- paste("y=",fitround[1],"+",fitround[2],"x")
print(fit$coefficients)
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
#hist.data$breaks = log(0.01+hist.data$breaks)
plot(hist.data)
lines(histbreaks, fit$coefficients[1]+histbreaks*fit$coefficients[2])
mtext(eq,3,line=-2)
plot(corr, ylim=c(0.001,1), log="y")
a <- spectrum(logtimes2)
a$freq = log(a$freq)
a$spec = log(a$spec)
plot(a$freq,a$spec, type = "p", pch=19, cex=0.1)

fit2 <- lm(a$spec ~ a$freq)
fitround2 <- round(coef(fit2),2)
eq2 <- paste("y=",fitround2[1],"+",fitround2[2],"x")
print(fit2$coefficients)
lines(a$freq, fit2$coefficients[1]+a$freq*fit2$coefficients[2])

