fakecor = read.delim("scatterplots.txt")
attach(fakecor)	

# Figure 1

tiff(file = "ch6.figure1.tiff", height = 1350, width = 2700, units = "px",
    compression = c("lzw"), res = 300)

par(mfrow = c(1, 2))		
plot(AX, AY, main = "A")		
plot(BX, BY, main = "B")		
par(mfrow = c(1, 1))

dev.off()

# Figure 2
fd = read.delim("freqdur.txt")

tiff(file = "ch6.figure2.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(fd[,2:5])

dev.off()

# Figure 3
fd = read.delim("freqdur.txt")

tiff(file = "ch6.figure3.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

qqnorm(fd$Freq)
qqline(fd$Freq)

dev.off()

# Figure 4
tiff(file = "ch6.figure4.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

fd$LogFreq = log(fd$Freq)
qqnorm(fd$LogFreq)
qqline(fd$LogFreq)

dev.off()

# Figure 5
tiff(file = "ch6.figure5.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(fd$LogFreq, fd$Dur, xlab = "Log Frequency", ylab = "Duration (ms)")

dev.off()

# Figure 6
fakecor = read.delim("scatterplots.txt")	# 將資料讀取為fakecor變數

tiff(file = "ch6.figure6.tiff", height = 1350, width = 2700, units = "px",
    compression = c("lzw"), res = 300)

par(mfrow = c(1, 3))

plotCor = function(x, y, title) {
  plot(x, y, main = paste(title, "r =", round(cor(x, y), 3)))
}

plotCor(fakecor$FX, fakecor$FY, "F;")
plotCor(fakecor$GX, fakecor$GY, "G;")
plotCor(fakecor$HX, fakecor$HY, "H;")

par(mfrow = c(1, 3))

dev.off()

# Figure 7

tiff(file = "ch6.figure7.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plotCor(fakecor$IX, fakecor$IY, "I;")

dev.off()

# Figure 8

tiff(file = "ch6.figure8.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plotCor(fakecor$EX, fakecor$EY, "E;")

dev.off()

# Figure 9
tiff(file = "ch6.figure9.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(AX, AY)
regress.line = lm(AY ~ AX)
abline(regress.line)		

dev.off()

# Figure 10
library(ggplot2)

ggplot(fakecor, aes(x = AX, y = AY)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = 0) +
  theme_bw()	

ggsave(filename = "ch6.figure10.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 11
tiff(file = "ch6.figure11.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(fd$LogFreq, fd$Dur, xlab = "Log Frequency", ylab = "Duration(ms)")
abline(lm(Dur ~ LogFreq, data = fd))

dev.off()

# Figure 12
tiff(file = "ch6.figure12.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(AX, AY)	
abline(lm(AY ~ AX), lwd = 2, lty = 1)	
lines(AX, AY, lty = 2)

dev.off()

# Figure 13

regdat = read.delim("regex.txt")
regdat.lm = lm(y ~ x, data = regdat)

tiff(file = "ch6.figure13.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(regdat$x, regdat$y, xlab = "x", ylab = "y")
abline(regdat.lm)

dev.off()

# Figure 14

x = regdat$x		  
y = regdat$y
n = nrow(regdat)	
yx.lm = lm(y ~ x)		
xy.lm = lm(x ~ y)		

tiff(file = "ch6.figure14.tiff", height = 1350, width = 2700, units = "px",
    compression = c("lzw"), res = 300)

par(mfrow=c(1, 2))

plot(x, y, xlab = "x", ylab = "y", main = "y ~ x")	
abline(yx.lm, lwd = 2)                           
segments(x, y, x, predict(yx.lm))
plot(x, y, xlab = "x", ylab = "y", main = "x ~ y")
lines(predict(xy.lm)[order(y)], y[order(y)], lwd = 2)
segments(x, y, predict(xy.lm), y)

par(mfrow=c(1, 1))

dev.off()

# Figure 15
tiff(file = "ch6.figure15.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

fakecor = read.delim("scatterplots.txt")	  
attach(fakecor)					                    
plot(DX, DY)					                     
abline(lm(DY ~ DX), lty = 2)			          
loess.75 = loess(DY ~ DX)			              
lines(predict(loess.75), lty = 1, lwd = 2)
loess.25 = loess(DY ~ DX, span = 0.25)	   
lines(predict(loess.25), lty = 1)		        
legend("topleft", lty = c(2, 1, 1), lwd = c(1, 2, 1),
       legend = c("Linear","Loess.25","Loess.75"))

dev.off()

detach(fakecor)

# Figure 16

set.seed(1)				
age = runif(100) - 0.5			
acc = age ^ 2 + rnorm(100) / 10

tiff(file = "ch6.figure16.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

plot(age, acc)			    
acc = acc[order(age)]		
age = age[order(age)]		
poly.lm = lm(acc ~ I(age ^ 2))	        
lines(age, predict(poly.lm), lty = 1)	
abline(lm(acc ~ age), lty = 2)			   
legend("topleft", lty = c(1, 2), 	
       legend=c("Poly", "Linear"), bg = "white")

dev.off()

# Figure 17
tiff(file = "ch6.figure17.tiff", height = 1350, width = 1800, units = "px",
     compression = c("lzw"), res = 300)

freqmax = 1000			    
wordrank = 1:100		
wordfreq = freqmax / wordrank	
plot(wordrank, wordfreq)

dev.off()

# Figure 18
tiff(file = "ch6.figure18.tiff", height = 1350, width = 1800, units = "px",
     compression = c("lzw"), res = 300)

log.wordrank = log(wordrank)		  
log.wordfreq = log(wordfreq)		  
plot(log.wordrank, log.wordfreq)	
log.word.lm = lm(log.wordfreq ~ log.wordrank)
abline(log.word.lm)

dev.off()

# Figure 19

library(languageR)		
fd = read.delim("freqdur.txt")

tiff(file = "ch6.figure19.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

pairscor.fnc(fd[,2:5 ])	

dev.off()