fakecor = read.delim("scatterplots.txt")
attach(fakecor)	

# Figure 1

png(file = "ch6.figure1.png", height = 900, width = 1800, units = "px",
    res = 200)

par(mfrow = c(1, 2))		
plot(AX, AY, main = "A")		
plot(BX, BY, main = "B")		
par(mfrow = c(1, 1))

dev.off()

# Figure 2

png(file = "ch6.figure2.png", height = 900, width = 1200, units = "px",
    res = 200)

plot(fd[,2:5])

dev.off()

# Figure 3
fd = read.delim("freqdur.txt")

png(file = "ch6.figure3.png", height = 900, width = 1200, units = "px",
    res = 200)

qqnorm(fd$Freq)
qqline(fd$Freq)

dev.off()

# Figure 4
png(file = "ch6.figure4.png", height = 900, width = 1200, units = "px",
    res = 200)

fd$LogFreq = log(fd$Freq)
qqnorm(fd$LogFreq)
qqline(fd$LogFreq)

dev.off()

# Figure 5
png(file = "ch6.figure5.png", height = 900, width = 1200, units = "px",
    res = 200)

plot(fd$LogFreq, fd$Dur, xlab = "Log Frequency", ylab = "Duration (ms)")

dev.off()

# Figure 6
fakecor = read.delim("scatterplots.txt")	# 將資料讀取為fakecor變數

png(file = "ch6.figure6.png", height = 900, width = 1800, units = "px",
    res = 200)

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

png(file = "ch6.figure7.png", height = 900, width = 1200, units = "px",
    res = 200)

plotCor(fakecor$IX, fakecor$IY, "I;")

dev.off()

# Figure 8

png(file = "ch6.figure8.png", height = 900, width = 1200, units = "px",
    res = 200)

plotCor(fakecor$EX, fakecor$EY, "E;")

dev.off()

# Figure 9
png(file = "ch6.figure9.png", height = 900, width = 1200, units = "px",
    res = 200)

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

ggsave(filename = "ch6.figure10.png", height = 900, width = 1200, units = "px",
       dpi = 200)

# Figure 11
png(file = "ch6.figure11.png", height = 900, width = 1200, units = "px",
    res = 200)

plot(fd$LogFreq, fd$Dur, xlab = "Log Frequency", ylab = "Duration(ms)")
abline(lm(Dur ~ LogFreq, data = fd))

dev.off()

# Figure 12
png(file = "ch6.figure12.png", height = 900, width = 1200, units = "px",
    res = 200)

plot(AX, AY)	
abline(lm(AY ~ AX), lwd = 2, lty = 1)	
lines(AX, AY, lty = 2)

dev.off()

# Figure 13

regdat = read.delim("regex.txt")
regdat.lm = lm(y ~ x, data = regdat)

png(file = "ch6.figure13.png", height = 900, width = 1200, units = "px",
    res = 200)

plot(regdat$x, regdat$y, xlab = "x", ylab = "y")
abline(regdat.lm)

dev.off()

# Figure 14

x = regdat$x		  
y = regdat$y
n = nrow(regdat)	
yx.lm = lm(y ~ x)		
xy.lm = lm(x ~ y)		

png(file = "ch6.figure14.png", height = 900, width = 1800, units = "px",
    res = 200)

par(mfrow=c(1, 2))

plot(x, y, xlab = "x", ylab = "y", main = "y ~ x")	
abline(yx.lm, lwd = 2)                           
segments(x, y, x, predict(yx.lm))
plot(x, y, xlab = "x", ylab = "y", main = "x ~ y")
lines(predict(xy.lm)[order(y)], y[order(y)], lwd = 2)
segments(x, y, predict(xy.lm), y)

par(mfrow=c(1, 1))

dev.off()