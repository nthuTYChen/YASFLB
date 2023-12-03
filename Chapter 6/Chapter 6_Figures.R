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