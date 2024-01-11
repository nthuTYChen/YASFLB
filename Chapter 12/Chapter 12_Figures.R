# Figure 1
rd = read.delim("richdeletion.txt")

png(filename = "ch12.figure1.png", width = 1200, height = 900, unit = "px",
    res = 200)

options(scipen = 5)
plot(Deletion ~ Income, data = rd)

dev.off()

# Figure 2
rd = read.delim("richdeletion.txt")

png(filename = "ch12.figure2.png", width = 1200, height = 900, unit = "px",
    res = 200)

options(scipen = 5)
plot(Deletion ~ Income, data = rd)

rd.lm = lm(Deletion ~ Income, data = rd)	
abline(rd.lm) 

dev.off()

# Figure 3
library(gtools)

png(filename = "ch12.figure3.png", width = 1200, height = 900, unit = "px",
    res = 200)

curve(logit(x), 0, 1)

dev.off()