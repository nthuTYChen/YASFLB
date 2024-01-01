# Figure 7
fd = read.delim("freqdur.txt")
fd$LogFreq = log(fd$Freq)

fd$FactorX = 1:nrow(fd) 		
fd$DurX = fd$Dur + fd$FactorX

fd.noX.lm = lm(DurX ~ LogFreq + AoA + Fam, data = fd)

fd.noX.resid = resid(fd.noX.lm)	

png(filename = "ch11.figure7a.png", height = 900, width = 1200, unit = "px",
    res = 200)

hist(fd.noX.resid)

dev.off()

png(filename = "ch11.figure7b.png", height = 900, width = 1200, unit = "px",
    res = 200)

qqnorm(fd.noX.resid)	
qqline(fd.noX.resid)

dev.off()

# Figure 8
fd.withX.lm = lm(DurX ~ LogFreq + AoA + Fam + FactorX, data = fd)
fd.withX.resid = resid(fd.withX.lm)

png(filename = "ch11.figure8a.png", height = 900, width = 1200, unit = "px",
    res = 200)

hist(fd.withX.resid)

dev.off()

png(filename = "ch11.figure8b.png", height = 900, width = 1200, unit = "px",
    res = 200)

qqnorm(fd.withX.resid)	
qqline(fd.withX.resid)

dev.off()