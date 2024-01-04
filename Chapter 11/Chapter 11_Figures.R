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

# Figure 9
fd = read.delim("freqdur.txt")
fd.int = lm(Dur ~ 1, data = fd)	

fd.sub = fd[1:50,]
x = 1:nrow(fd.sub)
y = fd.sub$Dur
fd.int.pred = predict(fd.int)

png(filename = "ch11.figure9.png", height = 900, width = 1200, unit = "px",
    res = 200)

plot(x, y, xlab = "", ylab = "Duration", 
     main = "An Intercept-only Model of freqdur.txt")
abline(fd.int, lwd = 2)
segments(x, y, x, fd.int.pred[1:50])

dev.off()

# Figure 10
x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x2 = c(0, 0, 0, 4, 4, 4, 7, 7, 7)
y = c(3, 2, 1, 6, 5, 4, 9, 8, 7) 

fake.simp.lm = lm(y ~ x1)

png(filename = "ch11.figure10.png", height = 900, width = 1200, unit = "px",
    res = 200)

plot(x1, y)		  		                   
lines(predict(fake.simp.lm), lwd = 2)  

dev.off()

# Figure 11
fake.int.lm = lm(y ~ x1 * x2)

png(filename = "ch11.figure11.png", height = 900, width = 1200, unit = "px",
    res = 200)

plot(x1, y)		  		
lines(predict(fake.simp.lm), lwd = 2)  
lines(predict(fake.int.lm), lty = 2) 　　	

dev.off()

# Figure 12
png(filename = "ch11.figure12.png", height = 900, width = 1200, unit = "px",
    res = 200)

plot(x2, y)

dev.off()

# Figure 13a
library(ggplot2)
library(effects)

syl = read.delim("NBUP.txt")
syl.noint = lm(MeanResp ~ NB + UP, data = syl)
syl.coefs = coef(syl.noint)
intercept = syl.coefs["(Intercept)"]
nb.b = syl.coefs["NB"]
up.b = syl.coefs["UP"]

syl.noint.ef = data.frame(NB = rep(c(0, 10, 20, 30, 40), 5),
                          UP = c(rep(1, 5), rep(100, 5), rep(200, 5), rep(300, 5), rep(400, 5)))

syl.noint.ef$fit = intercept + nb.b * syl.noint.ef$NB + up.b * syl.noint.ef$UP

ggplot() +
  geom_point(data = syl, mapping = aes(x = NB, y = MeanResp), 
             size = 2, color = "lightgrey", alpha = .9) +
  geom_line(data = syl.noint.ef, 
            mapping = aes(x = NB, y = fit, group = factor(UP)),
            stat = "identity", linewidth = 1) +
  geom_point(data = syl.noint.ef, 
             mapping = aes(x = NB, y = fit, group = factor(UP), shape = factor(UP)),
             stat = "identity", size = 3) +
  labs(x = "NB", y = "Predicted Acceptability", title = "MeanResp ~ NB + UP",
       shape = "UP") +
  theme_bw()

ggsave(filename = "ch11.figure13a.png", width = 1200, height = 900, unit = "px",
       dpi = 200)

# Figure 13b
library(ggplot2)
library(effects)

syl = read.delim("NBUP.txt")  
syl.int = lm(MeanResp ~ NB * UP, data = syl) 

syl.int.ef = as.data.frame(effect("NB:UP", syl.int))
ggplot() +
  geom_point(data = syl, mapping = aes(x = NB, y = MeanResp), 
             size = 2, color = "lightgrey", alpha = .9) +
  geom_line(data = syl.int.ef, 
            mapping = aes(x = NB, y = fit, group = factor(UP)),
            stat = "identity", linewidth = 1) +
  geom_point(data = syl.int.ef, 
            mapping = aes(x = NB, y = fit, group = factor(UP), shape = factor(UP)),
            stat = "identity", size = 3) +
  labs(x = "NB", y = "Predicted Acceptability", title = "MeanResp ~ NB * UP",
       shape = "UP") +
  theme_bw()

ggsave(filename = "ch11.figure13b.png", width = 1200, height = 900, unit = "px",
       dpi = 200)
