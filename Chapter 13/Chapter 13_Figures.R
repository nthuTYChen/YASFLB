# Figure 1
sdat = read.delim("sploink.txt")

tiff(filename = "ch13.figure1.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(Sploink ~ Age, data = sdat, main = "Sploink ~ Age",
     xlab = "Age", ylab = "Sploink Use")
abline(lm(Sploink ~ Age, data = sdat))

dev.off()

# Figure 2
library(ggplot2)
qplot(Age, Sploink, data = sdat, facets = ~ Child) + theme_bw()

ggsave(filename = "ch13.figure2.tiff", height = 1350, width = 2700, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 3
g = rep(1:10, 10)
lm.coefs = numeric(20) 
lme.coefs = numeric(20) 
set.seed(1)
for (i in 1:20) { 
  dat = data.frame(y = rnorm(100), g, x = runif(100)) 
  lm.i = lm(y ~ x, data = dat)
  coefs.lm = coef(lm.i)
  lm.coefs[i] = coefs.lm["x"] 
  lme.i = lmer(y ~ x + (x|g), data = dat)
  lme.sum = summary(lme.i)
  lme.coefs[i] = lme.sum$coefficients["x", 1]
} 

tiff(filename = "ch13.figure3.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(lm.coefs, lme.coefs, main = "Linear Regression vs. LME Coefficients",
     xlab = "Linear Regression X", ylab = "LME X") 

dev.off()

# Figure 4
library(lme4)
sdat.lmer = lmer(Sploink ~ Age + (Age | Child), data = sdat)

tiff(filename = "ch13.figure4.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(Sploink ~ Age, data = sdat, ylim = c(0, 20))
points(sdat$Age, predict(sdat.lmer), pch = 20)
legend("topright", pch = c(1, 20), legend = c("Observed", "LME predictions")) 

dev.off()

# Figure 5
sdat.lm = lm(Sploink ~ Age, data = sdat)
coefs = coef(sdat.lmer)
head(coefs$Child)
sdat.int.mean = mean(coefs$Child$`(Intercept)`)
sdat.age.mean = mean(coefs$Child$Age)

tiff(filename = "ch13.figure5.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(Sploink ~ Age, data = sdat, ylim = c(0, 20))
abline(sdat.lm, lty = 2, lwd = 3)
abline(a = sdat.int.mean, b = sdat.age.mean)
legend("topright", lty = c(1, 2), lwd = c(1, 3), legend=c("LME", "ordinary lm"))

dev.off()

# Figure 6
library(effects)
sdat.eff = allEffects(sdat.lmer)

tiff(filename = "ch13.figure6.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(sdat.eff)

dev.off()