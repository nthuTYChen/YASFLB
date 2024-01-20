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
for (i in 1:20) { 
  set.seed(i * 10)
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