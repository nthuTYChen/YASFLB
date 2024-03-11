# Figure 1
tiff(filename = "ch14.figure1.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

curve(dnorm(x), -3, 3, ylab = "Density") # Plot normal distribution (solid line) 
curve(dt(x,1), -3, 3, add = T, lty = 2) 
legend("topright", lty = c(1, 2), legend=c("Normal", "Cauchy"))

dev.off()

# Figure 2
tiff(filename = "ch14.figure2.tiff", height = 1800, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

par(mfrow=c(5, 5), mai=c(0, 0, 0, 0))
a.val = c(0.5, 1:4)
b.val = c(0.5, 1:4)
for (b in b.val) { 
  for (a in a.val) { 
    curve(dbeta(x, a, b), 0, 1, 
      xaxt = "n", yaxt = "n", ylim = c(0, 3)) 
      curve.label = paste("a=", a, ",b=", b, sep="")
      text(0.5, 2.5, labels = curve.label)
  } 
}

dev.off()

# Figure 3
tiff(filename = "ch14.figure3.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

likely.8.25 = function(theta) { theta ^ 8 * (1 - theta) ^ (25 - 8) }
par(mfrow = c(2, 3), mai = rep(0.6, 4)) 
curve(dbeta(x, 1, 1), 0, 1, main = "Uniform prior") 
curve(likely.8.25(x), 0, 1, main="Likelihood") 
curve(dbeta(x, 8+1, 25-8+1), 0, 1, main = "Posterior") 
curve(dbeta(x,100,100),0,1,main = "Informative prior") 
curve(likely.8.25(x), 0, 1, main="Likelihood") 
curve(dbeta(x, 8+100, 25-8+100), 0, 1, main = "Posterior") 

dev.off()

# Figure 4
tiff(filename = "ch14.figure4.tiff", height = 800, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

likely.80.250 = function(theta) { theta ^ 80 * (1 - theta) ^ (250 - 80) }
par(mfrow = c(1, 3), mai = rep(0.6, 4)) 
curve(dbeta(x, 100, 100), 0, 1,main = "Informative prior") 
curve(likely.80.250(x), 0, 1, main = "Likelihood") 
curve(dbeta(x, 80+100, 250-80+100), 0, 1, main = "Posterior") 

dev.off()

# Figure 5
tiff(filename = "ch14.figure5.tiff", height = 800, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

likely.8.25 = function(theta) { theta ^ 8 * (1 - theta) ^ (25 - 8) }
par(mfrow = c(1, 3), mai = rep(0.6, 4)) 
curve(dbeta(x, 0.5, 0.5), 0, 1,main = "Informative prior") 
curve(likely.8.25(x), 0, 1, main = "Likelihood") 
curve(dbeta(x, 8+0.5, 25-8+0.5), 0, 1, main = "Posterior") 

dev.off()

# Figure 6

data1 = c(rep(1, 6), rep(0, 10), 1)	
length(data1)				                
sum(data1)					                 
data2 = c(rep(0, 7), 1)			        
length(data2)				                
sum(data2)					                 
length(c(data1, data2))			        
sum(c(data1, data2))			          

tiff(filename = "ch14.figure6.tiff", height = 1350, width = 3000, unit = "px",
     compression = c("lzw"), res = 300)

likely.7.17 = function(theta) { theta ^ 7 * (1 - theta) ^ (17 - 7) } 
likely.1.8 = function(theta) { theta ^ 1 * (1 - theta) ^ (8 - 1) } 
par(mfrow = c(2, 4), mai = rep(0.6, 4)) 
curve(dbeta(x, 1, 1), 0, 1, main = "data1: Uniform prior") 
curve(likely.7.17(x), 0, 1, main = "data1: Likelihood") 
curve(dbeta(x, 7+1, 17-7+1), 0, 1, main = "data1: Posterior") 
frame() 
curve(dbeta(x, 7+1, 17-7+1), 0, 1, main="data2: Prior = data 1 posterior") 
curve(likely.1.8(x), 0, 1, main="data2: Likelihood") 
curve(dbeta(x, 1+(7+1), 8-1+(17-7+1)), 0, 1, lwd = 2, 
      main = "data2: Posterior") 
curve(dbeta(x, 8+1, 25-8+1), 0, 1, lwd = 2, 
      main = "All: Posterior (= Figure 3, top right)") 

dev.off()

# Figure 7
tdat = read.delim("tests.txt") 
tdat = tdat[order(tdat$Student),]	

subjmeans = tapply(tdat$Score, tdat$Student, mean)

library(lme4) 
tdat.lme = lmer(Score ~ 1 + (1|Student), data = tdat) 

tdat.lme.pred = predict(tdat.lme)
pred.seq = seq(1, length(tdat.lme.pred), by = 2)
subjmeans.lme = (tdat.lme.pred[pred.seq])

tiff(filename = "ch14.figure7.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(subjmeans, subjmeans.lme, xlim = c(50, 100), ylim=c(50, 100)) 
segments(x0 = 50, y0 = 50, x1 = 100, y1 = 100) 
abline(lm(subjmeans.lme ~ subjmeans), lty = 2)
legend("topleft", legend = c("Y = X", "Actual fit"), lty = c(1, 2))

dev.off()

# Figure 8
source("Lynch_Random.R")

subjmeans.bayes = alpha_i 
names(subjmeans.bayes) = 1:20 

tiff(filename = "ch14.figure8.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(subjmeans, subjmeans.bayes, xlim = c(50,100), ylim = c(50, 100)) 
segments(x0 = 50, y0 = 50, x1 = 100, y1 = 100) 
abline(lm(subjmeans.bayes ~ subjmeans), lty = 2)
legend("topleft", legend = c("Y = X", "Actual fit"), lty = c(1, 2))

dev.off()

# Figure 9
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(brms)

pr = prior(gamma(80, 15), class = "Intercept", lb = 0, ub = 100)
tdat.brm = brm(Score ~ 1 + (1 | Student), data = tdat, prior = pr)
brm.int = fixef(tdat.brm)[1]
brm.ranef = as.data.frame(ranef(tdat.brm))
subjmeans.brm = brm.ranef[1] + brm.int

tiff(filename = "ch14.figure9.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(subjmeans, subjmeans.brm$Student.Estimate.Intercept, 
     xlim = c(50, 100), ylim=c(50, 100), ylab = "subjmeans.brm") 
segments(x0 = 50, y0 = 50, x1 = 100, y1 = 100) 
abline(lm(subjmeans.brm$Student.Estimate.Intercept ~ subjmeans), lty = 2)
legend("topleft", legend = c("Y = X", "Actual fit"), lty = c(1, 2))

dev.off()