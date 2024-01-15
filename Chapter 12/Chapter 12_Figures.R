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

# Figure 4
rd = read.delim("richdeletion.txt")
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)

png(filename = "ch12.figure4.png", width = 1200, height = 900, unit = "px",
    res = 200)

options(scipen = 5)		        
plot(rd$Deletion ~ rd$Income)	
rd.pred = predict(rd.glm, type = "response")
curve.df = data.frame(Income = rd$Income, fitted = rd.pred)
curve.df = curve.df[order(curve.df$Income), ]
lines(x = curve.df$Income, y = curve.df$fitted)	

dev.off()

# Figure 5
png(filename = "ch12.figure5.png", width = 1200, height = 900, unit = "px",
    res = 200)

bins = cut(rd$Income, 10) 			
mean.x = tapply(rd$Income, bins, mean) 	
prob.y = tapply(rd$Deletion, bins, mean)	
options(scipen = 5)
plot(mean.x, prob.y, ylab = "Deletion rate", xlab = "Income")
lines(x = curve.df$Income, y = curve.df$fitted)

dev.off()

# Figure 6
bins = cut(rd$Income, 10)	
logit.bin = function(vector) {
  prob1 = mean(c(vector, 0, 1)) 
  prob0 = 1 - prob1 		         
  log.odds = log(prob1 / prob0) 
  return(log.odds)			          
}
mean.x = tapply(rd$Income, bins, mean) 
logit.y = tapply(rd$Deletion, bins, logit.bin)	

png(filename = "ch12.figure6.png", width = 1200, height = 900, unit = "px",
    res = 200)

plot(mean.x, logit.y, ylab = "Deletion (log odds)", xlab = "Income")
abline(lm(logit.y ~ mean.x)) 			     

dev.off()

# Figure 7
library(effects)

rd = read.delim("richdeletion.txt")
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)

png(filename = "ch12.figure7.png", width = 1200, height = 900, unit = "px",
    res = 200)

plot(allEffects(rd.glm))		

dev.off()

# Figure 8
library("neuralnet")
set.seed(1)
classifiers = read.delim("classifiers.txt", stringsAsFactor = T)
classifiers$tiao = ifelse(classifiers$Class == 1, 1, 0)
classifiers$gen = ifelse(classifiers$Class == 2, 1, 0)
classifiers$zhi = ifelse(classifiers$Class == 3, 1, 0)
classifiers$FlexibleNum = ifelse(classifiers$Flexible == "Yes", 1, 0)
classifiers$ThinNum = ifelse(classifiers$Thin == "Yes", 1, 0)
classifiers$RoundNum = ifelse(classifiers$Round == "Yes", 1, 0)

train.net = neuralnet(tiao + gen + zhi ~ FlexibleNum + ThinNum + RoundNum, 
                      data = classifiers, hidden = 0, rep = 5)

png(filename = "ch12.figure8.png", width = 1200, height = 900, unit = "px",
    res = 200)

plot(train.net, rep = "best")

dev.off()

# Figure 9
png(filename = "ch12.figure9.png", width = 1200, height = 900, unit = "px",
    res = 200)

x = 1:10					                         	
plot(x, dpois(x, lambda = 3))	

dev.off()

# Figure 10
dui = read.delim("DuiCounts.txt")
dui.glm = glm(Count ~ NumSyl + Oddness, family = "poisson", data = dui)

dui.pred = exp(predict(dui.glm, dui[1:2]))

png(filename = "ch12.figure10.png", width = 1200, height = 900, unit = "px",
    res = 200)

plot(Count ~ NumSyl, data = dui, xlab = "Number of Syllables", ylab = "Count", 
     pch = 0, cex = 2)
lines(Count ~ NumSyl, data = dui, lwd = 1.5)
points(dui.pred, pch = 16, cex = 2)
lines(dui.pred, lty = 2, lwd = 1.5)

legend(x = "topright", legend = c("Observed", "Model"), 
       lty = c(1, 2), pch = c(0, 16))

dev.off()

# Figure 11
dui.poly.pois = glm(Count ~ I(NumSyl ^ 2) * NumSyl * Oddness, 
                    family = "poisson", data = dui)

dui.pred = exp(predict(dui.poly.pois, dui[1:2]))

png(filename = "ch12.figure11.png", width = 1200, height = 900, unit = "px",
    res = 200)

plot(Count ~ NumSyl, data = dui, xlab = "Number of Syllables", ylab = "Count", 
     pch = 0, cex = 2)
lines(Count ~ NumSyl, data = dui, lwd = 1.5)
points(dui.pred, pch = 16, cex = 2)
lines(dui.pred, lty = 2, lwd = 1.5)

legend(x = "topright", legend = c("Observed", "Model"), 
       lty = c(1, 2), pch = c(0, 16))

dev.off()