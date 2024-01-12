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

png(filename = "ch12.figure6.png", width = 1200, height = 900, unit = "px",
    res = 200)

plot(allEffects(rd.glm))		

dev.off()