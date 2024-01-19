# Figure 1
rd = read.delim("richdeletion.txt")

tiff(filename = "ch12.figure1.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

options(scipen = 5)
plot(Deletion ~ Income, data = rd)

dev.off()

# Figure 2
rd = read.delim("richdeletion.txt")

tiff(filename = "ch12.figure2.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

options(scipen = 5)
plot(Deletion ~ Income, data = rd)

rd.lm = lm(Deletion ~ Income, data = rd)	
abline(rd.lm) 

dev.off()

# Figure 3
library(gtools)

tiff(filename = "ch12.figure3.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

curve(logit(x), 0, 1)

dev.off()

# Figure 4
rd = read.delim("richdeletion.txt")
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)

tiff(filename = "ch12.figure4.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

options(scipen = 5)		        
plot(rd$Deletion ~ rd$Income)	
rd.pred = predict(rd.glm, type = "response")
curve.df = data.frame(Income = rd$Income, fitted = rd.pred)
curve.df = curve.df[order(curve.df$Income), ]
lines(x = curve.df$Income, y = curve.df$fitted)	

dev.off()

# Figure 5
tiff(filename = "ch12.figure5.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

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

tiff(filename = "ch12.figure6.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(mean.x, logit.y, ylab = "Deletion (log odds)", xlab = "Income")
abline(lm(logit.y ~ mean.x)) 			     

dev.off()

# Figure 7
library(effects)

rd = read.delim("richdeletion.txt")
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)

tiff(filename = "ch12.figure7.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

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

tiff(filename = "ch12.figure8.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(train.net, rep = "best")

dev.off()

# Figure 9
tiff(filename = "ch12.figure9.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

x = 1:10					                         	
plot(x, dpois(x, lambda = 3))	

dev.off()

# Figure 10
dui = read.delim("DuiCounts.txt")
dui.glm = glm(Count ~ NumSyl + Oddness, family = "poisson", data = dui)

dui.pred = exp(predict(dui.glm, dui[1:2]))

tiff(filename = "ch12.figure10.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

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

tiff(filename = "ch12.figure11.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(Count ~ NumSyl, data = dui, xlab = "Number of Syllables", ylab = "Count", 
     pch = 0, cex = 2)
lines(Count ~ NumSyl, data = dui, lwd = 1.5)
points(dui.pred, pch = 16, cex = 2)
lines(dui.pred, lty = 2, lwd = 1.5)

legend(x = "topright", legend = c("Observed", "Model"), 
       lty = c(1, 2), pch = c(0, 16))

dev.off()

# Figure 12
jabberwocky = read.table("Jabberwocky_OnlyWords.txt")
jabberwocky = jabberwocky$V1        
jabberwocky.tab = table(jabberwocky)
jabberwocky.tabtab = table(jabberwocky.tab)   

library(zipfR)   					                 
token.freq = names(jabberwocky.tabtab) 	   
token.freq = as.numeric(token.freq)		      
type.freq = as.numeric(jabberwocky.tabtab)	
jabberwocky.spc = spc(m = token.freq, Vm = jabberwocky.tabtab)

jabberwocky.zm = lnre(type = "zm", jabberwocky.spc)

n.int = seq(1, 10000, length = 20)	
jabberwocky.vgc = lnre.vgc(jabberwocky.zm, n.int)

tiff(filename = "ch12.figure12.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(jabberwocky.vgc)		           

dev.off()

# Figure 13
rtacc = read.delim("RTacc.txt")

tiff(filename = "ch12.figure13.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(Acc ~ RT, data = rtacc)    

dev.off()

# Figure 14
tiff(filename = "ch12.figure14.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

scatter.smooth(rtacc$RT, rtacc$Acc)

dev.off()

# Figure 15
library(mgcv)
rtacc.gam = gam(Acc ~ s(RT), data = rtacc)

tiff(filename = "ch12.figure15.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(rtacc.gam)	

dev.off()

# Figure 16
tiff(filename = "ch12.figure16.tiff", width = 1800, height = 1350, unit = "px",
     compression = c("lzw"), res = 300)

plot(Acc ~ RT, data = rtacc)		  
RTrange = range(rtacc$RT)		     
rtacc.newRT = seq(RTrange[1], RTrange[2], by = 1)
rtacc.pred = predict(rtacc.gam, data.frame(RT = rtacc.newRT))
lines(rtacc.pred ~ rtacc.newRT)		

dev.off()

# Figure 17
rtacc.pred = predict(rtacc.gam, data.frame(RT = rtacc.newRT), se.fit = T)
rtacc.pred.df = as.data.frame(rtacc.pred)
rtacc.pred.df$RT = rtacc.newRT
library(ggplot2)
ggplot() + geom_point(data = rtacc, mapping = aes(x = RT, y = Acc),
                      color = "darkgrey", alpha = .7, size = 3) +
  geom_line(data = rtacc.pred.df, mapping = aes(x = RT, y = fit)) +
  geom_ribbon(data = rtacc.pred.df, 
              mapping = aes(x = RT, y = fit, ymin = fit - se.fit * 1.96,
                            ymax = fit + se.fit * 1.96), alpha = .3) +
  labs(title = "Generalized Additive Modeling", x = "RT", y = "Accuracy") +
  theme_bw()

ggsave(filename = "ch12.figure17.tiff", width = 1800, height = 1350, unit = "px",
       compression = c("lzw"), dpi = 300)