RTdat = read.table("RTdat.txt", header = T)

# Figure 6a
png(filename = "ch4.figure6a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

hist(RTdat$RT)

dev.off()

# Figure 6b
library(ggplot2)

ggplot(RTdat, aes(x = RT)) + 
  geom_histogram(color = "white") + 
  theme_bw()

ggsave(file = "ch4.figure6b.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 7a
png(filename = "ch4.figure7a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(density(RTdat$RT))

dev.off()

# Figure 7b
ggplot(RTdat, aes(x = RT)) + geom_density(color = "black") + theme_bw()

ggsave(file = "ch4.figure7b.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 8
set.seed(98)
dataA = rnorm(5000, mean = 1)
set.seed(102)
dataB = rnorm(5000, mean = 7, sd = 2)

allData = c(dataA, dataB)

png(filename = "ch4.figure8.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(density(allData))

dev.off()

# Figure 9
density = dnorm(seq(from = -4, to = 4, by = .01))
dataC = seq(from = -4, to = 4, by = .01)

png(filename = "ch4.figure9.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x = dataC, y = density, type = "l", main = "Normal Disitribution",
     xlab = "Value", ylab = "Density")

dev.off()

# Figure 10
png(filename = "ch4.figure10.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(density(RTdat$RT))				
abline(v = mean(RTdat$RT) - 2 * sd(RTdat$RT))	
abline(v = mean(RTdat$RT) + 2 * sd(RTdat$RT))	

dev.off()

# Figure 11
png(filename = "ch4.figure11.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

boxplot(RTdat$RT, outline = T, xlab = "Box Plot", ylab = "RT (ms)")

dev.off()

# Figure 12
png(filename = "ch4.figure12.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

v = -10:10
v.abs = abs(v)			
plot(v.abs ~ v, type = "l")

dev.off()

# Figure 13
png(filename = "ch4.figure13.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

v.sq = v^2
plot(v.sq ~ v, type = "l")

dev.off()

# Figure 14a
RTs = read.table("RTs_New.txt", header = T)

png(filename = "ch4.figure14a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

minRT = min(RTs$RT)	
maxRT = max(RTs$RT)	
subj4 = subset(RTs, Participant==4)	
subj5 = subset(RTs, Participant==5)	
plot(density(subj4$RT), xlim = c(minRT,maxRT), main = "RT distribution")

lines(density(subj5$RT), lty=2)

legend("topright", legend = c("Participant 4", "Participant 5"), lty = c(1,2))

dev.off()

# Figure 14b
RTs$Participant = as.factor(RTs$Participant)
ggplot(RTs, aes(x=RT, linetype=Participant)) +
  geom_density(color="black") +
  scale_linetype_manual(values=c("solid", "dashed")) +
  labs(title = "RT distribution") +	
  theme_classic()			

ggsave(file = "ch4.figure14b.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 15a
subj4$RT.y = subj4$RT - mean(subj4$RT)
subj5$RT.y = subj5$RT - mean(subj5$RT)

png(filename = "ch4.figure15a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(density(subj4$RT.y), main = "RT distribution (Centered)")

lines(density(subj5$RT.y), lty=2)

legend("topright", legend = c("Participant 4", "Participant 5"), lty = c(1,2))

dev.off()

# Figure 15b
RTs$RT.y = 0

participants = unique(RTs$Participant)

for(participant in participants) {
  RTs.sub = subset(RTs, Participant == participant)
  RT.mean = mean(RTs.sub$RT)
  RT.diff = RTs.sub$RT - RT.mean
  RTs[RTs$Participant == participant, ]$RT.y = RT.diff
}

ggplot(RTs, aes(x=RT.y, linetype=Participant)) +
  geom_density(color="black") +
  scale_linetype_manual(values=c("solid", "dashed")) +
  labs(title = "RT distribution (Centered)") +	
  theme_classic()			

ggsave(file = "ch4.figure15b.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 16a
subj4$RT.z = subj4$RT.y/sd(subj4$RT)
subj5$RT.z = subj5$RT.y/sd(subj5$RT)

png(filename = "ch4.figure16a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(density(subj4$RT.z), main = "RT distribution (z-scored)")

lines(density(subj5$RT.z), lty=2)

legend("topright", legend = c("Participant 4", "Participant 5"), lty = c(1,2))

dev.off()

# Figure 16b
RTs$RT.z = 0

for(participant in participants) {
  RTs.sub = subset(RTs, Participant == participant)
  RT.mean = mean(RTs.sub$RT)
  RT.sd = sd(RTs.sub$RT)
  RT.diff = RTs.sub$RT - RT.mean
  RT.z = RT.diff / RT.sd
  RTs[RTs$Participant == participant, ]$RT.z = RT.z
}

ggplot(RTs, aes(x=RT.z, linetype=Participant)) +
  geom_density(color="black") +
  scale_linetype_manual(values=c("solid", "dashed")) +
  scale_x_continuous(limits = c(-2, 3.5)) +
  labs(title = "RT distribution (z-scored)") +	
  theme_classic()			

ggsave(file = "ch4.figure16b.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 17
x = (-30:30)/10		
y = dnorm(x)		

png(filename = "ch4.figure17.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x,y,type="l")

dev.off()

# Figure 18
x = (-30:30)/10		
y = dnorm(x)			
dat = data.frame(x = x, y = y)	
dat.sub = subset(dat, x >= -2 & x <= 2)	

ggplot(data = dat, mapping = aes(x = x, y = y)) + 	
  geom_line(color="black") +
  geom_ribbon(data = dat.sub, mapping = aes(x = x, ymax = y, ymin = 0), 
              fill="grey") +
  geom_vline(aes(xintercept=-2), color="black", linetype="dashed") +
  geom_vline(aes(xintercept=2), color="black", linetype="dashed") +
  theme_classic()

ggsave(file = "ch4.figure18.png", width = 1200, height = 900, units = "px",
       dpi = 200)

# Figure 19

x = 35:65				
y = dnorm(x, mean = 50, sd = 5)		

png(filename = "ch4.figure19.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x, y , xlim=c(35, 65), type = "l")	
abline(v = (50 - 5))		
abline(v = (50 + 5))	

dev.off()

# Figure 20a

png(filename = "ch4.figure20a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

M.coin = 4 * 0.5			
sd.coin = sqrt(M.coin * (1 - 0.5))
x = 0:40/10
y = dnorm(x, M.coin, sd.coin)				
plot(x, y, xlim = c(0, 4), type = "l", main = "Coin Flip N = 4")	
points(0:4, dbinom(0:4, 4, 0.5))

dev.off()

# Figure 20b

png(filename = "ch4.figure20b.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

M.coin = 40 * 0.5			
sd.coin = sqrt(M.coin * (1 - 0.5))
x = 0:40
y = dnorm(x, M.coin, sd.coin)				
plot(x, y, xlim = c(0, 40), type = "l", main = "Coin Flip N = 40")	
points(0:40, dbinom(0:40, 40, 0.5))

dev.off()

# Figure 21
png(filename = "ch4.figure21.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

x = 0:10
plot(x, dpois(0:10, lambda = 3), xlim = c(0, 10), type = "p", main = "Poisson Disitribution")	

dev.off()

# Figure 22
png(filename = "ch4.figure22.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

RTdat.mean = mean(RTdat$RT)	
RTdat.sd = sd(RTdat$RT)		
RTdat.max = max(RTdat$RT)	
RTdat.min = min(RTdat$RT)	
RTdat.norm = dnorm(RTdat.min:RTdat.max, 
                   mean = RTdat.mean, sd = RTdat.sd)
plot(density(RTdat$RT))
lines(x = RTdat.min:RTdat.max, y = RTdat.norm, lty = 2)
legend("topright", legend = c("Sample", "Theoretical"), lty = c(1, 2))

dev.off()

# Figure 23
png(filename = "ch4.figure23.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

qqnorm(RTdat$RT)		
qqline(RTdat$RT)		

dev.off()

# Figure 24
png(filename = "ch4.figure24.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

set.seed(100)
fake = rnorm(100, 10, 2)	
qqnorm(fake)
qqline(fake)

dev.off()

# Figure 25
png(filename = "ch4.figure25.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(1:100, log(1:100))

dev.off()

# Figure 26
RTdat$logRT = log(RTdat$RT)	

png(filename = "ch4.figure26.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

par(mfrow = c(2, 2))		
hist(RTdat$RT, main = "Raw")	
hist(RTdat$logRT, main = "Log")	
qqnorm(RTdat$RT, main = "Raw")	
qqline(RTdat$RT)			
qqnorm(RTdat$logRT, main = "Log")	
qqline(RTdat$logRT)		
par(mfrow = c(1, 1))		

dev.off()