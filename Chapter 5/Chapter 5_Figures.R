# Figure 1
png(filename = "ch5.figure1.png", width = 1800, height = 900, units = "px",
    res = 200)

pop = c(rep(0, 5), rep(1, 5)) 	
pop 					     
all.samps = combn(pop, 5) 	 
all.samps 				

par(mfrow=c(1, 3)) 			
plot(density(pop),main="Population")	

plot(density(all.samps[, 1]), main="Samples", 
     xlim = c(-2, 3), ylim =c(0, 1.15)
) 	 
for (i in 2:252) { 
  lines(density(all.samps[, i])) 
} 

plot(density(dist.samp.means), main = "Sample means")
par(mfrow=c(1, 3)) 	

dev.off()

# Figure 2
png(filename = "ch5.figure2.png", width = 1200, height = 900, units = "px",
    res = 200)

qqnorm(dist.samp.means) 
qqline(dist.samp.means) 

dev.off()

# Figure 3
png(filename = "ch5.figure3.png", width = 1200, height = 900, units = "px",
    res = 200)
 
plot(0:60, dbinom(0:60, size=60, prob=0.5), ylim=c(0, 0.11)) 

segments(0:23, rep(0, 23), 0:23, dbinom(0:23, size = 60, prob = 0.5)) 

dev.off()

# Figure 4

png(filename = "ch5.figure4.png", width = 1200, height = 900, units = "px",
    res = 200)

plot(0:60, dbinom(0:60, size = 60, prob = 0.5), ylim = c(0, 0.11)) 
segments(0:23, rep(0, 23), 0:23, dbinom(0:23, size = 60, prob = 0.5)) 
segments(37:60, rep(0, 23), 37:60, dbinom(37:60, 60, 0.5)) 

dev.off()

# Figure 5

png(filename = "ch5.figure5.png", width = 1200, height = 900, units = "px",
    res = 200)

x = seq(from = -3, to = 3, by = 0.1)
plot(x, dnorm(x), xlim=c(-3, 3), lwd = 3, type = "l") 
 for (i in c(1, 5, 10)) {  
	lines(x, dt(x, df = i))	
}

dev.off()