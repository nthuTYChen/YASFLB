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
