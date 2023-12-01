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