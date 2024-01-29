# Figure 1
tiff(filename = "ch14.figure1.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

curve(dnorm(x), -3, 3, ylab = "Density") # Plot normal distribution (solid line) 
curve(dt(x,1), -3, 3, add = T, lty = 2) 
legend("topright", lty = c(1, 2), legend=c("Normal", "Cauchy"))

dev.off()
