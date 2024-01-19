# Figure 4
# Reproducing the tioh count in the TSM corpus
fakeMT2015 = data.frame(rep=c(1:10), count=c(3.8,2.6,2.9,2.2,1.9,1.3,0.8,0,0.6,0), 
                        modelFeet=c(3.8,2.6,2.9,2.2,1.95,1.32,1,0.3,0.1,-1), 
                        modelFeet=c(3.8,3,2.3,2.1,1.8,1.3,1.1,1,1,0.9))

tiff(filename = "ch1.figure4.tif", width = 1800, height = 1350, units = "px",
    bg = "white", res = 300)

plot(fakeMT2015$count~fakeMT2015$rep, 
     main = "Repetitions of \"tioh\" in Myers and Tsay (2015)",
     ylab = "Tokens (base 10 log)", 
     xlab = "Number of Repetitions", 
     xaxt = "n", 
     ylim=c(0,4), lwd = 2, pch = 16, type = "o")

axis(1, at=fakeMT2015$rep, labels=fakeMT2015$rep)
lines(fakeMT2015$modelFeet~fakeMT2015$rep, pch = 0, type = "o")
lines(predict(loess(fakeMT2015$modelFeet.1~fakeMT2015$rep)), lty = 2)

legend("topright", 
       c("Observed", "Model with Feet", "Model without Feet"), 
       lty = c(1,1,2), 
       lwd = c(2,1,1), 
       pch = c(16, 0, NA))

dev.off()

rm(fakeMT2015)