# Figure 1

png(filename = "ch7.figure1.png", width = 1200, height = 900, units = "px", res = 200)

plot(function(x) {df(x, df1=5, df2=5)}, xlim=c(0,3), ylim=c(0,1), ylab = "", xlab = "") 
plot(function(x) {df(x, df1=5, df2=10)}, xlim=c(0,3), add=T, lty=2) 
plot(function(x) {df(x, df1=10, df2=5)}, xlim=c(0,3), add=T, lwd=2) 
plot(function(x) {df(x, df1=10, df2=10)},
       xlim=c(0,3), add=T, lty=2, lwd=2) 
legend("topright", lty=c(1,2,1,2), lwd = c(1,1,2,2), 
       legend=c("df1 = 5, df2 = 5", "df1 = 5, df2 = 10", "df1 = 10, df2 = 5",
                    "df1 = 10, df2 = 10")) 

dev.off()