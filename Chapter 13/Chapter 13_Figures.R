# Figure 1
sdat = read.delim("sploink.txt")

tiff(filename = "ch13.figure1.tiff", height = 1350, width = 1800, unit = "px",
     compression = c("lzw"), res = 300)

plot(Sploink ~ Age, data = sdat, main = "Sploink ~ Age",
     xlab = "Age", ylab = "Sploink Use")
abline(lm(Sploink ~ Age, data = sdat))

dev.off()

# Figure 2
library(ggplot2)
qplot(Age, Sploink, data = sdat, facets = ~ Child) + theme_bw()

ggsave(filename = "ch13.figure2.tiff", height = 1350, width = 2700, units = "px",
       compression = c("lzw"), dpi = 300)