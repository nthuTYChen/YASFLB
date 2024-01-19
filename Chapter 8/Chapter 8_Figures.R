# Figure 1

vpat = read.table("vpat.txt")	
colnames(vpat) = "Words"		  
vpat$Vowel1 = substring(vpat$Words, 2, 2)
vpat$Vowel2 = substring(vpat$Words, 4, 4)

VowelCombos.mat = xtabs(~ Vowel1 + Vowel2, data = vpat)

VowelCombos.tab = table(vpat$Vowel1, vpat$Vowel2)

VCM = t(VowelCombos.mat)	
row.total = apply(VCM, 1, sum)
VCM.marg = cbind(VCM, RowTotal = row.total)
col.total = apply(VCM.marg, 2, sum)
VCM.marg = rbind(VCM.marg, ColTotal = col.total)

tiff(filename = "ch8.figure1.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

barplot(VowelCombos.mat,  		               		  
        beside = T,	  				                    
        names.arg = c("V2=a", "V2=i", "V2=u"),	  
        legend.text = c("V1=a", "V1=i", "V1=u"),	
        ylim = c(0, 40),				                  
        ylab = "Counts"				                  
)

dev.off()

# Figure 2
VMC.df = as.data.frame(VowelCombos.mat)

library(ggplot2)

ggplot(data = VMC.df, mapping = aes(x = Vowel2, y = Freq, 
                                    group = Vowel1, fill = Vowel1)) +
  geom_bar(position = position_dodge2(), stat = "identity") +
  scale_fill_manual(values = c("black", "darkgrey", "lightgrey")) +
  labs(title = "Distribution Frequency of Vowels in vpat.txt", 
       x = "Vowel 2", y = "Counts", fill = "Vowel 1") +
  theme_bw()

ggsave(filename = "ch8.figure2.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 3
tiff(filename = "ch8.figure3.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

mosaicplot(VowelCombos.mat, cex = 1, main = "VowelCombos") 

dev.off()

# Figure 4
tiff(filename = "ch8.figure4.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

install.packages("vcd", dependencies = T, ask = F)	
library(vcd)				

mosaic(VowelCombos.mat, highlighting = "Vowel2",
       highlighting_fill = c("black", "white", "grey"))

dev.off()

# Figure 5
tiff(filename = "ch8.figure5.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

VowelCombos.exp = VowelCombos.mat 	
VowelCombos.exp[,] = matrix(c(25.84, 4.18, 7.98,
                              17.68, 2.86, 5.46,
                              24.48, 3.96, 7.56), ncol = 3)

mosaicplot(VowelCombos.exp, cex = 1, main = "VowelCombos (Expected)")

dev.off()

# Figure 6
tiff(filename = "ch8.figure6.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

socdata = matrix(c(57, 112, 190, 11), ncol = 2) 
mosaicplot(socdata)

dev.off()

# Figure 7
fakeLangDist = data.frame(Freq = c(23, 68, 97, 85), 
                          Classifier = c("Yes", "Yes", "No", "No"),
                          Gender = c("Yes", "No", "Yes", "No"))

fakeLangDist$Perc = fakeLangDist$Freq / sum(fakeLangDist$Freq) * 100

library(ggplot2)
ggplot(data = fakeLangDist, mapping = aes(x = Classifier, y = Perc,
                                          group = Gender, fill = Gender)) +
  geom_bar(position = position_dodge2(), stat = "identity") +
  scale_fill_manual(values = c("black", "darkgrey")) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Fake language distribution", x = "Classifier", y = "Proportion (%)") +
  theme_bw()

ggsave(filename = "ch8.figure7.tiff", height = 1350, width = 1800, units = "px",
       compression = c("lzw"), dpi = 300)

# Figure 8
tiff(filename = "ch8.figure8.tiff", height = 1350, width = 1800, units = "px",
    compression = c("lzw"), res = 300)

chisq.value = seq(from = 0, to = 20, by = 0.01)
chisq.3.den = dchisq(x = chisq.value, df = 3)
plot(chisq.3.den, main = "Chi-squared Distribution", type = "l",
     lty = 1, xlab = "X2", ylab = "Density")
chisq.5.den = dchisq(x = chisq.value, df = 5)
chisq.10.den = dchisq(x = chisq.value, df = 10)
lines(chisq.5.den, lty = 2)
lines(chisq.10.den, lty = 3)
legend("topright", lty = c(1, 2, 3), legend = c("df = 3", "df = 5", "df = 10"))

dev.off()