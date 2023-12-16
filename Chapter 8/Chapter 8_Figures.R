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

png(filename = "ch8.figure1.png", height = 900, width = 1200, units = "px",
    res = 200)

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

ggsave(filename = "ch8.figure2.png", height = 900, width = 1200, units = "px",
       dpi = 200)

# Figure 3
png(filename = "ch8.figure3.png", height = 900, width = 1200, units = "px",
    res = 200)

mosaicplot(VowelCombos.mat, cex = 1, main = "VowelCombos") 

dev.off()