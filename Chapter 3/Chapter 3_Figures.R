#Figure 11
dataPoints = c(20, 33, 30, 44, 18, 24)
dataLabel = c(1, 2, 3, 4, 5, 6)

badLineData = data.frame(Score = dataPoints, Labels = dataLabel)

png(filename = "ch3.figure11.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(badLineData$Score~badLineData$Label, 
     main = "不適當的折線圖範例",
     ylab = "分數", 
     xlab = "受試者編號", 
     ylim = c(0, 60),
     lwd = 2, pch = 16, type = "o")

dev.off()

# Figure 12a
png(filename = "ch3.figure12a.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

barplot(badLineData$Score ~ badLineData$Label, 
     main = "適當的長條圖範例(分數與受試者編號順序無相關性)",
     ylab = "分數", 
     xlab = "受試者編號", 
     ylim = c(0, 60))

dev.off()

# Figure 12b
dataPoints = c(80, 60, 45)
dataLabel = c("動詞", "名詞", "其他")

goodBarPlotData = data.frame(Score = dataPoints, Labels = dataLabel)

png(filename = "ch3.figure12b.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

barplot(goodBarPlotData$Score ~ goodBarPlotData$Label, 
        main = "適當的長條圖範例(詞頻變化與詞類之間順序無相關性)",
        ylab = "數量", 
        xlab = "詞類", 
        ylim = c(0, 100))

dev.off()

# Figure 14
set.seed(100)
fakeAge = round(runif(100) * 100)
set.seed(200)
fakeVocSize = round(runif(100) * 30000)

fakeAgeVocCor = data.frame(Age = fakeAge, VocSize = fakeVocSize)

png(filename = "ch3.figure14.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(fakeAgeVocCor$VocSize ~ fakeAgeVocCor$Age,
     main = "年齡與字彙量散佈圖(根據隨機資料)",
     ylab = "字彙量", 
     xlab = "年齡", 
     lwd = 2, pch = 1, cex = 1.5, type = "p")

dev.off()

# Figure 16
RTs = c(670, 739, 780, 653)	
results.mat = matrix(RTs, nrow = 2)
rownames(results.mat) = c("nouns","verbs") 		
colnames(results.mat) = c("Exp. 1","Exp. 2")	

png(filename = "ch3.figure16.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

barplot(results.mat,			
	beside=TRUE,				
	names.arg=c("Exp. 1","Exp. 2"),	
	legend.text=c("noun","verb"),	
	ylim = c(0,1100),			
	ylab = "RT (ms)"			
)

dev.off()

# Figure 17
jabberwocky = readLines("Jabberwocky_OnlyWords.txt")
jw.table = sort(table(jabberwocky), decreasing=TRUE)

png(filename = "ch3.figure17.png", width = 2000, height = 900, units = "px",
    bg = "white", res = 200)

barplot(jw.table, beside=T, names.arg=names(jw.table), 
        ylab = "Token counts", main="Jabberwocky word frequencies", 
        xlab = "Sample of words")

dev.off()

# Figure 18
png(filename = "ch3.figure18.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x = 1:20, y = (1:20)^2,
     pch = 1, cex = 1.5, type = "p")

dev.off()

# Figure 19
jw.words = names(jw.table)	
jw.freq = as.vector(jw.table)	
word.lens = nchar(jw.words)	

png(filename = "ch3.figure19.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x = as.vector(jw.table), y = nchar(jw.words),
     pch = 1, cex = 1.5, type = "p")

dev.off()

# Figure 20
png(filename = "ch3.figure20.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x = 1:20, y = (1:20)^2, pch = 1:20,
     cex = 1.5, type = "p") 

dev.off()

# Figure 21
png(filename = "ch3.figure21.png", width = 1200, height = 900, units = "px",
    bg = "white", res = 200)

plot(x = 1:20, y = (1:20)^2, type = "l", lwd = 2)

dev.off()

# Figure 22
library(ggplot2)

expType = c(rep("Exp I",2), rep("Exp II", 2))
wordType = rep(c("noun", "verb"), 2)

RTs = c(670,739,780,653) 
results.data = data.frame(Exp = expType, Word = wordType, RT = RTs)

results.plot = ggplot(data = results.data, 
                      mapping = aes(x = Exp, y = RT, fill = Word))
results.plot = results.plot + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_fill_manual(values = c("black", "gray")) +
  ylab("RT (ms)") +
  theme_bw()

ggsave(file = "ch3.figure22.png", width = 1200, height = 900, units = "px",
       dpi = 200)