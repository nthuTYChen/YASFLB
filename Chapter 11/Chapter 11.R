# 別忘了先設定工作目錄

# 二之一節
# 探索字詞時長與詞頻、熟悉度與習得年齡的關係
fd = read.delim("freqdur.txt")
head(fd)
fd$LogFreq = log(fd$Freq)

# 建立多元回歸模型
fd.lm = lm(Dur ~ LogFreq + AoA + Fam, data = fd)
summary(fd.lm)

# 練習一
# 一
240 + -1.1815 * -1.32 + 1.6643 * 6.55 + 1.1302 * 2.76 # 255.5801
# 二
240 + -1.1815 * 0.17 + 1.6643 * 2.49 + 1.1302 * 1.58  # 245.729
# 三
240 + -1.1815 * 4.64 + 1.6643 * 3.07 + 1.1302 * 6.99  # 247.5273

# 計算fd物件中第2、第3與第6個欄位(分別為三個模型中的自變量)彼此的相關性
cor(fd[c(2:3,6)])

# 練習二
# 標準化 = 以scale()將原始數值轉換為z分數
fd$Fam.z = scale(fd$Fam)
fd$LogFreq.z = scale(fd$LogFreq)
# 這邊的分佈右側與左邊是以z = 0為分界。依此以subset()取出熟悉度偏高但對數詞頻偏低
# 的子集合
fd.sub = subset(fd, Fam.z > 0 & LogFreq.z < 0)
# 計算列數 = 詞數
nrow(fd.sub)  # 243

# 重新建立「沒有」對數詞頻的多元迴歸模型
fd.lm.nofreq = lm(Dur ~ AoA + Fam, data = fd)
summary(fd.lm.nofreq)
# 結果沒有對數詞頻的模型還是沒有呈現顯著的熟悉度效應。阿芺，你還是死了這條心吧。

# 取出迴歸模型中的殘餘值
fd.res = fd.lm$residuals
mean(fd.res)
sd(fd.res)

# 練習三
max(fd$Fam)
min(fd$Fam)
max(fd$Dur)
min(fd$Dur)

# 以多元迴歸模型進行變異數分析
anova(fd.lm)

# 跟剛剛的迴歸模型中的自變量相同，只是順序變了。
fd.lm.reorder = lm(Dur ~ Fam + LogFreq + AoA, data = fd)
summary(fd.lm.reorder)		
anova(fd.lm.reorder)		# 現在熟悉度有顯著差異但詞頻沒有！

# 二之二
# 繪製表示多元迴歸模型的3D圖
library(rlg)	# 記得先安裝rlg套件
fd = read.delim("freqdur.txt", header = T)
fd$LogFreq = log(fd$Freq)
attach(fd)		# 設定為常駐物件
# 繪製一個3D散佈圖，並指定每個變量分別對應到不同的軸上：
# 對數詞頻對應至Z軸、而習得年齡和熟悉度則分別對應至X與Y軸
plot3d(x = LogFreq, y = AoA, z = Fam)

# 建立一個多元迴歸模型，用對數詞頻和習得年齡來預測熟悉度的變化
fd.lm2 = lm(Fam ~ LogFreq + AoA)	
summary(fd.lm2)				# 看看結果如何！

# 提取出需要實際繪製最佳配適平面的迴歸係數，並實際繪製這個平面
coefs = coef(fd.lm2)		# 建立一個含有多元迴歸模型係數的物件。
coefs            				# 應該就是你剛剛用summary()看到的係數。

# 從模型係數中取出各個關鍵數字存入四個參數
a = coefs["LogFreq"]
b = coefs["AoA"]
c = -1
d = coefs["(Intercept)"]
# 「alpha」參數控制平面陰影程度：「0 = 清晰」、「1 = 黑色」、「0.3」就類似淺灰色了
planes3d(a, b, c, d, alpha = 0.3)

# 示範結束後解除fd的常駐狀態
detach(fd)
