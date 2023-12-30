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