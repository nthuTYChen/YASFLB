# 先設定工作目錄！

# 第二節
fakecor = read.delim("scatterplots.txt")	# 將資料讀取為fakecor變數
# 將fakecor設為常駐物件，讓我們可以直接輸入該物件欄位名稱讀取欄位資料
attach(fakecor)

# 產生呈現變量之間關聯性的散佈圖
par(mfrow = c(1, 2))		# 設定圖表空間為1列2欄的排版
plot(AX, AY, main = "A")		# 圖一左放至第一欄
plot(BX, BY, main = "B")		# 圖一右放至第二欄
par(mfrow = c(1, 1))		# 恢復預設的圖表空間排版設定

# 練習一
plot(CX, CY, main = "C")	# XY之間似乎沒有非常明顯的相關性？

# 計算兩個散佈圖中兩個變量的相關係數r
cor(AX, AY)
#[1] 0.9772931
cor(BX, BY)
#[1] -0.9716553

# 練習二
# C組圖已在練習一中呈現
cor(CX, CY)             # 實際相關性係數呈現輕度負相關
cor(DX, DY)             # 實際相關性係數呈現高度正相關
cor(DX, DY)             # 實際相關性係數呈現高度正相關
cor(EX, EY)             # 實際相關性係數呈現高度正相關
cor(FX, FY)             # 實際相關性係數呈現完美正相關
cor(HX, HY)             # 實際相關性係數呈現高度正相關
cor(GX, GY)             # 實際相關性係數呈現輕度正相關

par(mfrow = c(3, 3))    # 設定3x3圖表空間

# 建立一個自訂函數plotCor()，接收兩個變量x與y
plotCor = function(x, y) {
  # paste()將字串與r係數的數字「黏」在一起，而round()將r係數四捨五入至小數第三位
  # 最終以plot()函數產生含標題的散佈圖
  plot(x, y, main = paste("r =", round(cor(x, y), 3)))
}

# 接著直接使用plotCor()函數就好，可以省去重覆許多程式碼的過程
plotCor(AX, AY)
plotCor(BX, BY)
plotCor(CX, CY)
plotCor(DX, DY)
plotCor(EX, EY)
plotCor(FX, FY)
plotCor(GX, GY)
plotCor(HX, HY)
plotCor(IX, IY)

par(mfrow = c(1, 1))    # 恢復預設圖表空間

# 解除fakeCor的常駐狀態
detach(fakecor)

# 第二之二節
fd = read.delim("freqdur.txt")	# 讀取freqdur.txt為fd變數

# 產生每兩組變量之前的相關性散佈圖
plot(fd)		# 依fd變數中的每個變量欄位產生變量相關性圖表
# 但我們不需要字詞編號與其他變量的相關性，所以用兩個方式去掉字詞編號的部份
plot(fd[,2:5])	# 依fd變數中的第二到第五變量欄位產生變量相關性圖表
plot(fd[,-1])	# fd變數中去掉第一欄位後產生變量相關性圖表

# 計算除了字詞編號之外所有變量之間的相關性係數
cor(fd[,-1])

# 測試詞頻資料是否符合常態分佈
qqnorm(fd$Freq)	# 產生fd變數中Freq欄位的常態分位數比較圖
qqline(fd$Freq)	# 產生fd變數中Freq欄位的理論常態分佈線

# 將詞頻進行對數轉換後，再次檢查轉換後的詞頻是否符合常態分佈
fd$LogFreq = log(fd$Freq)
qqnorm(fd$LogFreq)
qqline(fd$LogFreq)

# 利用經過對數轉換的詞頻再次產生相關性散佈圖以及計算相關係數
plot(fd$LogFreq, fd$Dur, xlab = "Log Frequency", ylab = "Duration (ms)")
cor(fd$LogFreq, fd$Dur)
#[1] -0.07608054

# 使用cor.test()檢測對數詞頻與字詞時長的微弱負相關性是否有統計上的顯著性
cor.test(fd$LogFreq, fd$Dur)
