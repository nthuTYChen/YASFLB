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

# 二之三節
# 練習三
IX.z = (fakecor$IX - mean(fakecor$IX)) / sd(fakecor$IX) # 轉換為z分數
IY.z = (fakecor$IY - mean(fakecor$IY)) / sd(fakecor$IY) # 轉換為z分數
# 套用皮爾遜相關係數公式得到r
I.r = sum(IX.z * IY.z) / (length(IX.z) - 1)
I.r
# [1] 0.896903362335511

my.test = cor.test(BX, BY)	# 將相關性測試結果儲存至my.test物件
my.test$estimate			# 取得相關係數
#cor 
#-0.9716553
my.test$statistic			# 取得t值
#t 
#-17.43801
my.test$parameter			# 取得自由度
#df
#18
my.test$p.value			# 取得p值
#[1] 1.012081e-12

# 練習四
# 樣本數
n = length(fakecor$BX)
# 將原始資料轉換為z分數
BX.z = scale(fakecor$BX)
BY.z = scale(fakecor$BY)
# 套用相關係數公式
B.r = sum(BX.z * BY.z) / (n - 1)
# 套用相關係數的t值公式
B.t = B.r / (sqrt(1 - B.r ^ 2) / sqrt(n - 2))
# t值為負數，所以雙尾p值就是pt()給出的左尾p值乘2即可
pt(B.t, df = n - 2) * 2
# [1] 1.012081e-12

# 縮小fakecor的BX與BY的樣本數，再次計算相關係數的統計顯著性
# 設定亂數種子，以便我們進行相同的隨機抽樣
set.seed(2)				
# 隨機從fd物件中的Word欄位取出100筆資料
fd100.items = sample(fd$Word, 100)	
# 從fd物件中取出fd的Word欄位有出現在fd100.items裡的子集合
fd100 = subset(fd, is.element(fd$Word, fd100.items))
# p值不顯著
cor.test(fd100$LogFreq, fd100$Dur)

# 二之四節
# 建立原始時間序列(按鍵順序)
x = 
   c(1,1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,0,1,0,1,0,1,0,0,1,0,1,0,
     1,0,1,0,1,0,0,0,1,1,0,1,0,0,0,1,1,0,1,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,
     1,0,0,1,1,0,0,0,1,1,0,1,1,1,0,0,1,1,0,1,0,0,0,0,1,1,1,0,1,0,1,0,0,1,
     1,0,0,1,0,0,1,1,0,1,0,1,1,0,1,0,0,0)	
# 將x的最後一筆資料做為第一筆資料，再與剩下的x所有資料結合(延遲序列)
# x[length(x)] = 因為length(x)得到的是x的長度數值，所以將這個數值放入x[]
# 就能夠取得x的最後一筆資料
# x[1:length(x) - 1] = 按照上面的邏輯"1:length(x) - 1"代表的從"1"到"x長度-1"
# 的數值向量，所以把這個向量放入x[]，就能得到x的第一筆到倒數第二筆的資料
# 將最後一筆資料放在其他資料之前用c()結合，就得到了延遲序列
y = c(x[length(x)], x[1:length(x) - 1])
cor.test(x,y)

# 三之一節
plot(AX, AY)			# 使用scatterplots.txt裡的A組資料製作分佈圖
regress.line = lm(AY ~  AX)	# 產生y = a + bx的線性模型
abline(regress.line)		# 依線性模型方程式產生理想的迴歸線加到分佈圖上

library(ggplot2)
# 利用ggplot2產生相同的散佈圖
# 資料一樣是fakecor，而mapping的部份則分別將AX與AY對應至X軸與Y軸
ggplot(fakecor, aes(x = AX, y = AY)) + 
  # 加上每個資料點
  geom_point() + 
  # 以線性代數計算並加上迴歸線
  # se = 0的參數設定是省略繪製迴歸線的「標準誤」，稍後說明
  geom_smooth(method = "lm", se = 0) +
  theme_bw()	

# 利用類似的方式產生詞頻與時長的散佈圖與線性迴歸線
plot(fd$LogFreq, fd$Dur, xlab = "Log Frequency", ylab = "Duration(ms)")
# 利用lm()的參數data指定使用的資料物件，就可以在Y ~ X的語法中使用物件欄位名稱了
abline(lm(Dur ~ LogFreq, data = fd))

# 練習五
# 以ggplot2重製圖十一
library(ggplot2)
# 資料指定為fd，而mapping的部份則分別將LogFreq與Duration對應至X軸與Y軸
ggplot(fd, aes(x = LogFreq, y = Dur)) + 
  # 加上每個資料點
  geom_point() + 
  # 以線性代數計算並加上迴歸線
  # se = 0的參數設定是省略繪製迴歸線的「標準誤」，稍後說明
  geom_smooth(method = "lm", se = 0) +
  # 以labs()函數加上X軸與Y軸的標題
  labs(x = "Log Frequency", y = "Duration (ms)") +
  theme_bw()	

# 以predict()函數從線性迴歸模型中產生預測值
# 建立一個新的資料框，唯一一個欄位與線性迴歸模型中的X變量名稱相同(注意大小寫！)
# 這個欄位中只包含一個值：8
newfd = data.frame(LogFreq = 8)
# 以新的資料框中的X變量皆由線性迴歸模型預測Y變量(時長)的值
predict(lm(Dur ~ LogFreq, data = fd), newfd)
#1 
#243.0067
# 建立另一個新的資料框，這次代表X變量欄位中包含8至10三個值
newfd.2 = data.frame(LogFreq = 8:10) 

# 比較「完美」的模型與單純的線性模型
plot(AX, AY)		# 產生「scatterplots.txt」中A組資料的相關分佈圖
# 加上線型模型的迴歸線，調整寬度與樣式
abline(lm(AY ~ AX), lwd = 2, lty = 1)	
lines(AX, AY, lty = 2)	# 加上完美符合每個資料點的曲折線

# 再次以上面的資料框中的X變量皆由線性迴歸模型預測Y變量(時長)的值
predict(lm(Dur ~ LogFreq, data = fd), newfd.2)

# 三之二節
# 讀取regex檔案成為regdat資料框
regdat = read.delim("regex.txt")
# 檢視regdat物件裡的欄位名稱
colnames(regdat)				
#[1] "x" "y"
# 使用regdat建立y ~ x的線性迴歸模型
regdat.lm = lm(y ~ x, data = regdat)	
regdat.lm

plot(regdat$x, regdat$y, xlab = "x", ylab = "y")	# 產生xy散佈圖
abline(regdat.lm) # 加上迴歸線

# 呈現迴歸模型的摘要
summary(regdat.lm)

# 在相關性測試中發現與迴歸模型中斜率相同的相關係數p值
cor.test(regdat$x, regdat$y)

# 以anova()針對迴歸模型採用變異數分析，發現x的p值也和在迴歸模型中相同
anova(y ~ x, data = regdat)

# 三之三節
# 練習六
# 手動驗證迴歸模型中x自變量的p值
# 從迴歸模型中的coefficients欄位取出x自變量的係數，並從原始的表格格式轉換為向量
x.est = as.vector(regdat.lm$coefficients["x"])
x.est
#[1] -0.2300094
x.se = as.vector(regdat.lm$terms)