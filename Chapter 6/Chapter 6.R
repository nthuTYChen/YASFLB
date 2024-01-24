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

my.test = cor.test(fakecor$BX, fakecor$BY)	# 將相關性測試結果儲存至my.test物件
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
plot(fakecor$AX, fakecor$AY)			# 使用scatterplots.txt裡的A組資料製作分佈圖
regress.line = lm(AY ~ AX, data = fakecor)	# 產生y = a + bx的線性模型
abline(regress.line)		          # 依線性模型方程式產生理想的迴歸線加到分佈圖上

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
plot(fakecor$AX, fakecor$AY)		# 產生「scatterplots.txt」中A組資料的相關分佈圖
# 加上線型模型的迴歸線，調整寬度與樣式
abline(lm(AY ~ AX, data = fakecor), lwd = 2, lty = 1)	
lines(fakecor$AX, fakecor$AY, lty = 2)	# 加上完美符合每個資料點的曲折線

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
anova(lm(y ~ x, data = regdat))

# 三之三節
# 練習六
# 手動驗證迴歸模型中x自變量的p值
x.coef = -0.23
x.se = 0.1735
x.t = x.coef / x.se
x.n = length(regdat$x)
# 我們手動計算出的t值沒有進位，所以雙尾p值會稍微有點誤差。如果你的t值直接
# 輸入summary()中的-1.326，那就會獲得一樣的p值了。再次提醒，這邊的t值是負數
# 也代表pt()給的是左尾的p值，因此直接乘2就可以得到雙尾p值了
pt(x.t, df = x.n - 2) * 2

# 三之四節
# 呈現「以x值預測y值」以及「以y值預測x值」的差異
x = regdat$x		  # 把變量獨立儲存為不同的物件，簡化每行的程式碼
y = regdat$y
n = nrow(regdat)	# 將資料框物件的資料行數做為樣本數
yx.lm = lm(y ~ x)		# 以x預測y
xy.lm = lm(x ~ y)		# 以y預測x
plot(x, y, xlab = "x", ylab = "y", main = "y ~ x")	# 產生y~x的分佈圖
abline(yx.lm, lwd = 2)                            # 加上y~x模型的迴歸線
segments(x, y, x, predict(yx.lm))
# 以segments()函數畫出每個xy坐標到x坐標與線性模型預測的y坐標之間的直線
plot(x, y, xlab = "x", ylab = "y", main = "x ~ y")
# 產生x~y的分佈圖，但x軸仍然是x，而y軸仍然是y，以便比較
lines(predict(xy.lm)[order(y)], y[order(y)], lwd = 2)
# 因為x軸與y軸與x~y的線性迴歸模型相反，因此只能用lines()和order()等函數
# 反轉x軸與y軸座標，再畫出x~y模型的迴歸線
segments(x, y, predict(xy.lm), y)
# 以segments()函數畫出每個xy坐標到線性模型預測的x坐標與y坐標之間的直線

# 比較不同順序的兩個變量利用cor()產生的r2
cor(x, y)^2 == cor(y, x)^2	
#[1] TRUE
# 比較不同順序的兩個變量利用cor.test()的p值
cor.test(x, y)$p.value == cor.test(y, x)$p.value	
#[1] TRUE

# 三之五節
# 自訂的faker()函數，可依照預設參數或傳入的參數亂數產生X與Y向量
faker = function(n = 100, err.sd = 1, a = 0, b = 1) {
 	x = rnorm(n)			# 根據樣本數從常態分佈中產生對應數量的亂數x
	y = a+b*x+rnorm(n)*err.sd	# 依照各個參數以及亂數x，透過線性方程式產生y值
	return(data.frame(x, y))	# 將得到的x值與y值建立為資料框物件回傳
}

# 依預設參數產生X與Y向量，並將回傳的資料框物件存入fakeA
fakeA = faker()

#傳入新的n參數產生X與Y向量(其餘採用預設函數)，並將回傳的資料框物件存入fakeB
fakeB = faker(n = 1000)

fake1 = faker(err.sd = 3)	# 得到隨機誤差非常大的資料
plot(fake1$x, fake1$y)		# 產生相關分佈圖看看分佈情況
lm(y ~ x, data=fake1)		  # 有得到「截距為0且斜率為1」的線性模型嗎？
fake2 = faker(err.sd = 0.1)	# 得到隨機誤差極小的資料
plot(fake2$x, fake2$y)
lm(y ~ x, data = fake2)		# 有得到「截距為0且斜率為1」的線性模型嗎？

fake3 = faker(n = 1000)	# 樣本數很高
plot(fake3$x, fake3$y)	# 產生存在些許隨機誤差的資料分佈
lm(y ~ x, data = fake3)	# 有得到「截距為0且斜率為1」的線性模型嗎？
fake4 = faker(n = 10)	# 樣本數極小
plot(fake4$x, fake4$y)	# 存在些許隨機誤差，但資料分佈趨勢更不明顯
lm(y ~ x, data = fake4)	# 有得到「截距為0且斜率為1」的線性模型嗎？

# 回到詞頻與時長的例子
# 建立線性迴歸模型
fd.lm = lm(Dur ~ LogFreq, data = fd)
# 只取得線性模型摘要報告中的係數部份
summary(fd.lm)$coefficients	
#              Estimate Std. Error    t value    Pr(>|t|)
#(Intercept) 252.615617   1.228796 205.579717 0.000000000
#LogFreq      -1.201109   0.383258  -3.133944 0.001754423

# 以再抽樣的程序驗證LogFreq在迴歸模型中的斜率p值.0018
# fd線性模型的斜率(−1.201…)
read.slope = summary(fd.lm)$coefficient[2]	
# 累積有多少斜率大於|−1.201…|
count.slopes = 0			
# 指定亂數種子
set.seed(1)					
# 以for迴圈進行10000次再抽樣
for(i in 1:10000) {
  # 隨機抽出並排序LogFreq
	LogFreq.new = sample(fd$LogFreq)		
	# 隨機抽出並排序Dur
	Dur.new = sample(fd$Dur)			
	# 以隨機排序資料進行線性迴歸分析
	model.new = lm(Dur.new ~ LogFreq.new)	
	rand.slope = summary(model.new)$coefficient[2]	# 得到新斜率
	# 隨機斜率絕對值是否大於等於樣本斜率絕對值
	# 也就是隨機斜率是否大於1.205或小於-1.205
	if(abs(rand.slope) >= abs(read.slope)) {	
		count.slopes = count.slopes + 1	# 是的話累積次數
  }
}

# 計算大於1.205或小於-1.205樣本斜率的比例，接近.0018
count.slopes / 10000
# [1] 0.0021

# 練習七
# 設定沒有任何資料點的底圖
plot(fd$LogFreq, fd$Dur, type = "n", xlab = "Log Frequency", 
     ylab = "Duration (ms)")

# 指定亂數種子
set.seed(1)			
# 進行10000次再抽樣
for(i in 1:10000) {
  # 隨機抽出並排序LogFreq
  LogFreq.new = sample(fd$LogFreq)		
  # 隨機抽出並排序Dur
  Dur.new = sample(fd$Dur)			
  # 以隨機排序資料進行線性迴歸分析
  model.new = lm(Dur.new ~ LogFreq.new)	
  # 根據再抽樣的迴歸模型加上迴歸線
  abline(model.new)
}

# 加上原始迴歸模型的趨勢線
abline(fd.lm, lwd = 2, col = "red")

# 四之一
# 進行局部迴歸產生非線性的迴歸線
fakecor = read.delim("scatterplots.txt")	  # 再讀取一次檔案
attach(fakecor)					                    # 直接讀取物件欄位
plot(DX, DY)					                      # 以D組資料進行比較
abline(lm(DY ~ DX), lty = 2)			          # 繪製線性迴歸線
loess.75 = loess(DY ~ DX)			              # 預設的span參數為0.75
lines(predict(loess.75), lty = 1, lwd = 2)	# 繪製loess.75模型迴歸線
loess.25 = loess(DY ~ DX, span = 0.25)	    # 使用0.25做為span參數
lines(predict(loess.25), lty = 1)		        # 繪製loess.25模型迴歸線
# 加上圖例說明
legend("topleft", lty = c(2, 1, 1), lwd = c(1, 2, 1),
       legend = c("Linear","Loess.25","Loess.75"))

# 練習八之一
plot(AX, AY)					                      # 以A組資料進行比較
abline(lm(AY ~ AX), lty = 2)			          # 繪製線性迴歸線
loess.5 = loess(AY ~ AX, span = 0.5)	      # 使用0.5做為span參數
lines(predict(loess.5), lty = 1)		        # 繪製loess.5模型迴歸線

# A組接近線性

plot(CX, CY)					                      # 以C組資料進行比較
abline(lm(CY ~ CX), lty = 2)			          # 繪製線性迴歸線
loess.5 = loess(CY ~ CX, span = 0.5)	      # 使用0.5做為span參數
lines(predict(loess.5), lty = 1)		        # 繪製loess.5模型迴歸線

# C組不太線性

plot(GX, GY)					                      # 以G組資料進行比較
abline(lm(GY ~ GX), lty = 2)			          # 繪製線性迴歸線
loess.5 = loess(GY ~ GX, span = 0.5)	     
lines(predict(loess.5), lty = 1)		       

# G組明顯不是線性

plot(HX, HY)					                      # 以H組資料進行比較
abline(lm(HY ~ HX), lty = 2)			          # 繪製線性迴歸線
loess.5 = loess(HY ~ HX, span = 0.5)	     
lines(predict(loess.5), lty = 1)	

# H組明顯不是線性

detach(fakecor)

# 練習八之二
library(ggplot2)

# 使用fakecor資料框並定義欄位與XY軸的對應
ggplot(fakecor, aes(x = AX, y = AY)) +
  # 加上個別資料點
  geom_point() +
  # 加上線性迴歸線
  geom_smooth(method = "lm", linetype = 2) +
  # 加上局部迴歸線
  geom_smooth(method = "loess", linetype = 1, span = 0.5) +
  theme_bw()

# 其他組的比較可基於上述程式碼，不再重覆

# 四之二節
# 模擬非線性的兒童語言資料，呈現U-shape發展
set.seed(1)					# 設定隨機種子讓我們可以得到相同的結果
age = runif(100) - 0.5			# 假設隨機產生100組年齡的z分數
acc = age ^ 2 + rnorm(100) / 10	# 假設按照100個小孩年齡產生的字詞正確率

plot(age, acc)			    # 繪製資料分佈
acc = acc[order(age)]		# 以年齡數字從小到大排序隨機產生的正確率
age = age[order(age)]		# 以年齡數字從小到大排序隨機產生的年齡
poly.lm = lm(acc ~ I(age ^ 2))	          # 以多項式方程產生模型
lines(age, predict(poly.lm), lty = 1)		# 加上多項式模型迴歸線
abline(lm(acc ~ age), lty = 2)			    # 加上線性模型迴歸線
# 加上圖例，設定背景顏色蓋掉與圖例重疊的資料點
legend("topleft", lty = c(1, 2), 	
       legend=c("Poly", "Linear"), bg = "white")

# 四之三節
# 將原始數值轉換為「小至大」的排名
rank(c(5, 5, 9))
#[1] 1.5 1.5 3

# 比較皮爾遜相關係數與史匹曼等級相關係數
fakecor = read.delim("scatterplots.txt")
attach(fakecor)
plot(HX, HY)
cor.test(HX, HY)
cor.test(HX, HY, method="spearman")

# 直接計算史匹曼等級相關係數
cor.test(CX, CY, method="spearman")$p.value	
#[1] 0.3023548

# 數值轉換排名後的皮爾遜相關係數
cor.test(rank(CX),rank(CY))$p.value		
#[1] 0.3037551

detach(fakecor)


kidage = c(1,2,3)
score = c(5,5,9)
# 在特殊情況下且變量含有相同排名時，史匹曼等級相關係數無法計算精確版本的p值
cor.test(kidage, score, method="spearman")
cor.test(rank(kidage), rank(score))$p.value # 以皮爾遜相關係數計算排名值

# 四之四節
# 先安裝languageR套件
install.packages("languageR", dependencies = T, ask = F)
library(languageR)		# 使用前讀取套件
fd = read.delim("freqdur.txt")
# 使用languageR套件的paircor.fnc()函數產生含有相關性測試資訊的變量相關圖
pairscor.fnc(fd[,2:5 ])		# 排除第一個欄位Word