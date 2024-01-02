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
library(rgl)	# 記得先安裝rlg套件
fd = read.delim("freqdur.txt")
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

# 二之三節

# 檢視多元迴歸模型的殘餘值分佈，看看模型有多麼配適因變量的資料
# 如果你的三個自變量多元迴歸模型不見了，就重新建立一個吧！
fd.lm = lm(Dur ~ LogFreq + AoA + Fam, data = fd) 	
summary(fd.lm)

# 練習四
# 一
dur.pred = predict(fd.lm)
# 二
dur.pred.diff = dur.pred - fd$Dur
# 三
dur.resid = resid(fd.lm)
# 四：手動計算殘餘值向量減去直接從模型抽取出的殘餘值向量相減後加總
sum(dur.pred.diff - dur.resid)
# [1] -4.944586e-10 # 基本上等於0

# 殘餘值的平均數就應該是「0」
fd.resid = resid(fd.lm)	# 先以resid()函數從模型中直接取得殘餘值
mean(fd.resid)		      # 果然是0

# 將殘餘值的分佈視覺化，呈現它的常態分佈
hist(fd.resid)		  # 以直方圖呈現分佈型態，看起來的確是對稱的常態分佈
qqnorm(fd.resid)		# 再改用QQ-Plot呈現，而大多殘餘值都落在理想分佈線上
qqline(fd.resid)

# 示範迴歸模型中少了關鍵的自變量時的殘餘值分佈
fd$FactorX = 1:nrow(fd) 		# 用fd的行數建立一個神秘變量
# 因為fd行數的最大/最小值差異很大，這個神秘變量的變異數也特別大
var(fd$FactorX)			
# [1] 237867.5
# 我們將這個神秘變量加上原始音長，然後存成新的DurX變量
fd$DurX = fd$Dur + fd$FactorX

# 把「DurX」當作因變量，再建立一次跟之前一樣包含對數詞頻、熟悉度、
# 以及習得年齡三個自變量的多元迴歸模型
fd.noX.lm = lm(DurX ~ LogFreq + AoA + Fam, data = fd)
coef(fd.noX.lm)

fd.noX.resid = resid(fd.noX.lm)	# 取出殘餘值
hist(fd.noX.resid)			# 直方圖看起來不太像鐘型的常態分佈
qqnorm(fd.noX.resid)	
qqline(fd.noX.resid)			# 殘餘值分佈兩端大幅偏離理想分佈線

# 將FactorX也加入模型做為自變量
fd.withX.lm = lm(DurX ~ LogFreq + AoA + Fam + FactorX, data = fd)
fd.withX.resid = resid(fd.withX.lm)
hist(fd.withX.resid)		
qqnorm(fd.withX.resid)	
qqline(fd.withX.resid)

# 二之四節
# 在只有截距的迴歸模型中，截距就是因變量的平均數
fd.int = lm(Dur ~ 1, data = fd)	# 建立只包含截距的迴歸模型
summary(fd.int)				          # 截距為249.2647
mean(fd$Dur)                    # 因變量平均數也是249.2647

# 練習五
# 取出fd資料框的前50列，另存為物件
fd.sub = fd[1:50,]
# 建立包含1至子集合列數(50)的數值向量，做為X軸變量
x = 1:nrow(fd.sub)
# 取出子集合的50個時長數值，做為Y軸變量
y = fd.sub$Dur
# 從只包含截距的模型中取得Y變量的預測值，另存為物件
fd.int.pred = predict(fd.int)
# 先以50個X軸與Y軸數值產生散佈圖
plot(x, y, xlab = "", ylab = "Duration", 
     main = "An Intercept-only Model of freqdur.txt")
# 加上只含有截距迴歸模型的最佳配適線(因為最佳配適線是平均數，所以為完美的水平線)
abline(fd.int, lwd = 2)
# 利用segment()函數繪出X軸上每個實際Y值到預測值(截距)的直線
segments(x, y, x, fd.int.pred[1:50])

# 建立Myers et al. (2011)研究的多元迴歸模型
native = read.delim("nativism.txt")
native.lm = lm(Accuracy ~ AgeAcquire + YearsUsing, data = native)
summary(native.lm)		# 讓我們聚集在檢定報告裡的各個係數

# 建立不含截距的模型進行比較
native.lm.noint = lm(Accuracy ~ AgeAcquire + YearsUsing - 1, data = native)
native.lm.noint0 = lm(Accuracy ~ 0 + AgeAcquire + YearsUsing, data = native)
summary(native.lm.noint)
summary(native.lm.noint0)		# 兩個模型的統計數據應該一樣

# 練習六
# 建立有截距且習得年齡為唯一自變量的簡單線性迴歸模型
native.lm.sub = lm(Accuracy ~ AgeAcquire, data = native)
# 建立無截距且習得年齡為唯一自變量的簡單線性迴歸模型
native.lm.noint.sub = lm(Accuracy ~ AgeAcquire - 1, data = native)
# 繪製實際數據點的散佈圖，以Accuracy為Y軸，AgeAcquire為X軸
# Y軸的區間定義為「0至1」，因為無截距的模型最佳配適線的起點是在Y軸的「0」
plot(Accuracy ~ AgeAcquire, data = native, ylim = c(0, 1), 
     main = "Accuracy ~ AgeAcquire")
# 加上有截距模型的最佳配適線
abline(native.lm.sub, lwd = 2, lty = 1)
# 加上無截距模型的最佳配適線
abline(native.lm.noint.sub, lwd = 2, lty = 2)
# 很明顯是有截距的模型比較貼近實際的數據吧？這也說明了在我們的例子中，雖然
# 假設截距為「0」比較合理，但卻不一定能夠得到足以解釋資料的模型哦！

# 二之五節
# 將自變量原始係數轉換為標準化係數
coefs = coef(native.lm)			# 以coef()函數取出模型中的所有係數
AgeAcquire_b = coefs["AgeAcquire"]  	# 根據自變量名稱取出斜率
YearsUsing_b = coefs["YearsUsing"]
acc.sd = sd(native$Accuracy)		# 取得因變量與自變量的標準差
age.sd = sd(native$AgeAcquire)
years.sd = sd(native$YearsUsing)
AgeAcquire_b * (age.sd / acc.sd)	# 計算習得年齡的標準化係數
YearsUsing_b * (years.sd / acc.sd)	# 計算語言使用時間的標準化係數

# 練習七
# 先取得原始截距
acc.int = coefs["(Intercept)"]
# 步驟一
# 根據上面的結果，每當習得年齡增加1個標準差，正確率就降低-0.6492881個標準差
# 根據這些資訊，我們可以計算出正確率截距減少-0.6482881個標準差後的數值
acc.int + acc.sd * -0.6492881
# (Intercept) 
# 0.5641341
# 步驟二
# 利用線性迴歸方程式計算推估的截距數值。因為「不考慮」語言使用時間的
# 影響，就代表假設語言使用時間的係數為「0」，也就是在迴歸方程式中可以忽略的
# 意思。將原始習得時間的係數(斜率)乘上代表習得時間的數值，也就是一個標準差代表的
# 「13.01762...」，就可以計算出因為習得時間改變一個標準差時因變量改變的數值。
# 將這個改變的數值加到原始截距上，就可以得出習得時間改變一個標準差時推估的
# 因變量數值，結果與步驟一相同。驗證成功！
acc.int + AgeAcquire_b * age.sd
# (Intercept) 
# 0.5641341 

# 將所有變量轉換為z分數
native$Accuracy.z = scale(native$Accuracy)
native$AgeAcquire.z = scale(native$AgeAcquire)
native$YearsUsing.z = scale(native$YearsUsing)
# 建立相同的多元線性迴歸模型
native.lm.z = lm(Accuracy.z ~ AgeAcquire.z + YearsUsing.z, data = native)  
summary(native.lm.z)
