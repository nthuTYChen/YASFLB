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

# 三之二節
# 示範進行含有類別因子自變量的迴歸分析
exp1 = read.delim("ColoredRooms.txt")		# 讀取資料
head(exp1)						                  # 看看前六列內容

exp1$Color = as.factor(exp1$Color)	# 必須先把原本代表字串的欄位轉換為因子
contrasts(exp1$Color)    		        # 看看因子層次經過虛擬編碼後的對比

# 將Color因子中的參考基準以relevel()函數中的「ref」參數設定為「Yellow」，並且
# 另外存為「Color.y」因子
exp1$Color.y = relevel(exp1$Color, ref = "yellow")
contrasts(exp1$Color.y)  

# 比較變異數分析以及使用虛擬編碼的迴歸分析建立的模型
# 進行變異數分析。注意，從頭開始建立的變異數分析是使用aov()函數
color.aov = aov(Learning ~ Color, data = exp1)
summary(color.aov)
# 進行迴歸分析，以預設參考基準「Blue」的因子做為自變量
color.dum.lm = lm(Learning ~ Color, data = exp1)
summary(color.dum.lm)

# 檢視迴歸模型中的自變量是否有整體的顯著效應
anova(color.dum.lm)

# 以「yellow」層次做為參考基準的類別因子再進行一次迴歸分析
# 注意自變量名稱
color.y.lm = lm(Learning ~ Color.y, data = exp1)		
summary(color.y.lm)

# 轉換為總和編碼
exp1$Color.sum = exp1$Color   			# 先複製類別因子
Color.sum.levels = levels(exp1$Color.sum)	# 取出因子中所有層次
# 利用所有層次與contr.sum()產生總和編碼，並代替因子中原始的層次「對比」
contrasts(exp1$Color.sum) = contr.sum(Color.sum.levels)
# 看看新的層次對比
contrasts(exp1$Color.sum)
#       [,1] [,2]
#Blue      1    0
#Red       0    1
#Yellow   -1   -1
# 檢視因子欄位，仍然是原始的層次名稱，並不是底層的數值。
head(exp1)

# 以總和編碼因子建立迴歸模型
color.sum.lm = lm(Learning ~ Color.sum, data = exp1)
summary(color.sum.lm)   

# 對此迴歸模型進行變異數分析
anova(color.sum.lm)

# 三之三之一節
# 教室顏色對生字學習影響的第二個實驗，也就是加上受試者性別做為第二個自變量
exp2 = read.delim("ColoredRooms2.txt")
# 條件裡的「!=」就是「不相等」的意思囉！
exp2.nored = subset(exp2, Color != "Red")	
# 自己看看子集合的資料長怎樣吧！
head(exp2.nored)		

# 先將字元欄位轉換為因子
exp2.nored$Color = as.factor(exp2.nored$Color)    
exp2.nored$Gender = as.factor(exp2.nored$Gender)
# 「Gender * Color」= Gender + Color + Gender:Color，還記得嗎？
exp2.nored.aov = aov(Learning ~ Gender * Color, data = exp2.nored)
summary(exp2.nored.aov) 

# 練習八
# 進行自變量順序調換的變異數分析
exp2.rv.aov = aov(Learning ~ Color * Gender, data = exp2.nored)
# 檢定結果完全相同
summary(exp2.rv.aov)
# 根據之前的解釋，只有當資料平均分佈於自變量的各個層次(組合)時，自變量的順序
# 才不影響變異數分析的數據。以xtabs()驗證每個自變量層次組合的數量是否相同。
# 沒錯，每個層次組合都有5筆數值
xtabs(~ Color + Gender, data = exp2.nored)

# 練習九
# 直接以最方便的Tukey's HSD事後檢定吧！
TukeyHSD(exp2.nored.aov)
# 在每組事後比較中，我們關切的是不同性別在同一個教室顏色中的表現，也就是
# 「Male:Blue-Female:Blue」與「Male:Yellow-Female:Yellow」。在前者的比較中
# 性別差異在藍色教室中不顯著，而在後者的比較中性別差異在黃色教室中顯著。
# 這便是交互作用的來源啦！

# 先複製兩個因子

# 將類別因子以ifelse()手動轉換為總合編碼
# ifelse()可以檢查欄位的每個值是否符合某個層次的名稱，
# 並根據「是」或「否」的結果回傳「1」或「−1」
exp2.nored$Color.sum = ifelse(exp2.nored$Color == "Blue", 1, -1)
exp2.nored$Gender.sum = ifelse(exp2.nored$Gender == "Male", 1, -1)

# 將使用總合編碼的教室顏色與性別欄位相乘，得到代表兩個自變量交互作用的總合編碼
exp2.nored$ColorGender.sum = exp2.nored$Color.sum * exp2.nored$Gender.sum
head(exp2.nored)		# 查看一下編碼後的資料內容

# 以第4、5、6欄為Color.sum, Gender.sum和ColorGender.sum計算相關係數矩陣
cor(exp2.nored[, 4:6])	

# 練習十
# 去掉(負數)exp2.nored中的頭兩列
exp2.nored.sub = exp2.nored[c(-1, -2),]
# 再次產生相關係數矩陣，會發現三個變量開始出現微弱的相關性了。如果資料分佈
# 更不均勻，那麼變量之間的混雜程度就會更嚴重。
cor(exp2.nored.sub[, 4:6])	

# 用三個經過總合編碼轉換的變量進行多元迴歸分析
color.sum.lm = lm(Learning ~ Gender.sum + Color.sum +　ColorGender.sum, 
                 data = exp2.nored)
summary(color.sum.lm)

# 練習十一
# 先複製兩個自變量，並在複製的過程中確保有轉換為因子
exp2.nored$Color.sumr = as.factor(exp2.nored$Color)
exp2.nored$Gender.sumr = as.factor(exp2.nored$Gender)
# 對複製的自變量進行總合編碼轉換
# 因為我們知道兩個類別因子中都只有兩個層次，所以也可以直接在contr.sum()中放入層次
# 數量進行轉換
contrasts(exp2.nored$Color.sumr) = contr.sum(2)
contrasts(exp2.nored$Gender.sumr) = contr.sum(2)
# 可以自己用contrasts()看看轉換後的編碼
contrasts(exp2.nored$Color.sumr)
contrasts(exp2.nored$Gender.sumr)
# 建立含有交互作用的迴歸模型
color.sumr.lm = lm(Learning ~ Color.sumr * Gender.sumr, data = exp2.nored)
# 數據完全一樣
summary(color.sumr.lm)

# 三之三之一節
# 解釋迴歸中連續變量的交互作用
# 先建立三個變量包含的虛假語料
x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x2 = c(0, 0, 0, 4, 4, 4, 7, 7, 7)
y = c(3, 2, 1, 6, 5, 4, 9, 8, 7) 

# 以散佈圖和簡單線性迴歸檢查x1與y的相關性(正相關)
fake.simp.lm = lm(y ~ x1)	             # 簡單線性迴歸
summary(fake.simp.lm) 　　
plot(x1, y)		  		                   # 產生散佈圖
lines(predict(fake.simp.lm), lwd = 2)  # 加上呈現模型預測值的折線
# 加入x2自變量成為多元線性迴歸，x1效應變成負的
summary(lm(y ~ x1 + x2)) 　　　
# 含有連續變量交互作用的多元迴歸：交互作用是顯著的
fake.int.lm = lm(y ~ x1 * x2)
summary(fake.int.lm) 　　　	
# 以predict()取出預測值後在剛剛產生的散佈圖上以虛線呈現多元迴歸的預測值折線
lines(predict(fake.int.lm)) 　　

plot(y ~ x2)  # 檢視x2與y的關係

# 測試x1與x2的相關性
cor.test(x1, x2) 　　　	# 結果應該是r(7) = .95, p< .0001

# 練習十二
# 使用3D散佈圖與最佳配適平面檢查三個變量之間的相關性
library(rgl)				# 載入rgl套件
plot3d(x = x1, y = x2, z = y)		# 以原始變量做為產生3D散佈圖
coefs = coef(fake.int.lm)	     	# 從完整迴歸模式中取得係數
a = coefs["x1"]			          	# 利用係數產生最佳配適平面
b = coefs["x2"]
c = -1                          # 老樣子，z軸通常為-1
d = coefs["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.5)

# Myers (2015)的研究例子
syl = read.delim("NBUP.txt")  	# 讀取檔案
cor.test(syl$NB, syl$UP)	    	# 結果應該是不顯著的r(3185) = -.03, p = .09
cor.test(syl$NB, syl$MeanResp)	# r(3185) = 35.507, p < .001
cor.test(syl$UP, syl$MeanResp)	# r(3185) = 4.772, p < .001

syl.noint = lm(MeanResp ~ NB + UP, data = syl)  	# 建立沒有交互作用的模型
summary(syl.noint)					# 兩個變量都顯著

# 練習十三
# 一
1.392 * (10 ^ -1) + 1.178 * (10 ^ -2) * 6.3 + 1.137 * (10 ^ -4) * 15 # 0.215
# 二
1.392 * (10 ^ -1) + 1.178 * (10 ^ -2) * 15 + 1.137 * (10 ^ -4) * 1 # 0.316

syl.int = lm(MeanResp ~ NB * UP, data = syl) # 納入交互作用的模型
summary(syl.int)					

# 繪製<圖十三>的未含以及包含交互作用的圖表
library(effects)  # 別忘了安裝以及載入套件
library(ggplot2)

# 圖十三左
# 因為未含交互作用的迴歸模型使用effects套件取得估計值相加作用的趨勢線反而比較麻煩
# 所以我們自己建立可以用來繪製圖表的估計值資料框
# 從迴歸模型中取得係數
syl.coefs = coef(syl.noint)
# 分別取得截距以及自變量的係數
intercept = syl.coefs["(Intercept)"]
nb.b = syl.coefs["NB"]
up.b = syl.coefs["UP"]

# 先建立估計值資料框的NB與UP欄位。由於用來產生圖十三右的effects套件會在NB
# 使用[0, 10, 20, 30, 40]的級距，並在UP使用[1, 100, 200, 300, 400]的級距。
# 在這兩個欄位產生所有NB與UP級距的組合
syl.noint.ef = data.frame(NB = rep(c(0, 10, 20, 30, 40), 5),
                          UP = c(rep(1, 5), rep(100, 5), rep(200, 5), rep(300, 5), rep(400, 5)))

# 使用未含交互作用的迴歸方程式以及各個係數，還有NB以及UP欄位值產生對應的估計值
syl.noint.ef$fit = intercept + nb.b * syl.noint.ef$NB + up.b * syl.noint.ef$UP

# 可以看到資料框物件中，NB與UP欄位包含所有[0, 10, 20, 30, 40]與[1, 100, 200, 300, 400]
# 的組合，而fit欄位就是根據這些組合結合迴歸模型數據產生的預測值
head(syl.noint.ef)

# 接著使用ggplot2產生圖十三左
# ggplot()只負責當作基礎函數，因為我們會利用其他geom函數定義圖表
ggplot() +
  # 在圖表中加上來自syl物件的實際資料點，將NB對應至X軸而MeanResp對應至Y軸
  # 設定資料點的大小、顏色、以及透明度
  geom_point(data = syl, mapping = aes(x = NB, y = MeanResp), 
             size = 2, color = "lightgrey", alpha = .9) +
  # 在圖表中加上呈現來自syl.int.ef物件預測值的折線，將NB對應至X軸而fit對應至Y軸
  # 並以UP指定進行分組。因為UP原本是數值，但在繪圖中要當成分組的因子，所以我們
  # 會使用factor()函數讓ggplot2知道要把UP當成因子。
  geom_line(data = syl.noint.ef, 
            mapping = aes(x = NB, y = fit, group = factor(UP)),
            stat = "identity", linewidth = 1) +
  # 在圖表中加上呈現來自syl.int.ef物件預測值的數據點，將NB對應至X軸而fit對應至Y軸
  # 並以UP指定進行分組和使用不同樣式的數據點。使用factor()函數的原因同上
  geom_point(data = syl.noint.ef, 
             mapping = aes(x = NB, y = fit, group = factor(UP), shape = factor(UP)),
             stat = "identity", size = 3) +
  labs(x = "NB", y = "Predicted Acceptability", title = "MeanResp ~ NB + UP",
       shape = "UP") +
  theme_bw()

# 圖十三右
# 利用effects套件中的effect()函數，以迴歸模型中的交互作用產生估計值。
# 再利用as.data.frame()函數將effect()產生的資料重新組織為資料框物件
syl.int.ef = as.data.frame(effect("NB:UP", syl.int))
# 可以看到資料框物件中，NB與UP欄位包含所有[0, 10, 20, 30, 40]與[1, 100, 200, 300, 400]
# 的組合，而fit欄位就是根據這些組合結合迴歸模型數據產生的預測值
head(syl.int.ef)

# 接著使用ggplot2產生圖十三右
# ggplot()只負責當作基礎函數，因為我們會利用其他geom函數定義圖表
ggplot() +
  # 在圖表中加上來自syl物件的實際資料點，將NB對應至X軸而MeanResp對應至Y軸
  # 設定資料點的大小、顏色、以及透明度
  geom_point(data = syl, mapping = aes(x = NB, y = MeanResp), 
             size = 2, color = "lightgrey", alpha = .9) +
  # 在圖表中加上呈現來自syl.int.ef物件預測值的折線，將NB對應至X軸而fit對應至Y軸
  # 並以UP指定進行分組。因為UP原本是數值，但在繪圖中要當成分組的因子，所以我們
  # 會使用factor()函數讓ggplot2知道要把UP當成因子。
  geom_line(data = syl.int.ef, 
            mapping = aes(x = NB, y = fit, group = factor(UP)),
            stat = "identity", linewidth = 1) +
  # 在圖表中加上呈現來自syl.int.ef物件預測值的數據點，將NB對應至X軸而fit對應至Y軸
  # 並以UP指定進行分組和使用不同樣式的數據點。使用factor()函數的原因同上
  geom_point(data = syl.int.ef, 
             mapping = aes(x = NB, y = fit, group = factor(UP), shape = factor(UP)),
             stat = "identity", size = 3) +
  labs(x = "NB", y = "Predicted Acceptability", title = "MeanResp ~ NB * UP",
       linetype = "UP") +
  theme_bw()

# 進行標準化的多元線性迴歸模型
syl$MeanResp.z = scale(syl$MeanResp)		# 將所有變量先轉換為z分數
syl$NB.z = scale(syl$NB) 
syl$UP.z = scale(syl$UP)
# 以標準化後的變量建立含有交互作用的迴歸模型
syl.int.z = lm(MeanResp.z ~ NB.z * UP.z, data = syl)
summary(syl.int.z)

# 三之四節
# 讀取含有自訂函數lorch.myers.simple()的程式碼
source(lorch.myers.R)
lmd = read.delim("lorchmyersdat.txt") 
head(lmd)
# 以自訂函數進行重覆測量簡單線性迴歸
lorch.myers.simple(lmd)

# 練習十四
# 直接以Y ~ X的語法進行簡單線性迴歸：我們得到的結論仍然是X有顯著的主要效應，
# 但同時我們也會看到比較低的p值。這代表沒有考慮組內設計的迴歸分析會有著更高的
# 第二型誤差機率(錯誤地得到應該推翻虛無假設的結論)
summary(lm(Y ~ X, data = lmd))

# 進行重複測量變異數分析
lmd.aov = aov(Y ~ X + Error(as.factor(Subj)/X), data = lmd)
summary(lmd.aov)

# 四之一之一
# 以對數詞頻、熟悉度、習得年齡對單詞音長影響做為檢查模型適合度的例子
fd = read.delim("freqdur.txt")	# 過了這麼久，應該是需要重新讀取檔案了
fd$LogFreq = log(fd$Freq)
fd.lm = lm(Dur ~ LogFreq + AoA + Fam, data = fd)
summary(fd.lm)

yhat = predict(fd.lm) 			# 模型的估計值「ŷ」
n = nrow(fd) 				        # 樣本數量
SSM = sum((yhat - mean(fd$Dur)) ^ 2) 	# 模型與實際數據差距的平方和
k = 4 # 模型裡的係數數量：截距、詞頻(LogFreq)、習得年齡(AoA)、熟悉度(Fam) 
dfM = k - 1 			         	# 模型的自由度(df)
MSM = SSM/dfM 				      # 模型平方和平均數，也就是可解釋的變異
RSS = sum(resid(fd.lm) ^ 2) 		# 殘餘平方和(平方誤差總和)
dfE = n - k 				        # 誤差值的自由度
MSE = RSS/dfE 			        # 模型殘餘平方和平均數，也就是無法解釋的變異
f.val = MSM/MSE 			        # F值等於可解釋變異和不可解釋變異的比例
p.val = pf(f.val, df1 = dfM, df2 = dfE, lower.tail = F) # 取得F分佈右尾p值

# r2 = 迴歸模型中單詞音長估計值「ŷ」和實際單詞音長的變異數比率(ratio)
var(yhat)/var(fd$Dur)

# 以有無截距的迴歸模型示範以AIC進行模型比較
native = read.delim("nativism.txt")    # 以防萬一，重新讀取檔案
# 建立有截距的模型
native.lm = lm(Accuracy ~ AgeAcquire + YearsUsing, data = native)
AIC(native.lm)
#[1] -25.14485
# 建立無截距的模型
native.lm.noint = lm(Accuracy ~ 0 + AgeAcquire + YearsUsing, data=native)
AIC(native.lm.noint)
# [1] -0.1229758

# 進行似然比檢定
anova(native.lm.noint, native.lm) 

# 回到freqdur.txt的例子，以似然比檢定測試對數詞頻是否有顯著效應
# 先重新建立包含所有變量的迴歸模型檢查個別係數的效應顯著性
fd.lm = lm(Dur ~ LogFreq + AoA + Fam,data = fd)
summary(fd.lm)
# 建立少了詞頻的多元迴歸模型
fd.lm.nofreq = lm(Dur ~ AoA + Fam,data = fd)
# 進行兩個有著巢式結構差異模型的似然比檢定
anova(fd.lm.nofreq, fd.lm)

# 練習十五
# 一、建立只有截距且因變量為Accuracy的模型
native.b0 = lm(Accuracy ~ 1, data = native)
# 二、建立含有截距且自變量為AgeAcquire的模型
native.age = lm(Accuracy ~ AgeAcquire, data = native)
# 與(一)進行似然比檢定，加入AgeAcquire的模型比只有截距的模型解釋更多因變量變異
anova(native.b0, native.age)
# 三、保留AgeAcquire，再加入YearsUsing自變量
native.age.yr = lm(Accuracy ~ AgeAcquire + YearsUsing, data = native)
# 與(二)進行似然比檢定，加入YearsUsing並沒有解釋更多因變量變異！
anova(native.age, native.age.yr)
# 最終模型為Accuracy ~ AgeAcquire，與稍早的測試類似

# 以似然比檢定測式迴歸模型中不同變量效應之間是否有顯著差異。
# 建立複雜模型
native.lm = lm(Accuracy ~ AgeAcquire + YearsUsing, data = native)
# 建立簡單模型
native.lm.equal = lm(Accuracy ~ I(AgeAcquire + YearsUsing), data = native)
anova(native.lm.equal, native.lm)

# 四之二之一節
x = runif(10)				# 從均勻分佈中抽樣10筆數值
# 以x為基礎加上亂數除以2得出y值，這樣x和y雖不完全一樣，但就會部分相關了，對吧？
y = x + runif(10) / 2 			
cor(x, y) 					# 沒錯，r絕對值非常接近1但不等於1
# 根據x與y繪製散佈圖
plot(x, y, xlim = c(0, 1), ylim = c(0, max(y)))
# 繪製每個從原點指向(x, y)的箭頭，還蠻炫的吧！
arrows(0, 0, x, y)

# 示範當兩個自變量完全共現時進行迴歸分析時會產生的問題
set.seed(30)			  # 指定亂數種子
x1 = rnorm(100)			# 這裡不用指定亂數種子，因為不同抽樣結果都一樣
x2 = 2 * x1 + 1			# 以任何線性方程式產生x2都會讓x2和x1完全共線
cor(x1, x2) 			  # 相關性為1，完全共線
y = rnorm(100) 			# 不管你的因變量是什麼，結果都一樣
summary(lm(y ~ x1 + x2)) 	# R不開心，給你NA(Not Applicable)
summary(lm(y ~ x2 + x1)) 	# 順序不影響迴歸分析，也不影響R不開心的程度

# nativism.txt例子中兩個自變量高度相關
native = read.delim("nativism.txt")		# 假設你從頭進行
cor(native$AgeAcquire, native$YearsUsing)

# 以car套件中的vif()函數計算兩個自變量的VIF值
library(car)		# 還沒安裝套件的話就先安裝唷！
native.lm = lm(Accuracy ~ AgeAcquire + YearsUsing, data = native)
vif(native.lm)		# 將建立好的迴歸模型放入vif()函數即可

# 計算條件數測試是否自變量沒有共線性問題
kappa(native[2:3])　　# 第二欄為習得年齡、第三欄為語言使用時間

# 以languageR套件的collin.fnc()函數測試包含截距在內的共線性
library(languageR)			# 還是提醒先安裝並載入套件
# 一樣放入自變量的欄位就好，得到的共線性報告先存入native.col
native.col = collin.fnc(native[2:3])
# 從native.col的cnumber欄位取得條件數
native.col$cnumber

# 改以freqdur.txt的例子以三種方式測試共線性影響
fd = read.delim("freqdur.txt")			   	# 假設你從頭開始
fd.lm = lm(Dur ~ LogFreq + AoA + Fam,data = fd)
vif(fd.lm)						                 	# 通通小於5，沒問題！
kappa(fd[c("AoA","Fam","LogFreq")])			# 小於30，沒問題！
fd.col = collin.fnc(fd[c("AoA","Fam","LogFreq")])
fd.col$cnumber						              # 啊！超過30，糟糕了

# 四之二之二
# 建立只含有截距且因變量為Dur的迴歸模型
fd.lm0 = lm(Dur ~ 1, data = fd) 
summary(fd.lm0)
# 迴歸模型中截距數據等同於單一樣本t檢定的數據
t.test(fd$Dur)

# 進行逐步迴歸分析並將結果存入fd.steps
fd.steps = step(fd.lm0, Dur ~ LogFreq + AoA + Fam, data = fd)
summary(fd.steps)
