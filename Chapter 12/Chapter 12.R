# 請記得先設定工作目錄！

# 二之一節
# 火星人奢華性刪除現象資料
rd = read.delim("richdeletion.txt")
head(rd)

# 強迫R以五位整數顯示X軸數值。沒有此行時R會以「十的次方」格式呈現數值
options(scipen = 5)
plot(Deletion ~ Income, data = rd)	# 用之前Y~X方程式的語法也能畫圖！

rd.lm = lm(Deletion ~ Income, data = rd)	# 在線性迴歸中分析富豪型刪除機率
summary(rd.lm)					                  # 只擷取係數的部分
abline(rd.lm)                             # 將最佳配適線加到<圖一>上

# 將glm()函數中的family參數設定為binomial，以二項分佈函數建立廣義線型模型
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)
summary(rd.glm) 

# 二之二節
log(Inf)			# 沒錯！R可以讓你計算無限值！
log(0)

# 練習一
# 一
# 計算Deletion的平均值，得到有富豪型刪除的機率P1。
del.p = mean(rd$Deletion) # 0.44
# 套用公式二
log(del.p / (1 - del.p)) # -0.2411621

# 二
# 取得所有係數
coefs = coef(rd.glm)

# 對數的相反就是指數exp()，將截距係數轉換為原始機率
exp(coefs["(Intercept)"])  # 收入為0時，有富豪型刪除的預測機率也趨近於0

# 以gtools套件的函數快速在對數勝率與原始機率間轉換
library(gtools)			# 記得先安裝並載入套件
logit(0.5)          # 從機率計算對數勝率
#[1] 0

inv.logit(0)        # 從對數勝率轉換回機率
#[1] 0.5

# 二之三之一節
# 火星語的「-qfx」後綴例子
# 建立含有20個1(有後綴的名詞)和10個0(無後綴的名詞)的向量
cheeseNouns = c(rep(1, 20), rep(0, 10)) 
sum(cheeseNouns == 0)				# 確認一下是否1和0的數量正確
sum(cheeseNouns == 1)    

# 二項檢定
# 以「binom.test()」直觀地根據「1」的數量以及樣本數進行二項檢定
binom.test(x = sum(cheeseNouns == 1), n = length(cheeseNouns))

# 卡方檢定
# 先以xtabs()來計算0和1的數量再以列聯表進行卡方檢定
cheeseNouns.tab = xtabs(~ cheeseNouns)
chisq.test(cheeseNouns.tab) 

# 只包含截距的邏輯迴歸模型
# 因為cheeseNouns單純是一個數值向量，這裡我們可以直接以「y ~ 1」的語法
# 在未指定「data」參數時就建立只含有截距的模型
cheese.glm = glm(cheeseNouns ~ 1, family = "binomial")
summary(cheese.glm) 

# 練習二
# 先重新建立富豪型刪除的邏輯迴歸模型
rd = read.delim("richdeletion.txt")
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)
# 取出收入斜率
coefs = coef(rd.glm)
coef.inc = coefs["Income"]
# coef.inc為0.000554，也就是每當收入增加1，火星人呈現富豪型刪除(P1)的對數勝率
# 就增加0.000554
# 手動計算收入為35790時火星人呈現富豪型刪除的預測機率
# 先以迴歸方程式得出預測的P1對數勝率
del.logit = coefs["(Intercept)"] + 35790 * coef.inc  # -5.85
# 再以gtools套件的inv.logit()將預測的對數勝率轉換回機率
library(gtools)
inv.logit(del.logit)                                 # 0.00286033，大約0.2%的機率

# 練習三
# 要使用predict()根據某個自變量的值取得因變量的值，第一步是要建立一個資料框，而欄位
# 名稱必須與迴歸模型中的該自變量名稱相同，而在這個練習裡就是Income。另外也還記得
# 怎麼利用seq()函數產生一個每兩個數值之間有著相同間隔的連續數字嗎？
pred.df = data.frame(Income = seq(from = 20000, to = 60000, by = 1000))
# 使用建立的資料框和rd.glm從predict()中取得富豪型刪除的預測「對數勝率」
pred.logits = predict(rd.glm, pred.df)
# 再次利用gtools套件的inv.logit將對數勝率轉換為機率
library(gtools)
# 也可以進位至小數點第三位，比較好解讀數據
round(inv.logit(pred.logits), 3)

# 二之三之二
# 從邏輯迴歸模型的檢定報告中取出對數似然值、以內建的「logLik()」函數取出殘餘偏差值
# 最後再進行<公式五>的驗算
logLik(cheese.glm)               			# 直接從模型中取出對數似然值
#'log Lik.' -19.09543 (df=1) 	
cheese.glm.sum = summary(cheese.glm)	# 將模型的檢定報告另存為物件
dev = cheese.glm.sum$deviance		      # 從檢定報告中取出殘餘偏差值
dev
#[1] 38.19085
dev/(-2)    				                  # 以<公式五>從殘餘偏差值反推至似然值
#[1] -19.09543

# 驗證只含有截距的模型中的截距係數代表的就是P1的機率
# cheeseNouns的向量平均數就代表P1的機率
mean(cheeseNouns)		
#[1] 0.6666667
coefs = cheese.glm.sum$coefficients	# 從模型檢定報告中取出所有係數數據
coef.int = coefs[1, "Estimate"]	# 取得截距係數數據(1 = 數據中的第1列)
#[1] 0.6931472
library(gtools)              				# 記得安裝載入gtools套件
# 將截距係數的P1對數勝率反轉回P1機率，與前面的平均數相同！
inv.logit(coef.int)
#[1] 0.6666667

# 以單一樣本z檢定驗證模型中截距係數的p值
coefs[1, "Pr(>|z|)"]			          # 取得截距係數p值(第1列數據的p值)
#[1] 0.07350237
logit.z = coefs[1, "z value"] 	   	# 取得截距係數z值
2 * pnorm(logit.z, lower.tail = F)	# z值大於0所以取得右尾p值再計算雙尾p值
#[1] 0.07350237

logit.SE = coefs[1, "Std. Error"] 		# 只取出截距係數標準誤
coef.int/logit.SE             				# 公式六得到相同z值
#[1] 1.789699

# 以起司名詞後綴例子示範卡方值與邏輯迴歸截距z值的相似性
# 以起司後綴例子進行卡方檢定，而檢定報告另存為物件
cheese.chisq = chisq.test(c(10, 20))
cheese.chisq$statistics 				# 只取出報告中的卡方值
#X-squared 
#3.333333
logit.z ^ 2						# 計算邏輯迴歸截距z值平方
#[1] 3.203021 

# 二之三之三節
# 以火星人起司名詞為例進行以卡方檢定偏差分析為基礎的似然比檢定
cheese.glm0 = glm(cheeseNouns ~ 0, family = "binomial")	# 沒有截距的模型
anova(cheese.glm0, cheese.glm, test = "Chisq")	        # 與有截距模型比較

# 二之四節
# 假設火星人起司名詞後綴出現與詞頻負相關
Suffixed = c(rep(1, 20), rep(0, 10))	# 我們的二元因變量
Frequency = 1:30			   	            # 詞頻數據
cor.test(Suffixed, Frequency)	        # r = -.82

set.seed(1)				                    # 指定亂數種子
WordLength = sample(1:30)	            # 隨機從1-30再抽樣隨機排序數字

# 直接指定每個數據向量(而不是資料框中的欄位)做為因變量和自變量
cheese2.glm = glm(Suffixed ~ Frequency + WordLength, family = "binomial")
#Warning messages:
#  1: glm.fit: algorithm did not converge 
#  2: glm.fit: fitted probabilities numerically 0 or 1 occurred
summary(cheese2.glm)

# 在因變量與自變量的相關性加入一點雜訊，使得迴歸模型不再百分百完美
# 取出原始Suffixed變量的第2個到第30個值，再取出原始Suffixed變量的第1個值
# 然後用c()重新結合，將原始的第1個值變成新的變量中的最後一個值
Suffixed.f = c(Suffixed[2:30], Suffixed[1])
cheese3.glm = glm(Suffixed.f ~ Frequency + WordLength, family = "binomial")
summary(cheese3.glm)

# 練習四
# 先建立只包含詞頻的起司名詞後綴模型
cheese.glm.freq = glm(Suffixed.f ~ Frequency, family = "binomial")
# 也建立使用Suffixed.f做為因變量的只含截距模型
cheese.glm0.f = glm(Suffixed.f ~ 1, family = "binomial")
# 與完整模型(使用Suffixed.f做為因變量)的似然比檢定
# 缺少/增加單詞時長自變量並無顯著改變模型配適度
anova(cheese3.glm, cheese.glm.freq, test = "Chisq")
# 與只含有截距的模型進行似然比檢定，加入詞頻顯著減少殘餘值/增加配適度
anova(cheese.glm0.f, cheese.glm.freq, test = "Chisq")

# 二之五節
# 以火星人富豪型刪除資料示範正確的二元資料模型圖表繪製
rd = read.delim("richdeletion.txt")
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)

# 繪製符合因變量二元原始資料的最佳配適曲線
options(scipen = 5)		        # 強迫R以5位數整數而非e次方格式呈現數值
plot(rd$Deletion ~ rd$Income)	# 繪製散佈圖
rd.pred = predict(rd.glm, type = "response")	# 取得原始因變量的估計值
# 將收入數值與富豪型刪除估計值合併為一個資料框
curve.df = data.frame(Income = rd$Income, fitted = rd.pred)
# 再將整個資料框根據收入從小到大排序列，以便可以使用lines()函數依序將估計值按照
# 收入從小到大排序的收入數值連成一條曲線
curve.df = curve.df[order(curve.df$Income), ]
lines(x = curve.df$Income, y = curve.df$fitted)		

# 練習五
library(ggplot2)
ggplot(data = rd, mapping = aes(x = Income, y = Deletion)) + 
  # 加上實際數據點時設定為半透明，可以突顯最佳配適線
  geom_point(alpha = .5) +
  stat_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial")) +
    theme_bw()

# 以X軸與Y軸分組計算平均數，解決呈現個別數據點與機率變化趨勢的問題
# 將ｘ軸自變量放入十個範圍相同的分組
bins = cut(rd$Income, 10) 			
# 每個範圍分組裡算出一個收入平均數
mean.x = tapply(rd$Income, bins, mean) 	
# 計算每個範圍分組裡的呈現富豪型刪除與否的平均數，也就是呈現刪除的機率
prob.y = tapply(rd$Deletion, bins, mean)	
# 重覆繪圖的過程
options(scipen = 5)
# 使用經過分組計算的數值繪製散佈圖
plot(mean.x, prob.y, ylab = "Deletion rate", xlab = "Income")
# 以稍早產生的收入數值對應的估計機率的資料加上最佳配適曲線
lines(x = curve.df$Income, y = curve.df$fitted)

# 練習六
bins.df = data.frame(Income = mean.x, Prob = prob.y)

library(ggplot2)
# ggplot()函數不做任何對應，只建立空白底圖
ggplot() + 
  # 在geom_point()函數中設定分組計算資料框為資料來源
  geom_point(data = bins.df, mapping = aes(x = Income, y = prob.y), alpha = .5) +
  # 在stat_smooth()函數中設定模型估計值資料來源
  stat_smooth(data = rd, mapping = aes(x = Income, y = Deletion),
              method = "glm", se = FALSE, method.args = list(family = "binomial")) +
  theme_bw()

# 直接以對數勝率呈現線性的最佳配適線
bins = cut(rd$Income, 10)		# 同樣將X軸進行範圍分組
# 建立Y軸進行範圍分組且加入雜訊的函數
logit.bin = function(vector) {
 	prob1 = mean(c(vector, 0, 1)) 	# 為每個分組的P1計算多增加兩個雜訊數值
	prob0 = 1 - prob1 		         	# 計算範圍分組的P0
	log.odds = log(prob1 / prob0) 	# 加了雜訊後P1和P0都不可能是0
	return(log.odds)			          # 回傳分組對數勝率
}
mean.x = tapply(rd$Income, bins, mean) 	# 為每個X軸分組計算平均數
# 為每個Y軸分組以logit.bin計算平均數
logit.y = tapply(rd$Deletion, bins, logit.bin)	
# 產生散佈圖
plot(mean.x, logit.y, ylab = "Deletion (log odds)", xlab = "Income")
abline(lm(logit.y ~ mean.x)) 			      # 加上線性迴歸最佳配適線

# 以effects套件繪製圖表，結合圖五及圖六的優點
library(effects)			      # 記得先安裝載入套件
plot(allEffects(rd.glm))		# 取得rd.glm模型中的所有效應繪製圖表
