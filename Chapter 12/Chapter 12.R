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
