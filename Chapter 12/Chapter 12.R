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

# 二之六節
# 重新建立兩個我們要進行比較的模型。如果你的變量已經消失的，也往前翻幾頁重新建立吧
cheese0.glm = glm(Suffixed.f ~ 1, family = "binomial")
cheese3.glm = glm(Suffixed.f ~ Frequency + WordLength, family = "binomial")
# 以logLik()函數取出各個模型的對數似然值，套用公式八，再使用as.numeric()
# 函數將結果轉為不帶其他文字資訊的數值
as.numeric((logLik(cheese0.glm) - logLik(cheese3.glm)) / logLik(cheese0.glm))

# 從完整模型的檢定報告數據計算RL2
cheese3.sum = summary(cheese3.glm)
cheese3.LL = -2 * cheese3.sum$deviance	# 取得檢定報告中殘餘偏差似然值
cheese0.LL = -2 * cheese3.sum$null.deviance	# 取得檢定報告中零偏差似然值
(cheese0.LL - cheese3.LL) / cheese0.LL	# 公式八

# 進行類似標準化係數的邏輯迴歸分析
# 以scale()函數將自變量進行z分數轉換
cheese3.z.glm = glm(Suffixed.f ~ scale(Frequency) + 
       					scale(WordLength), family = "binomial")
summary(cheese3.z.glm)

# 利用似然比檢定比較兩個自變量的效應值是否有顯著差距
# 以I()函數告訴R「+」是將兩個自變量加在一起的數學運算，而不是加入獨立自變量的意思
cheese3.z.same.glm = glm(Suffixed.f ~ I(scale(Frequency) + 
                                          scale(WordLength)), family = "binomial")
# 注意要設定test參數哦！
anova(cheese3.z.same.glm, cheese3.z.glm, test = "Chisq")

# 以Menard (2004)的方法，利用因變量對數勝率預測值的標準差進行真的標準化邏輯迴歸分析
coefs = coef(cheese3.glm)		# 從模型中取出所有效應值
Frequency.coef = coefs["Frequency"]	# 取出詞頻效應值
WordLength.coef = coefs["WordLength"]	# 取出單詞長度效應值
# 使用<公式八>計算邏輯迴歸的確定係數，以as.numeric()函數將文字資訊從計算結果移除
RL = sqrt(as.numeric((logLik(cheese0.glm) -
                        logLik(cheese3.glm)) / logLik(cheese0.glm)))
# 計算自變量標準差
Frequency.sd = sd(Frequency)			
WordLength.sd = sd(WordLength)
# 取出因變量預測對數勝率計算標準差
logit.yhat.sd = sd(predict(cheese3.glm)) 	
# 計算標準化自變量係數
Frequency.beta = Frequency.coef * RL * (Frequency.sd / logit.yhat.sd)
WordLength.beta = WordLength.coef * RL * (WordLength.sd / logit.yhat.sd)
c(Frequency.beta, WordLength.beta)
#[1] -0.7674511 0.04311755 

# 三之一節
# 測試從名詞片語複雜與否會不會影響從名詞片語中移出某個詞到主題位置的合語法性
demo.dat = read.delim("demo.txt")
head(demo.dat)

# 練習七
# 新增原始類別因子並將預設的虛擬編碼改為總和編碼
demo.dat$ComplexNPFac = ifelse(demo.dat$ComplexNP == 1, "Yes", "No")
demo.dat$TopicFac = ifelse(demo.dat$Topic == 1, "Yes", "No")
# 先將新的欄位轉換為因子
demo.dat$ComplexNPFac = as.factor(demo.dat$ComplexNPFac)
demo.dat$TopicFac = as.factor(demo.dat$TopicFac)
# 檢查預設的虛擬編碼對比
contrasts(demo.dat$ComplexNPFac)
contrasts(demo.dat$TopicFac)
# 轉換為總和編碼
contrasts(demo.dat$ComplexNPFac) = contr.sum(levels(demo.dat$ComplexNPFac))
contrasts(demo.dat$TopicFac) = contr.sum(levels(demo.dat$TopicFac))
# 檢查改變後的總和編碼對比
contrasts(demo.dat$ComplexNPFac)
contrasts(demo.dat$TopicFac)

# 為每位母語者進行邏輯迴歸
CNP.coef = numeric(7)	# 建立長度為7的數值向量儲存每位母語者ComplexNP係數
Top.coef = numeric(7)	# 儲存每位母語者Topic係數的數值向量
CxT.coef = numeric(7)	# 儲存每位母語者交互作用係數的數值向量
for (i in 1:7) { 		# 以迴圈為每位母語者進行邏輯迴歸
  # 依編號取得每位母語者的資料子集合
  demo.dat.i = subset(demo.dat, demo.dat$Speaker == i) 
  # 進行邏輯迴歸
  glm.i = glm(Judgment ~ ComplexNP * Topic, 
  family = binomial, data = demo.dat.i)
  # 取出所有係數儲存至數值向量
  coefs = coef(glm.i)
 	CNP.coef[i] = coefs["ComplexNP"]
 	Top.coef[i] = coefs["Topic"]
 	CxT.coef[i] = coefs["ComplexNP:Topic"]
}

# 為每一組係數進行虛無假設為「0」的t檢定
t.test(CNP.coef)$p.value # 名詞複雜度t檢定，只取p值
#[1] 0.02073655
t.test(Top.coef)$p.value # 主題移位t檢定
#[1] 8.145483e-06
t.test(CxT.coef)$p.value # 交互作用t檢定
#[1] 0.02073655

# 三之二之一
# 我們可以先直接設定參數「stringsAsFactors」為「TRUE」，
# 代表字串欄位直接轉換成因子並以使用預設虛擬編碼系統
classifiers = read.delim("classifiers.txt", stringsAsFactors = TRUE)

# 把有著三個層次的因變量根據是否為「1(條)」、「2(根)」或「3(支)」
# 拆解成三個以「0」與「1」呈現的二元因變量
# 當Class為1，欄位tiao也為1，否則為0，以此類推
classifiers$tiao = ifelse(classifiers$Class == 1, 1, 0)
classifiers$gen = ifelse(classifiers$Class == 2, 1, 0)
classifiers$zhi = ifelse(classifiers$Class == 3, 1, 0)

# 針對每個新的二元因變量進行個別的邏輯迴歸分析
# 預測量詞是否為「條」的邏輯迴歸模型，以此類推
tiao.glm = glm(tiao ~ Flexible + Thin + Round, family = "binomial", 
               data = classifiers)
gen.glm = glm(gen ~ Flexible + Thin + Round, family = "binomial", 
                data = classifiers)
zhi.glm = glm(zhi ~ Flexible + Thin + Round, family = "binomial", 
                data = classifiers)

# 無法直接以glm()對含有超過兩個層次的因變量進行邏輯迴歸分析
summary(glm(Class ~ Flexible + Thin + Round, family = "binomial", 
            data = classifiers))

# 使用nnet套件進行多項邏輯迴歸分析
library(nnet) 			# 記得先安裝載入套件
# 建立模型的語法都相同，這裡以tiao為二元因變量進行示範
tiao.multinom = multinom(tiao ~ Flexible + Thin + Round, data = classifiers)
summary(tiao.multinom)

# 練習八
# 截距p值
int.z = -1.7979206 / 0.7164164
2 * pnorm(int.z)
# 彈性p值
flex.z = 4.7943608 / 0.8575578
2 * pnorm(flex.z, lower.tail = F)  # z值為正數，所以先取得右尾p值再乘以2
# 細長p值
thin.z = -1.226217 / 0.8255739
2 * pnorm(thin.z)
# 圓p值
round.z = 0.2615881 / 0.732752
2 * pnorm(round.z, lower.tail = F) # z值為正數，所以先取得右尾p值再乘以2

# 進行三元因變量的多項邏輯迴歸分析
allClass.multinom = multinom(Class ~ Flexible + Thin + Round, 
                             data = classifiers)
summary(allClass.multinom)

# 手動從多項迴歸分析模型中取得係數與標準誤，並計算z值與p值
results = summary(allClass.multinom)		# 將檢定報告另存
coef.vals = results$coefficients		# 取得係數矩陣
se.vals = results$standard.errors		# 取得標準誤矩陣
z.vals = coef.vals/se.vals			# 將係數矩陣除以標準誤矩陣取得z
# 以z值矩陣產生p值。由於z值可能大於或小於0，這裡一律以abs()先轉換為絕對z值
# 再加上「-」轉換為負數，所以不管原始z值為何，一律都是先取得左尾p值再乘以2
p.vals = 2*pnorm(-abs(z.vals))			
z.vals
p.vals

# 三之二之二
# 類神經網路
library("neuralnet")
set.seed(1)			# 指定亂數種子
# 有需要的話重新載入資料，並將三元因變量拆解為三個二元因變量
classifiers = read.delim("classifiers.txt", stringsAsFactor = T)
classifiers$tiao = ifelse(classifiers$Class == 1, 1, 0)
classifiers$gen = ifelse(classifiers$Class == 2, 1, 0)
classifiers$zhi = ifelse(classifiers$Class == 3, 1, 0)
# 也將三個自變量轉換為適用於建立類神經網路的二元數值形式
classifiers$FlexibleNum = ifelse(classifiers$Flexible == "Yes", 1, 0)
classifiers$ThinNum = ifelse(classifiers$Thin == "Yes", 1, 0)
classifiers$RoundNum = ifelse(classifiers$Round == "Yes", 1, 0)

# 建立類神經網路，設定無隱藏層且訓練重覆次數為5
train.net = neuralnet(tiao + gen + zhi ~ FlexibleNum + ThinNum + RoundNum, 
                      data = classifiers, hidden = 0, rep = 5)
# 檢視訓練完成的類神經網路
plot(train.net, rep = "best")

# 三之三
# 手動匯入Rbrul程式碼並執行Rbrul
source("http://www.danielezrajohnson.com/Rbrul.R")
rbrul()

# 四之一
# 泊松分佈
set.seed(1) 					                      # 指定亂數種子
pois.sample = rpois(n = 100000, lambda = 3)	# 從lambda為3的泊松分佈抽樣
mean(pois.sample) 				                  # 抽樣平均數
#[1] 2.99847
var(pois.sample)  				                  # 抽樣變異數，和平均數非常接近了
#[1] 3.015338
x = 1:10					                         	# 建立一個1至10的次數向量
plot(x, dpois(x, lambda = 3))		           	# 依次數的分佈密度繪製散佈圖
ppois(3, lambda = 3) 			                	# 分佈中次數大於3的機率
#[1] 0.6472319
ppois(3, lambda = 3, lower.tail = F)		    # 分佈中次數小於3的機率

# 台灣閩南語「著」的重覆N次的次數泊松迴歸分析
dui = read.delim("DuiCounts.txt")
# 建立只包含重覆N次與N次是否為奇數做為自變量的樂松迴歸模型
dui.glm = glm(Count ~ NumSyl + Oddness, family = "poisson", data = dui)
summary(dui.glm)

# 計算當「著」只重覆1次、且重覆次數為奇數(以「1」表示)時的出現次數估計
# 別忘了使用exp()將對數以指數反轉為原始次數
exp(6.74099 + 1 * -0.59614 + 1 * 0.45254)

# 練習八
# 以資料框的NumSyl與Oddness從迴型中取得估計的對數次數
dui.pred = predict(dui.glm, dui[1:2])
# 將對數次數以指數反轉回原始次數
dui.pred = exp(dui.pred)
# 以plot使用實際資料做出散佈圖，並以pch設定數據點以空心方塊呈現
plot(Count ~ NumSyl, data = dui, xlab = "Number of Syllables", ylab = "Count", 
     pch = 0, cex = 2)
# 以實際資料在底圖上加上折線
lines(Count ~ NumSyl, data = dui, lwd = 1.5)
# 以估計次數加上數據點，並以pch設定數據點以實心圓呈現
points(dui.pred, pch = 16, cex = 2)
# 以估計次數加上折線，並以lty設定為虛線呈現
lines(dui.pred, lty = 2, lwd = 1.5)
# 在圖表右上角加入圖例，而圖例文字與圖樣都根據上面加上數據的順序呈現
legend(x = "topright", legend = c("Observed", "Model"), 
       lty = c(1, 2), pch = c(0, 16))

# 四之二
# 以詞頻、熟悉度、習得年齡等變量的例子示範次序變量迴歸
fd = read.delim("freqdur.txt")
fd$AoA.ord = floor(fd$AoA)                      # 無條件捨去轉換成整數
fd$Fam.ord = floor(fd$Fam)                      # 無條件捨去轉換成整數 
fd$AoA.ord = as.factor(fd$AoA.ord)	            # 將數值轉換為因子層次
fd$Fam.ord = as.factor(fd$Fam.ord)            	# 同上
levels(fd$AoA.ord)			# 可以看到轉換成功的結果，Fam亦同

fd$AoA.ord.h = fd$AoA.ord	# 另外複製Helmert編碼使用的因子 
fd$Fam.ord.h = fd$Fam.ord
# 依兩個因子的層子產生Helmert編碼並取代預設的虛擬編碼
contrasts(fd$AoA.ord.h) = contr.helmert(levels(fd$AoA.ord.h)) 
contrasts(fd$Fam.ord.h) = contr.helmert(levels(fd$Fam.ord.h))
contrasts(fd$AoA.ord.h)		# 以習得年齡的對比檢視Helmert編碼


# 因變量「Dur」還是連續變量，所以我們和前一章一樣建立一般線性迴歸模型
freq.ord.lm = lm(Dur ~ log(Freq) + AoA.ord.h + Fam.ord.h, data = fd)
summary(freq.ord.lm)

# 假設對數詞頻與熟悉度都為「0」的情況
252.87363 + -1 * 1.18439		# 習得年齡層次「1」的估計單詞時長
252.87363 + 1 * 1.18439		  # 習得年齡層次「2」的估計單詞時長
252.87363 + -1 * 0.75505		# 習得年齡層次「1-4」的估計單詞時長
252.87363 + 1 * 0.75505		  # 習得年齡層次「5」的估計單詞時長

# 先複製另兩個因子欄位進行多項式編碼
fd$AoA.ord.p = fd$AoA.ord # Prepare for polynomial coding 
fd$Fam.ord.p = fd$Fam.ord # Ditto 
# 第一種多項式編碼方式：使用factor()函數並將參數「ordered」設定為TRUE
fd$AoA.ord.p = factor(fd$AoA.ord, ordered = T) 
fd$Fam.ord.p = factor(fd$Fam.ord, ordered = T) 
# 第二種方式：直接使用ordered()函數
fd$AoA.ord.p = ordered(fd$AoA.ord) 
fd$Fam.ord.p = ordered(fd$Fam.ord) 
# 第三種方式：以contr.poly()產生多項式編碼並取代預設的虛擬編碼
contrasts(fd$AoA.ord.p) = contr.poly(levels(fd$AoA.ord.p)) 
contrasts(fd$Fam.ord.p) = contr.poly(levels(fd$Fam.ord.p))

contrasts(fd$AoA.ord.p)

# 多項式編碼建立同樣的線性迴歸模型
freq.ord.poly = lm(Dur ~ log(Freq) + AoA.ord.p + Fam.ord.p, data = fd)
summary(freq.ord.poly)

# 以MASS套件進行勝算比例邏輯迴歸，並以詞頻預測(次序)熟悉度
library(MASS)
polr.all = polr(Fam.ord ~ log(Freq), data = fd)
summary(polr.all) 

# 以似然比檢定比較有無詞頻的模型，測試詞頻是否顯著地預測熟悉度次序
polr.int = polr(Fam.ord ~ 1, data = fd)	# 把詞頻拿掉只剩截距
anova(polr.int, polr.all) 		        	# 這裡的p只是非常小，不是真的0！

# 練習九
summary(polr.int)                    # 檢視只含有截距模型的截距係數
xtabs(~ Fam.ord, data = fd)          # 計算Fam.ord中每個層次的數量
log(6 / sum(63, 212, 555, 756, 97))           # logit(1|2) = -5.636574
log(sum(6, 63) / sum(212, 555, 756, 97))      # logit(2|3) = -3.156075
log(sum(6, 63, 212) / sum(555, 756, 97))      # logit(3|4) = -1.611571
log(sum(6, 63, 212, 555) / sum(756, 97))      # logit(4|5) = -0.02013093
log(sum(6, 63, 215, 555, 756) / 97)           # logit(5|6) = 2.799918

# 四之三
# 以傑伯沃基語料庫示範大量罕見事件模擬
jabberwocky = read.table("Jabberwocky_OnlyWords.txt")
head(jabberwocky)                   # V1(Variable 1)是預設的欄位名稱

jabberwocky = jabberwocky$V1        # 從V1欄位取出值成為向量，覆蓋原本的物件

jabberwocky.tab = table(jabberwocky)                  # 計算向量中相同值的頻率
jabberwocky.tab

jabberwocky.tabtab = table(jabberwocky.tab)           # 產生詞頻光譜
jabberwocky.tabtab

library(zipfR)   					                  # 記得先安裝
token.freq = names(jabberwocky.tabtab) 	    # 取出表示詞彙頻率的欄位名稱
token.freq = as.numeric(token.freq)		      # 將欄位名稱轉換為數值
type.freq = as.numeric(jabberwocky.tabtab)	# 將表格去掉欄位名稱保留數值
# 建立spc物件
jabberwocky.spc = spc(m = token.freq, Vm = jabberwocky.tabtab)
jabberwocky.spc

# 以jabberwocky.spc頻率光譜進行ZM齊夫定律迴歸模型
jabberwocky.zm = lnre(type = "zm", jabberwocky.spc)
jabberwocky.zm

n.int = seq(1, 10000, length = 20)	# 產生1至10000間的20個N相等間距
# 以jabberwocky.zm模型和N相等間距產生詞彙增長曲線
jabberwocky.vgc = lnre.vgc(jabberwocky.zm, n.int)
plot(jabberwocky.vgc)		           	# 繪製詞彙增長曲線

# 四之四節
# 以某個心理語言學實驗中對於某組刺激項目的正確率和反應時間進行GAM示範
rtacc = read.delim("RTacc.txt")
plot(Acc ~ RT, data = rtacc)    # 產生ACC ~ RT散佈圖

# 以scatter.smooth()函數加上LOESS趨勢線
scatter.smooth(rtacc$RT, rtacc$Acc)

# 以平滑函數轉換RT，建立GAM模型
library(mgcv)					# 請先安裝載入套件
rtacc.gam = gam(Acc ~ s(RT), data = rtacc)

# 繪製呈現GAM模型的散佈圖
plot(Acc ~ RT, data = rtacc)		  # 產生原始資料散佈圖
RTrange = range(rtacc$RT)		      # 取得原始反應時間最小/最大值
# 建立反應時間最小值到反應時間最大值以1為間隔的連續數值向量
rtacc.newRT = seq(RTrange[1], RTrange[2], by = 1)
# 以反應時間連續向量取得GAM模型中的估計值
rtacc.pred = predict(rtacc.gam, data.frame(RT = rtacc.newRT))
lines(rtacc.pred ~ rtacc.newRT)		# 以估計值與反應時間座標加上趨勢線

# 一次取得含有標準誤與估計值的數據
rtacc.pred = predict(rtacc.gam, data.frame(RT = rtacc.newRT), se.fit = T)
# 將模型數據轉換為ggplot2喜愛的資料框格式
rtacc.pred.df = as.data.frame(rtacc.pred)
# 將用來產生估計值的反應時間數據結合至rtacc.pred.df
rtacc.pred.df$RT = rtacc.newRT
library(ggplot2)
# 在geom_point()以實際數據rtacc在底圖上加上數據點
ggplot() + geom_point(data = rtacc, mapping = aes(x = RT, y = Acc),
                color = "darkgrey", alpha = .7, size = 3) +
  # 在geom_line()以模型預測值rtacc.pred.df加上最佳配適線
  geom_line(data = rtacc.pred.df, mapping = aes(x = RT, y = fit)) +
  # 在geom_ribbon()以模型預測值以及標準誤計算95%信賴區間，加上陰影範圍
  geom_ribbon(data = rtacc.pred.df, 
                mapping = aes(x = RT, y = fit, ymin = fit - se.fit * 1.96,
                              ymax = fit + se.fit * 1.96), alpha = .3) +
	labs(title = "Generalized Additive Modeling", x = "RT", y = "Accuracy") +
	theme_bw()

