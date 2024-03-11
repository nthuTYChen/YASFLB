# 記得先設定工作目錄！

# 二之一節
# 以二項分佈計算某個句子二元接受度判斷中8個人接受但17個人不接受的單尾p值
pbinom(8, 25, 0.5)

# 進行隨機的二元選擇且每個選擇機率各半時，大約會累積8個「0」(拒絕)才會累積8個
# 「1」(接受)。這裡我們測試17個「0」的累積數量超過理論值的8個，所以會出現在分佈的
# 右側。因此，我們以「1」減去「pnbinom()」產生的機率，才會是右尾單尾機率，也就是
# 累積8個「0」前需要累積17或更多「1」的機率
1 - pnbinom(17, 8, 0.5)

# 練習一
# 建立0至17的數值序列，代表取得第8個「成功/接受」之前的「失敗/拒絕」次數
n = 0:17
# dnbinom()中的x參數就代表取得目標成功數之前的失敗次數、size參數代表目標
# 成功數、而prob則是在每一次試驗時成功的機率。
nbinom.d = dnbinom(x = n, size = 8, prob = 0.5)
# 繪製分佈密度散佈圖
plot(n, nbinom.d, main = "Negative Binomial Distribution (Target = 8)",
     xlab = "N", ylab = "Density")
# 再加上分佈曲線
lines(n, nbinom.d)
abline(h = 1 - pnbinom(17, 8, prob = 0.5), col = "red")

# 二之二之二節
# 以貝氏定理推斷火星人孩童不說話是否是因為叭哺症(Bloopy)
p.notalk.bloopy = 1		    # P(D不說話|θ有叭哺症)
p.notalk.nobloopy = .1		# P(D不說話|θ無叭哺症)
p.bloopy = .0001			    # P(θ有叭哺症)
p.nobloopy = 1 - p.bloopy	# P(θ無叭哺症)
p.bloopy.notalk = p.notalk.bloopy * p.bloopy /
  (p.notalk.bloopy * p.bloopy + p.notalk.nobloopy * p.nobloopy)
p.bloopy.notalk			      # P(θ有叭哺症|D不說話)

# 練習一
# 依<表二>建立2x2的列聯表矩陣(設定列數為2)
poldrack.mat = matrix(c(166, 703, 199, 2154), nrow = 2)
# 進行雙因子卡方檢定，結果存入另一物件
poldrack.chisq = chisq.test(poldrack.mat)
# 根據卡方檢定結果，實際分佈與隨機分佈不同(X2(70.536), p < .001)
poldrack.chisq
# 產生預測的隨機分佈：可以看到隨機分佈中預期在語言研究中活躍的次數比實際少很多
# 而且在非語言研究中不活躍的次數也比實際少很多。換句話說，在語言研究中，該腦部
# 區域活躍的次數顯著地比預期得多，而在非語言研究中，該腦部區域不活躍的次數也比
# 預期得多。因此，該腦部區域極度可能與語言相關
poldrack.chisq$expected

# 練習二
# 先依Poldrack的資料建立含有「Active」與「Linguistics」欄位的資料框
poldrack.df = data.frame(Active = c(rep(1, 166), rep(0, 703), rep(1, 199), rep (0, 2154)),
                         Linguistics = c(rep("Yes", 166 + 703), rep("No", 199 + 2154)))
# 將自變量欄位轉換為因子
poldrack.df$Linguistics = as.factor(poldrack.df$Linguistics)

# 進行邏輯迴歸分析，指定以二項分佈建立模型
poldrack.glm = glm(Active ~ Linguistics, family = "binomial", data = poldrack.df)

# 檢視模型：LinguisticsYes有顯著效應(B = 0.93841, SE = 0.11374, z = 8.251, p < .001)
# 該效應係數為正值，代表當研究類型為語言研究時，腦部區域活躍的機率就會顯著增加。
summary(poldrack.glm)

# 實際計算不同研究類型的腦部區域活躍預測勝率比(odds)
# Linguistics為No時的預測勝率比(odds)，也就是以指數反轉截距的對數機率係數：0.09238598
exp(-2.38178)
# Linguistics為Yes時的預測勝率比(odds)，套用邏輯迴歸方程：0.2361307
exp(-2.38178 + 1 * 0.93841)

# 練習三
p.language = .5                         # 事前P(θ語言)
p.nonlanguage = .5                      # 事前P(θ非語言)
p.active.language = 166/(166 + 703)     # P(D活躍|θ語言)
p.active.nonlanguage = 199/(199 + 2154) # P(D活躍|θ非語言)
p.language.active = p.active.language * p.language / 
  (p.active.language * p.language + p.active.nonlanguage * p.nonlanguage)
p.language.active                       # 事後P(θ語言|D活躍)

# 二之二之三節
# 用只有4個火星語名詞且名詞之間語意特性彼此獨立的資料示範以貝氏分類𠾖
# 歸類量詞條的使用
# 建立4個名詞是否使用「條」、是否為長形、是否為動物的二元變量
tiao = c(1, 1, 0, 0)
oblong = c(1, 1, 0, 0)
animal = c(1, 0, 1, 0)
classifiers = data.frame(tiao, oblong, animal) # 建立資料框
# 以4個名詞做為資料框每列的名稱
rownames(classifiers) = c("snake", "rope", "bird", "apple") 
classifiers

p.tiao = sum(tiao) / length(tiao)			# P(條)
p.nottiao = sum(1 - tiao) / length(tiao)		# P(¬條)
# P(長形狀|條)：只有oblong和tiao都是1時，才會oblong * tiao = 1
p.oblong.tiao = sum(oblong * tiao) / sum(tiao) 	
p.animal.tiao = sum(animal * tiao) / sum(tiao) 	# P(動物|條)
# P(長形狀|¬條)
p.oblong.nottiao = sum(oblong * (1 - tiao)) / sum(1 - tiao)	
# P(動物|¬條)
p.animal.nottiao = sum(animal * (1 - tiao)) / sum(1 - tiao)

# 公式十七
p.oblong.tiao * p.animal.tiao * p.tiao / 
  (p.oblong.tiao * p.animal.tiao * p.tiao + 
 	p.oblong.nottiao * p.animal.nottiao * p.nottiao)

# 假設只知道名詞是動物，但不知是否有長形狀的特性時的「條」使用預測機率
# 計算中排除所有與「長形狀」(oblong)相關的機率
p.animal.tiao * p.tiao / (p.animal.tiao * p.tiao + 
                            p.animal.nottiao * p.nottiao)

# 二之三節
# 猜測以Howdy打招呼時對方回覆Howdy是否代表對方會說英語的貝氏因子
# P(howdy|會英語) / P(howdy|不會英語)
(9/10) / (1/10)

# 二之三之一節
vd = read.delim("voweldurationsR.txt")
head(vd)

# 進行假設變異數相等的雙樣本非成對t檢定
t.test(Duration ~ Vowel, data = vd, var.equal = T)

# 假設變異數相等的雙樣本非成對t檢定就是簡單線性迴歸
vd.lm = lm(Duration ~ Vowel, data = vd)
summary(vd.lm)

vd.lm1 = lm(Duration ~ 1, data = vd)	# 只包含截距的虛無假設模型
BIC1 = BIC(vd.lm)				              # 取得兩個模型的BIC值
BIC0 = BIC(vd.lm1)
B01 = exp((BIC1 - BIC0) / 2)      		# 公式二十三
B01

# 以BayesFactor套件進行單一樣本t檢定的精確虛無假設貝氏因子估算
library(BayesFactor)	# 會出現"Type BFManual() to open the manual."的訊息
BFManual()			      # 遵照指示就可開啟網頁版的使用手冊

i.set = subset(vd, Vowel == "i")	# 取出母音子集合與子集合時長資料
u.set = subset(vd, Vowel == "u")
i.Dur = i.set$Duration
u.Dur = u.set$Duration
ttestBF(i.Dur, u.Dur)			        # 將時長資料放入ttestBF()進行貝氏因子計算

# 可以用「1」除以「ttestBF()」產生偏好虛無假設的檢定報告
1 / ttestBF(i.Dur, u.Dur)

# 二之三之二節
# 貝式版本的卡方檢定
# 先建立鼻音韻尾與母音無關聯性的資料
socdata = matrix(c(190, 57, 112, 27), ncol = 2)	# 設定兩個欄位的2x2列聯表
rownames(socdata) = c("n", "N") 			# N = 軟顎鼻音
colnames(socdata) = c("a", "i") 
socdata 

# 傳統卡方檢定
chisq.test(socdata)

# 以超幾何分佈根據傳統卡方檢定的虛無假設進行貝氏統計版本的卡方檢定
socdata.bf = contingencyTableBF(socdata, sampleType = "hypergeom")
# 產生虛無假設相對於對立假設的檢定報告
1 / socdata.bf

# 在虛無假設中應該只設定欄位(母音)的邊際總數是固定的，以做為機率中的條件，
# 而鼻音韻尾的分佈則是根據母音的邊際總數進行隨機抽樣
socdata.bf2 = contingencyTableBF(socdata, sampleType = "indepMulti", 
                                 fixedMargin = "cols") 
1 / socdata.bf2

# 讓虛無假設允許讓每一列/欄的邊際總數自由變動
socdata.bf3 = contingencyTableBF(socdata, sampleType = "jointMulti") 
1 / socdata.bf3

# 三之一節
# 產生貝塔分佈圖
# 設定圖表版面為5x5的空間，並設定mai移除每個迷你圖表的邊界留白
par(mfrow=c(5, 5), mai=c(0, 0, 0, 0))
# 設定不同的a/b數值：都是[0.5, 1, 2, 3, 4]的向量
a.val = c(0.5, 1:4)
b.val = c(0.5, 1:4)
# 以雙層迴圈根據不同的a/b數值產生對應的貝塔分佈 
for (b in b.val) { 
  for (a in a.val) { 
  # 將目前的a/b值放入dbeta()函數中，並使用curve()函數
  # 畫出x=0到x=1的曲線
  		curve(dbeta(x, a, b), 0, 1, 
  		# 移除X/Y軸標題、設定Y軸範圍
  		xaxt = "n", yaxt = "n", ylim = c(0, 3)) 
  		# 以paste()根據目前的a/b值組合每個分佈的文字標籤
  		curve.label = paste("a=", a, ",b=", b, sep="")
  		# 在圖上根據座標加上文字標籤
   		text(0.5, 2.5, labels = curve.label)
  } 
}
par(mfrow = c(1, 1), mai = c(1, 0.8, 0.8, 0.4) + 0.02)	# 恢復預設版面

# 為25人的迷你句法實驗進行貝式二項檢定，看看只有8個人接受的句子是否真的是不能
# 接受的句子
# 計算實際資料似然值P(8,25|theta)的自訂函數
likely.8.25 = function(theta) { theta ^ 8 * (1 - theta) ^ (25 - 8) }
# 設定繪圖版面為2x3，每張圖之間四個邊界各有0.6的留白
par(mfrow = c(2, 3), mai = rep(0.6, 4)) 
# 以curve()函數利用dbeta()繪製a/b=1且x從0到1範圍的事前機率貝塔分佈曲線
curve(dbeta(x, 1, 1), 0, 1, main = "Uniform prior") 
# 根據likely.8.25()函數產生x從0到1範圍的實際資料分佈曲線，和事前分佈進行比較
curve(likely.8.25(x), 0, 1, main = "Likelihood") 
# 產生根據事前假設得到的事後機率分佈曲線
curve(dbeta(x, 8+1, 25-8+1), 0, 1, main = "Posterior") 
# 為另一個事前假設產生相同的三個圖表
curve(dbeta(x, 100, 100), 0, 1,main = "Informative prior") 
curve(likely.8.25(x), 0, 1, main = "Likelihood") 
curve(dbeta(x, 8+100, 25-8+100), 0, 1, main = "Posterior") 

par(mfrow = c(1, 1), mai = c(1, 0.8, 0.8, 0.4) + 0.02)	# 恢復預設版面

# 練習四
# 重覆同一個句法實驗，但受試者與「是」的回應數量都成長了十倍
# 以資訊明確的事前假設進行貝式二項檢定
# 為25人的迷你句法實驗進行貝式二項檢定，看看只有8個人接受的句子是否真的是不能
# 接受的句子
# 計算實際資料似然值P(80,250|theta)的自訂函數
likely.80.250 = function(theta) { theta ^ 80 * (1 - theta) ^ (250 - 80) }
# 設定繪圖版面為1x3，每張圖之間四個邊界各有0.6的留白
par(mfrow = c(1, 3), mai = rep(0.6, 4)) 
# N = 250, z = 80
curve(dbeta(x, 100, 100), 0, 1,main = "Informative prior") 
curve(likely.8.25(x), 0, 1, main = "Likelihood") 
curve(dbeta(x, 80+100, 250-80+100), 0, 1, main = "Posterior") 

par(mfrow = c(1, 1), mai = c(1, 0.8, 0.8, 0.4) + 0.02)	# 恢復預設版面

# 練習五
# 以原始實驗資料但雙峰事前機率分佈產生事後機率分佈
likely.8.25 = function(theta) { theta ^ 8 * (1 - theta) ^ (25 - 8) }
# 設定繪圖版面為1x3，每張圖之間四個邊界各有0.6的留白
par(mfrow = c(1, 3), mai = rep(0.6, 4)) 
# N = 25, z = 8, a = 0.5, b = 0.5
curve(dbeta(x, 0.5, 0.5), 0, 1,main = "Informative prior") 
curve(likely.8.25(x), 0, 1, main = "Likelihood") 
curve(dbeta(x, 8+0.5, 25-8+0.5), 0, 1, main = "Posterior") 

par(mfrow = c(1, 1), mai = c(1, 0.8, 0.8, 0.4) + 0.02)	# 恢復預設版面

# 以均勻事前機率分佈a/b=1以及原始實驗資料N=25,z=8產生的事後機率分佈計算θ平均值
set.seed(1)				# 指定亂數種子
# 以rbeta()函數根據a/b/N/z參數從對應的貝塔分佈取樣10000個數值再取平均數
mean(rbeta(10000, 8+1, 25-8+1))

# 按照Kruschke (2015, p.133)的白努力試驗的事後機率公式進行相同的計算
(8 + 1) / (25 + 1 + 1)

# 示範以兩個階段分開蒐集句法實驗資料，但最終結果一樣，並以此為前提進行貝式分析
data1 = c(rep(1, 6), rep(0, 10), 1)	# 第一階段的6個1以及10個0
length(data1)				                # N = 17
sum(data1)					                # z = 7 
data2 = c(rep(0, 7), 1)			        # 第二階段的1個1和7個0 
length(data2)				                # N = 8
sum(data2)					                # z = 1 
length(c(data1, data2))			        # 全部資料N = 25
sum(c(data1, data2))			          # z = 8

# 計算第一/第二階段資料似然值的函數
likely.7.17 = function(theta) { theta ^ 7 * (1 - theta) ^ (17 - 7) } 
likely.1.8 = function(theta) { theta ^ 1 * (1 - theta) ^ (8 - 1) } 
# 設定2x4的版面，將第一/第二階段的分析分別以兩列呈現
par(mfrow = c(2, 4), mai = rep(0.6, 4)) 
# 產生第一階段的資料分析
curve(dbeta(x, 1, 1), 0, 1, main = "data1: Uniform prior") 
curve(likely.7.17(x), 0, 1, main = "data1: Likelihood") 
curve(dbeta(x, 7+1, 17-7+1), 0, 1, main = "data1: Posterior") 
frame() # 跳過第1列第4欄的空間
# 產生第二階段的資料分析
# 以第一階段的資料產生第二階段分析的事前機率分佈
curve(dbeta(x, 7+1, 17-7+1), 0, 1, main="data2: Prior = data 1 posterior") 
curve(likely.1.8(x), 0, 1, main="data2: Likelihood") 
curve(dbeta(x, 1+(7+1), 8-1+(17-7+1)), 0, 1, lwd = 2, 
      main = "data2: Posterior") 
# 所有資料一起進行分析的事後機率分佈
curve(dbeta(x, 8+1, 25-8+1), 0, 1, lwd = 2, 
      main = "All: Posterior (= Figure 3, top right)") 
# 1+(7+1) = 8+1 = 9,  8-1+(17-7+1) = 25-8+1 = 18
par(mfrow = c(1, 1), mai = c(1, 0.8, 0.8, 0.4) + 0.02)	# 恢復預設版面

# 三之二節
# 建立尋找95%HDI的自訂函數betaHDI.try，並以實驗資料設定各個函數中使用參數的預設值
betaHDI.try = function(N = 25, z = 8, a = 1, b = 1, confidence = 0.95) { 
 	tail.L = 0		    # 事後分佈左尾的HDI初始值
	old.width = 1 	  # HDI範圍以最大值1為初始值，但目標是找到最小值 
 	not.done = T 		  # 告訴while()迴圈還沒找到HDI的變數，初始值為TRUE
	while(not.done) {	# 在not.done為TRUE的時候繼續進行迴圈
 		tail.L = tail.L + 0.000001		# 逐漸增加左端的HDI界線值
		# 以pbeta()計算根據目前左端HDI界線值得到的事後分佈左尾區域大小
		p.tail.L = pbeta(tail.L, z + a, N - z + b) 
		# 根據左尾區域大小以及設定的區間範圍計算右尾範圍，而左尾加右尾=5%
		p.tail.R = (1 - confidence) - p.tail.L 
		# 根據右尾區域大小決定目前右端HDI界線
 		tail.R = qbeta(p.tail.R, z + a, N - z + b, lower.tail = F)
		new.width = tail.R - tail.L 	  # 計算兩個界線之間的距離
		# 如果舊的HDI範圍比新的範圍更小，代表沒有改善
 		if (old.width < new.width) { 	
 			not.done = F 			            # 沒有改善就該結束循環計算了
		# 但如果舊的HDI範圍比新的範圍更大，代表有改善空間
 		} else { 	
 			old.width = new.width 	      # 以新範圍取代舊範圍繼續循環
		}					                      # if-else的段落結尾
	}						                      # while迴圈段落結尾
 	return(c(tail.L, tail.R))			    # 結束時回傳HDI範圍
} 							                    # betaHDI.try自訂函數結尾

# 以預設參數值執行自訂函數
betaHDI.try()

# 以Kruschke的簡化版本beyaHDI.R進行類似的95%HDI計算
source("betaHDI.R")	# 記得先下載檔案到你的工作目錄再讀取
betaHDI()			      # 執行betaHDI.R裡的自訂函數

# 以Kruschke的簡化版本beyaHDI.R進行類似的95%HDI計算，但使用資訊明確範圍更狹窄的
# 事前機率分佈
betaHDI(a = 100, b = 100) 

# 三之三節
# 以基於Lynch (2007)例子的資料進行貝式階層模型的示範
tdat = read.delim("tests.txt") 
# 重新以學生編號排序資料列以便清楚地呈現分組資訊
tdat = tdat[order(tdat$Student),]	
head(tdat) 

mean(tdat$Score)	        # mean = 79.175 
sd(tdat$Score)	          # sd = 13.26048
sd(tdat$Score) / sqrt(40) # SE = 2.096666

# 利用tapply()函數以mean()函數根據Student欄位分組進行Score欄位的計算
subjmeans = tapply(tdat$Score, tdat$Student, mean)
round(subjmeans, 0)	# 將平均數四捨五數進位至小數點第0位(整數)
var(subjmeans) 		  # 跨受試者平均測驗分數的變異數=140.7441

tdat.lm = lm(Score ~ 1, data = tdat) # 建立只包含截距的線性迴歸模型
summary(tdat.lm) 

library(lme4) 
# 這次同樣建立只包含截距但因變量「Score」以「Student」適當分組的混合效應模型
tdat.lme = lmer(Score ~ 1 + (1|Student), data = tdat) 
summary(tdat.lme) 

# 將混合效應模型中預測的每位學生的平均測驗分數與整體平均分數進行比較
# 從混合效應模型中取得每個數據點的估計值
tdat.lme.pred = predict(tdat.lme)
# 每個學生在兩個不同班級中都有測驗分數，但是在混合效應模型中並未依照班級分組
# 所以每個學生測驗分數估計值在不同班級都一樣。因此，我們只要從預測值中取得
# 第1,3,5,7,9...估計值，就是目前混合效應模型中對每個學生測驗分數的估計值了
pred.seq = seq(1, length(tdat.lme.pred), by = 2)
subjmeans.lme = (tdat.lme.pred[pred.seq])
# 四捨五入至整數
round(subjmeans.lme)
#subjmeans.lme = tdat.lme.pred.student 
#round(subjmeans.lme, 0)

# 產生實際平均數與估計值的相關性散佈圖
plot(subjmeans, subjmeans.lme, xlim = c(50, 100), ylim=c(50, 100)) 
# 加上呈現理想「Y = X」的趨勢線
segments(x0 = 50, y0 = 50, x1 = 100, y1 = 100) 
# 加上實際平均數與估計值的線性趨勢線(actual)，並以虛線呈現
abline(lm(subjmeans.lme ~ subjmeans), lty = 2)
# 加上圖例
legend("topleft", legend = c("Y = X", "Actual fit"), lty = c(1, 2))

# 以Lynch (2007)的程式碼進行貝式統計方法進行隨機變異模擬(請先從GitHub下載Lynch_Random.R)
source("Lynch_Random.R")

alpha		     	# 整體平均數估計值 77.79978 
tau2					# 學生分數隨機變異 140.2081 
sigma2				# 殘餘值隨機變異 74.23824
sqrt((tau2*s2)/(tau2 + n*s2))	# 整體平均數標準差 2.647624

subjmeans.bayes = alpha_i # 從alpha_i取得貝式階層模型對學生平均數的估計值
names(subjmeans.bayes) = 1:20 # 為向量的每個值加上學生編號，方便和混合效應模型預測值進行比較
round(subjmeans.bayes, 0) # 四捨五入為整數的估計值

# 改以貝式階層模型的估計值進行比較，其餘不變
plot(subjmeans, subjmeans.bayes, xlim = c(50,100), ylim = c(50, 100)) 
segments(x0 = 50, y0 = 50, x1 = 100, y1 = 100) 
abline(lm(subjmeans.bayes ~ subjmeans), lty = 2)
legend("topleft", legend = c("Y = X", "Actual fit"), lty = c(1, 2))

# 安裝Stan: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(brms)

# 建立事前假設為平均值70且標準差為10的常態分佈
pr = prior(gamma(80, 15), class = "Intercept", lb = 0, ub = 100)
# 建立只含截距之貝式混合效應模型
tdat.brm = brm(Score ~ 1 + (1 | Student), data = tdat, prior = pr)
# 以fixef()函數取得模型的固定效應(fixed effects)。模型中的固定效應只有
# 截距，所以只有一列資料。而資料的第一欄則為模型預測的截距數字。
brm.int = fixef(tdat.brm)[1]
# 以ranef()函數取得模型的隨機效應(random effects)並轉換為資料框物件
brm.ranef = as.data.frame(ranef(tdat.brm))
# 將預測的截距加上隨機效應的第一個欄位，得到模型預測的每位學生的分數
subjmeans.brm = brm.ranef[1] + brm.int

# 產生實際平均數與估計值的相關性散佈圖
plot(subjmeans, subjmeans.brm$Student.Estimate.Intercept, 
     xlim = c(50, 100), ylim=c(50, 100), ylab = "subjmeans.brm") 
# 加上呈現理想「Y = X」的趨勢線
segments(x0 = 50, y0 = 50, x1 = 100, y1 = 100) 
# 加上實際平均數與估計值的線性趨勢線(actual)，並以虛線呈現
abline(lm(subjmeans.brm$Student.Estimate.Intercept ~ subjmeans), lty = 2)
# 加上圖例
legend("topleft", legend = c("Y = X", "Actual fit"), lty = c(1, 2))