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
