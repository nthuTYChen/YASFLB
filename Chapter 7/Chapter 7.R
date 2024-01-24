# 記得先設定工作目錄！

# 二之一節
bg = read.table("BoysGirls.txt", header = T)		# 讀取資料
# 根據研究編號與性別將受試者歸類，得到每個類別的樣本大小
xtabs(~ Study + Gender, data = bg)	
#       Gender
# Study Boy Girl
#     1  10   10
#     2  10   10

study1 = subset(bg, Study == 1)			        # 將研究1的資料子集合取出
boys1 = subset(study1, Gender == "Boy") 		# 男孩資料取出成為boys1
girls1 = subset(study1, Gender == "Girl")		# 女孩資料取出成為girls1

# 將兩個子集合中的「Measure」數值取出，並進行假設變異數相同之
# 非成對樣本t檢定
t.test(boys1$Measure, girls1$Measure, var.equal = T)	

# 練習一
study2 = subset(bg, Study == 2)			        # 將研究2的資料子集合取出
boys2 = subset(study2, Gender == "Boy") 		# 男孩資料取出成為boys2
girls2 = subset(study2, Gender == "Girl")		# 女孩資料取出成為girls2

# 將兩個子集合中的「Measure」數值取出，並進行假設變異數相同之
# 非成對樣本t檢定，男孩與女孩的表現差異一樣呈現統計上的顯著性
t.test(boys2$Measure, girls2$Measure, var.equal = T)	

# 二之二節
# 讀取以組內設計取得的動詞與名詞反應時間資料
nv = read.table("NounsVerbs.txt", header = T) 
# 將研究3的子集合取出並儲存為study3
study3 = subset(nv, Study == 3) 	
# 將study3的名詞與動詞子集合另外取出儲存
nouns3 = subset(study3, WordType == "Noun") 
verbs3 = subset(study3, WordType == "Verb")
# 將研究4的資料做相同處理
study4 = subset(nv, Study == 4) 		
nouns4 = subset(study4, WordType == "Noun") 
verbs4 = subset(study4, WordType == "Verb") 
# 比較研究3每一個受試者對於動詞及名詞成對的反應時間差異
t.test(nouns3$Measure, verbs3$Measure, paired = T) 	
# 比較研究4每一個受試者對於動詞及名詞成對的反應時間差異	 
t.test(nouns4$Measure, verbs4$Measure, paired = T)

# 練習二
# 讀取資料
nv.messed = read.table("NounsVerbs_Messed.txt", header = T)
# 重新排序：在order()中，以Study為主要排序欄位、WordType為第一次要排序欄位、
# Participant為第二次要排序欄位。order()的參數decreasing預設為FALSE，因此無論是
# 數值或是字串，都是預設以順序先後遞增的方式排序
nv.cleaned = nv.messed[order(nv.messed$Study, nv.messed$WordType, 
                             nv.messed$Participant), ]
# 檢視整理過後的資料框，內容將等於原始NounsVerbs.txt的內容
head(nv.cleaned)

# 產生一些非真實的數據點，其中每對的差異為10，
# 但每組樣本的變異數為100(即SD = 10)，然後分別做不成對及成對t檢定
set.seed(567)	           		# 指定亂數種子
fakeA = rnorm(5, sd = 100)	# 從平均數為0且標準差為10常態分佈中取樣5筆數值
fakeB = fakeA + 10 		      # 將所有fakeA的數值加10，產生另一個數值向量 
cor(fakeA, fakeB)			      # 兩個向量之間差10，但有著超完美的關聯性
# 用不成對t檢定，樣本間平均值差異不顯著(p = .858)
t.test(fakeA, fakeB, var.equal = T)	

# 改用成對t檢定，但因為成對的差異都是10，所以在成對標準差為0的情況下
# R會出現錯誤訊息(稍後解釋囉！)
t.test(fakeA, fakeB, paired = T)
# Error in t.test.default(fakeA, fakeB, paired = T) : 
# data are essentially constant
set.seed(765)			                # 為了加入隨機的變異，再次指定亂數種子
fakeB = jitter(fakeB) 	  	      # 利用jitter()函數在fakeB的資料裡加點「雜訊」 
cor(fakeA, fakeB) 	     		      # 不完美，但還是高度相關 
t.test(fakeA, fakeB, paired = T) 	# 用成對t檢定，顯著的p值！ 

# 練習三
# 以迴圈進行最方便！
# 建立儲存產生顯著差異次數的物件
sig.count = 0
# 在1:20的序列裡進行重覆的步驟，也就是進行20次迴圈的意思
for(i in 1:20) {
  # 進行隨機抽樣
  fakeA = rnorm(5, sd = 100)
  fakeB = rnorm(5, sd = 100)
  # 進行非成對t檢定，假設變異數相同
  fake.t = t.test(fakeA, fakeB, var.equal = T)
  # 從統計檢定結果中取得雙尾p值
  fake.p = fake.t$p.value
  # 如果差異達到顯著水準
  if(fake.p < .05) {
    # 累積次數+1
    sig.count = sig.count + 1
  }
}
# 迴圈結束後印出顯著差異次數
print(sig.count)
# 雖然每次執行結果不一定相同，但顯著差異次數大約都會在1至2次之間
# 為什麼呢？因為在顯著水準設定在p < .05的情況下，代表顯著差異大約
# 有5%的機率是「隨機抽樣」產生的(Type I Error)。

# 二之三之一節
vowels = read.delim("voweldurationsR.txt") 	# 將資料讀取到R	
i = subset(vowels, Vowel == "i")	 	# 將兩樣本分別讀取 
u = subset(vowels, Vowel == "u")	 	# 注意vowels和Vowel的差別

# 算出音長平均數差異另存
dur.diff = mean(i$Duration) - mean(u$Duration)	
sd.i = sd(i$Duration)				# 算出/i/的音長標準差另存
sd.u = sd(u$Duration)				# 算出/u/的音長標準差另存
n.i = nrow(i)  		    			# 算出/i/的樣本數另存
n.u = nrow(u) 				    	# 算出/u/的樣本數另存
SE = sqrt(sd.i ^ 2 / n.i + sd.u ^ 2 / n.u)	# 套用標準誤公式
dur.t = dur.diff / SE 			# 套用非成對t值公式(二)
df = n.i + n.u - 2 				  # 套用非成對t檢定自由度計算(四)
# 因為t為正值，需以1減去pt()得到的數字，才能正確地得到右尾的p值。
p = 2 * (1 - pt(dur.t, df)) # 得出雙尾p值0.1907439

# 若兩個樣本數不同時，採用整合變異數進行非成對t檢定
bg = read.table("BoysGirls.txt", header = T)	# 假設我們全都重頭開始囉！
study1 = subset(bg, Study == 1) 
boys1 = subset(study1, Gender == "Boy")
girls1 = subset(study1, Gender == "Girl")
# 一步一步計算整合變異數 
SS.boy = sum((boys1$Measure - mean(boys1$Measure))^2)		# 公式(五)
SS.girl = sum((girls1$Measure - mean(girls1$Measure))^2) 
df.boy = nrow(boys1) - 1 					                    	# 公式(四)
df.girl = nrow(girls1) - 1 
var.pooled = (SS.boy + SS.girl) / (df.boy + df.girl)		# 公式(七)

M.boy = mean(boys1$Measure) 
M.girl = mean(girls1$Measure) 
n.boy = nrow(boys1) 
n.girl = nrow(girls1) 
SE = sqrt(var.pooled / n.boy + var.pooled / n.girl) 	# <公式八>標準誤
tval = (M.boy - M.girl) / SE  
dfval = n.boy + n.girl - 2 
pval = 2 * (1 - pt(tval, dfval))
# 跟t.test()產生的結果比較看看吧！
t.test(boys1$Measure, girls1$Measure, var.equal = T)

# 二之三之二
# 練習四
# 產生連續數值向量做為F值
fs = seq(from = 0, to = 3, by = 0.1)
# 取得F值分佈密度，df1 = 10，df2 = 5
fs.den = df(fs, df1 = 10, df2 = 5)
# 產生分佈密度圖
plot(x = fs, y = fs.den, xlab = "F values", ylab = "Density", type = "l",
     ylim = c(0, 1), main = "F distribution (df1 = 10, df2 = 5)")

# 再次使用bg資料以研究1的樣本進行變異數F檢定
bg = read.table("BoysGirls.txt", header = T)		# 讀取資料
study1 = subset(bg, Study == 1)			            # 將研究1的資料子集合取出
boys1 = subset(study1, Gender == "Boy") 		    # 男孩資料取出成為boys1
girls1 = subset(study1, Gender == "Girl")		    # 女孩資料取出成為girls1
var.test(boys1$Measure, girls1$Measure)  

#F test to compare two variances

#data:  boys1$Measure and girls1$Measure
#F = 1.9368, num df = 9, denom df = 9, p-value = 0.339
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.4810842 7.7977146
#sample estimates:
#  ratio of variances 
# 1.936842

# 因為F值比1大，我們將「alternative」參數設定為「greater」，計算F檢定的右尾p值
var.test(boys1$Measure, girls1$Measure, alternative = "greater")  

# 練習四
study2 = subset(bg, Study == 2)			            # 將研究2的資料子集合取出
boys2 = subset(study2, Gender == "Boy") 		    # 男孩資料取出成為boys2
girls2 = subset(study2, Gender == "Girl")		    # 女孩資料取出成為girls2
var.test(boys2$Measure, girls2$Measure)         # 雙尾p值=.002052
# F值比1小，因此計算「左尾」的單尾p值=.001026 x 2 = .002052
var.test(boys2$Measure, girls2$Measure, alternative = "less")

# 手動驗證，先計算F值
f.study2 = var(boys2$Measure) / var(girls2$Measure) # 0.0996
# F值比1小，因此pf()函數給予小於F值區域的機率就等於左尾p值
p.study2 = pf(f.study2, df1 = nrow(boys2) - 1, df2 = nrow(girls2) - 1) # .001026
p.study2 * 2 # .002052, 驗證成功

# 二之三之三
# 針對BG資料的研究2，對男孩女孩語言表現差異進行Welch t-test(不設定equal.var參數)
t.test(boys2$Measure, girls2$Measure)

# 二之三之四
# 以手機計算方式驗證成對t檢定的統計數字
# 你又重開R了對不對？好啦，我們再重頭開始。先逐步獲得子集合進行成對t檢定
nv = read.table("NounsVerbs.txt", header = T) 
study3 = subset(nv, Study == 3) 
nouns3 = subset(study3, WordType == "Noun") 
verbs3 = subset(study3, WordType == "Verb")
nv3.t = t.test(nouns3$Measure, verbs3$Measure, paired = T)
# 從成對t檢定結果中取得統計數據(t值)。原始資料型態為表格(table)
# 利用as.vector()轉換純數字向量
tval1 = as.vector(nv3.t$statistic)
# 用剛剛的公式們計算成對t檢定的t值
# 兩個成對的向量互減，得到每對成對數值的差異
Difs = nouns3$Measure - verbs3$Measure	
MD = mean(Difs) 			              		# 成對差異的平均數
SD = sd(Difs) 				                	# 標準差
SE = SD/sqrt(nrow(nouns3))	         		# 標準誤
tval2 = MD/SE 					                # 自己算出來的t值
tval1 == tval2 					                # 和t.test()算出來的一樣！

# 二之三之五
# 先假設你已經讀取了「BoysGirls.txt」，並以subset()函數取得了子集合
t.test(boys1$Measure, girls1$Measure) 				# 進行非成對Welch t檢定 
t.test(Measure ~ Gender, data = study1) 	    # 以Y ~ X語法進行相同檢定 
# 進行成對t檢定：再次提醒，每個受試者的名詞和動詞排序要一樣，才能真的「成對」
t.test(nouns3$Measure, verbs3$Measure, paired = T) 
# 以Y ~ X語法進行相同檢定
t.test(Measure ~ WordType, data = study3, paired = T) 

# 以Y ~ x語法使用boxplot()
boxplot(Measure ~ Gender, data = study1) 		# 看起來應該有顯著差異喔！

# 以Y ~ X語法驗證相關性測驗、非成對t檢定、與迴歸模型中相同的統計數據
# 先以條件判斷Gender欄位是否儲存「Girl」的字串，並得到儲存TRUE與FALSE的向量
girlsOrNot = (study1$Gender == "Girl")
# 將1乘上TRUE跟FALSE的向量，神奇的得到了虛擬編碼的向量！
study1$Girlness = 1 * girlsOrNot
head(study1)					                    # 確認看看吧！
cor.test(study1$Girlness, study1$Measure)	# 請注意df、t和p值 
# 有沒有看到一樣的df、t和p值？
summary(lm(Measure ~ Girlness, data = study1))	
# 直接以Gender做為自變量產生相同結果，但lm()比較聰明會自動轉換字串欄位為虛擬編碼 
summary(lm(Measure ~ Gender, data = study1))
# 進行非成對t檢定並假設變異數相同，得到相同結果
t.test(Measure ~ Girlness, data = study1, var.equal = T)	
# 非成對Welch t檢定的結果就不太一樣了
t.test(Measure ~ Girlness, data = study1) 	 	

# 三之一節
# 計算Cohen's d(t檢定的效應值)
# 要先安裝載入套件
install.packages("lsr", dependencies = T, ask = F)
library(lsr) 				 
# 假設你在R裡面還有剛剛整理過的BoysGirls.txt以及NounsVerbs.txt資料
cohensD(boys1$Measure) 		             		# 單一樣本t檢定效應值(假設µ0 = 0) 
mean(boys1$Measure)/sd(boys1$Measure)		  # 用<公式十五>算一次，結果一樣 
cohensD(boys1$Measure, girls1$Measure)  	# 非成對(相同變異數) t檢定效應值 
cohensD(Measure ~ Gender, data = study1)	# 用Y ~ X語法，結果和上面一樣 
# Welch's t檢定效應值 
cohensD(boys2$Measure, girls2$Measure, method = "unequal") 
# 成對t檢定效應值	 
cohensD(nouns3$Measure, verbs3$Measure, method = "paired") 

# 練習五
# 非成對(相同變異數) t檢定效應值
# 必須先計算整合變異數(公式七)
x.ss = sum((boys1$Measure - mean(boys1$Measure))^2)
y.ss = sum((girls1$Measure - mean(girls1$Measure))^2)
sp = sqrt((x.ss + y.ss) / (nrow(boys1) - 1 + nrow(girls1) - 1))
# 要以abs()函數取兩個樣本差異的絕對值，因為Cohen's d不可能為負數
abs(mean(boys1$Measure) - mean(girls1$Measure)) / sp

# 成對t檢定效應值
diffs = nouns3$Measure - verbs3$Measure
abs(mean(diffs)) / sd(diffs)

# 三之二節
# 假設你在R裡面還有剛剛整理過的BoysGirls.txt資料
# 成對假設變異數相等t檢定
t.test(Measure ~ Gender, study1)

# 練習六
# 因做為解答指定亂數種子，所以結果不一定和你自行抽樣一模一樣
set.seed(6)
# 一、從平均數為「0」以及標準差為「1」的常態分佈抽樣50個數值。
rand.sample = rnorm(n = 20, mean = 22, sd = 3)
# 二、將樣本和母體平均數虛無假設為「0」進行單一樣本t檢定。
t.test(x = rand.sample, mu = 20)
# 三、手動計算驗證單一樣本t檢定中的雙尾t值與p值。
rand.mean = mean(rand.sample)
rand.sd = sd(rand.sample)
n = length(rand.sample)
rand.t = (rand.mean - 20) / (rand.sd/sqrt(n))  # t值與檢定結果相同
# t值大於0，所以以1減去pt()回傳之數值以得到右尾p值
rand.p = 1 - pt(q = rand.t, df = n - 1)       
rand.p * 2  # 與t檢定雙尾p值相同
# 四、手動計算驗證單一樣本t檢定中的95%信賴區間
# 取得代表右尾2.5%區域界限的95%信賴區間上限
rand.int.upper = rand.mean + qt(p = 0.975, df = n - 1) * (rand.sd/sqrt(n))
# 取得代表左尾2.5%區域界限的95%信賴區間下限(因為qt()產生的t值為負數，所以
# 加總後的數值比樣本平均數低)
rand.int.lower = rand.mean + qt(p = 0.025, df = n - 1) * (rand.sd/sqrt(n))
# 五、推論95%信賴區間代表是否支持推翻虛無假設
# 我們的95%信賴區間21.094至24.068，並不包括虛無假設的「20」。這代表真實的母體
# 平均數(也就是妹妹所有的/t/的VOT數據平均)有可能不是「20」，而我們可以在顯著水準
# 為.05的情況下宣稱推翻虛無假設。
# 太好了，我們的妹妹不是火星人！那你的妹妹是嗎？:)

# 三之三
# 建立繪製誤差線的自訂函數
error.bar = function(x, y, upper, lower = upper, length = 0.1,...) { 
  if(length(x) != length(y) | length(y) !=length(lower) | 
     length(lower) != length(upper)) 
    stop("vectors must be same length") 
  	arrows(x, y + upper, x, y - lower, 
     		angle = 90, code = 3, length = length, ...) 
} 
# 我們用妹妹的VOT例子來試試看 
# 先以樣本平均數繪製長條圖並將長條圖暫存於sister.plot物件中
sister.plot = barplot(22, ylim = c(0,30), 
                      ylab = "Mean VOT (ms)", main = "Sister vs. Martians") 
# 根據<公式二十>計算在95%信賴區間的條件下，從平均值往上加以及往下減的範圍
conf95 = qt(0.05/2, df = 15) * (3/sqrt(16))
# 使用自訂函數產生含有信賴區間誤差線的長條圖
error.bar(sister.plot, 22, conf95) 
# 在圖上以虛線標出火星人的平均數(假設母體平均值)，落在妹妹VOT的95%信賴區間之外
abline(h = 20, lty = 2)
                                  
# 改以ggplot2繪製有信賴區間的長條圖
# ggplot2是以資料框格式為基礎，將不同的欄位對應到不同的圖表元素
# 所以讓我們建立一個只有一列但有兩個欄位的火星人妹妹VOT的資料框
sister.mean = data.frame(Speaker = "My Sister", VOT = 22)
library(ggplot2) # 記得先載入套件 
# 在底圖中將x軸對應到sister.mean的Speaker欄位，再將y軸對應到VOT欄位
ggplot(data = sister.mean, mapping = aes(x = Speaker, y = VOT)) + 
  # 使用geom_bar()加上長條圖呈現資料，線條採用黑色，而填滿採用白色
  # "stat = "identity"的意思是按照原始數值呈現長條高度，而非另外計算其他的統計數據
  geom_bar(fill = "white", color = "black", stat = "identity") + 	
  # 在geom_errorbar()裡將樣本平均數加上信賴區間上下限的數值對應到ymin與ymax元素
  # 並設定誤差線的寬度為0.2
  geom_errorbar(mapping = aes(ymin = VOT-conf95, 
                              ymax = VOT+conf95), width = 0.2) +
  theme_bw()

# 以男孩女孩例子示範如何從t檢定結果提取95%信賴區間數據，並產生長條圖
# 有需要就再讀取一次檔案吧！
bg = read.delim("BoysGirls.txt")
study1 = subset(bg, Study == 1)
# 進行非成對假設變異數相同t檢定，檢定結果存入t.res1
t.res1 = t.test(Measure ~ Gender, data = study1, var.equal = T) 
# 取得95%信賴區間的上下限值
conf95.upper = t.res1$conf.int[2]
conf95.lower = t.res1$conf.int[1]
# 計算95%信賴區間從平均數往上與往下變化的數值：想出來這個算式的邏輯了嗎？
conf95.2 = (conf95.upper - conf95.lower) / 2
boys1 = subset(study1, Gender == "Boy") 
girls1 = subset(study1, Gender == "Girl")
# 產生呈現兩組樣本平均數差異的長條圖，設定x軸與y軸標籤，再設定y軸範圍
# 產生呈現兩組樣本平均數差異絕對值的長條圖，設定x軸與y軸標籤，再設定y軸範圍
bg.plot = barplot(abs(mean(boys1$Measure) - mean(girls1$Measure)),
                  names.arg=c("Boys vs. Girls"), ylab = "Difference", 
                  ylim = c(0, 3.5)) 	
# 加上誤差線 
error.bar(bg.plot, abs(mean(boys1$Measure) - mean(girls1$Measure)), 
          conf95.2) 

# 改用ggplot2產生類似的圖表
# 先組合含有xLab、Diff、以及CI三個欄位的資料框，並放入對應的數值
bg.df = data.frame(xLab = "Boys vs. Girls", 
                   Diff = abs(mean(boys1$Measure) - mean(girls1$Measure)),
                              CI = conf95.2)
library(ggplot2) # 記得先載入套件
# 這裡比較新的函數是以scale_y_continuous()設定y軸的範圍，並且使用labs()
# 將x軸標籤隱藏
ggplot(data = bg.df, mapping = aes(x = xLab, y = Diff)) +
	geom_bar(color = "black", fill = "white", stat = "identity") +
 	geom_errorbar(mapping = aes(ymin = Diff - CI, ymax = Diff + CI),
						width = 0.2) +
	scale_y_continuous(limits = c(0, 3.5)) +
  labs(x = NULL, y = "Difference") +
	theme_bw()

# 用「NounsVerbs.txt」中的「研究3」的名詞與動詞例子繪製成對雙樣本t檢定的95%信賴區間
# 有需要就再讀取一次檔案吧！
nv = read.delim("NounsVerbs.txt")
study3 = subset(nv, Study == 3)
# 先進行成對雙樣本t檢定
t.res3 = t.test(Measure ~ WordType, data = study3, paired = T) 	
# 以同樣的方式從檢定結果中提取信賴區間上下限，並計算信賴區間的變動值
conf95.upper = t.res3$conf.int[2]
conf95.lower = t.res3$conf.int[1]
conf95.3 = (conf95.upper - conf95.lower) / 2
nouns3 = subset(study3, WordType == "Noun")
verbs3 = subset(study3, WordType == "Verb")
N = nouns3$Measure 
V = verbs3$Measure
# 計算平均成對差異
Diff.mean = mean(abs(N - V))
# 繪製圖表並命名
nv.plot = barplot(Diff.mean, names.arg=c("Nouns vs. Verbs"), 
                    ylab = "Mean of Paired Difference ", ylim=c(0, 3.5))
# 加上誤差線
error.bar(nv.plot, Diff.mean, conf95.3)

# 第四節
# 進行非母數檢定，請先確認你已經在之前的程式碼中讀取必要的資料囉！
wilcox.test(boys1$Measure, girls1$Measure) 			             # 曼－惠特尼U檢定
wilcox.test (Measure ~ Gender, data = study1)       	       # 用X ~ Y語法 
#Wilcoxon rank sum exact test

#data:  Measure by Gender
#W = 84, p-value = 0.008931
#alternative hypothesis: true location shift is not equal to 0
wilcox.test(nouns3$Measure, verbs3$Measure, paired = T)      # 威爾克森符號等級檢定
wilcox.test (Measure ~ WordType, data = study3, paired = T)  # 用X~Y語法
#       Wilcoxon signed rank exact test

#data:  Measure by WordType
#V = 55, p-value = 0.001953
#alternative hypothesis: true location shift is not equal to 0

# 比較大樣本偏態分佈下的成對假設變異數相等t檢定與U檢定的結果
t.sig = 0 			# 等等會累加t檢定p值小於.05的次數 
u.sig = 0 			# 等等會累加U檢定p值小於.05的次數 
conflict = 0 		# 等等會填入顯著性不同的次數 
set.seed(1)     # 指定亂數種子
for (i in 1:10000) { 
  # 從均勻分佈中抽樣20筆資料
  sample1 = runif(20) 
  sample2 = runif(20) 
  # 進行t檢定與U檢定取得p值
  t.p = t.test(sample1, sample2, var.equal = T)$p.value 
  u.p = wilcox.test(sample1, sample2)$p.value 
  if (t.p < 0.05) 
  { 
    t.sig = t.sig + 1   # 累積t檢定顯著的數目
  } 
  if (u.p < 0.05) 
  { 
    u.sig = u.sig + 1   # 累積u檢定顯著的數目
  } 
  # "|"是「或」的意思，你有辦法自己解讀這一行的條件嗎？
  if ((t.p < 0.05 & u.p >= 0.05) | (t.p >= 0.05 & u.p < 0.05)) 
  { 
    conflict = conflict + 1   # 累積兩個檢定顯著結果不同的數目
  }  
} 
t.sig / 10000 	# 0.0522
u.sig / 10000 	# 0.0401
conflict / 10000 	#  0.0129

# 練習七
# 改以極度偏態進行模擬
t.sig = 0 			# 等等會累加t檢定p值小於.05的次數 
u.sig = 0 			# 等等會累加U檢定p值小於.05的次數 
conflict = 0 		# 等等會填入顯著性不同的次數 
set.seed(1)
for (i in 1:10000) { 
  # 從常態分佈中抽樣20筆資料並轉換為指數
  sample1 = exp(rnorm(20))
  sample2 = exp(rnorm(20))
  # 進行t檢定與U檢定取得p值
  t.p = t.test(sample1, sample2, var.equal = T)$p.value 
  u.p = wilcox.test(sample1, sample2)$p.value 
  if (t.p < 0.05) 
  { 
    t.sig = t.sig + 1   # 累積t檢定顯著的數目
  } 
  if (u.p < 0.05) 
  { 
    u.sig = u.sig + 1   # 累積u檢定顯著的數目
  } 
  # "|"是「或」的意思，你有辦法自己解讀這一行的條件嗎？
  if ((t.p < 0.05 & u.p >= 0.05) | (t.p >= 0.05 & u.p < 0.05)) 
  { 
    conflict = conflict + 1   # 累積兩個檢定顯著結果不同的數目
  }  
} 
t.sig / 10000 	# 0.039
u.sig / 10000 	# 0.0495
conflict / 10000 	#  0.0393
# 從模擬結果可以發現，t檢定在樣本極度偏態且樣本偏小(n = 20)的情況下，更不容易
# 檢測到有顯著差異的樣本(3.9%)，而U檢定的結果則是更接近預期的5%機率(4.95%)。
# 兩種檢定相反結果的數量也增加了(3.9%)

# 再次進行模擬，樣本數增加至100
t.sig = 0 			
u.sig = 0 			
conflict = 0 		
set.seed(1)
for (i in 1:10000) { 
  # 從常態分佈中抽樣100筆資料並轉換為指數
  sample1 = exp(rnorm(100))
  sample2 = exp(rnorm(100))
  t.p = t.test(sample1, sample2, var.equal = T)$p.value 
  u.p = wilcox.test(sample1, sample2)$p.value 
  if (t.p < 0.05) 
  { 
    t.sig = t.sig + 1  
  } 
  if (u.p < 0.05) 
  { 
    u.sig = u.sig + 1   
  } 
  if ((t.p < 0.05 & u.p >= 0.05) | (t.p >= 0.05 & u.p < 0.05)) 
  { 
    conflict = conflict + 1   
  }  
} 
t.sig / 10000 	# 0.0438
u.sig / 10000 	# 0.0475
conflict / 10000 	#  0.0563

# 樣本數增加5倍的情況下，t檢定與U檢定的顯著結果機率都更接近理論值了，分別
# 只差距0.62%與0.25%。這次的模擬更說明當樣本數足夠時，t檢定在樣本分佈違反
# 常態分佈的情況下，仍然擁有接近非母數檢定的效力唷！