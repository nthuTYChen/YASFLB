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

