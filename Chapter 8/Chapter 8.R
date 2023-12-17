# 記得先設定工作目錄！

# 第二節
# 示範
vpat = read.table("vpat.txt")	# 沒有標題列，所以不需加入header = T
colnames(vpat) = "Words"		  # 變更唯一的欄位名稱
# 從Words欄位取出第二個字元做為第一個母音、存入Vowel1欄位
# substring()函數裡的三個參數分別為字串或字串向量、子字串的起點、子字串的終點
vpat$Vowel1 = substring(vpat$Words, 2, 2)
# 從Words欄位取出第四個字元做為第二個母音、存入Vowel2欄位
vpat$Vowel2 = substring(vpat$Words, 4, 4)
# 檢查資料框的架構，應該要有Words、Vowel1、Vowel2三個欄位
head(vpat)	

# xtabs()產生矩陣(matrix)
VowelCombos.mat = xtabs(~ Vowel1 + Vowel2, data = vpat)
VowelCombos.mat # 檢視矩陣物件內容

# table()產生列聯表
VowelCombos.tab = table(vpat$Vowel1, vpat$Vowel2)
VowelCombos.tab # 檢視列聯表物件內容

# 轉置後的矩陣
VCM = t(VowelCombos.mat)	
# 使用apply()函數計算VCM物件裡第1個邊際(「每一列」)的總和(sum)
row.total = apply(VCM, 1, sum)
# 計算的結果存入RowTotal物件裡，並利用cbind()函數將此物件做為欄位與VCM矩陣合併
VCM.marg = cbind(VCM, RowTotal = row.total)
# 使用apply()函數計算VCM.marg物件裡第2個邊際(「每一欄」)的總和(sum)
col.total = apply(VCM.marg, 2, sum)
# 總和存入ColTotal物件裡，並利用rbind()函數將此物件做為列與VCM.marg矩陣合併
VCM.marg = rbind(VCM.marg, ColTotal = col.total)
# 檢視成果
VCM.marg

# 將頻率呈現為長條圖
barplot(VowelCombos.mat,  		               		  # 使用矩陣物件的資料
        beside = T,	  				                    # 將長條水平並列，而不是上下相疊
        names.arg = c("V2=a", "V2=i", "V2=u"),	  # X軸的類別名稱
        legend.text = c("V1=a", "V1=i", "V1=u"),	# 圖例的名稱
        ylim = c(0, 40),				                  # 設定Y軸的範圍避免圖例擋到長條
        ylabs = "Counts"				                  # 設定Y軸的文字標籤
)

# 練習一
# 步驟一
VMC.df = as.data.frame(VowelCombos.mat)

library(ggplot2)

# 步驟二
ggplot(data = VMC.df, mapping = aes(x = Vowel2, y = Freq, 
                                    group = Vowel1, fill = Vowel1)) +
  # 步驟三
  geom_bar(position = position_dodge2(), stat = "identity") +
  # 步驟四
  scale_fill_manual(values = c("black", "darkgrey", "lightgrey")) +
  # 步驟五
  labs(title = "Distribution Frequency of Vowels in vpat.txt", 
       x = "Vowel 2", y = "Counts", fill = "Vowel 1") +
  # 步驟六
  theme_bw()

# 繪製馬賽克圖，以比例方式呈現列聯表資訊
# 以cex參數調整字體大小
mosaicplot(VowelCombos.mat, cex = 1, main = "VowelCombos") 
# 使用table()產生的列聯表繪製馬賽克圖會少了些資訊
mosaicplot(VowelCombos.tab, cex = 1, main = "VowelCombos")

# 使用vcs套件繪製資訊更豐富的馬賽克圖
# 假設你還沒安裝vcd套件的話，就先安裝，也別忘了載入套件！
install.packages("vcd", dependencies = T, ask = F)	
library(vcd)				
# 使用矩陣物件。highlighting參數決定根據哪個因子強調差異，並設定強調差異的拼貼顏色
mosaic(VowelCombos.mat, highlighting = "Vowel2",
         highlighting_fill = c("black", "white", "grey"))

# 產生預期平均分佈的矩陣並繪製對應的馬賽克圖
VowelCombos.exp = VowelCombos.mat 	# 複製舊的矩陣物件，以便保留列與欄的名稱
# 建立一個3x3的新矩陣，並取代複製物件中的矩陣數值，但保留列與欄的名稱
VowelCombos.exp[,] = matrix(c(25.84, 4.18, 7.98,
                              17.68, 2.86, 5.46,
                              24.48, 3.96, 7.56), ncol = 3)
VowelCombos.exp 				           # 檢查平均分佈的列聯表
# 產生預期平均分佈的馬賽克圖
mosaicplot(VowelCombos.exp, cex = 1, main = "VowelCombos (Expected)")

# 三之二節
# 使用卡方檢定驗證實驗觀察到的雙詞頻率是否顯著不同於理論的雙詞分佈
bobo = c(24, 21, 43, 12)		    # 將實際觀察到的分佈儲存為一個向量物件
chance = c(2/9, 2/9, 4/9, 1/9)	# 將每個類別對應的預期分佈機率儲存為另一向量
chisq.test(bobo, p = chance)	  # 第一個參數為實際資料，第二個參數p為預期機率
#    Chi-squared test for given probabilities
#
# data:  bobo
# X-squared = 0.3275, df = 3, p-value = 0.9548

# 練習二
# 計算<表十>列聯表的預期分佈
# 樣本總數
n = 370
# 計算列的邊際總數並轉換為比例
row.marg = c(179, 201) / n
# 計算欄的邊際總數並轉換為比例
col.marg = c(247, 123) / n
# 建立2x2的矩陣呈現<表十>以預期分佈機率的樣子
# 每一個資料格中的機率都是對應的邊際總數的比例相乘的結果
expected.prob = matrix(c(
  row.marg[1] * col.marg[1], row.marg[1] * col.marg[2],
  row.marg[2] * col.marg[1], row.marg[2] * col.marg[2]
), ncol = 2)
# 將樣本資料總數乘上整個機率矩陣，就可以得到實際的預期分佈了
expected.prob * n

# 實際以馬賽克圖呈現<表十>中的比例
socdata = matrix(c(57, 112, 190, 11), ncol = 2) # 根據<表十>建立2x2的列聯表
mosaicplot(socdata)

# 三之六節
# 不需指定分佈機率，直接進行鼻音社會變異資料的卡方檢定
chisq.test(socdata)	
# 因單因子只有兩個層次，自動進行葉茲校正
#Pearson's Chi-squared test with Yates' continuity correction

#data:  socdata
#X-squared = 150.2, df = 1, p-value < 2.2e-16

# 指定correct參數為false，取消自動校正
chisq.test(socdata, correct = F)

# 將矩陣轉換為列聯表物件
socdata.tab = as.table(socdata)	
# 以summary()函數取得列聯表物件卡方值
summary(socdata.tab)			
# Number of cases in table: 370 
# Number of factors: 2 
# Test for independence of all factors:
#  Chisq = 152.93, df = 1, p-value = 3.975e-35

# 對一開始的母音位置分佈資料進行卡方檢定。
# 每個因子各有三個層次，因此預設無須進行葉茲校正
chisq.test(VowelCombos.mat)	
# Pearson's Chi-squared test
# 
# data:  VowelCombos.mat
# X-squared = 4, df = 4, p-value = 0.4
summary(VowelCombos.tab)		# 與chisq.test()產生的數據完全相同

# 像前幾章一樣，進行檢定後先將檢定結果存為物件
vcm.chisq = chisq.test(VowelCombos.mat)
# 從卡方檢定結果物件取出expected欄位資訊，也就是預期的隨機分佈了
vcm.chisq$expected

# 三之七節
# 練習三
# 建立ggplot2可以使用的資料框物件。一列一數據、欄位描述數據點(語言)的不同特性
fakeLangDist = data.frame(Freq = c(23, 68, 97, 85), 
                          Classifier = c("Yes", "Yes", "No", "No"),
                          Gender = c("Yes", "No", "Yes", "No"))

# 轉原始頻率根據總語言數量轉換為百分比
fakeLangDist$Perc = fakeLangDist$Freq / sum(fakeLangDist$Freq) * 100

# 別忘了先載入套件
library(ggplot2)
# 應該不用解釋太多？重點是將Gender欄位中的分類對應到不同的分組與不同的填入顏色
ggplot(data = fakeLangDist, mapping = aes(x = Classifier, y = Perc,
                                          group = Gender, fill = Gender)) +
  # 參考練習一的步驟解釋
  geom_bar(position = position_dodge2(), stat = "identity") +
  scale_fill_manual(values = c("black", "darkgrey")) +
  # 設定y軸範圍
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Fake language distribution", x = "Classifier", y = "Proportion (%)") +
  theme_bw()

# 建立矩陣進行卡方檢定，注意數字順序的不同
fakeLangDist.mat = matrix(c(23, 97, 68, 85), ncol = 2)
chisq.test(fakeLangDist.mat)  # 實際分佈與預設的隨機平均分佈有顯著差異！

# 第四節
# 練習四
# 建立代表卡方值的連續向量，數值為0至20且數值間以0.01為分隔
chisq.value = seq(from = 0, to = 20, by = 0.01)
# 取得卡方值於自由度為3的卡方分佈中的分佈密度
chisq.3.den = dchisq(x = chisq.value, df = 3)
# 產生線條圖
plot(chisq.3.den, main = "Chi-squared Distribution", type = "l",
     lty = 1, xlab = "X2", ylab = "Density")
# 取得卡方值於自由度為5和10的卡方分佈中的分佈密度
chisq.5.den = dchisq(x = chisq.value, df = 5)
chisq.10.den = dchisq(x = chisq.value, df = 10)
# 在原圖上加上另外兩個線條並設定不同的線條樣式
lines(chisq.5.den, lty = 2)
lines(chisq.10.den, lty = 3)
# 加上圖例
legend("topright", lty = c(1, 2, 3), legend = c("df = 3", "df = 5", "df = 10"))

# 以pchisq()函數手動驗算鼻音變異單尾p值
# 用1減去分佈左側的比例，即得到分佈右側比例
1 - pchisq(q = 150.2, df = 1)
# 將lower.tail參數設定為false，得到分佈右尾比例 
pchisq(q = 150.2, df = 1, lower.tail = F) 

# 練習五
# 波波數據
pchisq(q = 0.3275, df = 3, lower.tail = F) 
# 母音位置數據
pchisq(q = 4, df = 4, lower.tail = F) 

# 第五節
# 練習六
# 只需要B和C的數據
B.n = 105
C.n = 75
# 公式八
BC.chisq = (abs(B.n - C.n) - 1) ^ 2 / (B.n + C.n)
# 使用pchisq()取得右尾p值
pchisq(q = BC.chisq, df = 1, lower.tail = F)
# 差異是顯著的！

# 第六節
# 練習七
A.n = 4
B.n = 0
C.n = 0
D.n = 4
# 以公式(九)驗證Fisher的奶茶例子p值
(factorial(A.n + B.n) * factorial(C.n + D.n) * 
    factorial(A.n + C.n) * factorial(B.n + D.n)) / 
  (factorial(A.n) * factorial(B.n) * factorial(C.n) * factorial(D.n) *
     factorial(sum(A.n, B.n, C.n, D.n)))

# 利用fisher.test()函數再次進行驗證
tea = matrix(c(4, 0, 0, 4), ncol = 2)	    # 建立列聯表
fisher.test(tea)				                  # 進行Fisher精確性檢驗，p = .02857
fisher.test(tea, alternative = "greater")	# 同樣的檢驗，但計算右尾p值

# 測試四捨五入後單尾p值兩倍是否等於雙尾p值
round(fisher.test(tea)$p.value, 10) ==
  round(fisher.test(tea)$p.value, 10)

# Schimtz & Schroder (2002)的語意判斷實驗結果
semdata = matrix(c(14, 8, 9, 16), ncol = 2)
# 進行經過葉茲校正的雙因子卡方檢定，單尾p值不顯著
chisq.test(semdata) # X-squared = 2.5562, df = 1, p-value = .1099
# 改進行Fisher精確性檢驗
fisher.test(semdata, alternative = "greater") # 單尾p值 = 0.0545接近顯著
fisher.test(semdata) # 雙尾p值 = .08198，還是比卡方檢定的p值低

