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
