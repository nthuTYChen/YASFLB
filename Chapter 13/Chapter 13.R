# 記得先設定工作目錄

# 練習一
# 讀取資料
sdat = read.delim("sploink.txt")
# 產生散佈圖，設定主標題與軸標題
plot(Sploink ~ Age, data = sdat, main = "Sploink ~ Age",
     xlab = "Age", ylab = "Sploink Use")
# 以lm()簡單線性迴歸產生以年齡解釋Sploink使用的模型
# 再以abline()將模型的最佳配適線加到散佈圖上
abline(lm(Sploink ~ Age, data = sdat))

# 檢視線性迴歸檢定報告
sdat.lm = lm(Sploink ~ Age, data = sdat)
summary(sdat.lm)

# 產生每個火星人孩童的資料
# 以內建函數產生網格圖
par(mfrow = c(2, 5))		# 總共有10位孩童，所以產生一個2×5的排版
rangey = range(sdat$Sploink)	# 取得Sploink指數的最小與最大值做為Y軸範圍
for(i in 1:10) {			# 進行10次重覆繪製散佈圖的迴圈
 	sdat.i = subset(sdat, Child == i)	# 取出每位孩童的資料子集合
  # 產生每位孩童Sploink使用與年齡的散佈圖，以rangey為Y軸範圍，i為主標題
 	plot(Sploink ~ Age, data = sdat.i, ylim = rangey, main = i)
}
# 以ggplot2產生的方式
library(ggplot2)
# 直接以最方便的qplot()函數產生，前兩個參數分別為X軸與Y軸變量，並設定「facets」
# 參數為「~ Child」語法，代表所有資料以Child裡的數值分組
qplot(Age, Sploink, data = sdat, facets = ~ Child)

# 練習二
# 以unique()取得所有Child欄位獨特的值，再以length()計算獨特值的向量長度，就是
# 孩童的數量了
n = length(unique(sdat$Child))  
# 以numeric(n)建立一個空的數值向量，長度等同於n。一個用來儲存截距係數，另一個
# 用來儲存自變量係數
b0 = numeric(n) 
b1 = numeric(n) 
# 進行n次迴圈
for (i in 1:n) { 
  sdat.i = subset(sdat, Child == i)       # 先取出子集合
  lm.i = lm(Sploink ~ Age, data = sdat.i) # 以子集合進行線性迴歸分析
  coefs = coef(lm.i)                      # 以coef()函數取出所有係數
  b0[i] = coefs["(Intercept)"]            # 取出截距係數，放入b0的第i個位置 
  b1[i] = coefs["Age"]                    # 取出自變量係數，放入b1的第i個位置 
} 
t.test(b0)  # 所有截距的單一樣本t檢定
t.test(b1)  # 所有自變量係數的單一樣本t檢定