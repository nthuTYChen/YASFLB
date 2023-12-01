# 二之三節
# 以read.table()讀取含有數值的檔案，並設定有欄位標題(header)
RTdat = read.table("RTdat.txt", header = T)
head(RTdat)	

# 從RTdat物件中取出RT欄位的資料以數字向量呈現
RTdat$RT

# 取出RT欄位後以內建的hist()函數產生分佈直方圖
hist(RTdat$RT)

# 載入ggplot2套件
library(ggplot2)

# ggplot()底圖函數中，設定x軸資料為RTdat的RT欄位
# geom_historgram()直方圖函數中，設定長條邊緣色彩為white
ggplot(RTdat, aes(x = RT)) + geom_histogram(color = "white") + theme_bw()

# 每組包含的數字範圍為100(如：1-100)
hist(RTdat$RT, break = 100)

# 將x軸資料在直方圖中分為10組呈現
ggplot(RTdat, aes(x=RT))+geom_histogram(bins = 10)+theme_bw()

# 以density()取得RTDat$RT的分佈曲線後，利用plot()函數製圖
plot(density(RTdat$RT))

# 在底圖加上geom_density()函數並設定曲線顏色參數
ggplot(RTdat, aes(x = RT)) + geom_density(color = "black") + theme_bw()

# 第三節
# 直接求出RT欄位的平均數
mean(RTdat$RT)			
# 以RT欄位的加總除以向量長度(資料數量)驗證平均數
sum(RTdat$RT)/length(RTdat$RT)	

median(RTdat$RT)		# 取得中位數
summary(RTdat$RT)		# 取得RT欄位的基本描述統計摘要

# 練習三
(717 - 657) / 3 + 657 # 677

# 建立一個my.mode函數物件，並接受一筆參數，在這個函數內叫做x
my.mode = function(x) {	
  # 使用內建的table()函數，計算出每個資料出現次數
	freq = table(x)	
	# 使用內建的sort()函數，根據出現次數由高到低(decreasing = T)排序
	freq = sort(freq, decreasing = T)
	# 使用names()函數，取得向量中第一筆資料的欄位名稱，就是出現次數最多的資料
	mode = names(freq[1])
	# 欄位名稱是字串型態，利用內建的as.numeric()函數轉為數字型態
	mode = as.numeric(mode)
	# 回傳眾數
	return(mode)
}

# 將RT欄位傳入my.mode()自訂函數做為第一個參數就可以得到眾數囉！
my.mode(RTdat$RT)		

# 四之一節
# 計算標準差
sd(RTdat$RT)

plot(density(RTdat$RT))				# 產生分佈密度圖
abline(v = mean(RTdat$RT) - 2 * sd(RTdat$RT))	# 畫出垂直線(v)顯示排除下限
abline(v = mean(RTdat$RT) + 2 * sd(RTdat$RT))	# 畫出垂直線(v)顯示排除上限

# 練習四
# 以subset()取出RTdat的子集合並存入RTdat.sub
# 取出的標準是RT欄位需大於等於RT欄位平均值減去兩個標準差「以及」(以&表示)
# RT欄位需小於RT欄位平均值加上兩個標準差
RTdat.sub = subset(RTdat, 
                   RT >= mean(RT) - 2 * sd(RT) &
                     RT <= mean(RT) + 2 * sd(RT))

# 計算排除前的資料列數量減去排除後的資料列數量得到46，也就是有46筆的反應時間
# 被當做離群值排除了
nrow(RTdat) - nrow(RTdat.sub)

# 四之二節
# 使用RT欄位資料繪製箱形圖，將outline參數設定為TRUE以顯示離群值
boxplot(RTdat$RT, outline = T, xlab = "Box Plot", ylab = "RT (ms)")

# 利用c()的結合函數，把「1重覆10次」(rep = repetition)，「2到99」
# 以及「100重覆100次」這三個數字向量結合為一個數字向量x
x = c(rep(1, 10), 2:99, rep(100, 100))

hist(x)	# 使用內建的直方圖函數產生x的分佈直方圖
mean(x)	# 使用內建的平均值函數產生x向量的平均值

# 四之四節
# 練習四
x = c(1,1,1,1,5)		# 建立一個數字向量產生歪斜的資料分佈
mean.x = mean(x)		# 計算資料分佈的平均值
deviations = x-mean.x	# 將整個數字向量減去平均值，得到偏差的數字向量
mean(deviations)		# 神奇的「0」！

# 假設每個數字代表一個數值與平均數的差異
v = -10:10
# 將數字向量v以abs()函數轉換為絕對值
v.abs = abs(v)			
# 以v.abs為y軸，v為x軸，設定類型參數type為"l"ine
plot(v.abs ~ v, type = "l")

# 練習五
# 計算以二次幂為基準計算個別數值與平均值差異
v.sq = v^2
# 以v.sq為y軸，v為x軸，設定類型參數type為"l"ine
plot(v.abs ~ v, type = "l")

# 四之五節
# 計算只有一筆數值的「樣本標準差」是沒有意義的
sd(9)