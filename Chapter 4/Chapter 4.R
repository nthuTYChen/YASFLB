# 別忘了設定工作目錄！

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
hist(RTdat$RT, breaks = 100)

# 將x軸資料在直方圖中分為10組呈現
ggplot(RTdat, aes(x = RT)) + geom_histogram(bins = 10) + theme_bw()

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

# 使用RT欄位資料以ggplot2套件繪製小提琴圖
library(ggplot2)		# 載入套件
# 使用RTdat做為資料來源，並將RT欄位對應到Y軸。箱形圖與小提琴圖的X軸並沒有對應至
# 任何欄位，所以設定為「NA」=「Not Applicable」，並且在labs()以及guides()函數中
# 將X軸資訊分別設定為NULL(空)以及"none"(無)，隱藏不重要的X軸資訊
ggplot(data = RTdat, mapping = aes(x = NA, y = RT)) + geom_violin() + 
  labs(x = NULL) + guides(x = "none") + theme_bw()

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

x = c(1,3,5,7,9)
# sqrt()函數計算平方根，length()函數計算向量中的資料數目
# 驗證以R函數計算出來的標準差與變異數是否「相等」(==)於
# 手動計算的結果
sd(x) == sqrt(sum((x - mean(x)) ^ 2)/(length(x) - 1))
var(x) == sum((x - mean(x)) ^ 2)/(length(x) - 1)

# 五之一節
RTs = read.table("RTs_New.txt", header = T)

minRT = min(RTs$RT)		# 取得所有反應時間最小值，做為x軸區間使用
maxRT = max(RTs$RT)		# 取得所有反應時間最大值，做為x軸區間使用
subj4 = subset(RTs, Participant==4)	# 取出受試者4號的資料子集合
subj5 = subset(RTs, Participant==5)	# 取出受試者5號的資料子集合
# 製作受試者4號的分佈密度圖，設定xlim參數定義x軸區間，設定main參數定義圖片標題
plot(density(subj4$RT), xlim = c(minRT,maxRT),
       +	main="RT distribution")
# 以lines()函數將受試者5號的分佈線加到現有圖片上，並設定lty線條樣式參數為虛線
lines(density(subj5$RT), lty=2)
# 以legend()函數在現有圖片上加上圖例解釋圖表。第一個參數為圖例的位置，
# 第二個參數為圖例的兩組文字，第三個參數則是圖例兩組文字對應到的線條(實線與虛線)
legend("topright", legend=c("Participant 4", "Participant 5"),
         +	lty=c(1,2))

# ggplot2的作法(別忘了先載入ggplot2套件)
RTs$Participant = as.factor(RTs$Participant)
# 將受試者欄位轉換為因素(factor)，讓ggplot2可以根據因素裡的類別來自動進行分類
ggplot(RTs, aes(x=RT, linetype=Participant))+
  # 以RTs資料框物件製作圖表，以RT欄位為x軸，並以Participant欄位分組給予不同線條樣式
 geom_density(color="black")+
  # 在geom_density()函數中將所有線條顏色設定為黑色
 scale_linetype_manual(values=c("solid", "dashed"))+
  # 利用scale_linetype_manual()函數手動設定線條樣式為實線與虛線
 labs(title = "RT distribution")+	# 利用labs()函數，設定圖片標題
 theme_classic()				# 使用theme_classic()的圖片主題樣式

# 取出subj4資料框中Item欄位值為14的列數，然後取得RT欄位資料
subj4[subj4$Item == 14,]$RT
#[1] 627
# 取出subj5資料框中Item欄位值為14的列數，然後取得RT欄位資料
subj5[subj5$Item == 14,]$RT
#[2] 652

# 新增RT.y欄位儲存兩位受試者中心化(每筆資料數據減去平均值)後的資料分佈
subj4$RT.y = subj4$RT - mean(subj4$RT)
subj5$RT.y = subj5$RT - mean(subj5$RT)

# 練習七
# 建立RT.y欄位並存入「0」的數值
RTs$RT.y = 0

# 從RTs的Participant欄位取得不重覆的值，就可得到所有受試者編號
participants = unique(RTs$Participant)

# 設定迴圈，依序在每次迴圈中從participants取出一個值，存入participant
for(participant in participants) {
  # 取出RTs中的Participant欄位符合participant中數值的資料子集合(注意大小寫)
  RTs.sub = subset(RTs, Participant == participant)
  # 計算該participant的平均反應時間
  RT.mean = mean(RTs.sub$RT)
  # 計算該participant反應時間與平均值的差異
  RT.diff = RTs.sub$RT - RT.mean
  # 將這些「中央化」的反應時間存回RTs中屬於該participant的RT.y欄位
  RTs[RTs$Participant == participant, ]$RT.y = RT.diff
}

subj4[subj4$Item == 14,]$RT.y
#[1] 103.7333
subj5[subj5$Item == 14,]$RT.y
#[2] -125.6667

# 五之二節
# 新增RT.z欄位儲存受試者以標準差轉換(中心化反應時間除以原始反應時間標準差)後的資料
subj4$RT.z = subj4$RT.y/sd(subj4$RT)
subj5$RT.z = subj5$RT.y/sd(subj5$RT)

subj4[subj4$Item == 14,]$RT.z
#[1] 1.182903 # 比受試者4的平均值高1.18個標準差
subj5[subj5$Item == 14,]$RT.z
#[2] -0.7447685 # 比受試者5的平均值低0.75個標準差

# 除了反應時間的z分數外，也顯示資料分佈平均值與標準差
scale(subj4$RT)	
# 將透過scale函數轉換後的反應時間z分數存入RT.scaled欄位。
subj4$RT.scaled = scale(subj4$RT)
# 與RT.z欄位比較看看是否相同？
head(subj4)

mean(subj4$RT.z)	# 轉換為z分數分佈的平均值極近似於0
sd(subj4$RT.z)	  # 轉換為z分數分佈的標準差是1

# 五之三節
# 讀取有語言學概論考試成績的檔案 
grades = read.table("example-grades.txt", header = T)
# 將所有學生的考試分數標準化為z分數
grades$TestScores.z = scale(grades$TestScores)
# 計算資料框中TestScores.z欄位小於-0.75子集合的列數，代表不及格學生的數量
nrow(subset(grades, TestScores.z < -0.75))
#[1] 23

# 六之一節
# 練習八
# 從介於0至1的均勻分佈區間產生1,000,000個亂數
randUniNums = runif(1000000)
# 產生直方圖呈現實際資料分佈
hist(randUniNums)
# 取得向量的分佈密度
rand.den = density(randUniNums)
# 產生線條圖呈現分佈密度
plot(rand.den, type = "l")

rnorm(2)	# 從平均值為0、標準差為1的常態分佈中抽樣兩個數字

# 利用for迴圈逐漸累積抽樣數字，產生分佈密度圖變化的動畫
# 先抽樣第一筆兩個數字存入nums為數字向量
nums = rnorm(2)			
# 利用for迴圈再另外抽樣10000次
for(i in 1:10000) {		
  # 用結合函數將新的抽樣數字累積至nums數字向量
	nums = c(nums, rnorm(2))	
	# 每次產生新的抽樣數字後製作nums的分佈密度圖
	plot(density(nums))		
}

# 產生-3,-2.9…2.9,3的連續數字向量
x = (-30:30)/10		
# 產生x中每個值在平均值為0標準差為1的理想常態分佈中的預期分佈密度
y = dnorm(x)		
# 產生分佈線狀圖(type="l")
plot(x, y, type = "l")	

# 練習九
# 產生1至30的整數並儲存至newX中
newX = 1:30
# 產生newX值在平均值為15標準差為3的常態分佈中的預期分佈密度
newY = dnorm(newX, mean = 15, sd = 3)
# 產生分佈線狀圖
plot(newX, newY, type = "l")

nums = rnorm(100)		# 從預設理想常態分佈抽樣100個數字
highNums = sum(nums > 2)	# 計算nums > 2為TRUE的總數
lowNums = sum(nums < -2)	# 計算nums < -2為TRUE的總數
(highNums+lowNums)/100	# 以大於2與小於-2的數字數目加總除100計算比例

# 計算在常態分佈中z分數-2至2之間的區域佔整個分佈區域的比例
pnorm(2)-pnorm(-2)
#[1] 0.9544997

# 在常態分佈區域上標出-2至2的區間
x = (-30:30)/10			# 產生-3,-2.9…2.9,3的連續數字向量
y = dnorm(x)			# 產生x中每個值在理想常態分佈中的密度
dat = data.frame(x=x, y=y)	# 將x與y數字向量合併為一個資料框物件dat
dat.sub = subset(dat, x>=-2 & x<=2)	# 取出-2到2區間的資料區間存入dat.sub

# 以ggplot2製作常態分佈圖，以dat中的x與y欄位資料分別做為圖表的x軸與y軸
ggplot(data = dat, mapping = aes(x = x, y = y)) + 	
    geom_line(color="black") +	# 以線狀圖呈現，顏色設定為黑色
    # 在geom_ribbon()函數以dat.sub資料為基礎填入顏色，且將顏色設定為灰色
    geom_ribbon(data = dat.sub, mapping = aes(x=x, ymax=y, ymin=0), 
                fill="grey") +
    # 利用geom_vline()函數在x軸的-2與2的位置加入垂直黑色虛線
    geom_vline(aes(xintercept=-2), color="black", linetype="dashed") +
    geom_vline(aes(xintercept=2), color="black", linetype="dashed") +
    theme_classic()

qnorm(0.025)	# 計算代表佔有常態分佈左方2.5%區域的z分數
#[1] -1.959964
qnorm(0.975)	# 計算代表佔有常態分佈左方97.5%區域的z分數
#[1] 1.959964

# 六之二節
x = 35:65					# 產生35至65的連續值
y = dnorm(x, mean = 50, sd = 5)		# 產生連續值在理想常態分佈中的密度
plot(x, y , xlim=c(35, 65), type = "l")	# 產生常態分佈線狀圖
abline(v = (50 - 5))				# 在−1標準差之處加上垂直線
abline(v = (50 + 5))				# 在1標差之處加上垂直線

# 以常態分佈計算丟擲四次硬幣結果的機率
M.coin = 4 * 0.5
sd.coin = sqrt(M.coin * (1 - 0.5))
dnorm(3, M.coin, sd.coin)	# 投擲4次出現3次正面在常態分佈中的密度
#[1] 0.2419707
dnorm(4, M.coin, sd.coin)	# 投擲4次出現4次正面在常態分佈中的密度
#[1] 0.05399097

# 練習十
# 以常態分佈計算丟擲四十次硬幣結果的機率
# 平均值與標準差計算
M.coin = 40 * 0.5
sd.coin = sqrt(M.coin * (1 - 0.5))
dnorm(12, M.coin, sd.coin)	# 投擲40次出現12次正面在常態分佈中的密度
#[1] 0.005142422
dnorm(25, M.coin, sd.coin)	# 投擲40次出現12次正面在常態分佈中的密度
#[1] 0.03614448

# 以二項分佈進行
dbinom(3, 4, 0.5)	# 丟擲4次出現3次正面，且每次正面出現的機率為0.5
#[1] 0.25
dbinom(4, 4, 0.5)	# 丟擲4次出現4次正面，且每次正面出現的機率為0.5
#[1] 0.05

M.coin = 40 * 0.5
sd.coin = sqrt(M.coin * (1-0.5))
dnorm(12, M.coin, sd.coin) - dbinom(12, 40, 0.5)	# 比較常態分佈與二項分佈的密度
#[1] 6.121147-e05
dnorm(25, M.coin, sd.coin) - dbinom(25,  40,0.5)	# 比較常態分佈與二項分佈的密度
#[1] -0.0004402538

# 練習十一
# 因為投擲了40次，所以將4改為40，並計算平均值與標準差
M.coin = 40 * 0.5			
sd.coin = sqrt(M.coin * (1 - 0.5))
# 產生0至40的整數，代表投擲40次得到0次、1次…至40次人頭的結果。
x = 0:40
# 根據平均值與標準差從常態分佈中得到0至40整數的密度
y = dnorm(x, M.coin, sd.coin)				
# 產生根據常態分佈得到的分佈圖
plot(x, y, xlim = c(0, 40), type = "l", main = "Coin Flip N = 40")	
# 根據二項分佈加上圓點，代表投擲40次且得到人頭機率0.5，結果是得到0次、1次…至40次
# 的分佈密度
points(0:40, dbinom(0:40, 40, 0.5))

# 比較實際樣本RTdat.txt的分佈密度與對應常態分佈的分佈密度
RTdat.mean = mean(RTdat$RT)	# 取得樣本反應時間平均值
RTdat.sd = sd(RTdat$RT)		# 取得樣本反應時間標準差
# 將x軸範圍定義為的0至1500
# 從相同平均值與標準差的理想常態分佈取得分佈密度
RTdat.norm = dnorm(0:1500, 
                   mean = RTdat.mean, sd = RTdat.sd)
# 先產生樣本的分佈密度圖
plot(density(RTdat$RT), xlim = c(0, 1500))
# 再加上以樣本的數值得到在常態分佈中的分佈密度曲線
lines(x = 0:1500, y = RTdat.norm, lty = 2)

#使用qqnorm()進行常態分位數的比較
qqnorm(RTdat$RT)		# 產生反應時間資料的常態分位數比較圖
qqline(RTdat$RT)		# 在比較圖上加上理想反應時間常態分佈的比較線

# 從常態分佈抽樣然後產生QQ Norm Plot看看樣本貼近常態分佈的呈現結果
# 設定隨機計算的「種子」，這樣我們就能得到相同的隨機計算結果
set.seed(100)
# 從平均值是10標準差為2的理想常態分佈抽樣100筆資料
fake = rnorm(100, 10, 2)	
qqnorm(fake)
qqline(fake)

# 練習十二
# 先假設你按照第三章的步驟產生了含有Jabberwocky詞頻統計的jw.freq物件
qqnorm(jw.freq)
qqline(jw.freq)

plot(1:100, log(1:100))	# 比較1至100以及1至100的對數轉換

# 比較反應時間進行對數轉換前後貼近常態分佈的差異
RTdat$logRT = log(RTdat$RT)	# 對數轉換
par(mfrow = c(2, 2))		# 準備一個2x2的排版，讓四張圖可以依此排列
hist(RTdat$RT, main = "Raw")	# 原始資料的分佈直方圖
hist(RTdat$logRT, main = "Log")	# 對數轉換資料的分佈直方圖
qqnorm(RTdat$RT, main = "Raw")	# 原始資料的常態分位數比較圖
qqline(RTdat$RT)			# 加上理想常態分佈線
qqnorm(RTdat$logRT, main = "Log")	# 對數轉換資料的常態分位數比較圖
qqline(RTdat$logRT)		# 加上理想常態分佈線
par(mfrow = c(1, 1))		# 恢復預設的1x1圖片輸出排版

# 練習十三
# 一樣先假設你按照第三章的步驟產生了含有Jabberwocky詞頻統計的jw.freq物件
qqnorm(log(jw.freq))
qqline(log(jw.freq))