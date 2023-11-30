# 別忘了先設定工作目錄至檔案所在的資料夾！

# 第三節
# 將目標檔案按行讀取成為一個向量，並儲存至jabberwocky物件中
jabberwocky = readLines("Jabberwocky_OnlyWords.txt")

head(jabberwocky)	# 顯示物件中的前六筆資料
#[1] "jabberwocky" "twas"        "brillig"     "and"         "the"        
#[6] "slithy"  

tail(jabberwocky)	# 顯示物件中的最後六筆資料
#[1] "borogoves" "and"       "the"       "mome"      "raths"    
#[6] "outgrabe"

# 整理出向量中每個值(單詞)出現的次數
table(jabberwocky)			
#all          and         arms           as       awhile 
#2           14            1            2            1 
#back bandersnatch      beamish       beware         bird 
#1            1            1            2            1
#...

class(table(jabberwocky))		# 判斷table()產生的物件類別
#[1] "table"

jw.table = table(jabberwocky)	# 將詞頻表儲存至物件

# 按照數字由高到低的順序排序詞頻表，並覆蓋原本的jw.table
jw.table = sort(jw.table, decreasing = TRUE)

# 查看詞頻表中前六筆資料
head(jw.table)		
#the        and         he         in jabberwock         my 
#19         14          7          6          3          3

# 也可查看詞頻表中前十筆資料
head(jw.table, 10)	
#the        and         he         in 
#19         14          7          6 
#jabberwock         my    through        all 
#3          3          3          2 
#as     beware 
#2          2

# 四之二節
# 以列為單位讀取文字檔中的資料成為向量並儲存為物件
jabberwocky.orig = readLines("Jabberwocky_Original.txt")

# 排除沒有字元的字串值並儲存為獨立的jabberwocky.clean
jabberwocky.clean = subset(jabberwocky.orig, jabberwocky.orig != "")

# 建立向量物件儲存常見的英文半形符號
punctuations = c("\"","'","\\.",",","—","\\?","!",";",":","-")

# 每一次迴圈依序從punctuations中取出一個符號存入punc中
for(punc in punctuations) {
  print(punc)	# 在迴圈內重覆印出punc的內容
}
#[1] "\""
#[1] "'"
#[1] "."
#[1] ","
#...

# 把"good dog"字串中的"g"一律取代為"Y"
gsub("g", "Y", "good dog")	
#[1] "Yood doY"

# 先複製一份jabberwocky.clean成為jabberwocky.nopunc
jabberwocky.nopunc = jabberwocky.clean
# 見四之二文章解釋迴圈功能
for(punc in punctuations) {
	jabberwocky.nopunc = gsub(punc, "", jabberwocky.nopunc)
}
head(jabberwocky.nopunc)
#[1] "Jabberwocky"                       "Twas brillig and the slithy toves"
#[3] "Did gyre and gimble in the wabe"   "All mimsy were the borogoves"     
#[5] "And the mome raths outgrabe"       "Beware the Jabberwock my son"

# 將所有文字轉換為小寫
jabberwocky.lower = tolower(jabberwocky.nopunc) 

# 以空白符號斷詞
jabberwocky.wordlists = strsplit(jabberwocky.lower," ")
# 斷詞完的結果是一個列表，每個列表為每一行句子的斷詞結果
class(jabberwocky.wordlists)
#[1] "list"
head(jabberwocky.wordlists)
#[[1]]
#[1] "jabberwocky"

#[[2]]
#[1] "twas"    "brillig" "and"     "the"     "slithy"  "toves"  

#[[3]]
#[1] "did"    "gyre"   "and"    "gimble" "in"     "the"    "wabe"  
#...

# 合併列表為包含單詞的單一向量
jabberwocky.words = unlist(jabberwocky.wordlists) 
head(jabberwocky.orig)
#[1] "jabberwocky" "twas"        "brillig"     "and"         "the"        
#[6] "slithy"

# 存檔單詞列表到工作目錄！
write(jabberwocky.words, "Jabberwocky_OnlyWordsByR.txt") 

# 練習三
# a.請試著在產生一個包含20個隨機數字的向量後
#   把這個向量轉成一個資料框(data frame)物件，名稱為「numData」且欄位名稱為「randNum」。
randNumbers = runif(20)
numData = data.frame(randNum = randNumbers)

# b.請試著產生1到20的連續數字向量，並儲存到「numData」中的「seqNum」欄位。
numData$seqNum = seq(1:20)

# c.請試著將「numData」物件中的「randNum」欄位中的數字和「seqNum」欄位中的數字相乘，
#   並將結果儲存到同一個物件中的「product」欄位。
numData$product = numData$randNum * numData$seqNum

# d.請試著理解以下for迴圈程式碼的意義，並照著操作一遍。

# 將numData中的product欄位資料取出為向量儲存為products
products = numData$product
# 使用迴圈依序將products中的每個數字取出儲存為num
for(num in products) {
  # 如果「num相等於數值0」為「真」(TRUE)
  if(num == 0)	{
    # 在Console中印出對應訊息
    print("It's a zero!")
  }
  # 以此類推
  if(num >= 5)	{
    print("It's a big number!")
  }
  if(num < 5) {
    print("It's a small number!")
  }
}

# 五之二
# 建立含有<圖十二>右側四個反應時間的向量物件
RTs = c(670, 739, 780, 653)	
# 根據該物件產生2×2的二維矩陣(nrow代表矩陣有2行)
results.mat = matrix(RTs, nrow=2)	
results.mat
#     [,1] [,2]
#[1,]  670  780
#[2,]  739  653

rownames(results.mat) = c("nouns","verbs") 		# 取代行的標籤
colnames(results.mat) = c("Exp. 1","Exp. 2")	# 取代欄的標籤
results.mat
#      Exp. 1 Exp. 2
#nouns    670    780
#verbs    739    653
  
barplot(results.mat,		          # 需要繪製的數據來源
	beside=TRUE,				            # 把長條圖並列，而不是堆疊
	names.arg=c("Exp. 1","Exp. 2"),	# 長條圖下方的標籤名稱
	legend.text=c("noun","verb"),	  # 圖例中的標籤名稱
	ylim = c(0,1100),			          # y軸的最小值和最大值
	ylab = "RT (ms)"			          # y軸名稱
)

jabberwocky = readLines("Jabberwocky_OnlyWords.txt") # 讀取斷詞檔
jw.table = sort(table(jabberwocky), decreasing=TRUE) # 產生排序詞頻表
head(jw.table)
#jabberWocky
#the        and         he         in jabberwock         my 
#19         14          7          6          3          3
barplot(jw.table, beside=T, names.arg=names(jw.table), 
	ylab = "Token counts", main="Jabberwocky word frequencies", 
	xlab = "Sample of words")

# 隨機產生100個介於0至1之間的數字
plot(runif(100))

for (i in 1:100) { 	# 按照順序從1到100的數字向量中取出一個值存到i，進行迴圈
	plot(runif(100)) 	# 所以這行會執行100次，享受動畫吧！
}

# 第二個向量(1:20)^2得出1至20每個數值的平方
plot(x = 1:20, y = (1:20)^2) 

jw.words = names(jw.table)	# 取得做為欄位名稱的單詞列表
jw.freq = as.vector(jw.table)	# 將詞頻列表轉換為純數值的向量
word.lens = nchar(jw.words)	# 計算每個單詞的字串長度
# 產生單詞詞頻與字串長度的相關性散佈圖
plot(x = as.vector(jw.table), y = nchar(jw.words))

plot(x = 1:20, y = (1:20)^2, pch = 1:20) # 每個點都用不同的符號

plot(x = 1:20, y = (1:20)^2, pch = 15, col = 1:20)		# 每個方形都不同顏色 
plot(x = 1:20, y = (1:20)^2, pch = 15, col = rainbow(20)) 	# 美呆了!

plot(x = 1:20, y = (1:20)^2, type = "l")	# 用type參數設定圖表類型

plot(x = 1:20, y = (1:20)^2, type = "l") 	# 繪製折線圖做為基礎
# 在已產生的圖上加上一條不同樣式的折線
# 注意這邊y軸數據是用*2產生，而不是^2，所以線條有所差異
lines(x = 1:20, y = (1:20)*2, type = "l", lty = 2, lwd = 2, col = "blue")
# 根據x軸(5)與y軸(200)的位置增加一個綠方點
points(x = 5, y = 200,pch = 15,col = "green") 	
# 根據x軸(5)與y軸(250)的位置增加一個深綠色字串
text(x = 5, y = 250, labels = "A green square", col = "darkgreen")
# 在左上角(topleft)增加說明兩條線的圖例
legend(x = "topleft", 
       legend = c("x^2","x*2"), lty = c(1,2), lwd = c(1,2),
       col = c("black","blue"))

# 安裝ggplot2及其關聯套件
install.packages("ggplot2", dependencies = TRUE, ask = FALSE)

library(ggplot2)	# 載入ggplot2套件

# 組合重覆兩次的Exp I與Exp II字串，儲存為expType物件，rep = repetition
expType = c(rep("Exp I",2), rep("Exp II", 2))

# 重覆noun與verb的字串組合兩次，儲存為wordType物件
wordType = rep(c("noun", "verb"), 2)
#將四個反應時間組合，儲存至RT物件
RTs = c(670,739,780,653) 	
# 將三個向量物件放入不同的欄位，用data.frame()函數轉為資料框，儲存至results.data
results.data = data.frame(Exp = expType, Word = wordType, RT = RTs)

results.data
#     Exp Word  RT
#1  Exp I noun 670
#2  Exp I verb 739
#3 Exp II noun 780
#4 Exp II verb 653

# 在基礎設定中，x軸以實驗類別分組呈現，y軸呈現反應時間
# 然後填滿的顏色則按照單詞類別做出差異
results.plot = ggplot(data = results.data,
                      mapping = aes(x = Exp, y = RT, fill = Word))

# 此時呼叫results.plot物件會得到一片空白的圖
results.plot

# 把geom_bar()函數加入到底圖中，定義數據以長條圖呈現
results.plot = results.plot + 
  geom_bar(position = "dodge", color = "black", stat = "identity")

# 再次呼叫results.plot物件會得到長條圖
results.plot

# 練習六
results.plot = results.plot +
  scale_fill_manual(values=c("black", "gray")) +
  ylab("RT (ms)") +
  theme_bw()

results.plot

# 也可以一氣呵成！
ggplot(results.data, aes(x=Exp, y=RT, fill=Word)) + 
  geom_bar(position = "dodge", color = "black", stat = "identity") +
  scale_fill_manual(values = c("black", "gray")) +
  # 看看這一行做了什麼改變？也可以試著估狗一下怎麼調整圖表標籤的位置！
  labs(title = "Reaction time results", y = "RT (ms)") +
  theme_bw()
