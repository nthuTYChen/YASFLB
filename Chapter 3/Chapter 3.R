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
  