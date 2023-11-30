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

