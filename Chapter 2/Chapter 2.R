# 第五節

# R基本上是個計算機
3+5 

# 利用sum()函數進行數字的加總，每個以逗號隔開的參數都是會被加總的數字
sum(3, 5)

# 建立一個物件並將3存入
number = 3		
# 再次呼叫建立的物件
number

# 並非相等，而是存入並覆蓋原本number的值
number = number + 1	
# 原本的number的值加1後，覆蓋number的原始值得到的結果
number		

#sum = 5 
#會覆蓋掉sum()原始的函數功能，要重新打開R才能使用sum()囉！太危險了先幫你標起來。

# 將五個數字結合成一個向量存入物件
numbers = c(1,3,5,7,9)	
# 呼叫物件，[1]是指第一筆資料為1
numbers			
#[1] 1 3 5 7 9

# 取得第一筆資料
numbers[1]			
#[1] 1

# 取得第二筆資料
numbers[2]		
#[1] 3

# 以第二筆資料乘以第五筆資料
numbers[2] * numbers[5]	
#[1] 27

# 計算整個向量的和
sum(numbers)		
#[1] 25

# 計算整個向量的平均數
mean(numbers)		
#[1] 5

# 利用函數判斷物件類別
class(number)		
#[1] "numeric"
class(numbers)
#[1] "numeric"

# 以雙引號包圍3，代表3是字串，不是數字
fakeNumber = "3"	
class(fakeNumber)
#[1] "character"

number+fakeNumber	# 以字元加上數字，因此產生錯誤訊息
#Error in number + fakeNumber : non-numeric argument to binary operator

# 建立字串向量
words = c("I","love","Excel","and","R")
# 以data.frame建立資料框物件
# 第一個參數代表把「words」這個字串向量，存入「Word」這個名稱的欄位之中
lang.data = data.frame(Word=words)	
# 讀取整個資料框
lang.data
#   Word
#1     I
#2  love
#3 Excel
#4   and
#5     R

class(lang.data)
#"data.frame"

# 以$符號取得lang.data中Word欄位的資料
lang.data$Word	
#[1] "I"     "love"  "Excel" "and"   "R"

# 計算lang.data的Word欄位中每個單詞的字元數量
nchar(lang.data$Word)	
#[1] 1 4 5 3 1

# 將nchar()針對lang.data的Word欄位運算結果存入資料框中新的WordLen欄位
lang.data$WordLen = nchar(lang.data$Word)

lang.data
#   Word WordLen
#1     I       1
#2  love       4
#3 Excel       5
#4   and       3
#5     R       1

# 取得第一列、第二欄的資料
lang.data[1,2]		
#[1] 1

# 取得第三列的所有資料
lang.data[3,]		
#   Word WordLen
#3 Excel       5

# 取得第一欄的所有資料
lang.data[,1]		
#[1] "I"     "love"  "Excel" "and"   "R"    

# 取得第二到第四列的所有資料
lang.data[2:4,]		
#   Word WordLen
#2  love       4
#3 Excel       5
#4   and       3

# 練習二
# 將rownames(也就是列編號)以rownames()函數取出
# 再存入lang.data裡新建立的Order欄位
lang.data$Order = rownames(lang.data)
# 新增SubjLen欄位，並將欄位中所有的值統一指定為數字1
lang.data$SubjLen = 1
# 建立wordCats向量，儲存五個字的詞類
wordCats = c("N", "N", "V", "C", "N")
# 將剛剛建立的向量儲存為lang.data中的WordCat欄位
lang.data$WordCat = wordCats

# 第六節
Number		# 大小寫有差異，因此產生找不到物件的錯誤訊息
#Error: object 'Number' not found

# 有邏輯的物件命名方式(儲存字串所以叫string)
string = "This is a string!"	
# 無厘頭的命名方式
a = "This is a string!"

# 組合多個字成為有意義且好讀的物件名稱
nameOfAuthors = c("James Myers", "Tsung-Ying Chen", "Yu-an Lu")

# 在建立向量時指定儲存混合數值與字串資料
mixedData = c(1, "Yes", 3, "No", 5)	
# 全部都轉換為字元了
class(mixedData)				
#[1] "character"
mixedData
#[1] "1"   "Yes" "3"   "No"  "5"

# 以連續雙引號定義空字串
emptyStr = ""		
nchar(emptyStr)		# 空字串長度為0

# 以paste()函數連結rose與s，而分隔符號(sep = separate)為空字串
paste("rose", "s", sep = "")

# 以strsplit()函數分隔字串，並將分隔單位split設定為空字串
strsplit("roses", split = "")
#[[1]]
#[1] "r" "o" "s" "e" "s"

# 讀取GitHub第二章資源的quotes.txt檔案
quotes = readLines("quotes.txt")
quotes
#[1] "\"The greatest glory in living lies not in never falling, but in rising every time we fall.\""                          
#[2] "\"The future belongs to those who believe in the beauty of their dreams.\""                                             
#[3] "\"You may say I'm a dreamer, but I'm not the only one. I hope someday you'll join us. And the world will live as one.\""

# 以gsub()函數將所有的雙引號字元以空字串取代，雙引號字元前以「\\」標註「跳脫」
gsub("\"", "", quotes)
#[1] "The greatest glory in living lies not in never falling, but in rising every time we fall."                          
#[2] "The future belongs to those who believe in the beauty of their dreams."                                             
#[3] "You may say I'm a dreamer, but I'm not the only one. I hope someday you'll join us. And the world will live as one."

# 有縮排的程式碼，自己執行看看輸出是什麼吧！
# 在R Console中，多行但屬於同一指令的程式碼會以「+」號代表。你不用輸入這些「+」號
for(i in 1:10) {
	print(i)
}

# 沒有縮排的程式碼，執行結果相同，但就是難讀呀！
for(i in 1:10) {
print(i)
}

# 第七節
# 使用RStudio
number = sum(10, 1)