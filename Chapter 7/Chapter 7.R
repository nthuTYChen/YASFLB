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
#