# 記得先設定工作目錄！

# 二之二節

# 五位華語學習者不同詞類的生詞學習表現
wordExp = read.delim("L2WordLearning.txt", header = T)
head(wordExp)

# 練習一
# 一、將資料按照生字學習由大到小進行排序
# 學習得比較好的好像大多是副詞與形容詞唷？
wordExp[order(wordExp$Learning, decreasing = T), ]

# 二、將資料按照詞類並將生字學習數量由大到小進行排序
# 以詞類為主要排序欄位生字學習數量為次要排序欄位
# 好像學生1號不論詞類都學得比別人好耶！
wordExp[order(wordExp$WordType, wordExp$Learning, decreasing = T), ]

# 進行單因子重複測量變異數分析
# 將Student欄位轉換為因子後取代原本資料框中的Student欄位
wordExp$Student = as.factor(wordExp$Student)

# 以<公式二>為基礎進行單因子重複計量變異數分析
wordExp.aov = aov(Learning ~ WordType + Error(Student / WordType), data = wordExp)
summary(wordExp.aov)
