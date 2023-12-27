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

# 與單因子獨立測量變異數分析進行比較
wordExp.aov.ind = aov(Learning ~ WordType, data = wordExp)
summary(wordExp.aov.ind)

# 以pf()函數驗證兩個變異數分析中的p值
pf(8.33, 3, 16, lower.tail = F)	# 獨立測量變異數分析
pf(25, 3, 12, lower.tail = F)	# 重複測量變異數分析

# 比較成對t檢定與單因子重複測量變異數分析的類似之處
# 利用subset()函數取出詞類是名詞或是(|)動詞的子集合
wordExp.nv = subset(wordExp, WordType == "Noun" | WordType == "Verb")
# 進行重複測量變異數分析
wordExp.nv.aov = aov(Learning ~ WordType + Error(Student / WordType),
                     data = wordExp.nv)
summary(wordExp.nv.aov)

# 進行成對t檢定
t.test(Learning ~ WordType, data = wordExp.nv, paired = TRUE)

# 二之三節
# 進行混合設計變異數分析的實例
# 測試「教育程度」、「語法詞類」、以及「詞頻」這三個因子和它們的交互作用對大腦處理火星語的影響

# 讀取dorami_part.txt
ddat = read.delim("dorami_part.txt")
head(ddat)

# 先把原本是數字的Participant欄位轉換為因子
ddat$Participant = as.factor(ddat$Participant)
# 進行三因子混合設變異數分析，而語法詞類與詞頻的組合是依照每位受試者分組
ddat.aov = aov(RT ~ Education * SynCat * Freq + 
                 Error(Participant / (SynCat * Freq)), data = ddat)
summary(ddat.aov)

# 練習二
# 進行三因子獨立測量變異數分析
ddat.aov.ind = aov(RT ~ Education * SynCat * Freq, data = ddat)
summary(ddat.aov.ind)

# 檢查兩邊的總變異數是否相等
ind.sum.sq = 58 + 26863 + 72381 + 166 + 26639 + 2168 + 740 + 486804
rep.sum.sq = 58 + 127079 + 26863 + 166 + 83111 + 72381 + 26639 + 94240 + 
  2168 + 740 + 182374

ind.sum.sq == rep.sum.sq  # TRUE

# 在獨立測量變異數分析中，教育程度與詞頻的交互作用(Education:Freq)
# 只呈現邊際效應(p = .051)，但在納入組內變異的混合設計變異數分析中，該交互作用
# 則呈現顯著(p = .037)，也代表混合設計變異數分析較低的第二型誤差機率。

# 二之四節
# 利用aggregate()函數計算詞頻高低差異的原始反應時間平均值
aggregate(RT ~ Freq, FUN = mean, data = ddat)

# 再次利用aggregate()計算根據教育程度與詞頻計算的原始反應時間平均值
ddat.avg = aggregate(RT ~ Education + Freq, FUN = mean, data = ddat)
# 檢查計算結果
ddat.avg
# 大學生高低詞頻平均值差異絕對值, 23.7
abs(ddat.avg$RT[1] - ddat.avg$RT[3])
# 高中生高低詞頻平均值差異絕對值, 96.7
abs(ddat.avg$RT[2] - ddat.avg$RT[4])

# 產生前述交互作用的折線圖
library(ggplot2)	# 別忘了載入ggplot2
# 在mapping參數中以aes()函數決定x與y軸的欄位以及按照顏色分組的欄位
# 在geom_line()中將stat參數設定為summary以及fun參數設定為mean，讓ggplot可以
# 計算y軸欄位「RT」的平均數並依此繪製折線圖
# 使用scale_color_manual()函數手動設定分組的灰階線條顏色
# 使用labs()加上主標題與軸標題
# 選擇theme_bw()的主題
ggplot(data = ddat, 
  mapping = aes(x = Freq, y = RT, group = Education, color = Education)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  scale_color_manual(values = c("black", "grey")) +
  labs(title = "Frequency x Education", x = "Frequency", 
       y = "Reaction Time (ms)") +
  theme_bw()

# 用coord_cartesian()函數定義y軸範圍
ggplot(data = ddat, 
       mapping = aes(x = Freq, y = RT, group = Education, color = Education)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  scale_color_manual(values = c("black", "grey")) +
  coord_cartesian(ylim = c(650, 825)) +
  labs(title = "Frequency x Education", x = "Frequency", 
       y = "Reaction Time (ms)") +
  theme_bw()

# 練習三
# 將aes()函數中的x參數改為Education欄位、並將group參數改為Freq欄位
# 這會讓不同詞頻的反應時間長條圖並列表示，更容易比較組間的詞頻效應差異
# aes()函數中的color參數改成fill，以便手動決定長條填滿的顏色
# 使用geom_bar()函數代替geom_line()產生長條圖，並將position參數設定為並排顯示
# 使用scale_fill_manual()代替scale_color_manual()決定長條填滿的顏色
# 其餘部份維持不變
ggplot(ddat, aes(y=RT, x=Education, fill=Freq, group=Freq)) +
  geom_bar(stat = "summary", fun = mean, position = "dodge") +
  scale_fill_manual(values=c("black", "darkgrey")) +
  labs(title = "Frequency x Education", x = "Frequency", 
       y = "Reaction Time (ms)") +
  theme_bw()

# 練習四
# 其實就是將x軸對應到的欄位從Freq改為SynCat即可
ggplot(data = ddat, 
       mapping = aes(x = SynCat, y = RT, group = Education, color = Education)) +
  geom_line(stat = "summary", fun = mean, linewidth = 1) +
  scale_color_manual(values = c("black", "grey")) +
  coord_cartesian(ylim = c(650, 825)) +
  labs(title = "Syntactic Category x Education", x = "Syntactic Category", 
       y = "Reaction Time (ms)") +
  theme_bw()

# 練習五
# 假設你已經讀取了wordExp資料了
grand.mean = mean(wordExp$Learning)
# 先計算Learning生字學習表現數字總方差
ss.total = sum((wordExp$Learning - grand.mean) ^ 2)
# 組內因子間平方和
wordExp.n = subset(wordExp, WordType == "Noun")
wordExp.v = subset(wordExp, WordType == "Verb")
wordExp.adj = subset(wordExp, WordType == "Adj")
wordExp.adv = subset(wordExp, WordType == "Adv")
wordExp.n.avg = mean(wordExp.n$Learning)
wordExp.v.avg = mean(wordExp.v$Learning)
wordExp.adj.avg = mean(wordExp.adj$Learning)
wordExp.adv.avg = mean(wordExp.adv$Learning)
# 公式<七>
# 每個詞類都有5個相同的受試者進行學習
ss.between.treatment = 5 * sum((wordExp.n.avg - grand.mean) ^ 2,
                               (wordExp.v.avg - grand.mean) ^ 2,
                               (wordExp.adj.avg - grand.mean) ^ 2,
                               (wordExp.adv.avg - grand.mean) ^ 2)

# 組內因子間變異50，等於變異數分析報告的WordType主要效應變異數

# WordType有4個層次，所以組內因子間自由度就是4 - 1
df.between.treatment = 4 - 1

# 公式<八>
# 總共有五位學生從1開始由小到大排序，所以Student欄位「最大」的數值是5，
# 減去1就是組內因子間分組單位自由度了
df.between.unit = max(wordExp$Student) - 1

# 先計算分組單位平方和
between.unit.avg = aggregate(Learning ~ Student, FUN = mean, data = wordExp)
# 詞類因子共有4個層次
ss.between.unit = 4 * sum((between.unit.avg$Learning - grand.mean) ^ 2)

# 受試者分組變異24，等同變異數分析報告的Student變異平方和

ss.total - ss.between.unit - ss.between.treatment

# 殘餘值為8，等同於檢定報告中的組內因子間無法解釋的殘餘值

# 三之一節
# 進行相同受試者在三個不同實驗條件裡產生資料的單因子重複測量變異數分析
maxong = read.delim("maxongR.txt")
head(maxong)

# 練習六
# 進行單因子重複測量
# 別忘了把受試者編號轉換為因子，所以每個編號會是因子中的層次，而非數值
maxong$Subject = as.factor(maxong$Subject)

# 建立模型，別忽略了分解受試者分組變異的部份
maxong.aov = aov(Response ~ Condition + Error(Subject / Condition), data = maxong)

summary(maxong.aov)

# 每兩個因子層次間成對差異變異數都非常不一樣
maxong.cond1 = subset(maxong, Condition == "Cond1")
maxong.cond2 = subset(maxong, Condition == "Cond2")
maxong.cond3 = subset(maxong, Condition == "Cond3")

# Condition 1 vs 2
var(maxong.cond1$Response - maxong.cond2$Response)
# Condition 1 vs 3
var(maxong.cond1$Response - maxong.cond3$Response)
# Condition 2 vs 3
var(maxong.cond2$Response - maxong.cond3$Response)

# 練習七
# 重點是要將lower.tail設定為FALSE以取得代表在隨機抽樣的情況下有趣變量高於無趣變量
# 的右尾機率囉！
pf(df1 =  1.59, df2 = 6.36, q = 4.73, lower.tail = F)

# 安裝ez套件
install.packages("ez", dependencies = T, ask = F)
library(ez)
# dv = 因變數(dependent variable), wid = 分組單位(within identifier)
# within = 組內因子(within-group factor)
ezANOVA(data = maxong, dv = Response, wid = Subject, within = Condition)

# 三之二節
# 練習八
# 跨受試者分析p值：第一個自由度是組內因子自由度，第二個則是受試者自由度
p.between.subj = pf(df1 = 1, df2 = 23, q = 4.5, lower.tail = FALSE) # .045
# 跨受測項目分析p值：第一個自由度是組內因子自由度，第二個則是受測項目自由度
p.between.item = pf(df1 = 1, df2 = 34, q = 4.5, lower.tail = FALSE) # .041
# min F' = 2.25，比兩個原始的F值都更低
minF = 4.5 * 4.5 / (4.5 + 4.5)
# 依公式十二與十三計算兩個min F'計算p值需要的df
# 分子自由度等於組內因子自由度
df.num = 1
# 分母自由度
df.denom = (4.5 + 4.5) ^ 2 / (4.5 ^ 2 / 34 + 4.5 ^ 2 / 23)
# min F'的p值：.139，不顯著唷！
minF.p = pf(df1 = df.num, df2 = df.denom, q = minF, lower.tail = FALSE)

# 三之二之一
# 讀取包含受測項目編號的哆啦妹實驗檔案
ddat.all = read.delim("doramiR.txt")
head(ddat.all)

# 排除資料框中在任一個欄位中為「NA」的所有行數
ddat.clean = na.omit(ddat.all) 
# 利用相同的「因變量 ~ 自變量」公式在aggregate()函數中計算平均值
# 將FUN參數設定為「mean」代表使用平均值函數計算「RT」欄位按照自變數分割後的數值
ddat.part = aggregate(RT ~ Participant * Education * SynCat * Freq,
                      FUN = mean, data = ddat.clean)

# 永遠別忘記將實際代表因子的數值欄位轉換為因子欄位的步驟
ddat.part$Participant = as.factor(ddat.part$Participant)
ddat.part.aov = aov(RT ~ Education * SynCat * Freq +
                        +	Error(Participant / (SynCat * Freq)), data = ddat.part)
# 試著仔細比對每一個自變量和交互作用的p值吧！
summary(ddat.part.aov)

# 準備進行跨受測項目分析的資料框物件。與GitHub資源中的dorami_item.txt一樣
ddat.item = aggregate(RT ~ Item * Education * SynCat * Freq,
                      FUN = mean, data = ddat.all)

# 不要忘記將受測項目編號轉換為因子層次囉！
ddat.item$Item = as.factor(ddat.item$Item)
ddat.item.aov = aov(RT ~ Education * SynCat * Freq +
                      Error(Item / Education), data = ddat.item)
summary(ddat.item.aov)

# 手動以上面兩個變異數分析中的詞頻主要效應數據計算min F'及其p值
f1.freq = 13.825
f2.freq = 7.393
minF = (f1.freq * f2.freq) / (f1.freq + f2.freq)		# 4.817
df.num = 1
df1 = 18
df2 = 16 								
df.denom = (f1.freq + f2.freq) ^ 2 / 
  (f1.freq ^ 2 / df2 + f2.freq ^ 2 / df1) 		  # 30.049
pf(minF, df.num, df.denom, lower.tail = F) 			# p = .036

# 三之三之一
# 重建教室顏色的單因子獨立測量變異數分析
exp1 = read.delim("ColoredRooms.txt", header = T)
color.aov = aov(Learning ~ Color, data = exp1)
summary(color.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# Color        2     30  15.000   11.25 0.00177 **
# Residuals   12     16   1.333
# 套用<公式二十>計算教室顏色主要效應的效應值
30 / (30 + 16)

# 使用非常簡單的線性迴歸來進行驗證變異數分析是一種特別的迴歸模型
# 因變量與自變量皆相同
color.lm = lm(Learning ~ Color, data = exp1)
# 注意「Multiple R-squared」的數據！
summary(color.lm)

library(lsr)	# 記得先安裝跟讀取套件唷
etaSquared(color.aov)

# 利用lsmeans套件產生每個進行比較樣本本身的信賴區間
library(lsmeans)
# 重新建立建立哆啦妹研究的變異數分析模型
ddat.all = read.delim("doramiR.txt")
ddat.clean = na.omit(ddat.all) 
ddat.clean$Participant = as.factor(ddat.clean$Participant)
ddat.part.aov = aov(RT ~ Education * SynCat * Freq +
                        +	Error(Participant / (SynCat * Freq)), data = ddat.clean)
# 在「lsmeans()」函數中放進模型以及所有模型的自變量進行每個比較的信賴區間計算
lsmeans(ddat.part.aov, c("Education", "SynCat", "Freq"))

# 使用ggplot2根據lsmeans()函數產生的資料繪製含有誤差線的長條圖
# 先將上面lsmeans函數產生的結果透過as.data.frame函數轉為資料框物件
ddat.lsmeans = as.data.frame(
  lsmeans(ddat.part.aov, c("Education", "SynCat", "Freq"))
  )
library(ggplot2)
# 由於之前的分析顯示我們的分析裡有顯著的教育程度與詞頻的交互作用，所以在ggplot
# 函數中我們以教育程度做為x軸的分組，而詞頻則在長條圖中使用不同的填入顏色在每一組
# 教育程度的分組中呈現差異。同時，我們的分析也包含語法詞類的主要效應，所以我們
# 利用facet_grid()函數再依據個別詞類呈現上述的交互作用。在geom_errorbar()函數中，
# 關鍵的參數是ymin與ymax，分別設定對應到為代表信賴區間上下限的欄位。
ggplot(data = ddat.lsmeans, 
       mapping = aes(x = Education, y = lsmean, group = Freq, 
                     color = Freq)) +
  facet_grid(. ~ SynCat) +
  geom_line(stat = "identity", linewidth = 1) + geom_point(stat = "identity") +
  geom_errorbar(mapping = aes(ymin = lower.CL, ymax = upper.CL), 
                width = .1, linewidth = 1, linetype = 1) +
  scale_color_manual(values = c("black", "darkgrey")) +
  coord_cartesian(ylim = c(550, 900)) +
  labs(x = "Education", y = "least square means",
       title = "Education x Frequency x SynCat", color="Frequency") +
  theme_bw()

# 三之四節
# 再次進行性別與教室顏色的例子
exp2 = read.delim("ColoredRooms2.txt", header = T)
# 自己看看資料框物件長得如何吧！
head(exp2)

colorgender.aov = aov(Learning ~ Gender * Color, data = exp2)
summary(colorgender.aov)

# 調換兩個自變量的順序，理論上主要效應與交互作用的統計數據都不會改變，因為
# 可以解釋的變量應該是相同
colorgender2 = aov(Learning ~ Color * Gender, data = exp2)
summary(colorgender2)

# 產生不平衡的資料分佈
# nrow(exp2)得到「exp2」資料框的行數數字，同時也是「exp2」資料框最後一行的編號
# [−nrow(exp2),]則是「排除nrow(exp2)數字代表的行數編號之外的資料框」
exp2a = exp2[-nrow(exp2),]
# 比較兩個資料框的倒數六行的差異
tail(exp2)
tail(exp2a)
# 以xtabs()函數分類每個因子組合的數量比較資料分佈
xtabs(~ Gender + Color, data = exp2)		# 平衡的資料分佈
xtabs(~ Gender + Color, data = exp2a)		# 不平衡的資料分佈

# 以不平衡的資料分佈測試自變量順序的影響
# 注意data參數的改變！
colorgender.aov = aov(Learning ~ Gender * Color, data = exp2a)
summary(colorgender.aov)

colorgender2.aov = aov(Learning ~ Color * Gender, data = exp2a)
summary(colorgender2.aov)

# 附錄
# 以矩陣方式讀取maxong.txt，將欄位數量設為3，並且將byrow參數設為TRUE以原始資料中
# 的列為基準單位進行排列，dimnames則是設定矩陣欄位名稱
maxong.mat = matrix(scan("maxong.txt"), ncol = 3, byrow = T,
                    dimnames = list(Subject = 1:5, 
                    Condition = c("Cond1", "Cond2", "Cond3")))
maxong.mat
mlm1 = lm(maxong.mat ~ 1)	# 基礎模型
mlm0 = lm(maxong.mat ~ 0)	# 隨機模型
anova(mlm1, mlm0, X = ~1, test = "Spherical")	# 模型的球度比較
