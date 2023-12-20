# 記得先設定工作目錄！

# 三之二
# 讀取教室顏色影響生字學習的資料
exp1 = read.delim("ColoredRooms.txt", header = T)
head(exp1) 

# 產生並儲存ANOVA的檢定結果
colors.aov = aov(Learning ~ Color, data = exp1)	
# ANOVA檢定的基本資訊
colors.aov

summary(colors.aov)	# 產生正式的統計檢定報告

# 練習一
# 依exp1的Color因子層次分組計算Learning平均數並整理為資料框
exp.avg = aggregate(Learning ~ Color, FUN = mean, data = exp1)
# 取出每個教室的子集合並計算Learning標準誤
exp.blue = subset(exp1, Color == "blue")
blue.se = sqrt(sum(((exp.blue$Learning - mean(exp.blue$Learning)) ^ 2)) / 
                 (sd(exp.blue$Learning) / nrow(exp.blue)))
exp.red = subset(exp1, Color == "red")
red.se = sqrt(sum(((exp.red$Learning - mean(exp.red$Learning)) ^ 2)) / 
                 (sd(exp.red$Learning) / nrow(exp.red)))
exp.yellow = subset(exp1, Color == "yellow")
yellow.se = sqrt(sum(((exp.yellow$Learning - mean(exp.yellow$Learning)) ^ 2)) / 
                (sd(exp.yellow$Learning) / nrow(exp.yellow)))

# 將標準誤數值依照exp.avg裡Classroom層次的順序合併為單一向量，並存入exp.avg
# 中的SE欄位
exp.avg$SE = c(blue.se, red.se, yellow.se)

library(ggplot2)

# 依教室顏色不同繪製型式不同的線條(linetype)
ggplot(data = exp.avg, mapping = aes(x = Color, y = Learning, group = Color, linetype = Color)) +
  # 繪製資料點而不是資料長條，因為標準誤有小於0的時候(長條圖的基準值為0)
  geom_point() +
  geom_errorbar(mapping = aes(ymin = Learning - SE, ymax = Learning + SE), width = 0.2)  +
  scale_y_continuous(limits = c(-10, 10)) +
  # 應該在圖表的附註說明誤差線的範圍指的是哪一種統計數據
  labs(title = "Vocabulary Learning by Classroom Color", x = "Classroom Color", 
       y = "Learning Performance", caption = "Error bars = SE") +
  theme_bw()

# 驗證教室顏色ANOVA檢定中的右尾p值
pf(q = 11.25, df1 = 2, df2 = 12, lower.tail = F)	# p值正是.00177

# 練習二
# 取出每個教室學生的子集合
exp.blue = subset(exp1, Color == "blue")
exp.red = subset(exp1, Color == "red")
exp.yellow = subset(exp1, Color == "yellow")

# 取得每個子集合樣本生字學習表現平均值
blue.mean = mean(exp.blue$Learning)
red.mean = mean(exp.red$Learning)
yellow.mean = mean(exp.yellow$Learning)

# 取得每個樣本的組內平方和
blue.ss = sum((exp.blue$Learning - blue.mean) ^ 2)
red.ss = sum((exp.red$Learning - red.mean) ^ 2)
yellow.ss = sum((exp.yellow$Learning - yellow.mean) ^ 2)

# 將平方和加總得到SSwithin
ss.within = sum(blue.ss, red.ss, yellow.ss)

# 練習三(假設你已進行過練習二所以仍然有著練習二的資料)
# 取得所有樣本平均數
grand.mean = mean(exp1$Learning)
# 計算每個樣本的組間平方和。雖然我們三組樣本數都一樣，我們這邊還是選擇使用
# nrow()取出每組樣本的列數，練習未來每組樣本可能大小不一樣的情況
blue.ss.within = nrow(exp.blue) * (blue.mean - grand.mean) ^ 2
red.ss.within = nrow(exp.red) * (red.mean - grand.mean) ^ 2
yellow.ss.within = nrow(exp.yellow) * (yellow.mean - grand.mean) ^ 2
# 將所有組間平方和加總，得到SSbetween
ss.between = sum(blue.ss.within, red.ss.within, yellow.ss.within)

# 三之三之二
# 進行教室顏色例子的事後比較
exp.blue = subset(exp1, Color == "blue")
exp.red = subset(exp1, Color == "red")
exp.yellow = subset(exp1, Color == "yellow")

# 成對t檢定(假設變異數相同)
t.test(exp.blue$Learning, exp.red$Learning, var.equal = T)    # 藍vs.紅 p = .0047
t.test(exp.blue$Learning, exp.yellow$Learning, var.equal = T) # 藍vs.黃 p = .0028
t.test(exp.red$Learning, exp.yellow$Learning, var.equal = T)  # 紅vs.黃 p = 1

# 三之三之三
# 計算Tukey HSD事後檢定的HsD值
# 先使用qtukey()函數計算q.crit值
q.crit = qtukey(1 - 0.05, 3, 12)
# 套用<公式十四>計算出HSD值
q.crit * sqrt(1.333 / 5) 

# 如同之前的方式產生並儲存ANOVA的檢定結果
colors.aov = aov(Learning ~ Color, data = exp1)	
# 根據ANOVA的模型進行事後比較檢定
TukeyHSD(colors.aov)

# 利用校正後的p值回推至兩樣本實際的平均數差異
qtukey(1 - 0.003832, 3, 12) * sqrt(1.333 / 5) 

# 四之一
# 練習四
# 別忘了載入套件
library(ggplot2)
# 根據數據建立資料框
semphon.1 = data.frame(SemRel = c(rep("Irrelevant", 2), 
                                  rep("Relevant",2  )), 
                       PhonRel = rep(c("Irrelevant", "Relevant"),2), 
                       RT = c(445, 410, 385, 350))

# 老樣子，將不同欄位對應到不同的繪圖元素。這裡的重點是將PhonRel欄位對應至分組
# 以及線條樣式
ggplot(data = semphon.1, aes(x = SemRel, y = RT, 
                           group = PhonRel, linetype = PhonRel)) +
  # 加上線條
  geom_line()+
  # 加上代表數值的點
  geom_point()+
  # 設定合理的Y軸範圍
  scale_y_continuous(limits = c(325, 475)) +
  # 設定合理的主標題、軸標題、以及線條樣式的圖例標題
  labs(title = "Additive (No Interaction)", x = "Semantic Level", 
       y = "Reaction Time (ms)", 
       linetype = "Phonological Level") + 
  theme_bw()

# 建立另外兩個資料框並使用相同邏輯產生圖表，注意圖表標題的差異
semphon.2 = data.frame(SemRel = c(rep("Irrelevant", 2), 
                                  rep("Relevant",2  )), 
                       PhonRel = rep(c("Irrelevant", "Relevant"),2), 
                       RT = c(445, 350, 360, 410))



ggplot(data = semphon.2, aes(x = SemRel, y = RT, 
                           group = PhonRel, linetype = PhonRel)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(325, 475)) +
  labs(title = "Opposite", x = "Semantic Level", 
       y = "Reaction Time (ms)", 
       linetype = "Phonological Level") + 
  theme_bw()

semphon.3 = data.frame(SemRel = c(rep("Irrelevant", 2), 
                                  rep("Relevant",2  )), 
                       PhonRel = rep(c("Irrelevant", "Relevant"),2), 
                       RT = c(445, 385, 365, 350))

ggplot(data = semphon.3, aes(x = SemRel, y = RT, 
                             group = PhonRel, linetype = PhonRel)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(325, 475)) +
  labs(title = "Reduced/Enhanced", x = "Semantic Level", 
       y = "Reaction Time (ms)", 
       linetype = "Phonological Level") + 
  theme_bw()

# 四之二節
exp2 = read.delim("ColoredRooms2.txt", header = T)

# 練習五
exp2 = data.frame(Gender = c(rep("Female", 15), rep("Male", 15)),  # 性別因子層次各重覆15次 
                  Color = rep(c(rep("Red", 5), rep("Blue", 5), rep("Yellow", 5)), 2), # 顏色因子 
                  Learning = c(c(3, 1, 1, 6, 4), c(2, 5, 9, 7, 7), c(9, 9, 13, 6, 8), # 女性的學習
                               c(0, 2, 0, 0, 3), c(3, 8, 3, 3, 3), c(0, 0, 0, 5, 0))) # 男性的學習

# 進行雙因子獨立測量ANOVA，並將檢定結果存入物件
colorgender.aov = aov(Learning ~ Gender * Color, data = exp2) 
summary(colorgender.aov) 

# 練習六
# 以*表達使用雙因子組合進行資料分類計算
exp2.avg = aggregate(Learning ~ Gender * Color, FUN = mean, data = exp2)
exp2.sd = aggregate(Learning ~ Gender * Color, FUN = sd, data = exp2)

# 複製一份exp2.avg
exp2.des = exp2.avg
# 新增SD欄位，並將exp2.sd中的標準資訊存入，合併成為單一資料框
exp2.des$SD = exp2.sd$Learning

# A*B = A + B + A:B，所以以下的雙因子ANOVA檢定結果與exp2.aov一樣。
summary(aov(Learning ~ Gender + Color + Gender:Color, data = exp2)) 

library(effects)				# 記得先安裝才有辦法載入套件
# 使用effects套件的allEffects()函數取得ANOVA模型中的「所有效應」
colorgender.ef = allEffects(colorgender.aov)	
# 直接使用內建函數plot()將所有ANOVA模型的效應繪製為折線圖
plot(colorgender.ef) 

# 進行Tukey HSD事後檢定
TukeyHSD(colorgender.aov) 

# 四之五
# 進行Levene檢定測試多層次因子間變異數是否有顯著差異
library(car)					                      # 記得先安裝載入套件
leveneTest(Learning ~ Color, data = exp1) 	

# 以BoysGirls.txt資料比較Levene Test與F檢定測試雙樣本變數是否結果
bg = read.table("BoysGirls.txt", header = T) 		# 重新讀取資料
boys1 = subset(bg, Study == 1 && Gender == "Boy") 
girls1 = subset(bg, Study == 1 && Gender == "Girl")

# 比較兩種變異數檢定 
var.test(boys1, girls1)				            # p =.339
leveneTest(Measure ~ Gender, data = bg)		# p =.7533

# 以變異非常大的資料試著進行Levene Test
# 讀取資料
exp1.het = read.delim("ColoredRooms_LargeVar.txt", header = T)

# 進行Levene Test
leveneTest(Learning ~ Color, exp1.het)

