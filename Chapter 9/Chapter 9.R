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