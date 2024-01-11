# 請記得先設定工作目錄！

# 二之一節
# 火星人奢華性刪除現象資料
rd = read.delim("richdeletion.txt")
head(rd)

# 強迫R以五位整數顯示X軸數值。沒有此行時R會以「十的次方」格式呈現數值
options(scipen = 5)
plot(Deletion ~ Income, data = rd)	# 用之前Y~X方程式的語法也能畫圖！

rd.lm = lm(Deletion ~ Income, data = rd)	# 在線性迴歸中分析富豪型刪除機率
summary(rd.lm)					                  # 只擷取係數的部分
abline(rd.lm)                             # 將最佳配適線加到<圖一>上

# 將glm()函數中的family參數設定為binomial，以二項分佈函數建立廣義線型模型
rd.glm = glm(Deletion ~ Income, family = "binomial", data = rd)
summary(rd.glm) 

# 二之二節
log(Inf)			# 沒錯！R可以讓你計算無限值！
log(0)

# 練習一
# 一
# 計算Deletion的平均值，得到有富豪型刪除的機率P1。
del.p = mean(rd$Deletion) # 0.44
# 套用公式二
log(del.p / (1 - del.p)) # -0.2411621

# 二
# 取得所有係數
coefs = coef(rd.glm)

# 對數的相反就是指數exp()，將截距係數轉換為原始機率
exp(coefs["(Intercept)"])  # 收入為0時，有富豪型刪除的預測機率也趨近於0

# 以gtools套件的函數快速在對數勝率與原始機率間轉換
library(gtools)			# 記得先安裝並載入套件
logit(0.5)          # 從機率計算對數勝率
#[1] 0

inv.logit(0)        # 從對數勝率轉換回機率
#[1] 0.5
