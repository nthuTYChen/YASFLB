# 記得先設定工作目錄

# 練習一
# 讀取資料
sdat = read.delim("sploink.txt")
# 產生散佈圖，設定主標題與軸標題
plot(Sploink ~ Age, data = sdat, main = "Sploink ~ Age",
     xlab = "Age", ylab = "Sploink Use")
# 以lm()簡單線性迴歸產生以年齡解釋Sploink使用的模型
# 再以abline()將模型的最佳配適線加到散佈圖上
abline(lm(Sploink ~ Age, data = sdat))

# 檢視線性迴歸檢定報告
sdat.lm = lm(Sploink ~ Age, data = sdat)
summary(sdat.lm)

# 產生每個火星人孩童的資料
# 以內建函數產生網格圖
par(mfrow = c(2, 5))		# 總共有10位孩童，所以產生一個2×5的排版
rangey = range(sdat$Sploink)	# 取得Sploink指數的最小與最大值做為Y軸範圍
for(i in 1:10) {			# 進行10次重覆繪製散佈圖的迴圈
 	sdat.i = subset(sdat, Child == i)	# 取出每位孩童的資料子集合
  # 產生每位孩童Sploink使用與年齡的散佈圖，以rangey為Y軸範圍，i為主標題
 	plot(Sploink ~ Age, data = sdat.i, ylim = rangey, main = i)
}
par(mfrow = c(1, 1))    # 將排版恢復預設值

# 以ggplot2產生的方式
library(ggplot2)
# 直接以最方便的qplot()函數產生，前兩個參數分別為X軸與Y軸變量，並設定「facets」
# 參數為「~ Child」語法，代表所有資料以Child裡的數值分組
qplot(Age, Sploink, data = sdat, facets = ~ Child) + theme_bw()

# 練習二
# 以unique()取得所有Child欄位獨特的值，再以length()計算獨特值的向量長度，就是
# 孩童的數量了
n = length(unique(sdat$Child))  
# 以numeric(n)建立一個空的數值向量，長度等同於n。一個用來儲存截距係數，另一個
# 用來儲存自變量係數
b0 = numeric(n) 
b1 = numeric(n) 
# 進行n次迴圈
for (i in 1:n) { 
  sdat.i = subset(sdat, Child == i)       # 先取出子集合
  lm.i = lm(Sploink ~ Age, data = sdat.i) # 以子集合進行線性迴歸分析
  coefs = coef(lm.i)                      # 以coef()函數取出所有係數
  b0[i] = coefs["(Intercept)"]            # 取出截距係數，放入b0的第i個位置 
  b1[i] = coefs["Age"]                    # 取出自變量係數，放入b1的第i個位置 
} 
sd(b0)      # 5.769358
sd(b1)      # 1.877498
t.test(b0)  # 所有截距的單一樣本t檢定
t.test(b1)  # 所有自變量係數的單一樣本t檢定

# 進行重複測量變異數/共變異分析，以Child為測量單位
sdat.aov = aov(Sploink ~ Age + Error(factor(Child)/Age), data = sdat)
summary(sdat.aov)

# 以nlme套件建立混合效應迴歸模型
library(nlme)	# 一樣記得先安裝和載入
# 設定「random」參數，並使用「~ 組內因子 | 測量單位」的語法表達隨機效應的結構
sdat.lme = lme(Sploink ~ Age, random = ~ Age|Child, data = sdat) 
summary(sdat.lme)

# 練習三
library(ggplot2)
sdat = read.delim("sploink.txt")

# 在ggplot()函數中除了設定x與y軸的對應，也把分組與顏色元素對應至Child欄位。
# 將Child以factor()函數轉換為因子，每個孩童編號才能被視為不同的層次，並以
# 完全不同的顏色呈現。如果沒有使用factor()，那麼Child會被視為連續變量，
# 而ggplot2會以漸層呈現不同線條
ggplot(data = sdat, mapping = aes(x = Age, y = Sploink, 
                                  group = factor(Child), color = factor(Child))) +
  # 在geom_smooth()中設定參數method為lm，代表以線性迴歸模型呈現分組資料的最佳配適線
  # 將se參數設定為FALSE，不繪製標準誤範圍，避免標準誤範圍遮擋了最佳配適線
  geom_smooth(method = "lm", se = F, linewidth = 1.5) +
  labs(title = "By-subject random intercepts and slopes", color = "Child") +
  theme_bw()

# 二之三
# 以lme4套件的lmer()函數建立混合效應模型
library(lme4)
sdat.lmer = lmer(Sploink ~ Age + (Age | Child), data = sdat)
summary(sdat.lmer) 

# 直接將t值當作z值，使用pnorm()函數計算雙尾p值
# t值為負數，所以直接按照pnorm()預設的方式取得左尾p值後再乘以2
intercept.p = 2 * pnorm(-2.055) # 0.039879 
Age.p = 2 * pnorm(-2.442) # 0.01460615

# 以似然比檢定測試年齡的效應
# 建立少了年齡自變量的截距模型
sdat.lmer.noAge = lmer(Sploink ~ 1 + (Age | Child), data = sdat) 
anova(sdat.lmer.noAge, sdat.lmer) # 進行似然比檢定 

# 以afex套件連帶載入lmerTest與lme4套件的方式計算混合效應模型中的p值
detach("package:lme4", unload = TRUE)	# 卸載lme4
library("afex")				                # 載入afex並連帶載入lme4

# 再次以lmer()建立相同的模型
sdat.lmer.S = lmer(Sploink ~ Age + (Age|Child), data = sdat)
summary(sdat.lmer.S)

# 二之四
# 以隨機抽樣示範線性迴歸與混合效應模型係數並不一定非常接近的例子
g = rep(1:10, 10)       # 產生10個各有10個數據點的測量單位
lm.coefs = numeric(20)  # 建立儲存20個一般線性迴歸自變量係數的容器
lme.coefs = numeric(20) # 建立儲存20個混合效應模型自變量係數的容器
for (i in 1:20) {       # 進行20次迴圈
  set.seed(i * 10)      # 每次迴圈根據i值指定不同的亂數種子
  # 因變量y從常態分佈抽樣100個值、自變量y從均勻分佈抽樣100個值
  # 這些值再與測量單位編號結合成為資料框
  dat = data.frame(y = rnorm(100), g, x = runif(100)) 
  lm.i = lm(y ~ x, data = dat)  # 一般線性迴歸y ~ x
  coefs.lm = coef(lm.i)         # 從線性模型中取得係數
  lm.coefs[i] = coefs.lm["x"]   # 從線性模型係數中取得x係數，存入容器中第i個位置
  lme.i = lmer(y ~ x + (x|g), data = dat) # 混合效應模型以g為測量單位
  lme.sum = summary(lme.i)      # 取得混合效應模型檢定報告
  lme.coefs[i] = lme.sum$coefficients["x", 1] # 從檢定報告中取得x係數
} 
# 產生線性模型與混合效應模型x係數的相關性散佈圖
plot(lm.coefs, lme.coefs, main = "Linear Regression vs. LME Coefficients",
     xlab = "Linear Regression X", ylab = "LME X") 