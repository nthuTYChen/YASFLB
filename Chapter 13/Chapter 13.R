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
set.seed(1)             # 指定亂數種子
for (i in 1:20) {       # 進行20次迴圈
  #set.seed(i * 10)      # 每次迴圈根據i值指定不同的亂數種子
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

# 計算從模型中得到的跨受測單位的截距與斜率的平均數，得到混合效應模型的最佳配適線
coefs = coef(sdat.lmer) # 從混合效應模型中取得跨測量單位的截距與斜率
head(coefs$Child)       # 數據以Child分組
# 從截距列表取得截距平均數。「`」符號的功用是將括號做為欄位名稱而非程式碼一部份
sdat.int.mean = mean(coefs$Child$`(Intercept)`)
# 從斜率列表取得截距平均數
sdat.age.mean = mean(coefs$Child$Age)
# 產生實際數據的散佈圖
plot(Sploink ~ Age, data = sdat, ylim = c(0, 20))
# 以線性模型加上最佳配適線，並設定以粗的虛線呈現
abline(sdat.lm, lty = 2, lwd = 3)
# 以混合效應模型的測量單位截距平均數(3.75)與斜率平均數(1.45)加上最佳配適線
abline(a = sdat.int.mean, b = sdat.age.mean)
# 加上圖例
legend("topright", lty = c(1, 2), lwd = c(1, 3), legend=c("LME", "ordinary lm"))

# 以effects套件產生的資訊將混合效應模型視覺化
library(effects)
sdat.eff = allEffects(sdat.lmer) # 從混合效應模型中取得最佳配適線資訊
plot(sdat.eff)				# 以plot()繪製模型最佳配適線折線圖

# 練習四
library(effects)
# 利用effects套件的effect()函數從混合效應模型中取得Age的效應
# 再利用as.data.frame()函數將模型資訊轉換為資料框物件
sdat.eff.df = as.data.frame(effect("Age", sdat.lmer))

library(ggplot2)

# 因為不同的geom_xxxx()函數會使用不同的資料來源，所以ggplot()函數不進行任何
# 底圖設定
ggplot() +
  # 以實際數據sdat加上數據點產生散佈圖
  geom_point(data = sdat, mapping = aes(x = Age, y = Sploink), 
             size = 2.5, alpha = .5) +
  # 以模型Age效應估計值(fit)加上最佳配適線
  geom_line(data = sdat.eff.df, mapping = aes(x = Age, y = fit),
            linewidth = 2) +
  # 以模型Age效應的標準誤計算95%信賴區間，做為繪製誤差範圍的最大與最小值
  # 並將陰影透明度設定為0.2避免覆蓋了實際數據點
  geom_ribbon(data = sdat.eff.df, 
              mapping = aes(x = Age, y = fit, ymin = fit - se * 1.96,
                            ymax = fit + se * 1.96),
              alpha = .2) +
  # 在圖表標題title呈現模型結構，並在註解caption說明陰影範圍代表的意義
  labs(title = "Sploink ~ Age + (Age | Child)", x = "Age", y = "Sploink", 
       caption = "Ribbons = 95% CI") +
  theme_bw()

# 三之一節
# 以第十章哆拉妹的研究示範同時將兩個隨機變量納入分析
ddat = read.delim("doramiR.txt", stringsAsFactor = T)	# 檔案也可在第十三章資源中找到
# 缺少的反應時間資料會在RT欄位中以「NA」呈現，所以使用na.omit()函數排除任何欄位
# 有著「NA」的資料列
ddat.clean = na.omit(ddat)
# 以aov()進行含有兩個隨機變量的變異數分析，產生錯誤訊息		
ddat.aov = aov(RT ~ Education * SynCat * Freq + 
  # 受試者編號需以as.factor()轉換為因子
  # 詞類與詞頻高低都是受試者的組內因子
 	Error(as.factor(Participant) / (SynCat * Freq)),
  # 受試者編號需以as.factor()轉換為因子，教育程度是受測項目的組內因子
 	Error(as.factor(Item) / Education), 
 	data = ddat)

# 以Clark (1973)的方式進行跨受試者與跨受試項目計算詞頻效應的minF與p值
attach(ddat) # 設定ddat為常駐物件簡化以下的程式碼 
# 以受試者為測量單位：
# 根據每個受試者/教育程度計算對每個詞類和詞頻高低計算反應時間平均數
ddat.part = aggregate(RT, list(Participant, Education, SynCat, Freq), mean) 
# 重新依照每個欄位代表的資料重新命名欄位名稱
colnames(ddat.part) = c("Participant", "Education", "SynCat", "Freq", "RT") 
# 以受測項目為測量單位：
# 根據每個受測項目計算不同教育程度對每個詞類和詞頻高低的反應時間平均數
ddat.item = aggregate(RT, list(Item, Education, SynCat, Freq), mean) 
# 同上，重新命名各個欄位
colnames(ddat.item) = c("Item", "Education", "SynCat", "Freq", "RT") 
detach(ddat) # 利用完常駐狀態的好處後別忘了解除常駐狀態
# 將受試者編號轉換為因子
ddat.part$Participant = as.factor(ddat.part$Participant)
# 進行跨受試者變異數分析，並將檢定報告存入bypart.aov
bypart.aov = summary(aov(RT ~ Education * SynCat * Freq +
                         Error(Participant / (SynCat * Freq)), 
                         data = ddat.part)) 
# 將受測項目編號轉換為因子
ddat.item$Item = as.factor(ddat.item$Item)
# 進行跨受測項目變異數分析，並將檢定報告存入byitem.aov
byitem.aov = summary(aov(RT ~ Education * SynCat * Freq +
                         Error(Item / Education), data = ddat.item)) 

# 參照第十章公式十至十四取得變異數分析報告中各項關鍵數據進行minF'計算
# 報告中的第三個部份第一個表格的第一列第四欄數值，依此類推
F1.Freq = bypart.aov[[3]][[1]][1, 4]   # 跨受試者分析的詞頻效應F值
dfnum1 = bypart.aov[[3]][[1]][1, 1]    # 詞頻效應的自由度
dfdenom1 = bypart.aov[[3]][[1]][5, 1]  # 跨受試者分析殘餘值自由度
F2.Freq = byitem.aov[[1]][[1]][3, 4]   # 跨受測項目的詞頻效應F值
dfdenom2 = byitem.aov[[1]][[1]][7, 1]  # 跨受測項目的殘餘值自由度
# 第十章公式十至十四
minF = (F1.Freq * F2.Freq) / (F1.Freq + F2.Freq)
dfnum.minF = dfnum1 
dfdenom.minF = (F1.Freq + F2.Freq) ^ 2 / (F1.Freq ^ 2 / dfdenom2 + F2.Freq ^ 2 / dfdenom1) 
# 以minF'和對應的自由度計算F分佈右尾p值
pf(minF, dfnum.minF, dfdenom.minF, lower.tail = F)

# 重新以lme4進行納入兩個隨機變量的示範
detach("package:afex", unload = T)	# 先卸載afex套件(假設你還在用他的話)
library(lme4)

# 將所有的二元類別因子的層次對比總和編碼(sum coding)取代預設的虛擬編碼(dummy coding)
contrasts(ddat$Education) = contr.sum(levels(ddat$Education)) 
contrasts(ddat$SynCat) = contr.sum(levels(ddat$SynCat)) 
contrasts(ddat$Freq) = contr.sum(levels(ddat$Freq)) 
# 建立混合效應模型：前半部包含了固定效應和交互作用、後半部包含了隨機效應
ddat.lmer = lmer(RT ~ Education * SynCat * Freq +			
                   (SynCat * Freq | Participant) + (Education | Item), 
                 data = ddat)

summary(ddat.lmer)

# 以t = z的假設根據常態分佈計算詞頻效應雙尾p值
pnorm(-2.598) * 2
# 利用afex套件以Satterwaite方法進行計算
detach("package:lme4", unload=TRUE) 
library(afex) 
ddat.lmer.p = lmer(RT ~ Education * SynCat * Freq + 
  (SynCat * Freq|Participant) + (Education|Item), 
 	data = ddat)
summary(ddat.lmer.p)

# 以實際數據與模型估計值計算皮爾遜相關係數二次冪，以呈現模型配適度/效應值
# 注意這裡是和已經排除NA資料的ddat.clean進行比較
cor(ddat.clean$RT, predict(ddat.lmer)) ^ 2

# 以MuMIn套件計算更精確的效應值
library(MuMIn)			     # 安裝和載入時注意名稱大小寫
r.squaredGLMM(ddat.lmer) # 把混合效應模型放進去函數

# 照著結果指示查看函數說明文件
?r.squaredGLMM

# 以一般線性迴歸模型示範r2很接近R2m
ddat.lm = lm(RT ~ Education * SynCat * Freq, data = ddat)
summary(ddat.lm)	# adjusted r2 = 0.03422

# 以<公式四>計算混合效應模型中的詞頻效應值
# 分子
# 注意，這裡使用的資料來源是清除NA列數的ddat.clean
ddat.highFreq = subset(ddat.clean, Freq == "High")
ddat.lowFreq = subset(ddat.clean, Freq == "Low")
Freq.RT.diff = abs(mean(ddat.highFreq$RT) - mean(ddat.lowFreq$RT)) # 約62

# 可以用「lme4」套件的「VarCorr()」函數取得模型中所有效應的變異數
ddat.lmer.var = VarCorr(ddat.lmer)
# 用「as.data.frame()」轉換為資料框物件會更容易閱讀這些數據
ddat.lmer.var = as.data.frame(ddat.lmer.var)

var.int.part = ddat.lmer.var[1, "vcov"]	  # 跨受試者截距變異數
var.int.item = ddat.lmer.var[11, "vcov"]	# 跨受測項目截距變異數
var.slope.part = ddat.lmer.var[3, "vcov"]	# 跨受試者詞頻斜率變異數
var.slope.item = 0 # 每個受測項目的詞頻是固定的，所以沒有跨受測項目的詞頻斜率
var.resid = ddat.lmer.var[14, "vcov"]		 # 模型未分組殘餘值

# 計算分母
sd.total = sqrt(var.int.part + var.slope.part + var.slope.item + var.resid)
sd.total

# 公式四
cohens.d.val = Freq.RT.diff / sd.total 
cohens.d.val

# 三之二
# 以似然比檢定證明隨機斜率的重要性
sdat = read.delim("sploink.txt")	# 假設需要重新讀取資料
# 最大化模型
sdat.lmer = lmer(Sploink ~ Age + (Age | Child), data = sdat) 
# 只含隨機截距模型
sdat.lmer.int = lmer(Sploink ~ Age + (1 | Child), data = sdat)
anova(sdat.lmer.int, sdat.lmer)	# 似然比檢定

# 以哆啦妹的實驗示範是否需要納入受測項目隨機效應
ddat = read.delim("doramiR.txt", stringsAsFactor = T) # 假設需要重新讀取資料
# 最大化模型
ddat.lmer.max = lmer(RT ~ Education * SynCat * Freq + 
                       (SynCat * Freq | Participant) + (Education | Item), data = ddat)
# 排除受測項目隨機效應之模型
ddat.lmer.part = lmer(RT ~ Education * SynCat * Freq +
                          (SynCat * Freq | Participant), data = ddat) 

anova(ddat.lmer.part, ddat.lmer.max)	# 似然比檢定

# 示範連續預測變量尺度差距過大為混合效應模型帶來的問題
set.seed(3)			  # 每個隨機抽樣步驟前都指定亂數種子，才能得到相同結果 
y = rnorm(100)		# 適合進行線性迴歸分析的連續尺度因變量 
set.seed(4)
x1 = runif(100)		# 介於0與1之間的預測/自變量
set.seed(5)
x2 = runif(100) * 10000	# 介於0與10000之間的預測/自變量
g = rep(1:10, 10) # 假設有某個數量為10的測量單位，每個單位提供10筆數據
# 準備秀逗的最大化模型
yxgmodel.lmer1 = lmer(y ~ x1 * x2 + (x1 * x2 | g))		

# 將預測變量轉換為z分數後再次建立最大化模型
x1.z = scale(x1)
x2.z = scale(x2) 
yxgmodel.lmer2 = lmer(y ~ x1.z * x2.z + (x1.z * x2.z | g))

# 看看R的說明文件給予什麼解決奇點問題的建議
?isSingular

# 以最大化模型進行不同最佳化演算法的模擬
install.packages(c("optimx", "dfoptim")) 
library(afex)
allFit(yxgmodel.lmer2)	

# 以看似沒有奇點問題的兩個演算法建立混合效應模型
yxgmodel.lmer3 = lmer(y ~ x1.z * x2.z + (x1.z * x2.z|g), 
                      control = lmerControl(optimizer = "nmkbw"))
# 要使用nloptwrap的NLOPT_LN_NELDERMEAD演算法必須在lmerControl()中另外設定
# optCtrl參數
yxgmodel.lmer3 = lmer(y ~ x1.z * x2.z + (x1.z * x2.z|g), 
            control = lmerControl(optimizer = "nloptwrap", 
                          optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD")))

# 增加運算循環次數為200000次，以nmkbw演算法建立混合效應模型
yxgmodel.lmer3 = lmer(y ~ x1.z * x2.z + (x1.z * x2.z|g), 
 			control = lmerControl(optimizer = "nmkbw", optCtrl = list(maxfeval = 200000)))

# 以貝氏統計進行混合效應模型
library(blme) 
yxgmodel.lmer4 = blmer(y ~ x1.z * x2.z + (x1.z * x2.z|g))

# 四之一節

# 以句法接受度判斷實驗進行混合效應邏輯迴歸的示範
# 不將物件取名為ddat的原因是在這章裡這個名稱已經被哆啦妹搶走了，所以要避免混淆
demdat = read.delim("demo.txt")

# 第十二章三之一節的重複測量邏輯迴歸
int.coef = numeric(7) # 新增的部份：儲存每位受試者的截距
CNP.coef = numeric(7) 
Top.coef = numeric(7) 
CxT.coef = numeric(7) 
for (i in 1:7) { 
  demdat.i = subset(demdat, demdat$Speaker == i) 
  glm.i = glm(Judgment ~ ComplexNP * Topic, family = "binomial", data = demdat.i) 
  coefs = coef(glm.i)
  int.coef[i] = coefs["(Intercept)"]  # 新增的部份：取得截距係數
  CNP.coef[i] = coefs["ComplexNP"]
  Top.coef[i] = coefs["Topic"]
  CxT.coef[i] = coefs["ComplexNP:Topic"]
} 
t.test(int.coef) # 新增的部份：截距的單一樣本t檢定
t.test(CNP.coef)
t.test(Top.coef)
t.test(CxT.coef) 
# 新增的部份：取得每組係數的標準誤
as.numeric(mean(int.coef)/t.test(int.coef)$statistic) 
as.numeric(mean(CNP.coef)/t.test(CNP.coef)$statistic) 
as.numeric(mean(Top.coef)/t.test(Top.coef)$statistic) 
as.numeric(mean(CxT.coef)/t.test(CxT.coef)$statistic)

library(lme4)	# 假設你從零開始的話就先載入套件
# 按照Matuschek et al. (2017)的建議從最大化模型開始
demdat.glmer = glmer(Judgment ~ ComplexNP * Topic +
  	(ComplexNP * Topic | Speaker) + (1 | Item), family = "binomial", 
 	data = demdat)

# 按照Matuschek et al. (2017)的建議建立無隨機相關性模型
demdat.glmer.nocorr = glmer(Judgment ~ ComplexNP * Topic + 
 	(ComplexNP * Topic || Speaker) + (1 | Item), family = "binomial", 
 	data = demdat)

# 按照Matuschek et al.的建議，維持無隨機相關性模型，但排除自變量交互作用的
# 隨機效應簡化模型
demdat.glmer.nocorr.noCxT = glmer(Judgment ~ ComplexNP * Topic + 
 	(ComplexNP + Topic || Speaker) + (1 | Item), family = "binomial", 
 	data = demdat)

# 直接跳到最簡化只包含隨機截距的模型
# 注意這裡沒有隨機斜率，所以也不需要使用「||」
demdat.glmer.onlyrandint = glmer(Judgment ~ ComplexNP * Topic + 
 	(1 | Speaker) + (1 | Item), family = "binomial", data = demdat)
summary(demdat.glmer.onlyrandint)

# 進行模型比較，看看缺少受測項目隨機效應是否造成較差的模型配適度
demdat.glmer.onlyrandint.part = glmer(Judgment ~ ComplexNP * Topic + 
                                        (1 | Speaker), family = "binomial", data = demdat)
# test = "Chisq"在這裡不是必要的，因為glmer()建立的模型比較聰明知道要用什麼測試法
anova(demdat.glmer.onlyrandint.part, demdat.glmer.onlyrandint)

summary(demdat.glmer.onlyrandint.part)

# 四之二節
# 以afex的mixed()函數建立混合效應模型，避免採用渥得檢定計算p值
# 假設已經載入lme4的話就先卸載，讓afex載入lme4
detach("package:lme4", unload = TRUE) 
library(afex)
# 在mixed()函數中設定method參數為LRT，使用似然比檢定
demdat.mixed.onlyrandint.part.LRT = mixed(Judgment ~ ComplexNP * Topic + 
    (1 | Speaker), method = "LRT", family = "binomial", data = demdat) 
demdat.mixed.onlyrandint.part.LRT

# 練習五
cor(demdat$Judgment, predict(demdat.glmer.onlyrandint.part)) ^ 2
# [1] 0.6828915

# 以只含受試者隨機截距的模型進行交叉驗證
N = nrow(demdat)            # 取得樣本數，方便後續進行比例分配
size.n = round(0.85 * N, 0) # 計算N x 0.85後小數點四捨五入後的數量 = 119
prop.correct = numeric(100) # 儲存100筆預測成功比例的容器
set.seed(1)                 # 指定亂數種子，讓你也進行相同的抽樣
for (i in 1:100) {          # 進行100次模型重建
  S.i = sample(1:N, size.n) # 從1至N的連續數字抽樣size.n(119)筆的數字
  # 以S.i數值做為列數編號，並依此從demdat取出只有這些列數的子集合做為重建模型資料
  demdat.train.i = demdat[S.i,]
  # 利用setdiff()取得1:N中不屬於S.i的列數編號，也就是剩下15%用於測試的資料列
  test.rows = setdiff(1:N, S.i)
  # 從demdat中取得屬於測試資料列的子集合
  demdat.test.i = demdat[test.rows,] 
  # 以訓練資料重建模型
  demdat.i.glmer = glmer(Judgment ~ ComplexNP * Topic + (1 | Speaker), 
                         family = "binomial", data = demdat.train.i)
  # 以測試資料從重建模型中取得估計值
  predict.i = predict(demdat.i.glmer, demdat.test.i, type = "response")
  # 估計值>0 = 1，否則 = 0
  predict.i = ifelse(predict.i > 0, 1, 0)
  # 計算估計值與實際資料一致的總數
  hits.i = sum(predict.i == demdat.test.i$Judgment)
  # 計算正確預測的比例
  prop.correct[i] = hits.i / nrow(demdat.test.i)
} 
mean(prop.correct)           # 預測正確比例平均數
sd(prop.correct) 