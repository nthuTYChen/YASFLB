# 別忘了先設定工作目錄！

# 二之一節

# 模擬10,000次猜測火星人布魯諾是誰的結果
# 有三個名字各叫A, B, C的火星人
martians = c("A","B","C") 		
# 產生一個包含10000個為數字0的向量
first.guess.acc = numeric(10000)	
# 另產生一個包含10000個為數字0的向量
change.guess.acc = numeric(10000)	
# 進行10000次模擬
for (i in 1:10000) { 			
  # 隨機使用sample()函數選其中一個火星人為真實的布魯諾
 	real.bruno = sample(martians, 1) 	
 	# 隨機使用sample()函數猜測其中一個火星人為布魯諾
 	first.guess = sample(martians, 1)	
 	# 隨機選一個說自己不是布魯諾的火星人
 	# setdiff()函數幫忙從martians物件排除第一次猜測的火星人與真實的布魯諾
 	non.bruno = sample(setdiff(martians, c(real.bruno, first.guess)), 1) 
 	# 產生更換猜測的人選(排除第一次猜測的火星人與說自己不是布魯諾的火星人)
 	change.guess = setdiff(martians, c(first.guess, non.bruno))
 	# 如果第一次猜測是對的，會將1存入對應的「第一次猜測結果」的向量
 	# 1 * FALSE = 0；1 * TRUE = 1
 	first.guess.acc[i] = 1 * (real.bruno == first.guess) 
 	# 如果更換猜測人選後是對的，會將1存入對應的「更換後猜測結果」的向量
 	change.guess.acc[i] = 1 * (real.bruno == change.guess) 
} 

prob.first.guess = mean(first.guess.acc) 	# 第一次猜測正確的概率 
prob.change.guess = mean(change.guess.acc) 	# 第二次猜測的概率 
prob.first.guess 					# 大約是0.33，也就是1/3 
prob.change.guess 				# 大約是0.67，也就是2/3

# 三之一節
# 以擲硬幣的例子解釋從二項分佈中得到某種結果的機率
# 在擲三次硬幣的情況下，得出完全沒有正面的機率 
p0 = dbinom(x = 0, size = 3, prob = 0.5)
# 得出剛好1個正面的機率 
p1 = dbinom(x = 1, size = 3, prob = 0.5)
# 得出剛好2個正面的機率 
p2 = dbinom(x = 2, size = 3, prob = 0.5)
# 得出剛好3個正面的機率
p3 = dbinom(x = 3, size = 3, prob = 0.5) 
# 產生得出這四種情況的分佈圖 
plot(0:3, dbinom(0:3, size = 3, prob = 0.5), ylim = c(0,0.5)) 
# 得出最多2個正面的機率(0, H, 或HH)
p0 + p1 + p2 		
# 得出最少2個正面的機率(HH, HHH)
p2 + p3 				 
# 得出最多2個正面的機率(0, H, 或HH)
pbinom(q = 2, size = 3, prob = 0.5) 	 
# 得出最少2個正面的機率
pbinom(q = (3-2), size=3, prob=0.5) 
# 在擲60次硬幣的情況下，得到最少23個正面的機率
pbinom(q = 23, size = 60, prob = 0.5)  

# 模擬從母體火星文抽樣香蕉字詞的過程
# 符合虛無假設的母體，詞A與詞B各50,000個
null.population = c(rep("A", 50000), rep("B", 50000)) 
# 建立物件儲存得到至少37個詞A的抽樣次數 
hit.count = 0 
# 進行10,000次抽樣
for (i in 1:10000) { 
  # 用sample()從虛無假設的母體中隨意採樣60個「香蕉」字詞
  our.sample = sample(null.population, 60) 
  # 用sum()把所有樣本裡是A的數量加起來
  a.count = sum(our.sample == "A") 	
  # 如果a.count至少是(大於或等於)37
  if (a.count >= 37) 
  {  
     # 抽樣成功次數累加1
     hit.count = hit.count+1 
	} 
} 
# 把所有至少是37的抽樣結果數量除以10000次的採樣
hit.count/10000

# 三之三節
# 以「數值」的方式看待正確與否(0 = 不正確，1 = 正確)
samp = c(0,0,0,1,1) 
samp.mean = mean(samp) 		# 也就是正確率為2/5

# 直接先用二項檢定測試得到正確率為2/5的機率
pbinom(q = 2, size = 5, prob = 0.5) 	# 在5個樣本中，至少其中2個正確的機率 
#[1] 0.5 

# 改用常態分佈抽樣測試看看
mean(c(rep(0, 5), rep(1, 5))) # 假設母體為重覆5個0再重覆5個1
#[1] 0.5 

choose(10, 5) 		# 從有著10筆資料的母體任選5筆樣本資料的可能子集合數量
#[1] 252 

# 產生所有含有5個樣本的母體子集合
pop = c(rep(0, 5), rep(1, 5)) 	# 我們虛無假設的母體 
pop 					     
# 從母體中生成所有5個樣本的集合並放在一欄中
all.samps = combn(pop, 5) 	 
# 雖然很多子集合看起來一樣，但都是不同的組合 
all.samps 				

# 計算all.samps矩陣中每一欄的平均數 (1 = 行，2 = 欄) 
dist.samp.means = apply(all.samps, 2, mean) 

# 產生到目前為止步驟產出的結果的圖表
par(mfrow=c(1, 3)) 			# 把三個圖放在一列三欄的圖表空間 
plot(density(pop),main="Population")	# 繪製母體的分佈密度圖

# 接著繪製位於第一欄子集合的分佈密度圖
plot(density(all.samps[, 1]), main="Samples", 
	xlim = c(-2, 3), ylim =c(0, 1.15)	# 調整x軸和y軸的範圍
) 	 
for (i in 2:252) { 
	lines(density(all.samps[, i])) 	 	# 加上其他欄的子集合分佈密度圖
} 
# 最後繪製樣本的分佈密度圖
plot(density(dist.samp.means), main = "Sample means")
# 重設圖表空間為1列1欄
par(mfrow=c(1, 1)) 	

# 子集合平均數小於或等於(<=) 樣本平均數的數量，除以所有可能子集合數(252)
sum(dist.samp.means <= samp.mean) / length(dist.samp.means) 
#[1] 0.5 

# 模擬1,000次上述的抽樣計算過程
# 創建一個有1,000個數字的向量做為儲存的容器
rand.samp.means = numeric(1000) 	 
set.seed(1) # 指定亂數種子
for (i in 1:1000) { 
  # 隨意取出五個樣本子集合的計算平均數並存入rand.samp.means
	rand.samp.means[i] = mean(sample(pop, 5)) 
} 
# 子集合平均數小於或等於(<=) 樣本平均數的數量，除以所有可能子集合數(1,000)
sum(rand.samp.means <= samp.mean) / length(rand.samp.means)
#[1] 0.484

# 把樣本平均分佈當成常態分佈
# 常態分佈的平均值
mean.of.means = mean(dist.samp.means) 
mean.of.means 
#[1] 0.5
# 常態分佈的標準差
sd.of.means = sd(dist.samp.means) 
sd.of.means 
#[1] 0.1669983
# 我們實際平均數.4轉換為 z分數 
samp.mean.z = (samp.mean - mean.of.means)/sd.of.means 
samp.mean.z 
#[1] -0.5988083

# 在常態分佈中低於-0.5988083標準差的分佈區域佔比為27.5%
pnorm(samp.mean.z) 
#[1] 0.2746503  

# 三之四節
# 子集合平均數標準差 vs. 實際樣本標準誤
sd(dist.samp.means) # 0.1669983 
sd(pop)/sqrt(5) 		# 0.2357023 (比0.167更接近母體的sd(pop) = 0.527)  

# 以更大的母體與樣本比較母體標準差與樣本標準誤
# 假設一樣無模式的母體，但母體樣本增加到 N = 10,000，0和1的數量各半
big.pop = c(rep(0, 5000), rep(1, 5000)) 
# 樣本增加到 n = 50，而0和1的比例維持3:2
big.samp = c(rep(0, 30), rep(1, 20)) 	
# 創建有100,000個數值的物件，以便儲存子集合平均數
big.rand.samp.means = numeric(100000) 
# 注意，這次模擬資料太多，無法用combn()函數產生子集合，所以改以迴圈採樣
# 速度也會有點慢
set.seed(1) # 指定亂數種子
for (i in 1:100000) { 
  # 每次迴圈從母體中採樣50筆資料成為子集合，計算平均數後存入儲存容器
  big.rand.samp.means[i] = mean(sample(big.pop, 50)) 
} 
# 我們得出母體標準差：0.07028086
sd(big.rand.samp.means) 
# 中央極限定理的估算樣本標準誤：0.06998542，非常接近樣本標準差了！ 
sd(big.samp)/sqrt(50) 

# 四之一節
# 複習banana vs. ananab的二項檢定
# 在取樣60個火星「香蕉」詞的情況下，得到最少23個「banana」的機率
pbinom(q = 23, size = 60, prob = 0.5) 
#[1] 0.04623049 

# 取樣60次火星「香蕉」的所有情況 
plot(0:60, dbinom(0:60, size = 60, prob = 0.5), ylim = c(0, 0.11)) 
# 以線條標出取樣到0至23個[banana]的子集合(用?segments看看這個函數的用法)
# segments()函數是在即有的圖表上加上線條。在此例中，是依照x軸0至23的位置
# 加上線條，而線條的高度是按照dbinom產生的密度數值決定，也因此會和plot()
# 函數產生的點的高度一致。有些線條不明顯是因為密度趨近於0。
segments(0:23, rep(0, 23), 0:23, dbinom(0:23, size = 60, prob = 0.5)) 

# 上面是單尾檢定，這邊改成雙尾檢定
# 抽樣60次的所有可能情況
plot(0:60, dbinom(0:60, size = 60, prob = 0.5), ylim = c(0, 0.11)) 
# 左邊尾巴：以線條標出0至23次banana的情況
segments(0:23, rep(0, 23), 0:23, dbinom(0:23, size = 60, prob = 0.5)) 
# 右邊尾巴：以線條標出37至60次banana的情況
segments(37:60, rep(0, 23), 37:60, dbinom(37:60, 60, 0.5)) 

# 四之二節

# 把母體擴大但維持設為37比23的變異體比例進行抽樣與單尾檢定
population = c(rep("A", 37000), rep("B", 23000)) 
# 等等會把計算有顯著的單尾二項檢定數目
sig.count = 0 		
set.seed(1) 	# 設定亂數種子
# 進行10,000次抽樣	 
for (i in 1:10000) { 	
	our.sample = sample(population,60) 	# 從母體採集60個樣本
	a.count = sum(our.sample == "A")   	# 60個樣本中A的數目
	pval = pbinom(min(a.count, 60-a.count), 60, 0.5)  # 單尾檢定
	if (pval< 0.05) { 				# 如果p值小於.05 
		sig.count = sig.count+1 		# 就在剛剛創建的sig.count加1
	} 
} 
# 從母體採集的樣本子集合中得到A數目與母體顯著不同的數量
sig.count/10000 
#[1] 0.5619

# 五之一節
# 練習二
# 20位閩南語兒童詞彙平均數100，母體英語兒童詞彙平均數160，標準差50
z.score = (160 - 100) / (50 / sqrt(20))
(1 - pnorm(z.score)) * 2
#[1] 8.025111e-08

# 五之二節
# 以t.test()函數進行單一樣本t檢定，檢查假的妹妹VOT資料是否與母體有顯著差異
set.seed(19483) 					# 先指定亂數種子確保抽樣結果相同 
# 從常態分佈中抽樣妹妹發音/t/16次的VOT長度並以round()函數四捨五入
sister = round(rnorm(n = 16, mean = 22, sd = 3)) 	
sister 						# 看看長怎樣
#[1] 20 23 29 21 22 21 18 25 25 16 21 21 25 22 21 22
mean(sister) 					# 樣本平均數等於22
sd(sister) 						# 標準差3.03315，離3夠近了
# 進行單一樣本t檢定，母體虛無假設H0為mu = 20 (t.test()預設mu = 0)
t.test(sister, mu = 20) 	

# 上述單一樣本t檢定的檢定力計算
power.t.test(n = 16, sig.level = 0.05, 
             delta = abs(mean(sister) - 20), sd = sd(sister),
             type = "one.sample", alternative = "two.sided")

# 改成在相同樣本與母體差異為顯著情況下指定檢定力要達到.8，計算所需樣本為何
power.t.test(power = 0.8, sig.level = 0.05, 
             delta = abs(mean(sister) - 20), sd = sd(sister), 
             type = "one.sample", alternative = "two.sided")
