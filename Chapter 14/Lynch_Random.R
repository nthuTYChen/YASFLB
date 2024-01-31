set.seed(2) # 指定隨機種子
# 按照班級取出分數子集合，並將分數以cbind()合併為兩欄，再以as.matrix()轉換為矩陣
class1 = subset(tdat, Class == 1)
class2 = subset(tdat, Class == 2)
y = as.matrix(cbind(class1$Score, class2$Score)) 
# 「安全」的事前假設初始值，請參閱書中解釋
m = 0
s2 = 100000
a = c =.00001
b = d =.00001
tau2 = 1
sigma2 = 1
alpha = 0 
n = nrow(y) # 測量單位(學生)的數量
# 以Gibbs抽樣進行1,000,000次循環運算
for(B in 1:1000000) {  
  alpha_i = rnorm(n, # 學生測驗分數平均數的估計值分佈 
                  mean = (((tau2*(y[,1] + y[,2])) + sigma2*alpha) / (2*tau2 + sigma2)), 
                  sd = sqrt((tau2*sigma2) / (2*tau2 + sigma2))) 
  alpha = rnorm(1, # 整體測驗分數平均數的估計值分佈 
              mean = (tau2*m + s2 *sum(alpha_i)) / ((tau2 + n*s2)), 
              sd = sqrt((tau2*s2) / (tau2 + n*s2))) 
  tau2 = 1 / rgamma(1, # 學生隨機變異的估計值分佈 
                shape=(n/2 + a), 
                rate=(sum((alpha_i - alpha)^2) + 2*b)/2) 
  sigma2 = 1 / rgamma(1, # 殘餘值的估計值分佈 
                  shape = n + c, 
                  rate = (sum((y - alpha_i)^2) + 2*d)/2) 
}

