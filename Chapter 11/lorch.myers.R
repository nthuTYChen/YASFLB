lorch.myers.simple = function(data) {
  # 此函數會接收一個資料框，而這個資料框必須含有Subj、X與Y三個欄位
  # 以unique()函數取得Subj欄位所有沒有重覆的值，這些值的向量長度就是受試者數量
  n = length(unique(data$Subj)) # 受試者人數
  # 建立儲存迴歸模型係數的向量
  b0 = NULL # 受試者內截距
  b1 = NULL # 受試者內x變量係數
  # 利用for迴圈，進行n次重覆的動作
  for (i in 1:n) {
    # 以subset()函數依每個受試者編號從資料框取出子集合，再進行簡單線性迴歸
    # 迴歸模型存入lm.i
    lm.i = lm(Y ~ X,data = subset(data, Subj == i))
    # 取出截距與自變量斜率
    coefs = coef(lm.i)
    # 在向量的第i個位置儲存第i位受試者的係數
    b0[i] = coefs["(Intercept)"] # 放入受試者i的截距係數
    b1[i] = coefs["X"] # 放入受試者i的x變量係數
  }
  # 將所有受試者的截距與向量進行單一樣本t檢定，並將結果做成列表後回傳
  return(list(t.test(b0), t.test(b1)))
} 