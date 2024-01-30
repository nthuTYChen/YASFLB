# Kruschke (2015, pp.138−139的「HDIofICDF.R」與「BernBeta.R」的HDI計算函數簡化版
# 自訂函數
betaHDI = function(N = 25, z = 8, a = 1, b = 1, confidence = 0.95) { 
 	p.tails = 1 - confidence            # 被排除的尾端區域大小加總
 	find.width = function(p.L, conf) {  # 計算的左端與右端界線距離函數
    L = qbeta(p.L, z+a, N-z+b)        # 根據左/右尾區域大小計算左/右端界線
    R = qbeta(p.L + conf, z+a, N-z+b) # 用簡單的物件名稱L/R避免與其他物件混淆
    return(R - L)                     # 計算並回傳左右界線的距離
 	} 
 	# 左尾被排除區間的大小
  p.tail.L = optimize(find.width,        # 使用optimize()最佳化find.width函數的執行
                      # 另一個代表信賴區間範圍的固定變數 
                      conf = confidence, 
                      # 尋找最小左端線距離的範圍，最大會小於(1 - confidence)
                      # 避免右尾沒有被排除的區域
                      interval = c(0, p.tails), 
                      # 設定一個非常嚴格的正確率容差(0.00000001) 
                      # 當改善的值小於容差時，就會停止循環
                      tol=1e-8 
  )$minimum           # optimize()會回傳找到的左右界線距離的最小/最大值，我們只需要前者
  # 依最小左尾被排除區域的大小計算HDI左側與右側邊界
  tail.L = qbeta(p.tail.L, z+a, N-z+b) 
  tail.R = qbeta(p.tail.L + confidence, z+a, N-z+b) 
  # 回傳最終的的HDI區間
  return(c(tail.L, tail.R)) 
} 
