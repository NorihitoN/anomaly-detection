#########################################
# file  : performance.R
# auther: norih
# date  : Dec 18th, 2020
# Accuracy	どれだけ正確に予測できているか
# Recall（再現性）	どれだけ取りこぼしなく予測することができたか
# Precision（適合率）	正と予測したものがどれだけ正しかったかv
# Specificity(特異性) 真偽性
#########################################

score <- c(.91, .86, .17, .12, .04, .78, .16, .51, .57, .27) #異常値スコア
anomaly <- c(F,T,F,F,F,T,F,T,F,F) #flag

data0 <- cbind(score, anomaly)

rownames(data0) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")

barplot(data0[,"score"], ylim=c(0,1), col=data0[,"anomaly"])

data1 <- data0[order(score, decreasing = TRUE),]
score_sorted <- data1[,"score"]
anomaly_sorted <- data1[, "anomaly"]

n_total <- length(anomaly)
n_anom <- sum(anomaly); n_norm <- n_total - n_anom
recalls <- rep(0, n_total)
specifs <- rep(1, n_total)
for(i in c(1:n_total)) {
  recalls[i] <- sum(anomaly_sorted[1:i]) / n_anom
  specifs[i] <- (n_total - i - sum(anomaly_sorted[-(1:i)])) / n_norm
}

plot(score_sorted, type = 'l', recalls)
par(new = TRUE)
plot(score_sorted, type = 'l', specifs)
