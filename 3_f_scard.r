

###############################################################################
###############################################################################
# 做评分卡并打分
f_scard <- function(datainput1=soft_model_scard, varid=2:ncol(datainput1), model1=model_choose){
# datainput1: 输入的数据集(只包含预测目标(第一列)及已经分组的输入变量)
# varid:      自变量所在列
# model1:     选用的模型

m1 <- matrix(NA, nrow=2, ncol=11)
m1 <- as.data.frame(m1)
m0 <- m1[-(1:2), ]
names(m0) <- c('var','value','N','N0','N1','P0','P1','WOE','IV','est','score')
names(m1) <- c('var','value','N','N0','N1','P0','P1','WOE','IV','est','score')
for(j in varid){
d <- data.frame(u=datainput1[,j], v=datainput1[,1])
d[, 1] <- factor(d[, 1])
m <- matrix(NA, nrow=nlevels(d[,1])+2, ncol=11)
m <- as.data.frame(m)
names(m) <- c('var','value','N','N0','N1','P0','P1','WOE','IV','est','score')
m[, 1] <- colnames(datainput1)[j]
m[-c(1,nrow(m)), 2] <- levels(d[, 1])
m[nrow(m), 2] <- 'sum'
m[1, 2] <- 'missing'
model1_est <- summary(model1)$coef[, 1]
constant <- model1_est[['(Intercept)']]
for(i in 2:(nrow(m)-1)){
m$N[i] <- sum(d[, 1]==m[i, 2] & !is.na(d[, 1]) & d[, 2] %in% c('0','1'))
m$N0[i] <- sum(d[, 1]==m[i, 2] & !is.na(d[, 1]) & d[, 2]=='0')
m$N1[i] <- sum(d[, 1]==m[i, 2] & !is.na(d[, 1]) & d[, 2]=='1')
m$est[i] <- model1_est[paste('WOE_', colnames(datainput1)[j], sep='')]
rm(i)
}
m$N[1] <- sum(is.na(d[, 1]) & d[, 2] %in% c('0','1'))
m$N0[1] <- sum(is.na(d[, 1]) & d[, 2]=='0')
m$N1[1] <- sum(is.na(d[, 1]) & d[, 2]=='1')
m$est[1] <- model1_est[paste('WOE_', colnames(datainput1)[j], sep='')]
m[nrow(m), 3:5] <- colSums(m[-c(1,nrow(m)), 3:5])
m$P0[-1] <- (m$N0[-1])/(m$N0[nrow(m)])
m$P1[-1] <- (m$N1[-1])/(m$N1[nrow(m)])
m$WOE <- log(m$P1/m$P0)
m$WOE[1] <- 0
m$score <- ((-1)*m$WOE*m$est+(constant-log(15)+log(2)*30)/length(varid))/(log(2)/20)
m$IV <- (m$P1-m$P0)*m$WOE
m$IV[nrow(m)] <- sum(m$IV[-c(1,nrow(m))])
m0 <- rbind(m0, m, m1)
}
write.csv(m0, file='scorecard.csv', row.names=F)
print(m0)

for(i in varid){
m00 <- m0[m0[,1]==names(datainput1)[i] & !is.na(m0[,1]) & !is.na(m0[,11]),c(1,2,11)]
score_var <- paste('score_',names(datainput1)[i],sep='')
datainput1[[score_var]] <- round(m00$score[match(datainput1[, i], m00$value)])
datainput1[[score_var]][is.na(datainput1[, i])] <- round(m00$score[1])
rm(i)
}

return(list(scorecard=m0, scoredata=datainput1))
}

# scard_data <- f_scard(datainput1=a3_1, model1=model_choose5)

