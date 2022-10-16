library(MASS)
library(ggplot2)
library(e1071)
library(isotree)

USArrests_data <- USArrests
View(USArrests_data)
str(USArrests_data)

nrow(USArrests_data)
head(USArrests_data)

g <- ggplot()
g <- g + theme_bw()
g <- g + geom_point(data = USArrests_data, aes(Murder, Rape), color='brown')
g <- g+ggtitle('Murder vs Rape')+theme(plot.title = element_text(size=15,                                                                                 face="bold", margin = margin(10, 0, 10, 0)))
g <- g + labs(x = "Murder", y="Rape") +theme(text =element_text(size=15))
g


# kernel density function
USArrests_mat <- as.matrix(USArrests_data[,1:4])
USArrests_mat
USArrests_den <- density(USArrests_mat)
USArrests_den

df_density <- as.data.frame(USArrests_den$y)
df_density

min_density <- min(df_density$`USArrests_den$y`)
mean_density <- mean(df_density$`USArrests_den$y`)
bench <- 0.20*min_density/mean_density
bench

df_density$outlier <- ifelse(df_density$`USArrests_den$y`< bench,0,1)
densities <- df_density
densities$outlier <- as.factor(densities$outlier)
levels(densities$outlier)

summary(densities$outlier)
plot(densities$outlier, main="Anomaly detection using kernal density ftn",
     xlab="Levels", ylab="Number ", pch=19, col=hcl(c(0, 120, 240), 50, 70), ylim=c(0,500))


# One-Class SVM
model_oneclasssvm <- svm(USArrests_data,type='one-classification',kernel = "radial",gamma=0.05,nu=0.05)
model_oneclasssvm

pred_oneclasssvm <- predict(model_oneclasssvm,USArrests_data)
pred_oneclasssvm
summary(pred_oneclasssvm)

plot(pred_oneclasssvm, main="Anomaly detection using One-Class SVM",
     xlab="Cities", ylab="outliers ", pch=19, col='firebrick', xlim=c(0,50))


# Isolation Forest
iforest <- isolation.forest(USArrests_data, ntrees = 10, nthreads = 1)

USArrests_data$pred <- predict(iforest, USArrests_data, type = "score")
USArrests_data$outlier <- as.factor(ifelse(USArrests_data$pred >=0.50, "outlier", "normal"))
USArrests_data$outlier

summary(USArrests_data$outlier)

plot(USArrests_data$outlier, main="Anomaly detection using Isolation Forest",
     xlab="levels", ylab="Number", pch=19, col='dodgerblue', ylim=c(0,40))
