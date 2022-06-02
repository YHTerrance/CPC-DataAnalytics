library(lavaan)
library(xlsx)
library(tidyverse)
library(gridExtra)
library(data.table)
library(ggbiplot)
library(OpenMx)
library(tidySEM)
library(ggplot2)
library(dplyr)
library(semPlot)
library(Hmisc)
dataset <- read.xlsx("data_oil_pollute.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_pollute <- scale((dataset[,27:34]))
dataset <- cbind(dataset[,2:8], dataset_std_pollute)
#取得PCA
pca_pollute <- prcomp(formula =~ Hg + Cd + Cr + Cu + Ni + Pb + Zn + As,   
                    data = data.frame(dataset_std_pollute),
                    scale = TRUE)
# 取得PCA中的參數
pca_pollute_vars <- (pca_pollute$sdev)^2
pca_pollute_props <- pca_pollute_vars / sum(pca_pollute_vars)
cumulative.pca_pollute_props <- cumsum(pca_pollute_props)

top5_pca_pollute.data <- pca_pollute$x[, 1:5]
top5.pca_pollute.eigenvector <- pca_pollute$rotation[, 1:5]
first.pca_pollute <- top5.pca_pollute.eigenvector[, 1]
second.pca_pollute <- top5.pca_pollute.eigenvector[, 2]
third.pca_pollute <- top5.pca_pollute.eigenvector[, 3]
dotchart(first.pca_pollute[order(first.pca_pollute, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(second.pca_pollute[order(second.pca_pollute, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC2",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(third.pca_pollute[order(third.pca_pollute, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC3",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

## pca1 & pca2
pca_pollute_rotation<-data.frame(pca_pollute$rotation[, 1:5])
ggbiplot(pca_pollute,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
         theme_dark() + 
         scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("../plot/pollute_PC1&PC2_season.png")


ggbiplot(pca_pollute,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("../plot/pollute_PC1&PC2_position.png")

## pca2 & pca3

ggbiplot(pca_pollute, choice=2:3,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("../plot/pollute_PC2&PC3_season.png")


ggbiplot(pca_pollute,choice=2:3, scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("../plot/pollute_PC2&PC3_position.png")


## pca1 & pca3

ggbiplot(pca_pollute, choice=c(1,3),scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("../plot/pollute_PC1&PC3_season.png")


ggbiplot(pca_pollute,choice=c(1,3), obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("../plot/pollute_PC1&PC3_position.png")


