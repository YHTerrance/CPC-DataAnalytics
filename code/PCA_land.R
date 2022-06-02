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
dataset <- read.xlsx("data_oil_land.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_land <- scale(dataset[,23:26])
dataset <- cbind(dataset[2:8], dataset_std_land)
#取得PCA
pca_land <- prcomp(formula =~ sand + rock + coarl + water,   
                    data = data.frame(dataset_std_land),
                    scale = TRUE)
# 取得PCA中的參數
pca_land_vars <- (pca_land$sdev)^2
pca_land_props <- pca_land_vars / sum(pca_land_vars)
cumulative.pca_land_props <- cumsum(pca_land_props)

top2_pca_land.data <- pca_land$x[, 1:4]
top2.pca_land.eigenvector <- pca_land$rotation[, 1:2]
first.pca_land <- top2.pca_land.eigenvector[, 1]
second.pca_land <- top2.pca_land.eigenvector[, 2]
dotchart(first.pca_land[order(first.pca_land, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(second.pca_land[order(second.pca_land, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC2",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

## pca1 & pca2
pca_land_rotation<-data.frame(pca_land$rotation[, 1:2])
ggbiplot(pca_land,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
         theme_dark() + 
         scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("../plot/land_PC1&PC2_pos.png")

pca_land_rotation<-data.frame(pca_land$rotation[, 1:2])
ggbiplot(pca_land,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("../plot/land_PC1&PC2_season.png")
