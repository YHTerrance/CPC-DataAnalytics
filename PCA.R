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
dataset <- read.xlsx("data_oil_mod.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_water <- scale(cbind(dataset[,11:20],dataset[,35:36]))

#取得PCA
pca_water <- prcomp(formula =~ temp+sal+pH+DO+Turb+NO2+NO3+NH3+PO43,   
                    data = data.frame(dataset_std_water),
                    scale = TRUE)
# 取得PCA中的參數
pca_water_vars <- (pca_water$sdev)^2
pca_water_props <- pca_water_vars / sum(pca_water_vars)
cumulative.pca_water_props <- cumsum(pca_water_props)

top5_pca_water.data <- pca_water$x[, 1:5]
top5.pca_water.eigenvector <- pca_water$rotation[, 1:5]
first.pca_water <- top5.pca_water.eigenvector[, 1]
second.pca_water <- top5.pca_water.eigenvector[, 2]
third.pca_water <- top5.pca_water.eigenvector[, 3]
fourth.pca_water <- top5.pca_water.eigenvector[,4]
fifth.pca_water <- top5.pca_water.eigenvector[,5]
dotchart(first.pca_water[order(first.pca_water, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(second.pca_water[order(second.pca_water, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC2",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(third.pca_water[order(third.pca_water, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC3",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")
dotchart(fourth.pca_water[order(fourth.pca_water, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for P4",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")
dotchart(fifth.pca_water[order(fifth.pca_water, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC5",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

## pca1 & pca2
pca_water_rotation<-data.frame(pca_water$rotation[, 1:5])
cumulative.pca_water_props
ggbiplot(pca_water,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
         theme_dark() + 
         scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("plot/PC1&PC2_season.png")


ggbiplot(pca_water,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("plot/PC1&PC2_position.png")

## pca2 & pca3

ggbiplot(pca_water, choice=2:3,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("plot/PC2&PC3_season.png")


ggbiplot(pca_water,choice=2:3, scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("plot/PC2&PC3_position.png")


## pca1 & pca3

ggbiplot(pca_water, choice=c(1,3),scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("plot/PC1&PC3_season.png")


ggbiplot(pca_water,choice=c(1,3), obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("plot/PC1&PC3_position.png")


cfa_model <-'
            # measurement model
            PC1 =~ temp + NO3
            PC2 =~ DO + NH3
            PC3 =~ pH
            '

cfa_res <- cfa(cfa_model, data = dataset_std_water)
summary(cfa_res, fit.measures = T, rsq=T)
semPaths(object = cfa_res,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree",
         rotation = 2,
         color = "lightblue")

sem_model <-'
            # measurement model
            PC1 =~ temp + NO3
            PC2 =~ DO + NH3
            PC3 =~ pH
            
            # regression model
            non_CCA_cover ~ PC1+PC2+PC3
            CCA_coverage ~ PC1+PC2+PC3
            '
semModel_res = sem(sem_model, data=dataset_std_water)
summary(semModel_res, fit.measures=T)
semPaths(object = semModel_res,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree",
         rotation = 2,
         color = "lightblue")


