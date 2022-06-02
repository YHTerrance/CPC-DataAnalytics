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

#取得PCA
pca_land <- prcomp(formula =~ sand + rock + coarl + water,   
                    data = data.frame(dataset_std_land),
                    scale = TRUE)
# 取得PCA中的參數
pca_land_vars <- (pca_land$sdev)^2
pca_land_props <- pca_land_vars / sum(pca_land_vars)
cumulative.pca_land_props <- cumsum(pca_land_props)

top5_pca_land.data <- pca_land$x[, 1:4]
top5.pca_land.eigenvector <- pca_land$rotation[, 1:5]
first.pca_land <- top5.pca_land.eigenvector[, 1]
second.pca_land <- top5.pca_land.eigenvector[, 2]
third.pca_land <- top5.pca_land.eigenvector[, 3]
dotchart(first.pca_land[order(first.pca_land, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(second.pca_land[order(second.pca_land, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC2",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

dotchart(third.pca_land[order(third.pca_land, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC3",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")

## pca1 & pca2
pca_land_rotation<-data.frame(pca_land$rotation[, 1:5])
cumulative.pca_land_props
ggbiplot(pca_land,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
         theme_dark() + 
         scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("plot/PC1&PC2_season.png")


ggbiplot(pca_land,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("plot/PC1&PC2_position.png")

## pca2 & pca3

ggbiplot(pca_land, choice=2:3,scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("plot/PC2&PC3_season.png")


ggbiplot(pca_land,choice=2:3, scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("plot/PC2&PC3_position.png")


## pca1 & pca3

ggbiplot(pca_land, choice=c(1,3),scale=1, obs.scale = 1, var.scale = 1, 
         groups = dataset$season, labels = dataset$season, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue"))
ggsave("plot/PC1&PC3_season.png")


ggbiplot(pca_land,choice=c(1,3), obs.scale = 1, var.scale = 1, 
         groups = dataset$pos, labels = dataset$pos, colour = clarity) +
  theme_dark() + 
  scale_color_manual(values=c("orange", "purple", "red", "blue", "yellow"))
ggsave("plot/PC1&PC3_position.png")


cfa_model <-'
            # measurement model
            PC1 =~ temp + NO3
            PC2 =~ DO + NH3
            PC3 =~ 1*pH
            '

cfa_res <- cfa(cfa_model, data = dataset_std_land)
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
            PC3 =~ 1*pH
            
            # regression model
            non_CCA_cover ~ PC1+PC2+PC3
            CCA_coverage ~ PC1+PC2+PC3
            '
semModel_res = sem(sem_model, data=dataset_std_land)
summary(semModel_res, fit.measures=T)
semPaths(object = semModel_res,
         whatLabels = "std",
         edge.label.cex = 1,
         layout = "tree",
         rotation = 2,
         color = "lightblue")


