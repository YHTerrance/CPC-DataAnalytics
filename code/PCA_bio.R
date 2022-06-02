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
dataset <- read.xlsx("data_oil_bio.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_water <- scale(cbind(dataset[,11:20]))

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

cfa_model <-'
            # measurement model
            PC1 =~ temp + NO3
            PC2 =~ DO + NH3
            PC3 =~ 1*pH
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
            PC3 =~ 1*pH
            
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


