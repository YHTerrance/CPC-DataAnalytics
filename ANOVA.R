library(lavaan)
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
library(gplots)
dataset <- read.xlsx("data_oil_mod.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_water <- scale(cbind(dataset[,11:20],dataset[,35:36]))

#取得PCA
pca_water <- prcomp(formula =~ temp+sal+pH+DO+Turb+NO2+NO3+NH3+PO43,   
                    data = data.frame(dataset_std_water),
                    scale = TRUE)

# PC1 : -0.479 * NO3, 0.411 * temp, -0.393 * NO2
# PC2 :  0.628 * DO, -0.438 * NH3
# PC3 :  0.723 * pH, 
# PC4 : -0.745 * Turb

PC1 <- -0.479 * dataset_std_water[,7] + 0.411 * dataset_std_water[,1] - 0.393 * dataset_std_water[,6]
PC2 <- 0.628 * dataset_std_water[,4] - 0.438 * dataset_std_water[,8]
PC3 <- 0.723 * dataset_std_water[,3]
PC4 <- -0.745 * dataset_std_water[,5]

PC_score <- data.frame(cbind(dataset[,2],dataset[,3], as.character(dataset[,6]),PC1, PC2, PC3, as.numeric(PC4)))
colnames(PC_score) <- c('year','month','pos', 'PC1', 'PC2', 'PC3', 'PC4')

tapply(PC_score$PC1,PC_score$year,mean)
aov_pc = aov(PC1 ~ factor(year), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
ggplot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(PC1 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

