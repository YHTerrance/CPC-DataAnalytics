library(stringr)
library(xlsx)
library(tidyverse)
library(gridExtra)
library(data.table)
library(ggbiplot)
library(ggplot2)
library(dplyr)
library(gplots)
library(ggiraphExtra)
dataset <- read.xlsx("data_oil_land.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_land <- scale(cbind(dataset[,23:26]))
dataset <- cbind(dataset[2:8], dataset_std_land)
#取得PCA
pca_land <- prcomp(formula =~ sand + rock + coarl + water,   
                    data = data.frame(dataset_std_land),
                    scale = TRUE)
pca_land_vars <- (pca_land$sdev)^2
pca_land_props <- pca_land_vars / sum(pca_land_vars)
cumulative.pca_land_props <- cumsum(pca_land_props)
# PC1 : 0.681 * sand - 0.71  * coarl 46%
# PC2 : 0.697 * rock - 0.671 * water 28%

PC1 <- 0.681 * dataset[,8] - 0.71  * dataset[,10]
PC2 <- 0.697 * dataset[,9] - 0.671 * dataset[,11]

a <- str_c(dataset[,1] , dataset[,2], sep = ".")
PC_score <- data.frame(cbind(dataset[,1], dataset[,2], a, dataset[,4], dataset[,5],PC1, PC2))
colnames(PC_score) <- c('year','month','time','season','pos', 'PC1' , 'PC2')


######## PC1 ANOVA ###################
aov_pc = aov(PC1 ~ factor(year) + factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(PC1 ~ factor(year) , data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))


aov_pc = aov(PC1 ~ factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(PC1 ~ factor(season) + factor(pos) + factor(season) * factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(PC1 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC1 ~ factor(time), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
######## PC2 ANOVA ###################
aov_pc = aov(PC2 ~ factor(year) + factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC2 ~ factor(year), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC2 ~ factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))


aov_pc = aov(PC2 ~ factor(season) + factor(pos) + factor(season) * factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC2 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))