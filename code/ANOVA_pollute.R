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
dataset <- xlsx::read.xlsx("data_oil_pollute.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_pollute <- scale((dataset[,27:34]))
dataset <- cbind(dataset[,2:8], dataset_std_pollute)
#取得PCA
pca_pollute <- prcomp(formula =~ Hg + Cd + Cr + Cu + Ni + Pb + Zn + As,   
                    data = data.frame(dataset_std_pollute),
                    scale = TRUE)
pca_pollute_vars <- (pca_pollute$sdev)^2
pca_pollute_props <- pca_pollute_vars / sum(pca_pollute_vars)
cumulative.pca_pollute_props <- cumsum(pca_pollute_props)
# PC1 : 0.421 * Cr + 0.411 * Zn  # 44.9%
# PC2 : -0.523 * Cd -0.475 * Hg  # 21.2%
PC1 <- -0.421 * dataset[,10] + 0.411 * dataset[,14]
PC2 <- -0.523 * dataset[,9] -0.475 * dataset[,8]

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
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC1 ~ factor(season) + factor(pos) + factor(season) * factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC1 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(PC1 ~ factor(time), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
ggHSD(TukeyHSD(aov_pc))
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
