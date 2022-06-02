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
dataset <- read.xlsx("data_oil_mod.xlsx", sheetIndex = 1, encoding="UTF-8-BOM", fill = TRUE)


# 標準化
dataset_std_water <- scale(cbind(dataset[,11:20],dataset[,35:36]))

#取得PCA
pca_water <- prcomp(formula =~ temp+sal+pH+DO+Turb+NO2+NO3+NH3+PO43,   
                    data = data.frame(dataset_std_water),
                    scale = TRUE)
pca_water_vars <- (pca_water$sdev)^2
pca_water_props <- pca_water_vars / sum(pca_water_vars)
cumulative.pca_water_props <- cumsum(pca_water_props)
# PC1 : -0.479 * NO3, 0.411 * temp, -0.393 * NO2  # 季節和營養鹽 39%
# PC2 :  0.628 * DO, -0.438 * NH3 # 氮循環 16%
# PC3 :  0.723 * pH, # 酸鹼度 11%
# PC4 : -0.745 * Turb

PC1 <- -0.479 * dataset_std_water[,7] + 0.411 * dataset_std_water[,1] - 0.393 * dataset_std_water[,6]
PC2 <- 0.628 * dataset_std_water[,4] - 0.438 * dataset_std_water[,8]
PC3 <- 0.723 * dataset_std_water[,3]
# PC4 <- -0.745 * dataset_std_water[,5]
a <- str_c(dataset[,2] , dataset[,3], sep = ".")
PC_score <- data.frame(cbind(dataset[,2],dataset[,3], a, dataset[,5], as.character(dataset[,6]),PC1, PC2, PC3))
colnames(PC_score) <- c('year','month','time','season','pos', '生物代謝物', '氮循環', '酸鹼度')


######## PC1 ANOVA ###################
aov_pc = aov(生物代謝物 ~ factor(year) + factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(生物代謝物 ~ factor(year) , data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))


aov_pc = aov(生物代謝物 ~ factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(生物代謝物 ~ factor(season) + factor(pos) + factor(season) * factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(生物代謝物 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(生物代謝物 ~ factor(time), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
######## PC2 ANOVA ###################
aov_pc = aov(氮循環 ~ factor(year) + factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(氮循環 ~ factor(year), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(氮循環 ~ factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))


aov_pc = aov(氮循環 ~ factor(season) + factor(pos) + factor(season) * factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(氮循環 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))
######## PC3 ANOVA ###################
aov_pc = aov(酸鹼度 ~ factor(year) + factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(酸鹼度 ~ factor(season), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))

aov_pc = aov(酸鹼度 ~ factor(year), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))



aov_pc = aov(酸鹼度 ~ factor(season) + factor(pos) + factor(season) * factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))

aov_pc = aov(酸鹼度 ~ factor(pos), data = PC_score)
summary(aov_pc)
TukeyHSD(aov_pc)
plot(TukeyHSD(aov_pc, conf.level = 0.95))
ggHSD(TukeyHSD(aov_pc))


