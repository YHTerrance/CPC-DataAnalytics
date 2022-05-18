library(utf8)
library(scales)
library(dplyr)
library(janitor)
library(faraway)
library(MASS)
library(lars)
library(pls)
library(factoextra)

rm(list=ls())

saveplot = function(filename) {
  dev.copy(png, filename)
  dev.off()
}


#### Data preprocessing #### 

arcsqrt = function(a) {
  a = a + 0.0001
  a = pmin(1, pmax(-1, a))
  return(asin(sqrt(a)))
} 


# skip first two unused rows
df = read.csv("./data/rawdata.csv", fill=T, header=T)
names(df) = df[2, ]
df = df[-1:-2, ]
# df transform specific columns into numeric type
df[, 10:ncol(df)] = mutate_all(df[, 10:ncol(df)], function(x) as.numeric(as.character(x)))
# perform necessary transformations
df = mutate(df, `DO-absolute` = log(`DO-absolute`), `DO-relative` = arcsqrt(`DO-relative` / 100), NO2 = log(NO2), NO3 = log(NO3), NH3 = log(NH3), `PO43-` = log(`PO43-`), SS = log(SS), `sand-area` = arcsqrt(`sand-area`), `pebble-area` = arcsqrt(`pebble-area`), `reef-area` = arcsqrt(`reef-area`), `water-area` = arcsqrt(`water-area`), Hg = log(Hg), Cd = log(Cd), Cr = log(Cr), Cu = log(Cu), Ni = log(Ni), Pb = log(Pb), Zn = log(Zn), As = log(As), `hermatypic-coral` = arcsqrt(`hermatypic-coral`), `ahermatypic-coral` = arcsqrt(`ahermatypic-coral`)) 


#### Water Quality PCA Analysis ####

# Select only low tide data
df_L = subset(df, tide == "L")
cols = c('season', 'sample', 'temp', 'sal', 'pH', 'DO-absolute', 'DO-relative', 'turb', 'NO2', 'NO3', 'NH3', 'PO43-', 'SS', 'hermatypic-coral', 'ahermatypic-coral')
df_L_water = df_L[cols]
# Remove rows with empty temperature values
df_L_water = df_L_water[!(is.na(df_L_water$temp)), ]

# Identify data with large number of NAs (over 50% makes the factor invalid)
NA_ratio = colSums(is.na(df_L_water)) / nrow(df_L_water)
print(label_percent()(NA_ratio))
invalid_factors = names(NA_ratio[NA_ratio > 0.5])
df_L_water_valid = na.omit(dplyr::select(df_L_water, -all_of(invalid_factors)))
print(head(df_L_water_valid))

# Perform PCA analysis
water = df_L_water_valid[,3:(ncol(df_L_water_valid) - 2)] # set data for PCA analysis here

water.pca = prcomp(water[,], center = TRUE, scale. = TRUE)
attributes(water.pca)
summary(water.pca)

# Scree plot
plot(water.pca, ylim = c(0,4) ,type = "line", main = "Scree plot for Water Quality")
abline(h=1, col="blue")

# Pareto plot
vars = (water.pca$sdev)^2
props = vars / sum(vars)
cumulative.props = cumsum(props)
plot(cumulative.props,ylim = c(0, 1.05), main = "Pareto plot for Water Quality", type="b", col="blue", pch=19)
text(cumulative.props, labels=label_percent()(cumulative.props), cex=0.9, font=2, pos=3)
# saveplot("./plots/pca_pareto.png")
# dev.off()

# Analysis of the composition of the PCs
top3.pca.eigenvector = water.pca$rotation[, 1:3]
top3.pca.eigenvector

first.pca = top3.pca.eigenvector[, 1]
second.pca = top3.pca.eigenvector[, 2]
third.pca = top3.pca.eigenvector[, 3]


plotPC = function(pca, title) {
  dotchart(pca[order(pca, decreasing = F)],
           main = title,
           xlab = "Variable Loadings",
           col = "red")
}

plotPC(first.pca, "PC1")
plotPC(second.pca, "PC2")
plotPC(third.pca, "PC3")

# Variable plot
fviz_pca_var(water.pca,
             col.var = "contrib", # Control variable color using their contributions to the PC
             gradient.cols = c("#70f6ff", "#00AFBB", "#ffd224",
                               "#d8ac00", "#FC4E07", "#a73203"),
             repel = TRUE,     # Avoid text overlapping
             ggtheme = theme_minimal()
)

# Biplot
fviz_pca_biplot(water.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
ggbiplot(water.pca, groups=df_L_water_valid$season)
ggbiplot(water.pca, groups=df_L_water_valid$sample)

#### PCA Regression Analysis ####
eig.val <- get_eigenvalue(water.pca); eig.val # Eigenvalue in PCA is the variance
summary(water.pca)
fviz_eig(water.pca) 

# Perform principal component regression to determine the number of components needed
pcr.fit <- pcr(`hermatypic-coral` ~  temp + sal + pH + `DO-absolute` + `DO-relative` + turb + NO2 + NO3 + NH3 + `PO43-`, data = df_L_water_valid, ncomp = 8, validation='CV', segments=10) # Total 8 components

# RMSE is the average of the mean square error taking from every fold
rmse = function(x,y) {sqrt(mean((x-y)^2))}

# Compute the RMSE of prediction, which is critical for PCR model
rmseCV <- RMSEP(pcr.fit, estimate ='CV'); rmseCV 

plot(rmseCV, xaxt="n", main='Predicted RMSE by number of components') 
axis(1, at=c(0,1,2,3,4,5,6,7,8), labels = c(0,1,2,3,4,5,6,7,8)) # Customize x-axis



