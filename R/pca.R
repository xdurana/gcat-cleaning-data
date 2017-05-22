library(ggfortify)
library(tidyverse)
library(car)

library(FactoMineR)
library(factoextra)
library(corrplot)

heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))
variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))

vn <- c('entity_id', (variables %>% filter(type %in% c('numeric')) %>% select(name))$name)
ds_numeric <- heritability %>% select(one_of(vn))


vs <- c('entity_id', (variables %>% filter(type %in% c('binary', 'numeric')) %>% select(name))$name)

ds <- heritability %>% select(one_of(vs)) %>% select(-starts_with("atc_")) %>% select(-entity_id)
ds[is.na(ds)] <- 0

### descriptive statistics

ds.active_stats <- data.frame(
  Min = apply(ds, 2, min), # minimum
  Q1 = apply(ds, 2, quantile, 1/4), # First quartile
  Med = apply(ds, 2, median), # median
  Mean = apply(ds, 2, mean), # mean
  Q3 = apply(ds, 2, quantile, 3/4), # Third quartile
  Max = apply(ds, 2, max) # Maximum
)
ds.active_stats <- round(ds.active_stats, 1)
head(ds.active_stats)

### correlation matrix

cor.mat <- round(cor(ds), 2)
head(ds[, 1:6])
corrplot(cor.mat, type="upper", order="hclust", tl.col="black", tl.srt=45)

### men

men <- ds %>%
  filter(gender == 0) %>%
  select(-gender,
         -height,
         -weight,
         -hip,
         -waist,
         -age_at_menarche,
         -age_at_menopause,
         -age_at_fist_birth,
         -oc_use)

men <- men[1:200,]

men_scale <- as.data.frame(scale(men)) %>% rapply(f=function(x) ifelse(is.nan(x),0,x), how="replace")

### PCA

res.pca <- PCA(men_scale, graph = FALSE)
print(res.pca)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

fviz_screeplot(res.pca, ncp=10)
plot(res.pca, choix = "var")
fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="contrib")

plot(res.pca, choix = "ind")
fviz_pca_biplot(res.pca,  geom = "text")
fviz_pca_ind(iris.pca, label="none")

### 

men_scale <- as.data.frame(scale(men)) %>% rapply(f=function(x) ifelse(is.nan(x),0,x), how="replace")

#scatterplotMatrix(men[2:6])
#plot(men$bmi, men$whr)
#text(men$bmi, men$whr, men$icd9_code3_041, cex=0.7, pos=4, col="red")

men_scale.pca <- prcomp(men_scale)
screeplot(men_scale.pca, type="lines")

plot(men_scale.pca$x[,1], men_scale.pca$x[,2])
#text(men_scale.pca$x[,1], men_scale.pca$x[,2], men_scale$icd9_code3_401, cex=0.7, pos=4, col="red")

