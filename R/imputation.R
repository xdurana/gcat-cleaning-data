library(tidyverse)
library(mice)
library(VIM)
library(lattice)
library(missMDA)
library(FactoMineR)

heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))
variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))

ds <- heritability %>%
  select(
    -one_of((variables %>% filter(type %in% c('date', 'string')))$name)
  )

data_women <- ds %>% filter(gender == 1)
data_men <- ds %>% filter(gender == 0)

ds.women <- prepare(data_women, variables, 'women')
ds.men <- prepare(data_men, variables, 'men')

impute_mice(ds.women, 'women')
impute_mice(ds.men, 'men')

prepare <- function(data, variables, name) {

  # imputation procedure from https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
  # http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
  # if missing data for a certain feature is more than 5% then leave that feature out
  
  variables.complete <- variables %>%
    filter(
      nas < 1*nrow(data)
    )
  
  data.complete <- data %>%
    select(
      one_of(variables.complete$name)
    )
  
  # filter participants with multiple missings
  
  hist(rowSums(is.na(data.complete)), breaks = 0:80)
  data.complete.rows <- data.complete[rowSums(is.na(data.complete)) < nrow(variables.complete)*0.05,]
  
  # select numeric variables
  
  variables.complete.to_impute_numeric <- variables.complete %>%
    filter(
      #type %in% c('numeric') &
        !(name %in% c('bmi', 'whr', 'days_follow_up'))
    )
  
  data.to_impute <- data.complete.rows %>%
    select(
      one_of(variables.complete.to_impute_numeric$name)
    )
  
  ds <- data.to_impute %>%
    select(
      -contains('icd9_'),
      -contains('atc_')
    )
  
  png(file.path('output/check/heritability', sprintf('missings_%s.png', name)), width = 800, height = 600)
  aggr_plot <- aggr(
    ds,
    col=c('navyblue','red'), 
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(ds),
    cex.axis=.7,
    gap=0,
    ylab=c("Histogram of missing data","Pattern")
  )
  dev.off()
  
  #marginplot(ds[, c("height","waist")])
  
  ds
}  

impute_mice <- function(ds, name) {
  
  data.imp <- mice(ds, seed = 1233)
  summary(data.imp)
  data.imputed <- complete(data.imp)
  
  # Inspecting the distribution of original and imputed data
  png(file.path('output/check/heritability', sprintf('xyplot_%s.png', name)), width = 800, height = 600)
  xyplot(data.imp, weight ~ height + hip + waist, pch=18, cex=1)
  dev.off()
  
  # The density of the imputed data for each imputed dataset
  png(file.path('output/check/heritability', sprintf('density_%s.png', name)), width = 800, height = 600)
  densityplot(data.imp)
  dev.off()
  
  # The distributions of the variables as individual points
  #stripplot(data.imp, pch = 20, cex = 1.2)
  
  # PCA
  png(file.path('output/check/heritability', sprintf('pca_%s.png', name)), width = 800, height = 600)
  PCA(data.imputed, scale.unit=TRUE, ncp=5, graph=T)
  dev.off()
}

impute_pca <- function(data.to_impute) {
  
  nb <- estim_ncpPCA(data.to_impute[1:2], scale = TRUE)
  comp <- imputePCA(data.to_impute, ncp = 2, scale = TRUE)
  res.pca <- PCA(comp$completeObs)
}

mca <- function(data.to_impute) {
  
  data(vnf)
  
  ds <- data %>%
    select(
      age,
      height,
      weight,
      hip,
      waist
    ) %>%
    slice(1:100)
  
  nb <- estim_ncpPCA(ds, scale = TRUE)
  imputeFAMD(ds)
  
  mca1 = MCA(data, graph = FALSE)
  PCA(ds, scale.unit=TRUE, ncp=5, graph=T)
}

mosaic <- function(data.to_impute) {
  library(graphics)
  mosaicplot(data.imp, shade = TRUE, las=2, main = "height")
}