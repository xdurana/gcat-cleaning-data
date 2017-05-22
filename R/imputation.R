library(tidyverse)
library(mice)
library(VIM)
library(lattice)
library(missMDA)

heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))
variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))

data <- heritability %>%
  filter(
    gender == 1
  ) %>%
  select(
    -one_of((variables %>% filter(type %in% c('date', 'string')))$name)
  )

mca <- function(data.to_impute) {
  library(FactoMineR)
  
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

prepare <- function(data, variables) {

  # imputation procedure from https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
  # http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
  # if missing data for a certain feature is more than 5% then leave that feature out
  
  variables.complete <- variables %>%
    filter(
      nas < 0.05*nrow(data)
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
}  

impute_pca <- function(data.to_impute) {
  
  nb <- estim_ncpPCA(data.to_impute[1:2], scale = TRUE)
  comp <- imputePCA(data.to_impute, ncp = 2, scale = TRUE)
  res.pca <- PCA(comp$completeObs)
}

impute_mice <- function(data.to_impute) {
  
  ds <- data.to_impute %>%
    slice(1:1000)
  
  # impute
  
  aggr_plot <- aggr(
    ds,
    col=c('navyblue','red'), 
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(data.to_impute),
    cex.axis=.7,
    gap=3,
    ylab=c("Histogram of missing data","Pattern")
  )
  
  marginplot(ds[, c("height","waist")])
  
  pbox(ds, pos=1, int=FALSE, cex=1.2)
  
  data.imp <- mice(ds, seed = 1233)
  summary(data.imp)
  data.imputed <- complete(data.imp)
  
  # Inspecting the distribution of original and imputed data
  xyplot(data.imp, weight ~ age + height, pch=18, cex=1)
  
  # The density of the imputed data for each imputed dataset
  densityplot(data.imp)
  
  # The distributions of the variables as individual points
  #stripplot(data.imp, pch = 20, cex = 1.2)
  
  # PCA
  PCA(data.imputed, scale.unit=TRUE, ncp=5, graph=T)
}