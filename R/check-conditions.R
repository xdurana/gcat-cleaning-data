library(dplyr)
library(tidyr)
library(xlsx)
library(stringi)
library(limma)
library(readr)
library(data.table)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
directory_conditions <- 'output/check/conditions'

#' @title Get conditions from GCAT
#' 
#' @export
get_conditions <- function() {

  questionari <- read.csv(file.path(directory, 'QUESTIONARI/data.csv'), sep = ',')
  participants <- read.csv(file.path(directory, 'Participants/data.csv'), sep = ',')
  all <- merge(participants, questionari, by='entity_id')
  
  ### DIABETES
  
  #all$ENFERMEDADES_DIABETES_T1DM <- ifelse(all$DIABETES_TIPO == 1, 1, 0)
  #all$ENFERMEDADES_DIABETES_T2DM <- ifelse(all$DIABETES_TIPO == 2, 1, 0)
  #all$ENFERMEDADES_DIABETES_GESTACIONAL <- ifelse(all$DIABETES_TIPO == 3, 1, 0)
  
  ### MAIN CONDITIONS
  
  enfermedades <- colnames(all)[grep('^ENFERMEDADES_', colnames(all))]
  enfermedades <- enfermedades[!enfermedades %in% colnames(all)[grep('^ENFERMEDADES_E[0-9]', colnames(all))]]
  
  enfermedades.ds <- all[, c('entity_id', enfermedades)]
  enfermedades.ds.long <- gather(enfermedades.ds, condition, cardinal, 2:ncol(enfermedades.ds), factor_key=TRUE)
  enfermedades.ds.long <- enfermedades.ds.long[enfermedades.ds.long$cardinal > 0 & !is.na(enfermedades.ds.long$cardinal),]

  columns <- c('entity_id', 'condition')

  otras <- colnames(all)[grep('^ENFERMEDADES_E[0-9]$', colnames(all))]
  otras.ds <- all[, c('entity_id', otras)]
  otras.ds.long <- gather(otras.ds, cardinal, condition, ENFERMEDADES_E1:ENFERMEDADES_E7, factor_key=TRUE)
  otras.ds.long <- otras.ds.long[otras.ds.long$condition != '',]
  otras.ds.long$condition <- toupper(otras.ds.long$condition)
  all.ds <- rbind(enfermedades.ds.long[, columns], otras.ds.long[, columns])

  ### ALL CONDITIONS
  
  all.ds <- filter(all.ds, (!(condition %in% c('ENFERMEDADES_NINGUNA', 'ENFERMEDADES_NS_NC', 'ENFERMEDADES_OTRAS'))))
  all.ds$condition <- gsub("ENFERMEDADES_", "", all.ds$condition)
  #all.ds$condition <- stri_trans_general(all.ds$condition, "Latin-ASCII")
  
  ### TRANSLATE
  
  all.ds <- all.ds %>% mutate(count = 1)
  
  ### REMOVE NOT WANTED CHARACTERS
  
  all.ds <- all.ds %>% mutate(condition = iconv(condition, to='ASCII//TRANSLIT'))
  
  ### SUMMARY
  
  all.summary <- arrange(data.frame(table(all.ds$condition)), desc(Freq))
  
  ### FILTER BY FREQUENCY
  
  all.ds <- all.ds %>% unique()
  
  all.wide <- all.ds %>% spread(condition, count)
  all.wide[is.na(all.wide)] <- 0
  
  write.csv2(all.summary, file.path(directory_conditions, 'text/summary.csv'), row.names = FALSE, quote = TRUE)
  write.csv2(all.ds, file.path(directory_conditions, 'text/long.csv'), row.names = FALSE, quote = TRUE)
  write.csv2(all.wide, file.path(directory_conditions, 'text/wide.csv'), row.names = FALSE, quote = TRUE)
}

#' @title Get conditions dataset
#' @param filename Conditions file
#' @export
get_conditions_ds <- function(filename) {
  long <- fread(filename)
  summary <- fread('inst/extdata/conditions/summary-cim9.csv')
  phenotype <- merge(long, summary, by.x = 'condition', by.y = 'Text')
  phenotype.icd9 <- phenotype %>%
    filter(!is.na(Codi) & Codi != '') %>%
    mutate(Descr_codi = stri_trans_general(Descr_codi, "Latin-ASCII")) %>%
    mutate(nom = gsub(' ', '_', paste(Codi, Descr_codi)),
           grup = gsub(' ', '_', paste(Gran_grup_classif, Descr_gran_grup_clasif)))
  phenotype.icd9
}

#' @title Get conditions dataset
#' @export
get_conditions_cod_3_ds <- function(filename) {
  phenotype.icd9.nom <- get_conditions_ds(filename) %>%
    mutate(Descr_codi_3 = stri_trans_general(Descr_codi_3, "Latin-ASCII")) %>%
    mutate(ICD9 = gsub(' ', '_', paste(Codi_3, Descr_codi_3))) %>%
    select(entity_id, ICD9) %>%
    dplyr::rename(id=entity_id) %>%
    gather(variable, value, ICD9) %>%
    unique()
  phenotype.icd9.nom
}

#' @title Get conditions from genotyped participants dataset
#' @export
get_conditions_genotyped_ds <- function() {
  conditions <- get_conditions_ds(file.path(directory_conditions, 'others_long.csv')) %>%
    select(-Freq)
  
  as.data.table(table(conditions$condition)) %>% arrange(desc(N)) %>% write.xlsx2('output/conditions/all.xlsx')
  
  genotyped <- read.csv2(file.path('output/genotyped', 'data.csv'), sep = ',', stringsAsFactors = FALSE) %>%
    merge(conditions)
  genotyped
}

#' @title Summary of the conditions of the genotyped participants
#' @export
save_conditions_genotyped <- function() {

  conditions_genotyped <- get_conditions_genotyped_ds()
  as.data.table(table(conditions_genotyped$condition)) %>% arrange(desc(N)) %>% write.xlsx2('output/conditions-genotyped/genotyped.xlsx')
  
  conditions_genotyped.rl <- conditions_genotyped %>%
    filter(sampleType == 'RL')
  
  grouped.genotyped <- group_by(conditions_genotyped, condition, Codi, Descr_codi)
  grouped.rl <- group_by(conditions_genotyped.rl, condition, Codi, Descr_codi)

  ds <- summarise(grouped.genotyped, count=length(condition))
  ds.rl <- summarise(grouped.rl, count.rl=length(condition))
  
  ds <- merge(
    summarise(grouped.rl, count.rl=length(condition)),
    summarise(grouped.genotyped, count=length(condition))
  ) %>%
    arrange(desc(count))

  write.xlsx(ds, 'icd9_genotyped.xlsx')

  ### VENN DIAGRAM
  
  conditions <- read.csv2(file.path(directory_conditions, 'wide.csv'), sep = ',', stringsAsFactors = FALSE)
  conditions <- read.csv2(file.path('output/genotyped', 'data.csv'), sep = ',', stringsAsFactors = FALSE) %>%
    merge(conditions)
  
  conditions <- conditions %>%
    filter(sampleType == 'RL')
  
  HIPERCOLESTEROLEMIA <- (conditions$ENFERMEDADES_HIPERCOLESTEROLEMIA == 1)
  ALERGIA <- (conditions$ENFERMEDADES_ALERGIA == 1)
  HTA <- (conditions$ENFERMEDADES_HTA == 1)
  ASMA <- (conditions$ENFERMEDADES_ASMA == 1)
  RINITIS <- (conditions$ENFERMEDADES_RINITIS == 1)
  DIABETIS <- (conditions$ENFERMEDADES_DIABETES == 1)

  vennDiagram(
    vennCounts(
      cbind(
        HIPERCOLESTEROLEMIA,
        HTA,
        DIABETIS,
        ALERGIA,
        ASMA
      )
    ),
    circle.col = c("red", "blue", "green", "chocolate", "deepskyblue")
  )
  
  vennDiagram(
    vennCounts(
      cbind(
        HIPERCOLESTEROLEMIA,
        HTA,
        DIABETIS
      )
    ),
    circle.col = c("red", "blue", "green", "chocolate", "deepskyblue")
  )
}

#' @title Merge CIM9MC
mergeCIM9MC <- function() {
  summary <- fread(file.path(directory_conditions, 'text/summary.csv')) %>%
    dplyr::rename(
      Text=Var1
    )
  summary.cim9 <- fread('inst/extdata/conditions/summary-cim9-updated.csv') %>%
    select(
      -Freq
    )
  summary.cim9 <- merge(summary, summary.cim9, all.x = TRUE)
  summary.cim9 <- summary.cim9[with(summary.cim9, order(-Freq)),]
  write.csv(summary.cim9, file.path(directory_conditions, 'summary-cim9-updated.csv'), row.names = FALSE, na = '')
}

#' @title Read CIM9CM file
#' @param filename name of the CIM9CM file
#' @export
#' @examples
#' readCIM9MC(file.path('inst/extdata/conditions', 'CIM9MC_2014_2015_20140313.txt'))
readCIM9MC <- function(filename) {
  CIM9MC <- read.fwf(
    file = filename,
    widths = c(1, 10, 8, 1, 15, 255, 60, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 3, 1, 1, 2, 255, 5, 255, 2, 255),
    header = FALSE,
    fileEncoding = 'UTF-8',
    stringsAsFactors = FALSE
  )
  
  colnames(CIM9MC) <- c(
    
    'Accio',
    'Classificacio',
    'Data_inici_vig',
    'Tipus_codi',
    'Codi',
    'Descr_codi',
    'Descr_abreuj_codi',
    'Indespecific',
    'Criteris_excl_gen',
    'Criteris_excl_pro',
    'Criteris_incl',
    'Codi_add',
    'Perinatal',
    'Causa_E_assoc',
    'Edat_incongr',
    'Edat_maxima',
    'Edat_minima',
    'Compat_sexe',
    'Dx_secund',
    'Dx_cronic',
    'Dx_Charlson',
    'Px_dx_tx',
    'Px_bilat',
    'Gran_grup_classif',
    'Descr_gran_grup_clasif',
    'Categoria_CCS',
    'Descr_categoria_CCS',
    'Gran_grup_CCS',
    'Desc_gran_grup_CCS'
  )
  
  CIM9MC <- CIM9MC[CIM9MC$Tipus_codi == 'D', ]
  CIM9MC
}

#' @title Explore conditions by text
#' @param text text to match
#' @examples
#' explore('[Hh]ipercolesterolemia')
exploreCIM9MC <- function(text) {
  icd9 <- CIM9MC[grepl(sprintf('\\b%s', text), CIM9MC$Descr_codi) | grepl(sprintf('\\b%s', text), CIM9MC$Descr_abreuj_codi) ,]
  View(icd9)
}

#' @title Get conditions summary
getSummary <- function() {
  
  cim9mc <- readCIM9MC(file.path('inst/extdata/conditions', 'CIM9MC_2014_2015_20140313.txt'))
  
  summary.cim9 <- fread(file.path(directory_conditions, 'summary-cim9-updated.csv')) %>%
    select(Text,
           Freq,
           Codi)
  
  cim9mc$Codi <- gsub("^\\s+|\\s+$", "", cim9mc$Codi)
  cim9mc$Descr_codi <- gsub("^\\s+|\\s+$", "", cim9mc$Descr_codi)
  cim9mc$Gran_grup_classif <- gsub("^\\s+|\\s+$", "", cim9mc$Gran_grup_classif)
  cim9mc$Descr_gran_grup_clasif <- gsub("^\\s+|\\s+$", "", cim9mc$Descr_gran_grup_clasif)
  cim9mc$Gran_grup_CCS <- gsub("^\\s+|\\s+$", "", cim9mc$Gran_grup_CCS)
  cim9mc$Desc_gran_grup_CCS <- gsub("^\\s+|\\s+$", "", cim9mc$Desc_gran_grup_CCS)
  
  cim9mc.select <- select(cim9mc,
                          Codi,
                          Descr_codi,
                          Gran_grup_classif,
                          Descr_gran_grup_clasif,
                          Gran_grup_CCS,
                          Desc_gran_grup_CCS)
  
  summary.cim9.merge <-
    merge(summary.cim9, cim9mc.select, all.x = TRUE, by.x = 'Codi', by.y = 'Codi') %>%
    select(Text,
           Freq,
           Codi,
           Descr_codi,
           Gran_grup_classif,
           Descr_gran_grup_clasif
           )
  
  summary.cim9.merge <- summary.cim9.merge %>%
    mutate(Codi_3 = substring(summary.cim9.merge$Codi, 1, 3))
  
  c3 <- cim9mc %>%
    select(Codi, Descr_codi) %>%
    dplyr::rename(Codi_3 = Codi) %>%
    dplyr::rename(Descr_codi_3 = Descr_codi)
  
  summary.cim9.merge.3 <- merge(summary.cim9.merge, c3, by='Codi_3', all.x = TRUE) %>%
    select(Text,
           Freq,
           Codi,
           Descr_codi,
           Codi_3,
           Descr_codi_3,
           Gran_grup_classif,
           Descr_gran_grup_clasif
           )
  
  summary.cim9.merge.3[is.na(summary.cim9.merge.3)] <- ''
  
  summary.cim9.merge.3 <- summary.cim9.merge.3 %>%
    arrange(desc(Freq))
  
  write.csv2(summary.cim9.merge.3, file.path(directory_conditions, 'summary-cim9-updated.csv'), row.names = FALSE, fileEncoding = 'utf-8')
}

save <- function() {
  
  participants <- read_csv(file.path(directory, 'Participants/data.csv')) %>%
    select(
      entity_id
    )
  
  save_ds(
    participants,
    get_conditions_ds(file.path(directory_conditions, 'text/long.csv')) %>%
      dplyr::rename(
        codi=Codi
      ) %>%
      select(
        entity_id,
        codi
      ),
    'icd9'
    )
  
  save_ds(
    participants,
    get_conditions_ds(file.path(directory_conditions, 'text/long.csv')) %>%
      dplyr::rename(
        codi=Codi_3
      ) %>%
      select(
        entity_id,
        codi
      ),
    'icd9_3'
  )
  
  save_ds(
    participants,
    get_conditions_ds(file.path(directory_conditions, 'text/long.csv')) %>%
      dplyr::rename(
        codi=Gran_grup_classif
      ) %>%
      select(
        entity_id,
        codi
      ),
    'icd9_grup'
  )
  
}

save_ds <- function(participants, ds, prefix) {

  #ICD9 long
  ds %>%
    unique() %>%
    write.csv2(file.path(directory_conditions, file.path(prefix, 'long.csv')), row.names = FALSE, quote = FALSE)
  
  #ICD9 wide
  ds_wide <- ds %>%
    transform(
      value=1
    ) %>%
    unique() %>%
    filter(
      !is.na(codi) & codi != ''
    ) %>%
    spread(codi, value) %>%
    right_join(participants)
  
  ds_wide[is.na(ds_wide)] <- 0
  
  ds_wide %>%
    write.csv2(file.path(directory_conditions, file.path(prefix, 'wide.csv')), row.names = FALSE, quote = FALSE)

  arrange(data.frame(table(ds$codi)), desc(Freq)) %>%
    write.csv2(file.path(directory_conditions, file.path(prefix, 'summary.csv')), row.names = FALSE, quote = FALSE)

  #lapply(unique(icd9_3$icd), function(icd_code) {
  #  attr(icd9_3_wide$variable, icd_code) <- as.character(unique((icd9_3 %>% filter(icd==icd_code))$description))
  #})
}

graphics <- function() {
  read_csv2(file.path(directory_conditions, 'icd9_grup/wide.csv')) %>% correlation_matrix
}

correlation_matrix <- function(ds) {
  
  library(corrplot)
  library(RColorBrewer)
  
  ds <- ds %>%
    select(-entity_id) %>%
    as.matrix()
  
  res <- cor(ds)
  corrplot(res, type = "upper", order = "hclust", tl.col = "black", col=brewer.pal(n=8, name="RdBu"), tl.srt = 45)
}

get_conditions()
mergeCIM9MC()
getSummary()
save()