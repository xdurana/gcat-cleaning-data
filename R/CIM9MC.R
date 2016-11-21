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

#' @title Merge CIM9MC
mergeCIM9MC <- function() {
  summary <- read.csv2(file.path('inst/extdata', 'summary.csv'), sep = ',')
  summary.cim9 <- read.csv2(file.path('inst/extdata/conditions', 'summary-cim9.csv'), sep = ';')
  summary.cim9 <- merge(summary, summary.cim9, all.x = TRUE)
  summary.cim9 <- summary.cim9[with(summary.cim9, order(-Freq)),]
  write.table(summary.cim9, 'output/summary-cim9.csv', sep = ',', row.names = FALSE)
}

#' @title Get conditions summary
getSummary <- function() {
 
  cim9mc <- readCIM9MC(file.path('inst/extdata/conditions', 'CIM9MC_2014_2015_20140313.txt'))  
   
  summary.cim9 <- read.csv2(file.path('inst/extdata/conditions', 'summary-cim9.csv'), sep = ';', stringsAsFactors = FALSE) %>%
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
           Descr_gran_grup_clasif,
           Gran_grup_CCS,
           Desc_gran_grup_CCS)
  
  summary.cim9.merge <- summary.cim9.merge %>%
    mutate(Codi_3 = substring(summary.cim9.merge$Codi, 1, 3))
  
  c3 <- cim9mc %>%
    select(Codi, Descr_codi) %>%
    rename(Codi_3 = Codi) %>%
    rename(Descr_codi_3 = Descr_codi)
  
  summary.cim9.merge.3 <- merge(summary.cim9.merge, c3, all.x = TRUE) %>%
    select(Text,
           Freq,
           Codi,
           Descr_codi,
           Codi_3,
           Descr_codi_3,
           Gran_grup_classif,
           Descr_gran_grup_clasif,
           Gran_grup_CCS,
           Desc_gran_grup_CCS)
  
  summary.cim9.merge.3[is.na(summary.cim9.merge.3)] <- ''
  write.csv2(summary.cim9.merge.3, 'output/conditions/summary-cim9.csv', row.names = FALSE)
}