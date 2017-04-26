library(xlsx)

paisos <- read.xlsx2('/home/labs/dnalab/xduran/Downloads/Untitled spreadsheet.xlsx', sheetIndex = 1, header = FALSE)

fileConn <- file("/home/labs/dnalab/xduran/Downloads/output.txt")
writeLines(paste(shQuote(paisos$X1, type="cmd"), collapse=", "), fileConn)
close(fileConn)
