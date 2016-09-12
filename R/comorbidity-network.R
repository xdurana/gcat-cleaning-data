library(circlize)

relative_risk <- read.csv2('output/relative-risk/data.csv', sep = ',', stringsAsFactors = FALSE)
rr_matrix <- subset(relative_risk, select = -c(1))
rr_matrix <- as.matrix(as.data.frame(lapply(rr_matrix, as.numeric)))
rr_matrix[lower.tri(rr_matrix, diag = FALSE)] <- 0

colnames(rr_matrix) <- gsub('ENFERMEDADES_', '', colnames(rr_matrix))
rownames(rr_matrix) <- colnames(rr_matrix)

threshold <- 10
rr_matrix[rr_matrix <threshold] <- rr_matrix[rr_matrix <threshold]*0

circos.par(gap.degree = 8)

chordDiagram(rr_matrix, directional = FALSE, annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA)