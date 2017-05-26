library(ggplot2)
library(mice)
library(VIM)

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

missings_plot <- function(ds, directory) {
  aggr_plot <- aggr(
    ds,
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(ds),
    ylab=c("Histogram of missing data","Pattern")
  )
  svg(sprintf('output/check/%s/aggr_plot.svg', directory))
  aggr_plot <- aggr(
    ds,
    numbers=TRUE, 
    sortVars=TRUE, 
    labels=names(ds),
    ylab=c("Histogram of missing data","Pattern")
  )
  dev.off()
}

pair_plot <- function(ds, directory, i) {
  marginplot(ds)
  svg(sprintf('output/check/%s/marginplot_%s.svg', directory, i))
  marginplot(ds)
  dev.off()
}

histogram_plot <- function(ds, directory, variable) {
  ggplot(ds, aes_string(x=variable)) +
    geom_histogram(color="darkblue", fill="lightblue") +
    scale_color_grey() + theme_classic() +
    theme(legend.position="top")
  svg(sprintf('output/check/%s/histogram_%s.svg', directory, variable))
  ggplot(ds, aes_string(x=variable)) +
    geom_histogram(color="darkblue", fill="lightblue") +
    scale_color_grey() + theme_classic() +
    theme(legend.position="top")
  dev.off()
}
