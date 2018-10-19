# Heatmap zeichnen (fuer ACOLS)


#' @export
plotMatrix <- function(M){
  m <- apply(M, 2, rev)
  image(1:ncol(m), 1:nrow(m),  t(m), col = topo.colors(100), axes = FALSE)
}
