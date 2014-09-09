distanc <- function(base){
  DIST <- matrix(0, nrow = 3486, ncol = 3)
  row <- 1
  for(i in 1:83){
    for(j in (i+1):84){
      DIST[row, 1] <- i
      DIST[row, 2] <- j
      DIST[row, 3] <- base[i, j]
      row <- row + 1
    }
  }
  DIST
}