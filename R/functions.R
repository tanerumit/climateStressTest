

gridInterpolate <- function(x, y, z = NULL, res, ...) {

  # Interpolation for three-dimensional array
  if (is.null(z)) {z <- rep(0, length(x))}

  z <- data.frame(z)

  df1 <- lapply(seq_len(ncol(z)), function(i) akima::interp(x, y, z[, i],
                                                            xo = seq(min(x), max(x), length = res),
                                                            yo = seq(min(y), max(y), length = res)), ...)

  df2 <- do.call("cbind", lapply(df1, function(x) c(x$z)))
  df3 <- as_tibble(cbind(expand.grid(x = df1[[1]]$x, y = df1[[1]]$y), z = df2))

}




binCentered <- function(x) {return(x[-length(x)] + (x[2] - x[1])/2)}
