#' myseq_df
#'
#' @param df tibble
#'
#' @return
#' @export myseq_df
#'

myseq_df <- function(df){
  vx <- df$x
  vy <- df$y
  vz <- df$z
  vn <- df$n
  rows <- length(vn)
  output <- c()
  row_count = 1

  while (row_count <= rows){
    x <- vx[c(row_count)]
    y <- vy[c(row_count)]
    z <- vz[c(row_count)]
    n <- vn[c(row_count)]
    combo_vec <- c(x, y, z)
    xn = myseq_n(combo_vec, n)
    output <- append(output, xn)
    row_count = row_count + 1
  }

  df$output <- output
  ggplot2::ggplot(data = df,
                  ggplot2::aes(x = n,
                               y = output)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "My Sequence",
                  x = "n",
                  y = "output")
}

