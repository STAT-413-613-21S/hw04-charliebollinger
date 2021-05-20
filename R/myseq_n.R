#' myseq_n
#'
#' @param x list
#' @param n factor
#'
#' @return
#' @export
#'
#' @examples myseq_n(x = c(2, 3, 3), n = 3)


myseq_n <- function(x, n){

  x1 <- x[c(1)]
  x2 <- x[c(2)]
  x3 <- x[c(3)]
  xn = x3
  count = 3

  if (n <= 0){
    stop("enter positive n")
  } else if (length(x) != 3){
    stop("enter vector of length 3")
  } else {
    while (count < n){
      xn = x3 + ((x1-x2)/(count + 1))
      x1 = x2
      x2 = x3
      x3 = xn
      count = count + 1
    }
    return(xn)
  }
}
