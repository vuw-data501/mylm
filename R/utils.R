#' Matrix Multiplication
#'
#' @param a First matrix
#' @param b Second matrix
#' @param method How should the multiplication be done?
#'
#' @description If method='default' then the %*% operator is used
#' otherwise 'loop' runs an R loop, and 'c' does the same in
#' compiled C code
#'
#' @export
matrix_multiply <- function(a, b, method=c("default","loop","c")) {
  na <- dim(a)
  nb <- dim(b)
  if(na[2]!=nb[1]) {
    stop("Matrices are not conformable")
  }
  if(method[1]=="default") {
    c <- a%*%b
  } else if(method[1]=="loop") {
    c <- array(0, dim=c(na[1],nb[2]))
    for(i in 1:na[1]) {
      for(j in 1:nb[2]) {
        c[i,j] <- 0
        for(k in 1:na[2]) {
          c[i,j] <- c[i,j] + a[i,k]*b[k,j]
        }
      }
    }
  } else if(method[1]=="c") {
    c <- matrix_multiply_c(a, b)
  } else {
    stop(paste0("Method ",method," not recognised"))
  }
  return(c)
}
