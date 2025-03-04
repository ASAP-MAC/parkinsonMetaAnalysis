cohen_d_from_lm <- function(n1,n2,t) {
  d <- (t*(n1+n2)/(sqrt(n1*n2)*sqrt(n1+n2-2)))
  return(d)
}

cohen_d_StdErr <- function(n1,n2,d) {
  se_d <- sqrt(((n1+n2-1)/(n1+n2-3))*((4/(n1+n2))*(1+((d**2)/8))))
  return(se_d)
}