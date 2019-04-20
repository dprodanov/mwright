
#' Wright tools
#' (C) Dimiter Prodanov, 2019
#'
#' Mainardi-Wright function
#' @param z position
#' @param a exponent
#' @keywords Mainardi-Wright
#' @export mwright(z,a)
#' @examples
#' mwright(0.5, 0.5)
mwright<-function(z,a) {
  result<-0
  z<-abs(z)
  ker1 <- function(x){exp(- z*x* cos(pi *a) - x^(1/a))*sin( z*x * sin(pi *a) - pi*a )}
  result <- integrate(ker1 ,lower=0,upper=Inf)$value/(pi *a)
  return(-result)
}

#' Integral of the Mainardi-Wright function
#' @param z position
#' @param a exponent
#' @keywords Mainardi-Wright
#' @export mwrighti(z,a)
#' @examples
#' mwrighti(0.5, 0.5)
mwrighti<-function(z,a) {
  result<-0
  b<-1
  ifelse (z<0, b<- 0.5, b<-(-0.5) )
  z<-abs(z)
  a<- (-a)
  ker1 <- function(x){
	ifelse ( x<= 1e-8, 0.0, exp(- z*x* cos(pi *a) - 1/x^(1/a))*sin( z*x * sin(pi *a) )/x)
  }
  result <- 0.5 - b*integrate(ker1 ,lower=0,upper=Inf)$value/(a*pi)
  return(result)
}



