
#' web3Sub
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' z <- R2web3S(iris)
#' web3Sub(x = "https://bafkreiabt5id676i65g2f25quzub2ib2ecz6xn6xzyalp7opmhvg7ybxum.ipfs.w3s.link/")

web3Sub <- function(x){
  if (is.character(x))  {
    x <- HGICPcalculator::web3S2R(x)
    if (!is.data.frame(x)) {stop("\nI was expecting x to be the web3storrage address of a dataframe\n")}
  }
  x
}
