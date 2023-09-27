#' Title
#'
#' @param x object
#' @param web3 Logical. If FALSE (default) returns x, else
#' returns a web3storage address resulting from R2web3S(x)
#'
#' @return
#' @export
#'
#' @examples iris %>% web3lify(web3 = TRUE)

web3lify <- function(x, web3 = FALSE){
  if (is.null(web3)) return(x)
  if ( grepl("false", web3, ignore.case = TRUE) | web3 == FALSE ) return(x)
  R2web3S(x)
}
