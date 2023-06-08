
#' web3S2R
#' Resolve IPFS https url and retrive the Rds file
#' @param ipfsURI
#' @param fnmR Character. If NULL generates a tempfile
#'
#' @return R object
#' @export
#'
#' @examples web3S2R("https://bafkreigbspthbmhuza3ypk4vzdcvzclmt4kqusdybepxl54kjocjgcsdla.ipfs.w3s.link/")

web3S2R <- function(ipfsURI, fnmR = NULL){

  if (is.null(fnmR)) fnmR <- tempfile()
  res <- httr::GET(url = ipfsURI)
  writeBin(object = res$content, con = fnmR)
  readr::read_rds(file = fnmR)
}




R2web3S <- function(x,
                    AT = Sys.getenv("IPFS_STORAGE_API_KEY"),
                    fnmW = NULL){

  if (is.null(fnmW)) fnmW <- tempfile()

  readr::write_rds(x = x,
                   file = fnmW,
                   compress = "none",
                   text = FALSE)

  res <- httr::POST(
    url = "https://api.web3.storage/upload",
    body = httr::upload_file(path = fnmW),
    httr::add_headers(
      Authorization = sprintf("Bearer %s", AT)))
  res <- httr::content(x = res, as = "parsed")
  ipfsURI <- sprintf("https://%s.ipfs.w3s.link/", res$cid)
  ipfsURI

}

