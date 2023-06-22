#' ipfs2R
#' @description
#' Get data written with R2ipfs() from IPFS and return as an R object
#'
#' @param key Character
#'
#' @return R object
#' @export
#' @example ipfs2R(R2ipfs("mtcars")$hash)

ipfs2R <- function(key){
  z <- ipfs_cat(key)
  length(z)
  last = length(z)
  thirdlast = last - 3
  xx <- invisible(rawToChar(z[-c(1:5, thirdlast:last)]))
  eval(parse(text = xx))
}

#' R2ipfs
#' @description
#' Place an R object on IPFS. Can be read again with ipfs2R
#'
#' @param objname
#' @param outdir
#'
#' @return
#' @export
#'
#' @examples R2ipfs("mtcars")

R2ipfs<- function(objname){
  obj <- get(objname, envir = .GlobalEnv)
  tmp2 <- tempfile()
  chr <- dput(obj, file = "") %>% capture.output()
  writeLines(chr, tmp <- tempfile())
  res <- ipfs::ipfs_add(tmp)
  rm(tmp)
  rm(tmp2)
  res
}

#' ls2ipfs
#' Add all non-function objects in the Global Environment to IPFS
#' @return tibble
#' @export
#'
#' @examples ls2ipfs()

ls2ipfs <- function(storage = c("IPFS", "WEB3STORAGE")[2]){
  objects <- setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv))

  if (storage == "IPFS") {
    return(purrr::map_df(objects, ~{ tb <- R2ipfs(.); tb$name <- .; tb}))
  }
  if (storage == "WEB3STORAGE") {
    return( purrr::map_df(objects, ~ dplyr::tibble(name = .,
                                         key = R2web3S(get(., envir = .GlobalEnv)))) )
  }
}

#' ipfsDF
#' @description
#' Convenience function to use dfIPFS to recover an R object from IPFS
#'
#' @param dfIPFS tibble. Typically resulting from ls2ipfs()
#' @param nn Character.  Name of object to recover
#'
#' @return R Object
#' @export
#'
#' @examples ER = ipfsDF("ER")  # same as  ipfsDF(df = dfIPFS, nn = "ER")
ipfsDF <- function(df = dfIPFS, nn = "ER"){
  key <- df[df[["name"]] == nn, "hash"]
  ipfs2R(key)
}

recoverIPFS <- function(df = dfIPFS){
  l <- as.list(df[["name"]])
  map(l, ~{cat(., "\n");
    x <- ipfsDF(nn = .)
    assign(., x, envir = .GlobalEnv)

    })
}


