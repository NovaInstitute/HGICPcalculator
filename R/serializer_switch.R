
#' serializer_switch
#'
#' @return function
#' @export
#'
#' @examples
#' register_serializer("switch", serializer_switch)


serializer_switch <- function() {
  function(val, req, res, errorHandler) {
    tryCatch({
      format <- attr(val, "serialize_format")
      if (is.null(format) || format  == "json") {
        type <- "application/json"
        sfn <- jsonlite::toJSON
      } else if (format == "csv") {
        type <- "text/csv; charset=UTF-8"
        sfn <- readr::format_csv
      } else if (format == "rds") {
        type <- "application/rds"
        sfn <- function(x) base::serialize(x, NULL)
      } else if (format == "htmlTable") {
        type <- "text/html; charset=utf-8"
        sfn <- function(x)  htmlTable::htmlTable(x)
      }
      val <- sfn(val)
      res$setHeader("Content-Type", type)
      res$body <- val
      res$toResponse()
    }, error = function(err) {
      errorHandler(req, res, err)
    })
  }
}
