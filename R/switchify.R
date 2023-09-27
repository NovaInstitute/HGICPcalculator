#' switchify
#' @description
#' Use the serialiser_switch function and the "switch" serializer to switch between json. csv and html
#' serialisations when using function as a API
#'
#' @param out tibble
#' @param format Character of NULL
#'
#' @return tibble
#' @export
#'
#' @examples
#' result %>% switchify()

switchify <- function(out, format = NULL){

  #  check if switch is registers, else do it
  if (!"switch" %in% plumber::registered_serializers()) plumber::register_serializer("switch", HGICPcalculator::serializer_switch)

  if (is.null(format)) format <- "json"
  if (format != "rds") out <- out %>% units::drop_units()
  if (format %in% c("html", "htmlTable")) out <- out %>% dplyr::mutate(dplyr::across(dplyr::where(~is.numeric(.)), ~round(., 2)))
  attr(out, "serialize_format") <- format
  out
}
