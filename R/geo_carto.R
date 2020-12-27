#' Return NaturalEarth Bound box
#'
#' @return An sf class boundbox
#' @export
carto_ne_get_boundbox <- function() {
  return(internal$boundbox)
}
