#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param housing
#' @return
#' @author Nicholas Tierney
#' @export
gg_vis_miss_housing <- function(housing) {

  vis_miss(housing,
           cluster = TRUE,
           sort_miss = TRUE,
           show_perc_col = FALSE)

}
