#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @return
#' @author Nicholas Tierney
#' @export
glue_ignore <- function(x){
  glue::glue("\\.{x}$") %>% 
    glue::glue_collapse(sep = "|") %>% 
    as.character()
}
