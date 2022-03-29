##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title

##' @return
##' @author Nicholas Tierney
##' @export
zip_for_jss <- function(paper, paper_purl) {
  
  dir_create("jss")
  zip_name <- "jss/jss-submission.zip"
  
  to_ignore <- glue_ignore(c("html",
                             "aux",
                             "bbl",
                             "blg",
                             "sty",
                             "bldg",
                             "brf",
                             "out"))
  
  files_to_zip <- dir_ls(
    path = "paper/",
    recurse = TRUE,
    regexp = paste0(c("images|diagram|review|*cache|"),
                    to_ignore),
    invert = TRUE
  )
  
  
  if (file_exists(zip_name)) {
    file_delete(zip_name)
  }
  
  zip(zipfile = zip_name,
      files = files_to_zip)
  
  
}
