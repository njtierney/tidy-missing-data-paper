## Load your packages, e.g. library(targets).
source("./packages.R")
source("./conflicts.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

# target = function_to_make(arg), ## drake style
  tar_target(paper_bib,
             "paper/tidy-missing-data-zotero.bib",
             format = "file"),
  tar_render(paper, 
             "paper/tidy-missing-data-paper.Rmd"),
  # ideally this would only run if the report has been rendered,
  # not sure how to detect that.
  tar_target(arxiv_paper,
             zip_for_arxiv()),
  tar_target(rjournal,
             zip_for_jss())
  

# tar_target(target2, function_to_make2(arg)) ## targets style

)
