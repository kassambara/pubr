#' @include utilities.R
NULL
#' Paste Data into R
#' @description Paste data from different sources (spreadsheet or data frame)
#'   into R.
#' @param input can be data from clipboard (default), a vector, data.frame, a
#'   tibble or a (csv|tsv) file containing a data frame.
#' @param ... other arguments passed to the functions
#'   \code{readr::read_tsv()} or \code{readr::read_csv()}
#'   depending on the input extension.
#' @export
paste_data <- function(input = "clipboard", ...){
  if("clipboard" %in% input){
    input <- clipr::read_clip_tbl()
  }
  else if(is.character(input)){
    if(file.exists(input)){
      if(is_tsv(input)) input <- readr::read_tsv(input, ...)
      else if(is_csv(input)) input <- readr::read_csv(input, ...)
    }
  }
  datapasta::dpasta(input)
}
