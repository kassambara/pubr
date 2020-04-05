#' @importFrom  magrittr %>%


# Temp files ---------------------------------------------------
create_temp_file <- function(content, extension = "", prefix = "file") {
  temp_file <- tempfile(pattern = prefix, fileext = extension)
  readr::write_file(content, temp_file)
  temp_file
}

get_temp_file_path <- function(extension = "", prefix = "file") {
  tempfile(pattern = prefix, fileext = extension)
}


# Clipboard -------------------------------------------------
read_clipboard <- function(){
  clipr::read_clip() %>%
    paste(collapse = "\n")
}

write_clipboard <- function(content){
  clipr::write_clip(content)
}

# Rstudio API ----------------------
rstudio_open_file <- function(file){
  if(interactive()){
    rstudioapi::navigateToFile(file)
  }
}

# Can write text to clipboard
dn_write_file <- function(content, file){
  if(file %in% c("clipboard", "plain_text")){
    results <- clipr::write_clip(content)
  }
  else{
    readr::write_file(content, file)
    results <- file
  }
  invisible(results)
}



file_ext <- function(x){
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

is_csv <- function(path){
  file_ext(path) == "csv"
}
is_tsv <- function(path){
  file_ext(path) == "txt"
}


