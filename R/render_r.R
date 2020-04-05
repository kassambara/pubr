#' @include utilities.R
#' @importFrom knitr knit opts_knit opts_chunk
NULL
#' Convert Reproducible R script and Rmd into Publishable HTML Block
#' @description Makes it easy to share reproducible R code in (wordpress) website comments and blog posts.
#'
#' \preformatted{
#'  1. Copy the code
#'  2. Run render_r() for pure R script or render_rmd() for Rmd input. Input is read from the clipboard. The output is also written in the clipboard.
#'  3. Paste into the website comments text area or blog post
#'  }
#'
#'
#' @param session_info logical. If TRUE, adds R session info.
#' @return a html code to directly post on datanovia
#' @param ... other arguments passed to \code{\link[reprex]{reprex}()} function.
#' @examples
#' \donttest{
#' render_r({y <- 1:4; mean(y)})
#' }
#'
#' @describeIn render_r Create a reproducible output from R scripts
#' @export
render_r <- function(..., session_info = FALSE) {
  reprex_content <- reprex::reprex(
    ...,
    style = TRUE, advertise = FALSE,
    venue = "html", session_info = session_info
  ) %>%
    remove_reprex_html_header() %>%
    paste(collapse = "\n")
  if(interactive()){
    reprex_content <- clipr::write_clip(reprex_content)
  }
  invisible(reprex_content)
}

remove_reprex_html_header <- function(reprex_content) {
  header_start <- which(reprex_content == "<head>")
  header_end <- which(reprex_content == "</head>")
  reprex_content[-c(header_start:header_end)]
}



#' @describeIn render_r Create a reproducible output from Rmd file
#' @param input an Rmd file. If NULL, the function tries to read the clipboard.
#' @param fig.show possible value "asis" or "hold"
#' @param toc logical. If TRUE, adds toc.
#' @param html_preview logical. If TRUE, shows an html preview.
#' @export
render_rmd <- function(input = NULL, fig.show = "asis", toc = FALSE, html_preview = TRUE) {
  opts_knit$set(upload.fun = knitr::imgur_upload, base.url = NULL)
  opts_chunk$set(
    fig.width = 3.5, fig.height = 3.5, fig.show = fig.show, cache = TRUE,
    warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, error = TRUE
    )
  if (toc) {
    add_toc_in_md_options()
  }
  else {
    remove_toc_from_md_options()
  }

  if (is.null(input)) {
    input <- clipr::read_clip() %>%
      paste(collapse = "\n") %>%
      create_temp_file(extension = ".Rmd", prefix = "datanovia-")
  }
  input <- embed_in_rdoc_class(file = input)
  md_file <- gsub(".Rmd", ".md", input)
  md_file <- knit(input, output = md_file)
  rmd_file <- embed_in_rmd_template(md_file)
  html <- render_html(rmd_file)
  rdoc_fragment <- extract_rdoc_html_fragment(html)

  if (html_preview) {
    message("Creating preview...\n")
    preview(md_file)
  }
  opts_knit$restore()
  clipr::write_clip(rdoc_fragment)
}


# Rendering md into datanovia format ------------------
# Embed file content in rdoc class
embed_in_rdoc_class <- function(file){
  content <- readr::read_file(file)
  content <- paste0(
    '<!--DOC_START-->\n',
    '<div class="rdoc">\n',
    content,
    '\n</div>',
    '<!--DOC_END-->'
    )
  readr::write_file(content, file)
  invisible(file)
}


render_html <- function(input){
  output <- gsub("\\.Rmd|\\.md", ".html", input)
  output <- rmarkdown::render(input = input, output_file = output)
  output
}

extract_rdoc_html_fragment <- function(input){
  . <- NULL
  readr::read_file(input) %>%
    strsplit("<!--DOC_START-->|<!--DOC_END-->", fixed = FALSE, perl = TRUE) %>%
    unlist() %>%
    .[2]
}

embed_in_rmd_template <- function(md_file){
  output <- gsub(".md", ".Rmd", md_file)
  md_content <- readr::read_file(md_file)
  rmd_template <- readr::read_file(system.file("template-copy2datanovia.Rmd", package = "datanovia"))
  embeded <- rmd_template %>% stringr::str_replace("_RDOC_CONTENT_", md_content)
  readr::write_file(embeded, output)
  invisible(output)
}

get_active_file_path <- function(){
  path <- rstudioapi::getSourceEditorContext()$path
}

# Setting knitr and markdown option ---------------------------
remove_toc_from_md_options <- function() {
  md_options <- markdown::markdownHTMLOptions(default = FALSE) %>%
    setdiff("toc")
  options(markdown.HTML.options = md_options)
}

add_toc_in_md_options <- function() {
  md_options <- markdown::markdownHTMLOptions(default = FALSE)
  if (!("toc" %in% md_options)) md_options <- c(md_options, "toc")
  options(markdown.HTML.options = md_options)
}




# Preview of md file  ------------------------------
preview <- function(input) {
  preview_file <- rmarkdown::render(
    input,
    output_dir = get_temp_file_path(prefix = "datanovia-"),
    clean = FALSE, quiet = TRUE, encoding = "UTF-8",
    output_options = if (pandoc2.0()) {
      list(pandoc_args = "--quiet")
    }
  )
  viewer <- getOption("viewer")
  if (is.null(viewer)) viewer <- utils::browseURL
  viewer(preview_file)
  invisible(preview_file)
}


pandoc2.0 <- function() {
  rmarkdown::pandoc_available("2.0")
}


