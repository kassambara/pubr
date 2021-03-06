% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render_r.R
\name{render_r}
\alias{render_r}
\alias{render_rmd}
\title{Convert Reproducible R script and Rmd into Publishable HTML Block}
\usage{
render_r(..., session_info = FALSE)

render_rmd(input = NULL, fig.show = "asis", toc = FALSE, html_preview = TRUE)
}
\arguments{
\item{...}{other arguments passed to \code{\link[reprex]{reprex}()} function.}

\item{session_info}{logical. If TRUE, adds R session info.}

\item{input}{an Rmd file. If NULL, the function tries to read the clipboard.}

\item{fig.show}{possible value "asis" or "hold"}

\item{toc}{logical. If TRUE, adds toc.}

\item{html_preview}{logical. If TRUE, shows an html preview.}
}
\value{
a html code to directly post on datanovia
}
\description{
Makes it easy to share reproducible R code in (wordpress) website comments and blog posts.

\preformatted{
 1. Copy the code
 2. Run render_r() for pure R script or render_rmd() for Rmd input. Input is read from the clipboard. The output is also written in the clipboard.
 3. Paste into the website comments text area or blog post
 }
}
\section{Functions}{
\itemize{
\item \code{render_r}: Create a reproducible output from R scripts

\item \code{render_rmd}: Create a reproducible output from Rmd file
}}

\examples{
\donttest{
render_r({y <- 1:4; mean(y)})
}

}
