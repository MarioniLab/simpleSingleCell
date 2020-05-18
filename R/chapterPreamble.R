#' Execute chapter preamble code
#'
#' Execute code to set up the compilation environment at the start of every chapter.
#'
#' @param cache Logical indicating whether to cache code chunks.
#'
#' @details
#' Compilation is performed with no tolerance for errors, no printing of package start-up messages, 
#' and no printing of warnings.
#' 
#' Numbers are printed to 4 digits of precision.
#'
#' The \pkg{BiocStyle} package is automatically attached.
#'
#' HTML elements are defined using \code{\link{setupHTML}}.
#'
#' @return See \code{\link{setupHTML}}.
#' 
#' @author Aaron Lun
#' 
#' @examples
#' tmp <- tempfile(fileext=".Rmd")
#' write(file=tmp, "```{r, echo=FALSE, results='asis'}
#' rebook::chapterPreamble()
#' ```
#'
#' ```{r}
#' pi # four digits!
#' ```
#'
#' ```{r}
#' warning('ASDASD') # warnings and messages are not saved in the HTML.
#' ```
#'
#' ```{r, results='asis'}
#' prettySessionInfo()
#' ```")
#'
#' rmarkdown::render(tmp)
#'
#' if (interactive()) browseURL(sub(".Rmd$", ".html", tmp))
#'
#' @export
#' @importFrom knitr opts_chunk
chapterPreamble <- function(cache = FALSE) {
    opts_chunk$set(message = FALSE, warning = FALSE, error = FALSE, cache = cache)
    options(digits = 4)
    try(attachNamespace("BiocStyle"), silent=TRUE)
    setupHTML()
}
