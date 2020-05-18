#' Pretty session info
#'
#' Wraps the session information output chunk in a collapsible HTML element
#' so that it doesn't dominate the compiled chapter.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{setupHTML}} and \code{\link{chapterPreamble}}, to set up the code for the collapsible element.
#' 
#' @export
#' @examples
#' tmp <- tempfile(fileext=".Rmd")
#' write(file=tmp, "```{r, echo=FALSE, results='asis'}
#' rebook::setupHTML()
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
#' @importFrom utils capture.output sessionInfo
prettySessionInfo <- function() {
    ## grab session info printed output
    X <- capture.output(sessionInfo())

    ## print session info out into collapsible div
    cat('<button class="aaron-collapse">View session info</button>
<div class="aaron-content">\n')
    cat(c("```", X, "```"), sep="\n")
    cat("</div>\n")

    invisible(NULL)
}

