#' @importFrom callr r
#' @importFrom rmarkdown render
.compile <- function(target) 
# Compiles all dependent workflows in an enslaved R session.
{
    if (!file.exists(paste0(target, ".html"))) {
        script <- paste0(target, ".Rmd")
        r(function(target) rmarkdown::render(script), show=TRUE)
    }
    invisible(NULL)
}
