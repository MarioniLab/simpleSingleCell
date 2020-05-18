#' Compile a Rmarkdown file
#'
#' Compile a Rmarkdown file - typically a chapter of a book - 
#' so that \code{\link{extractCached}} calls work correctly in other chapters.
#'
#' @param path String containing a path to an Rmarkdown file.
#' @param cache Logical scalar indicating whether the compilation should be cached.
#'
#' @return
#' All files are (re)compiled to generate the corresponding \code{*_cache} directories.
#' \code{NULL} is invisibly returned.
#'
#' @details
#' Compilation is performed in an isolated session using \code{\link{r}} from the \pkg{callr} package.
#' This ensures that settings from one chapter do not affect the next chapter.
#'
#' If an error is encountered during compilation of any Rmarkdown file,
#' the standard output of \code{\link{render}} leading up to the error is printed out before the function exists.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{extractCached}}, which calls this function.
#'
#' @examples
#' tmp <- tempfile(fileext=".Rmd")
#' write(file=tmp, "```{r, echo=FALSE, results='asis'}
#' rebook::chapterPreamble()
#' ```
#'
#' ```{r}
#' rodan <- 1
#' ```")
#' 
#' compileChapter(tmp)
#' 
#' file.exists(sub(".Rmd$", ".html", tmp)) # output HTML exists.
#' file.exists(sub(".Rmd$", "_cache", tmp)) # output cache exists.
#' exists("rodan") # FALSE
#' @export
#' @importFrom callr r
#' @importFrom rmarkdown render
#' @importFrom methods is
compileChapter <- function(path, cache=TRUE) {
    logfile <- tempfile(fileext=".log")
    on.exit(unlink(logfile))

    E <- try(
        r(
            function(input, cache) { 
                knitr::opts_chunk$set(cache=cache)
                rmarkdown::render(input) 
            }, 
            args = list(input=path, cache=cache), 
            stdout=logfile, 
            stderr=logfile, 
            spinner=FALSE
        ),
        silent=TRUE
    )

    if (is(E, "try-error")) {
        message(sprintf("# %s\n", readLines(logfile)))
        stop(sprintf("failed to compile '%s'", path))
    }

    invisible(NULL)
}
