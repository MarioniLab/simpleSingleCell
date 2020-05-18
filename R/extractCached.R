#' Extract cached objects
#' 
#' Extract specific R objects from the knitr cache of a previously compiled Rmarkdown file (the \dQuote{donor})
#' so that it can be used in the compilation process of another Rmarkdown file (the \dQuote{acceptor}).
#'
#' @param path String containing the path to the donor Rmarkdown file.
#' @param chunk String containing the name of the requested chunk.
#' @param objects Character vector containing variable names for one or more objects to be extracted.
#' @param envir Environment where the loaded objects should be stored.
#'
#' @details
#' Each R object is extracted in its state at the requested \code{chunk} and inserted into \code{envir}.
#' Note that the object does not have to be generated or even referenced in \code{chunk},
#' provided it was generated in a previous chunk.
#'
#' The parser in this function is rather limited,
#' so the donor Rmarkdown file is subject to several constraints:
#' \itemize{
#' \item All chunks involved in generating the requested objects (indirectly or otherwise) should be named.
#' \item All named chunks should be executed; \code{eval=FALSE} is not respected.
#' \item All relevant code occurs within triple backticks, i.e., any inline code should be read-only.
#' }
#' 
#' Unnamed chunks are allowed but cannot be referenced and will not be shown in the output of this function.
#' This should not be used for code that might affect variables in the named chunks,
#' i.e., code in unnamed chunks should be \dQuote{read-only} with respect to variables in the named chunks.
#' Chunks with names starting with \code{unref-} are considered to be the same as unnamed chunks and will be ignored;
#' this is useful for figure-generating chunks that need to be referenced inside the donor report.
#'
#' Obviously, this entire process assumes that donor report has already been compiled with \code{cache=TRUE}.
#' If not, \code{extractCached} will compile it (and thus generate the cache) using \code{\link{compileChapter}}.
#'
#' @return Variables with names \code{objects} are created in \code{envir}.
#' A markdown chunk (wrapped in a collapsible element) is printed that contains all commands needed to generate those objects, 
#' based on the code in the named chunks of the donor Rmarkdown file.
#' 
#' @author Aaron Lun
#' @examples
#' # Mocking up an Rmarkdown report.
#' donor <- tempfile(fileext=".Rmd")
#' write(file=donor, "```{r some-monsters}
#' destoroyah <- 1
#' mecha.king.ghidorah <- 2
#' ```
#'                                                                 
#' ```{r more-monsters}
#' space.godzilla <- 3
#' ```
#'
#' ```{r}
#' msg <- 'I am not referenced.'
#' ```
#'
#' ```{r unref-figure}
#' plot(1, 1, main='I am also not referenced.')
#' ```
#' 
#' ```{r even-more-monsters}
#' megalon <- 4
#' ```")
#' 
#' # Extracting stuff from it in another report.
#' acceptor <- tempfile(fileext=".Rmd")
#' dpb <- deparse(basename(donor))
#' write(file=acceptor, sprintf("```{r, echo=FALSE, results='asis'}
#' chapterPreamble()
#' ```
#'                                                                 
#' ```{r, results='asis', echo=FALSE}
#' extractCached(%s, chunk='more-monsters', 
#'    objects=c('space.godzilla', 'destoroyah'))
#' ```
#'
#' ```{r}
#' space.godzilla * destoroyah
#' ```
#'
#' ```{r, results='asis', echo=FALSE}
#' extractCached(%s, chunk='even-more-monsters', 
#'    objects=c('megalon', 'mecha.king.ghidorah'))
#' ```
#'
#' ```{r}
#' mecha.king.ghidorah * megalon
#' ```", dpb, dpb))
#' 
#' rmarkdown::render(acceptor)
#'
#' if (interactive()) browseURL(sub(".Rmd$", ".html", acceptor))
#' 
#' @seealso
#' \code{\link{setupHTML}} and \code{\link{chapterPreamble}}, to set up the code for the collapsible element.
#'
#' \code{\link{compileChapter}}, to compile a Rmarkdown report to generate the cache.
#' 
#' @export
extractCached <- function(path, chunk, objects, envir=topenv(parent.frame())) {
    prefix <- sub("\\.rmd$", "", path, ignore.case = TRUE)
    cache_path <- file.path(paste0(prefix, "_cache"), "html/")
    if (!file.exists(cache_path)) {
        compileChapter(path)
    }

    chunks <- .extract_chunks(path, chunk)
    .load_objects(cache_path, chunks, objects=objects, envir=envir)

    # Pretty-printing the chunks.
    cat('<button class="aaron-collapse">View history</button>
<div class="aaron-content">
   
```r\n')
    first <- TRUE
    for (x in names(chunks)) {
        if (!first) {
            cat("\n")
        } else {
            first <- FALSE
        }
        cat(sprintf("#--- %s ---#\n", x))
        cat(chunks[[x]], sep="\n")
    }
    cat("```

</div>\n")

    invisible(NULL)
}

.extract_chunks <- function(fname, chunk) 
# Extracting chunks until we get to the one with 'chunk'.
{
    all.lines <- readLines(fname)
    named.pattern <- "^ *```\\{r ([^,]+).*\\}"
    opens <- grep(named.pattern, all.lines)

    chunks <- list()
    for (i in seq_along(opens)) {
        if (i < length(opens)) {
            j <- opens[i+1] - 1L
        } else {
            j <- length(all.lines)
        }

        available <- all.lines[(opens[i]+1):j]
        end <- grep("^ *```\\s*$", available)
        if (length(end)==0L) {
            stop("unterminated chunk")         
        } 

        curname <- sub(named.pattern, "\\1", all.lines[opens[i]])
        if (!grepl("^unref-", curname)) {
            current.chunk <- available[seq_len(end[1]-1L)]
            chunks[[curname]] <- current.chunk
        }

        if (curname==chunk) {
            return(chunks)
        }
    }

    stop(sprintf("could not find chunk '%s'", chunk))
}

#' @importFrom knitr opts_knit load_cache
#' @importFrom CodeDepends readScript getInputs
.load_objects <- function(cache_path, chunks, objects, envir) {
    # This is required so that the cache_path is interpreted correctly 
    # outside of a Rmarkdown compilation.
    if (is.null(old <- opts_knit$get("output.dir"))) {
        opts_knit$set(output.dir=".")
        on.exit(opts_knit$set(output.dir=old))
    }

    # Setting 'rev' to get the last chunk in which 'obj' was on the left-hand side of assignment.
    for (x in rev(names(chunks))) {
        unpacked <- readScript(txt=chunks[[x]], type="R")
        deparsed <- getInputs(unpacked)
        lhs <- unlist(lapply(deparsed, function(x) c(x@outputs, x@updates)))

        present <- intersect(objects, lhs)
        for (p in present) {
            assign(p, envir=envir, value=load_cache(label=x, object=p, path=cache_path))
        }

        objects <- setdiff(objects, present)
        if (length(objects)==0L) {
            break
        }
    }

    if (length(objects)) {
        stop(sprintf("could not find '%s'", objects[1]))
    }

    invisible(NULL)
}
