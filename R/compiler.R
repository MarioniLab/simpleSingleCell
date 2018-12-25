.compiler <- function(target) 
# Compiles all dependent workflows in an enslaved R session.
{
    if (!file.exists(paste0(target, "_cache"))) {
        script <- paste0(target, ".Rmd")
        rcmd <- file.path(R.home("bin"), "R")
        code <- system2(rcmd, c("--no-save", "--slave", "-e", sprintf("'rmarkdown::render(\"%s\")'", script)))
        if (code) {
            stop(sprintf("could not run '%s'", script))
        }
    }
    invisible(NULL)
}
