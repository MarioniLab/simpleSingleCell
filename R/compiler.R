.compiler <- function(target) 
# Compiles all dependent workflows in an enslaved R session.
{
    if (!file.exists(paste0(target, "_cache"))) {
        rcmd <- file.path(R.home("bin"), "R")
        system2(rcmd, c("--no-save", "--slave", "-e", sprintf("'rmarkdown::render(\"%s\")'", paste0(target, ".Rmd"))))
    }
    invisible(NULL)
}
