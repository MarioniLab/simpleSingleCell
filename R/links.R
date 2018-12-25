#' @importFrom BiocStyle Biocpkg
.link <- function(vignette, section, label) 
# Define links between vignettes.
{
    if (!is.null(section)) {
        section <- gsub(" +", "-", tolower(section))
        vignette <- paste0(vignette, ".html#", section)
    }
    Biocpkg("simpleSingleCell", vignette=vignette, label=label)
}
