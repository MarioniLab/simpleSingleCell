#' Set up HTML elements
#'
#' Set up Javascript and CSS elements for the custom collapsible class.
#'
#' @return Prints HTML to set up JS and CSS elements.
#'
#' @details
#' The custom collapsible class allows us to hide details until requested by the user.
#' This improves readability by reducing the clutter in the compiled chapter.
#'
#' @author Aaron Lun
#' 
#' @seealso
#' \code{\link{chapterPreamble}}, which calls this function.
#'
#' \code{\link{extractCached}} and \code{\link{prettySessionInfo}}, which use the custom collapsible class.
#'
#' @inherit prettySessionInfo examples
#' @export
setupHTML <- function() {
    cat('<script>
document.addEventListener("click", function (event) {
    if (event.target.classList.contains("aaron-collapse")) {
        event.target.classList.toggle("active");
        var content = event.target.nextElementSibling;
        if (content.style.display === "block") {
            content.style.display = "none";
        } else {
            content.style.display = "block";
        }
    }
})
</script>

<style>
.aaron-collapse {
  background-color: #eee;
  color: #444;
  cursor: pointer;
  padding: 18px;
  width: 100%;
  border: none;
  text-align: left;
  outline: none;
  font-size: 15px;
}

.aaron-content {
  padding: 0 18px;
  display: none;
  overflow: hidden;
  background-color: #f1f1f1;
}
</style>')
}
