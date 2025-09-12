#' Type in file string with new line break
#'
#' @param text A string of text
#' @param to_file Way to file
#'
#'
#' @return Append string in file
#' @export
#'
#' @examples
#' escribir("Hello world!", "./textfile.txt")

escribir <- function(text,to_file) {
  text <- paste0(text,"\n")
  cat(text, file = to_file, append = TRUE)
}
