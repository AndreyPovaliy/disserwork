#' Type all text from file
#'
#' @param way_to_text Way to text
#' @param to_file Way to file
#'
#'
#' @return Append text from file to target file
#' @export
#' @importFrom utils read.table
#'



escribir_de_file <- function(way_to_text, to_file) {
  answer <- read.table(way_to_text, sep = "_")
  for (i in 1:nrow(answer)) {
    escribir(paste(answer$V1[i], "\n\n"), to_file)

  }
}
