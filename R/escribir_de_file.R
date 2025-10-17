#' Type all text from file
#'
#' @param way_to_text Way to text
#' @param to_file Way to file
#'
#'
#' @return Append text from file to target file
#' @export
#'



escribir_de_file <- function (way_to_text,to_file){
  rT <- read.table(way_to_text,sep = "_")
  for (i in 1:nrow(rT)) {
    escribir(paste(rT$V1[i],
                   "\n\n"),
             to_file
             )

  }
}
