#' Rewrite bib keys
#'
#' @param link_file Links from file
#' @param bib_file Actual bib File
#' @param n Number in text
#'
#'
#' @return String for table
#' @export

#' @import tidytext
#' @import stringr
#' @import tibble
#' @import dplyr
#' @import readxl
#' @importFrom bib2df bib2df
#' 
#'  @examples
#' # links <- "links.xlsx"
#' # bib_lnks <- "references.bib"
#' # n <- 34



find_key <- function(link_file, bib_file, n){
  links <- read_excel(link_file)
  bib_lnks <- bib2df(bib_file)
  
  for (i in 1:nrow(bib_lnks)){
    if(
      grepl(str_sub(bib_lnks$TITLE[i], 1, 30), links$link[links$n == n], 
            fixed = TRUE)
    ){
      print(bib_lnks$BIBTEXKEY[i])
      print(links$link[links$n == n])
    }
  }
  
}
