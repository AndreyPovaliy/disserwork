#' Add file with hel srtucture
#'
#' @param folder_name Way to file
#'
#'
#' @return Append file hlper.R
#' @export
#' @import glue
#'


add_helper <- function(folder_name){
    helper <- "{folder_name}helper.R"
    file.create(glue("helper"))

    cat(glue("library(tidyverse)
library(disserwork)
library(glue)

way <- {folder_name}

ls(\"package:disserwork\")
# create_calc_block(way)

add_article_frame(file_name = glue(\"{way}index.qmd\"))


review_check(pages = 1,
             actual_year = 2018, 
             file_Rmd =glue(\"{way}index.qmd\") ,
             file_bib = glue(\"{way}references.bib\")
             )
    
    "), file =helper)

}
