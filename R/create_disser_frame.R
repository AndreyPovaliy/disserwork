#' Create disser frame by heading
#'
#' @param file_name Adress to json file
#'
#'
#' @return Create .qmd files to disser
#' @export



create_disser_frame <- function(file_name){
  json_data <- jsonlite::fromJSON(readLines(file_name))


  for (i in 1:length(json_data$file_name)){
    name <- paste0(json_data$file_name[i],".qmd")
    file.create(name)
    cat(paste0("# ", json_data$name[i],"\n\n"),file = name, append = T)
    for (j in 1:length(json_data$content[i][[1]])){
     cat(paste0("## ",json_data$content[i][[1]][j],"\n\n"), file = name, append = T)
    }


  }

  for (i in 1:length(json_data$file_name)){
    cat(paste0("- ",json_data$file_name[i],".qmd\n"))
  }

}

