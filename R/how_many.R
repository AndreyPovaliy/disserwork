#' Type all text from file
#'
#' @param vector Vector for calculation
#' @param descripror Introductory explanation for forming relevant text
#' @param file_name Introductory explanation for forming relevant text
#'
#'
#' @return String with all cases to target file
#'
#' @examples
#' df <- mtcars
#' file.create("./text.txt")
#' file_name = "./text.txt"
#' how_many(df$mpg,"Всего в исследование вошло",file_name)


how_many <- function(vector,descripror,file_name){
  answer <- print(paste(descripror,length(vector), "пациентов.\n"))
  cat(answer, file = file_name, append = TRUE)
}