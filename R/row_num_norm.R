#' Type norm data table row
#'
#' @param parametr Which parameter
#' @param vector vector Vector for calculation
#' @param file_name To target file
#'
#'
#' @return String for table
#'
#' @examples
#' df <- mtcars
#' file.create("./text.txt")
#' file_name = "./text.txt"
#' row_num_norm("Miles/(US) gallon ",df$mpg,file_name)


row_num_norm <- function(parametr, vector, file_name) {
  mean <- round(mean(vector, na.rm = TRUE), 2)
  sd <- round(sd(vector, na.rm = TRUE), 2)
  answer <- paste("|:", parametr, "|:", mean , "±", sd, ":|\n")
  cat(answer, file = file_name, append = TRUE)
}