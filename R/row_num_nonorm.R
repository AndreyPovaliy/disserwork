#' Type no norm data table row
#'
#' @param parametr Which parameter
#' @param vector vector Vector for calculation
#' @param file_name To target file
#'
#'
#' @return String for table
#'
#' @examples
#' # df <- mtcars
#' # file.create("./text.txt")
#' # file_name = "./text.txt"
#' # row_num_nonorm("Miles/(US) gallon ",df$mpg,file_name)


row_num_nonorm <- function(parametr, vector, file_name) {
  median <- round(median(vector, na.rm = TRUE),2)
  quan <- summary(vector, na.rm = TRUE)
  answer <- paste("|:", parametr, "|:", median, "[ Q1-Q3:",
                  round(quan[2], 2), ";", round(quan[5], 2), "] :|")
  cat(answer, file = file_name, append = TRUE)
}