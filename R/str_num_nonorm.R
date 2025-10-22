#' Type no norm data string
#'
#' @param vector Vector for calculation
#' @param parametr Which parameter
#' @param descripror Introductory explanation for forming relevant text
#' @param mesurement Unit of measurement
#' @param file_name Introductory explanation for forming relevant text
#'
#'
#' @return Append text from file to target file
#'
#' @examples
#' df <- mtcars
#' file.create("text.txt")
#' file_name = "text.txt"
#' str_num_nonorm(df$mpg, "Miles/(US) gallon ",
#'              "во всей выборке составил ","miles",file_name)


str_num_nonorm <- function(vector,
                           parametr, descripror, mesurement, file_name) {
  median <- round(median(vector, na.rm = TRUE), 2)
  quan <- summary(vector, na.rm = TRUE)
  answer <- paste0("Медиана ", parametr, descripror, median,
                   " [Q1-Q3: ", round(quan[2], 2), "; ",
                   round(quan[5], 2), "] ", mesurement, ".\n")

  cat(answer, file = file_name, append = TRUE)
}