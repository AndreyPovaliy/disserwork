#' Type norm data string
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
#' # df <- mtcars
#' # file.create("./text.txt")
#' # file_name = "./text.txt"
#' # str_num_norm(df$mpg, "Miles/(US) gallon ", "во всей выборке составил ","miles",file_name)


str_num_norm <- function(vector, parametr, descripror, mesurement, file_name){
  mean <- round(mean(vector, na.rm = TRUE), 2)
  sd <- round(sd(vector, na.rm = TRUE), 2)
  answer <- paste("Средний ", parametr,
                  descripror, mean, " ± ", sd, " ", mesurement, ".\n", sep = "")

  cat(answer, file = file_name, append = TRUE)
}