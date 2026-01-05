#' Print colnames
#'
#' @param df Data frame from which needs colnames
#'
#'
#'
#' @return formated string in terminal
#' @export
#'
#' @examples
#' # df <- mtcars
#' # print_col_names(df)



print_col_names <- function(df){
  cat(paste0("\"", colnames(df), "\"",
             collapse = ", "))

}