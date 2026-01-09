#' Type categoric data in short form data to string
#'
#' @param vector Vector for calculation
#' @param parametr Which parameter
#' @param file_name Introductory explanation for forming relevant text
#'
#'
#' @return Print categoric data in short form data to string
#' @export 
#'
#' @examples
#' # parametr = "Engine is "
#' # file_name <- "text.txt"
#' # quantity_discr_short(parametr = parametr, vector = as.factor(mtcars$vs),file_name)




quantity_discr_short <- function(parametr,vector,file_name){
  a <- paste(parametr, "\n")
  cat(a, file = file_name, append = TRUE)
  n <- length(data.frame(table(vector), row.names = TRUE )$Freq)
  i <- 0
  while (i < n) {
    descr <- print(paste(
                         data.frame(table(vector))[i+1,1],
                         " в ",
                         data.frame(round(prop.table(table(vector))*100,2))[i+1,2],
                         "% (", 
                         data.frame(table(vector))[i+1,2],
                         ")", sep=""))
    i <- i + 1
    a <- paste(descr, "\n")
    cat(a, file = file_name, append = TRUE)
  }
}