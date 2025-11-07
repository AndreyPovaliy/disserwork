#' Create text from df with numeral results
#'
#' @param df df with numeral results
#' @param filename1 way to save text in file
#' @param mes vector with names of measures
#'
#'
#' @return  text in file with text
#' @export


print_text_num <- function(df,filename1,mes){
  out_text <-c()
  for(i in 1:nrow(df)){
    costr_t <- paste0(df[i,1]," был в группе А ",df[i,2]," ", mes[i], ", в группе Б ",df[i,3]," ", mes[i])
    out_text <- append(out_text,costr_t)
  }
  cat(out_text,sep = ".",file = filename1,append = F)
}