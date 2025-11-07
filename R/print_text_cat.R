#' Create text from df with categorical results
#'
#' @param df df with categorical results
#' @param filename1 way to save text in file
#'
#'
#' @return  text in file with text
#' @export


print_text_cat <- function(df,filename1){
  out_text <-c()
  for(i in 1:nrow(df)){
    out_text <- append(out_text,paste0(df[i,1]," в группе ",df[i,2], " была ",df[i,3]," была в ",df[i,4],"(",df[i,5],")"))
  }
  cat(out_text,sep = ".",file = filename1,append = F)
}