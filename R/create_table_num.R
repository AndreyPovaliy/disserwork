#' Create df with numeric measures
#'
#' @param df Exploring dataframe
#' @param dev Vector with devide
#' @param transl Vector with names to final text
#'
#'
#' @return  df with numeric measures
#' @export
#' @import dplyr
#' @import magrittr
#' @import ggplot2
#'
#' @examples
#' # df <- dplyr::tibble(
#' # a = as.factor(c("D","F","F","D")),
#' # b = c(1:4),
#' # c = c(5:8)
#' # dev <- "a"
#' # transl <- с("group","gender","head color")
#' # create_table_num(df, dev, transl)



create_table_num <- function(df, dev, transl) {
    
  combined_df <- tibble(
    name = character(),
    group_1 = character(),
    group_2 = character(),
    p_value = character()
  )


  for (j in 2:ncol(df)){
    param1 <- c(colnames(df[j]))
    
    for (i in 1:length(unique(df[[dev]]))){
      a <- subset(df[[j]], df[[dev]] == sort(unique(df[[dev]]))[i])
      if (shapiro.test(a)[["p.value"]] > 0.05){
        param_1 <- round(mean(a,na.rm = T),2)
        param_2 <- round(sd(a,na.rm = T),2)
        param <- paste0(param_1," \u00B1 ",param_2)
      }else {
        param_1 <-  round(median(df[[j]],na.rm = T),2)
        q_mi <- quantile(a,na.rm = T)[2]
        q_ma <-quantile(a,na.rm = T)[4]
        param_2 <-  paste0(" [",q_mi,";",q_ma,"]")
        param <- paste0(param_1,param_2)
        
      }
      param1 <- append(param1,param)
    }
    
    
    if(shapiro.test(df[[j]])[["p.value"]] > 0.05){
      p_v <- round(t.test(df[[j]]~df[[dev]])[["p.value"]],2)
    }else{
      p_v <- round(wilcox.test(df[[j]]~df[[dev]])[["p.value"]],2)
      
      
    }
    if(p_v<0.05){
      p_v <- paste0(p_v,"**")
    }
    
    param1 <- append(param1,p_v)
    
    combined_df <- combined_df %>% 
      add_row(name = param1[1], 
              group_1 = param1[2],
              group_2 = param1[3],
              p_value = param1[4])
    
    
  }
  combined_df$name <- transl
  colnames(combined_df) <- c("Параметр","Группа А","Группа Б","p-уровень")
  return(combined_df)
}