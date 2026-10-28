#' Create table with numeric measures summary
#'
#' @param df Data frame containing numeric variables and grouping variable.
#' @param dev Name of the column to group by (must be a factor or character).
#' @param transl Vector of strings corresponding to the columns in `df` (excluding `dev`).
#' @param normuse Boolean, for using normality test (`shapiro.test`). If FALSE, uses median/IQR always.
#' @return A data frame with numeric measures (mean/median +/- sd/[range]), group labels and p-value.
#'         Note: Column names of the output are set to Russian ("Параметр", levels of `dev`, "p-уровень") based on current logic.
#' @export
#' @import dplyr
#' @examples
#' # df <- dplyr::tibble(
#' #   a = factor(c("D", "F", "F", "D"), levels = c("D", "F")),
#' #   b = c(1:4),
#' #   c = c(5:8)
#' # )
#'
#' # dev <- "a"
#' # transl <- c("group", "gender", "head color")
#' # create_table_num(df, dev, transl)
#' # Helper function two_level_num for 2 levels (t-test or Wilcoxon)
#' # Helper function three_level_num for 3 levels (ANOVA or Kruskal-Wallis)
#' # Helper function one_level_num for 1 level (Single group summary only)


create_table_num <- function(df, dev, transl, normuse = TRUE){
    if (length(levels(df[[dev]])) == 2){
      two_level_num(df, dev, transl, normuse)
    }else if (length(levels(df[[dev]])) == 3){
      three_level_num(df, dev, transl, normuse)
      }else if (length(levels(df[[dev]])) == 1){
        one_level_num(df, dev, transl, normuse)
  }else{
        print(paste('Уровней:',length(levels(df[[dev]]))))
  }
}




three_level_num <- function(df, dev, transl, normuse = TRUE){
  print('Три уровня')
  combined_df <- tibble(
    name = character(),
    group_1 = character(),
    group_2 = character(),
    group_3 = character(),
    p_value = character()
  )
  
  
  for (j in 2:ncol(df)){
    param1 <- c(colnames(df[j]))
    
    for (i in 1:length(unique(df[[dev]]))){
      a <- subset(df[[j]], df[[dev]] == sort(unique(df[[dev]]))[i])
      if (shapiro.test(a)[["p.value"]] > 0.05 | normuse == FALSE){
        param_1 <- round(mean(a,na.rm = T),1)
        param_2 <- round(sd(a,na.rm = T),1)
        param <- paste0(param_1," \u00B1 ",param_2)
      }else {
        param_1 <-  round(median(df[[j]],na.rm = T),1)
        q_mi <- round(quantile(a,na.rm = T)[2],1)
        q_ma <-round(quantile(a,na.rm = T)[4],1)
        param_2 <-  paste0(" [",q_mi,";",q_ma,"]")
        param <- paste0(param_1,param_2)
        
      }
      param1 <- append(param1,param)
    }

  
  
  if(shapiro.test(df[[j]])[["p.value"]] > 0.05){
    p_v <- round(summary(aov(df[[j]]~df[[dev]]))[[1]][["Pr(>F)"]][[1]],3)
  }else{
    p_v <- round(kruskal.test(df[[j]]~df[[dev]])[["p.value"]],3)
  }
  
  param1 <- append(param1,p_v)
  
  combined_df <- combined_df %>% 
    add_row(name = param1[1], 
            group_1 = param1[2],
            group_2 = param1[3],
            group_3 = param1[4],
            p_value = param1[5])
  
  }
  
  combined_df$name <- transl
  coln_1 <- levels(df[[dev]])[1]
  coln_2 <- levels(df[[dev]])[2]
  coln_3 <- levels(df[[dev]])[3]
  colnames(combined_df) <- c("Параметр",coln_1,coln_2,coln_3,"p-уровень")
  return(combined_df)
  
}




