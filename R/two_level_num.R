#' Создание таблицы с числовыми мерными показателями для двух группировок
#'
#' @param df Data frame содержащий переменные измерения.
#'         Должен содержать: одну колонку для группировки (`dev`) и несколько числовых колонок.
#' @param dev Имя колонки, по которой будет производиться группировка (фактор или character). 
#'             *Важно:* Функция ожидает ровно два уникальных уровня в этой колонке.
#' @param transl Вектор строк, соответствующих названиям числовых колонок в `df` (в порядке следования).
#'               Эти строки станут первыми столбцами таблицы результатов (переменные).
#' @param normuse Булево, для использования теста нормальности (`shapiro.test`).
#'                Если TRUE: проверяет нормальность. Если нормальны - Mean +/- SD.
#'                Если FALSE или тест неудачен: Median [Q1; Q3].
#' @return Data frame with numeric measures. Columns: "Параметр", values of group 1, values of group 2, p-уровень.
#'         Note: Column names are set based on the levels of `dev` (if using old logic) or fixed columns if refactored.
#' @export
#' @import dplyr
#' @examples
#' # df <- tibble(
#' #   group = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
#' #   score1 = c(1, 2, 3, 4),
#' #   score2 = c(5, 6, 7, 8)
#' # )
#'
#' # transl <- c("Очки 1", "Очки 2")
#' # two_level_num(df, "group", transl)

two_level_num <- function(df, dev, transl, normuse = TRUE){
  print('Два уровня')
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
    p_v <- round(t.test(df[[j]]~df[[dev]])[["p.value"]],3)
  }else{
    p_v <- round(wilcox.test(df[[j]]~df[[dev]])[["p.value"]],3)
  }
  
  
  
  
  param1 <- append(param1,p_v)
  
  combined_df <- combined_df %>% 
    add_row(name = param1[1], 
            group_1 = param1[2],
            group_2 = param1[3],
            p_value = param1[4])
  
  
  }
  combined_df$name <- transl
  coln_1 <- levels(df[[dev]])[1]
  coln_2 <- levels(df[[dev]])[2]
  colnames(combined_df) <- c("Параметр",coln_1,coln_2,"p-уровень")
  return(combined_df)
}