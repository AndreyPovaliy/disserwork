#' Создание сводной таблицы с числовыми показателями для одной группы
#'
#' @param df Data frame содержащий переменные измерения. Должен содержать одну колонку группировки (`dev`)
#'            и несколько числовых колонок.
#' @param dev Имя колонки, по которой будет производиться группировка (фактор или character).
#'             *Важно:* В данной функции предполагается ровно одно уникальное значение (одна группа).
#' @param transl Вектор строк, соответствующих названиям числовых колонок в `df` (в порядке следования).
#'  @param normuse Булево, для использования теста нормальности (`shapiro.test`).
#'                Если TRUE: проверяет нормальность. Если нормальные - Mean +/- SD, p-value via ANOVA (F-test).
#'                Если FALSE или тест неудачен: Median [Q1; Q3], p-value via Kruskal-Wallis.
#' @return Data frame with numeric measures. Columns: "Параметр", values of group 1, values of group 2,
#'         values of group 3, p-уровень. Rows correspond to variables in `transl`.
#'         Note: Column names are set based on the levels of `dev` (if using old logic) or fixed columns if refactored.
#' @export
#' @import dplyr
#' @examples
#' # df <- tibble(
#' #   group = factor(c("A", "A", "A", "A", "A", "A"), levels = c("A")),
#' #   score1 = c(1, 2, 3, 4, 5, 6),
#' #   score2 = c(7, 8, 9, 10, 11, 12)
#' # )
#'
#' # transl <- c("Очки 1", "Очки 2")
#' # one_level_num(df, "group", transl)

one_level_num <- function(df, dev, transl, normuse = TRUE){
  print('Кагорта')
  
  combined_df <- tibble(
    name = character(),
    group_1 = character()
  )
  
  
  for (j in 2:ncol(df)){
    param1 <- c(colnames(df[j]))
    
    
    a <- subset(df[[j]], df[[dev]] == sort(unique(df[[dev]])))
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
    

  
  combined_df <- combined_df %>% 
    add_row(name = param1[1], 
            group_1 = param1[2])
  }
  combined_df$name <- transl
  coln_1 <- levels(df[[dev]])[1]
  colnames(combined_df) <- c("Параметр",coln_1,"p-уровень")
  return(combined_df)
  }

