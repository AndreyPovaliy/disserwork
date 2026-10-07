#' Create df with factor measures
#'
#' @param df Exploring dataframe
#' @param dev Vector with devide (name of the grouping column)
#' @param transl Vector with names to final text
#'
#' @return df with factor measures
#' @export
#'
#' @examples
#' # df <- dplyr::tibble(
#' #   a = as.factor(c("D","F","F","D")),
#' #   b = as.factor(c("male","female","female","male")),
#' #   c = as.factor(c("blond","redhead","blond","brunet")))
#' # dev <- "a"
#' # transl <- c("group","gender","head color")
#' # create_table_fact(df, dev, transl)

create_table_fact <- function(df, dev, transl) {
  
  # --- проверка входных данных ---
  if (!dev %in% names(df)) {
    stop("Column '", dev, "' not found in df")
  }
  
  combined_df <- data.frame(
    name  = character(),
    group = character(),
    count = character(),
    n     = integer(),
    pr    = character(),
    pvl   = numeric(),
    stringsAsFactors = FALSE
  )
  
  index_dev <- which(names(df) == dev)
  
  for (i in 2:ncol(df)) {
    
    # --- тест ---
    tab_xy <- table(df[[i]], df[[dev]])
    
    if (all(tab_xy > 5)) {
      pvl <- chisq.test(tab_xy, simulate.p.value = TRUE)$p.value
    } else {
      pvl <- fisher.test(tab_xy, simulate.p.value = TRUE)$p.value
    }
    
    # --- сборка таблицы ---
    table_prt_2 <- df %>%
      dplyr::group_by_at(c(index_dev, i)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by_at(index_dev) %>%
      dplyr::mutate(
        # ПРАВКА 1: сначала умножаем на 100, потом округляем до 1 знака
        pr = paste0(round(n / sum(n) * 100, 1), "%")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        name = colnames(df)[i],
        pvl  = round(pvl, 3)
      ) %>%
      dplyr::relocate(name, .before = 1)
    
    # ПРАВКА 2: правильный порядок столбцов после relocate + mutate
    # name, group, count, n, pr, pvl
    colnames(table_prt_2) <- c("name", "group", "count", "n", "pr", "pvl")
    
    combined_df <- rbind(combined_df, table_prt_2)
  }
  
  # --- ПРАВКА 3: перевод названий параметров ---
  uniq_names <- unique(combined_df$name)
  
  if (length(transl) < length(uniq_names)) {
    warning("transl короче числа уникальных параметров: ",
            length(uniq_names), " vs ", length(transl))
  }
  
  for (i in seq_along(uniq_names)) {
    combined_df$name[combined_df$name == uniq_names[i]] <- transl[i]
  }
  
  # --- финальные имена ---
  colnames(combined_df) <- c("параметр", "группа", "значения",
                             "количество", "процент", "p-уровень")
  
  return(combined_df)
}