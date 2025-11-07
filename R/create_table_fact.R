#' Create df with factor measures
#'
#' @param df Exploring dataframe
#' @param dev Vector with devide
#' @param transl Vector with names to final text
#'
#'
#' @return  df with factor measures
#' @export
#'
#' @examples
#' df <- dplyr::tibble(
#' a = as.factor(c("D","F","F","D")),
#' b = as.factor(c("male","female","female","male")),
#' c = as.factor(c("blond","redhead","blomd","brunet")))
#' dev <- "a"
#' transl <- c("group","gender","head color")
#' create_table_fact(df, dev, transl)



create_table_fact <- function(df, dev, transl) {
  combined_df <- data.frame(name = character(),
                            group = factor(),
                            count = factor(),
                            n = integer(),
                            pr = integer(),
                            pvl = integer())
  for (i in 2:ncol(df)){
    tab_xy <- table(df[[i]], df[[dev]])
    if (all(tab_xy > 5)) {
      pvl <- chisq.test(tab_xy, simulate.p.value = TRUE)$p.value
    }else {
      pvl <- fisher.test(tab_xy, simulate.p.value = TRUE)$p.value
    }
    index_dev <- c(grep(dev, colnames(df)))
    table_prt_2 <- df %>% 
      dplyr::group_by_at(c(index_dev,i)) %>% 
      dplyr::summarise(
        name = colnames(df)[i],
        n = dplyr::n(),
        pr = paste0(round(n/nrow(df),2)*100,"%"),
        pvl = round(pvl,3)
      ) %>% 
      dplyr::relocate(name,.before = 1)
    colnames(table_prt_2) <- c("name","group","count","n", "pr","pvl")
    combined_df <- rbind(combined_df, table_prt_2)
    print(combined_df)
  }
  for (i in 1:length(unique(combined_df$name))){
    combined_df$name[combined_df$name == unique(combined_df$name)[i]] <- transl[i]
    print(combined_df)
  }
  colnames(combined_df) <- c("параметр", "группа", "значения", "количество", "процент","p-уровень")
  return(combined_df)
}