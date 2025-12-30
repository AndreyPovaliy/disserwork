#' Print steps to prepare and names
#'
#'
#'
#' @return Instruction
#' @export


prepare_data <- function(){

  cat("
# 1) read -------------------------------------------------------------------------

# 2) clear -------------------------------------------------------------------------

# 3) preparing -------------------------------------------------------------------------
# разделить на таблициы 
# материалы и результат
# категориальные и численные данные
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df$name <- as.character(df$name) # ожно вывести из таблицы
input_metrics <- с(NAMES)
output_metrics <- с(NAMES)
devide <- с(NAMES)
index_dev <- c(grep(devide[1], colnames(df)))


df_in <- df[input_metrics]

df_out <- df[output_metrics]

df_fact_in <- df_in[sapply(df_in, is.factor)]
rus_df_fact_in <- c(RUS_NAMES)

df_fact_out <- df_out[sapply(df_out, is.factor)]
rus_df_fact_out - c(RUS_NAMES)

df_num_in <- cbind(df[index_dev],df_in[sapply(df_in, is.numeric)])
rus_df_2_num_in <- c(RUS_NAMES)

mes_df_2_num_in <- c(MESUEMENTS)

df_num_out <- cbind(df[index_dev],df_out[sapply(df_out, is.numeric)])
rus_df_2_num_out <- c(RUS_NAMES)
mes_df_2_num_out <- c(MESUEMENTS)

USE! print_col_names(df)

  ")

}