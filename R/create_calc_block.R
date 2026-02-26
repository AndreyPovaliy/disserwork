#' Create folders and frame to calculate
#'
#' @param path_folder Way to folder where create block
#'
#'
#' @return Folders to calculate
#' @export
#'



create_calc_block <- function(path_folder){
  if (!dir.exists(paste0(path_folder,"/calc"))) {
    dir.create(paste0(path_folder,"/calc"))
  }
  
  if (!dir.exists(paste0(path_folder,"/calc/input"))) {
    dir.create(paste0(path_folder,"/calc/input"))
  }
  
  if (!file.exists(paste0(path_folder,"/calc/input/get_data.R"))) {
    file.create(paste0(path_folder,"/calc/input/get_data.R"))
    print(paste("Created new file:", paste0(path_folder,"/calc/input/get_data.R")))
  } else {
    print(paste("File already exists:", paste0(path_folder,"/calc/input/get_data.R")))
  }
  
  cat("
# 1) read -------------------------------------------------------------------------

# 2) clear -------------------------------------------------------------------------

# 3) preparing -------------------------------------------------------------------------
# разделить на таблициы 
# материалы и результат
# категориальные и численные данные
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df$name <- as.character(df$name) # можно вывести из таблицы
input_metrics <- c(NAMES)
output_metrics <- c(NAMES)
devide <- c(NAMES)
index_dev <- c(grep(devide[1], colnames(df)))


df_in <- df[input_metrics]

df_out <- df[output_metrics]

df_fact_in <- cbind(df[index_dev],df_in[sapply(df_in, is.factor)])
rus_df_fact_in <- c(RUS_NAMES)

df_fact_out <- cbind(df[index_dev],df_out[sapply(df_out, is.factor)])
rus_df_fact_out <- c(RUS_NAMES)

df_num_in <- cbind(df[index_dev],df_in[sapply(df_in, is.numeric)])
rus_df_num_in <- c(RUS_NAMES) # without df[index_dev]
mes_df_num_in <- c(MESUEMENTS) # without df[index_dev]

df_num_out <- cbind(df[index_dev],df_out[sapply(df_out, is.numeric)])
rus_df_num_out <- c(RUS_NAMES) # without df[index_dev]
mes_df_num_out <- c(MESUEMENTS) # without df[index_dev]

# USE! print_col_names(df)

  ",file = paste0(path_folder,"/calc/input/get_data.R"))
  
  if (!dir.exists(paste0(path_folder,"/calc/processing"))) {
    dir.create(paste0(path_folder,"/calc/processing"))
  }
  if (!file.exists(paste0(path_folder,"/calc/processing/get_tables.R"))) {
    file.create(paste0(path_folder,"/calc/processing/get_tables.R"))
    print(paste("Created new file:", paste0(path_folder,"/calc/processing/get_tables.R")))
  } else {
    print(paste("File already exists:", paste0(path_folder,"/calc/processing/get_tables.R")))
  }
  
  
  cat("source(\"./calc/input/get_data.R\")

# matherials
table_n_1 <- create_table_fact(df_fact_in,devide[1],rus_df_fact_in)
table_n_2 <- create_table_num(df = df_num_in,dev = devide[1],transl = rus_df_num_in)


# results
table_n_3 <- create_table_fact(df_fact_out,devide[1],rus_df_fact_out)
table_n_4 <- create_table_num(df_num_out,devide[1],rus_df_num_out)

# p-value
#table_n_3_st <- subset(table_n_3, table_n_3$`p-уровень` <0.05)
#table_n_4_st <- subset(table_n_4, table_n_4$`p-уровень` <0.05)



# write tables
write.csv(table_n_1,file = \"/calc/result/tbl/table_n_1.csv\",row.names = F)

write.csv(table_n_2,file = \"/calc/result/tbl/table_n_2.csv\",row.names = F)

write.csv(table_n_3,file = \"/calc/result/tbl/table_n_3.csv\",row.names = F)

write.csv(table_n_4 ,file = \"/calc/result/tbl/table_n_4.csv\",row.names = F)", file = paste0(path_folder,"/calc/processing/get_tables.R"))
  
  if (!file.exists(paste0(path_folder,"/calc/processing/get_text.R"))) {
    file.create(paste0(path_folder,"/calc/processing/get_text.R"))
    print(paste("Created new file:", paste0(path_folder,"/calc/processing/get_text.R")))
  } else {
    print(paste("File already exists:", paste0(path_folder,"/calc/processing/get_text.R")))
  }

    cat("source(\"./calc/processing/get_tables.R\")

text_3 <- read.csv(\"./calc/result/tbl/table_n_3.csv\")

text_4 <- read.csv(\"./calc/result/tbl/table_n_4.csv\")


df <- text_3
filename1 <- \"./calc/result/txt/text_3.txt\"
print_text_cat(df,filename1)


df <- text_4
filename1 <- \"./calc/result/txt/text_4.txt\"
print_text_num(df,filename1)", file = paste0(path_folder,"/calc/processing/get_text.R"))
  
  
  if (!file.exists(paste0(path_folder,"/calc/processing/get_images.R"))) {
    file.create(paste0(path_folder,"/calc/processing/get_images.R"))
    print(paste("Created new file:", paste0(path_folder,"/calc/processing/get_images.R")))
  } else {
    print(paste("File already exists:", paste0(path_folder,"/calc/processing/get_images.R")))
  }
  
      cat("library(ggpubr)
library(ggplot2)
# fig_1
# fig_2
img <- ggarrange(fig_1, fig_2, fig_3, fig_4, fig_5,fig_6 + rremove(\"x.text\"), 
          labels = c('А', 'Б', 'В','Г', 'Д','Е'),
          ncol = 3, nrow = 2)


ggsave(img, filename = \"./calc/result/img/img_2.png\",width = 9,height = 6)
", file = paste0(path_folder,"/calc/processing/get_images.R"))

  if (!dir.exists(paste0(path_folder,"/calc/result"))) {
    dir.create(paste0(path_folder,"/calc/result"))
  }
  
  if (!dir.exists(paste0(path_folder,"/calc/result/txt"))) {
    dir.create(paste0(path_folder,"/calc/result/txt"))
  }
  
  if (!dir.exists(paste0(path_folder,"/calc/result/tbl"))) {
    dir.create(paste0(path_folder,"/calc/result/tbl"))
  }
  
  if (!dir.exists(paste0(path_folder,"/calc/result/img"))) {
    dir.create(paste0(path_folder,"/calc/result/img"))
  }
  
}
