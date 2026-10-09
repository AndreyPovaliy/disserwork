#' Create an ETL template file
#'
#' Creates a folder (if it does not exist) and writes a template R script
#' with a standard ETL pipeline skeleton: reading raw data, building a
#' dictionary, transformation, missing-value diagnostics, and export.
#'
#' @param path Character. Directory where the template file will be created.
#'   Defaults to \code{"data"} relative to the current working directory.
#' @param file_name Character. Name of the template file.
#'   Defaults to \code{"etl_file.R"}.
#' @param overwrite Logical. If \code{TRUE}, overwrites an existing file.
#'   Defaults to \code{FALSE}.
#'
#' @return Invisibly returns the full path to the created file.
#' @export
#'
#' @examples
#' \dontrun{
#' etl_file()
#' etl_file(path = tempdir(), file_name = "my_etl.R", overwrite = TRUE)
#' }
etl_file <- function(path = "data",
                     file_name = "etl_file.R",
                     overwrite = FALSE) {
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  file_path <- file.path(path, file_name)
  
  if (file.exists(file_path) && !overwrite) {
    stop("Файл уже существует: ", file_path,
         ". Используйте overwrite = TRUE.")
  }
  
  template <- c(
    "library(readxl)",
    "library(tidyverse)",
    "library(openxlsx)",
    "library(naniar)",
    "",
    "# чтение сырых данных ---------------------------------------------------------------",
    "",
    "# data",
    "",
    "",
    "# словарь ---------------------------------------------------------------",
    "# rus_name",
    "",
    "",
    "# преобразование и очистка данных ---------------------------------------------------------------",
    "",
    "",
    "# работа с пропущенными значениями ---------------------------------------------------------------",
    "# miss_var_summary(data)",
    "",
    "# График пропусков по переменным",
    "# gg_miss_var(data)",
    "",
    "# собрать df для документирования столбцов ---------------------------------------------------------------",
    "",
    "",
    "# Вывод -------------------------------------------------------------------",
    "# write.xlsx(data, \"./data/data_pro/data.xlsx\")",
    "# write.xlsx(data_names, \"./data/data_pro/data_names.xlsx\")",
    "",
    "",
    "# Очистить окружение -------------------------------------------------------------------",
    "# rm(list = ls())"
  )
  
  writeLines(template, con = file_path, useBytes = TRUE)
  
  invisible(file_path)
}