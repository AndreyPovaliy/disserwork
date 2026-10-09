#' Create a data dictionary for a data frame
#'
#' Builds a tibble describing the columns of a data frame: English names,
#' Russian names, category labels, detected classes, and a placeholder for
#' metric designation. Also returns the names of numeric columns that are
#' candidates for numeric metrics.
#'
#' @param df A data frame whose columns should be described.
#' @param rus_names Character vector of Russian column names.
#'   Must have the same length as \code{ncol(df)}.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{data_names}{A tibble with columns \code{eng_names},
#'     \code{rus_names}, \code{cat_names}, \code{data_classes},
#'     \code{num_metrics}.}
#'   \item{to_num_metrics}{Character vector of English column names
#'     whose columns are numeric (candidates for numeric metrics).}
#' }
#'
#' @details
#' The \code{cat_names} column is filled with the placeholder value
#' \code{"dev or method or net or result"} for every column and is
#' intended to be edited later.
#'
#' The \code{num_metrics} column is initialised with \code{NA_character_}
#' and is meant to be filled in manually (e.g. with metric names).
#'
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#' df <- data.frame(
#'   id    = 1:3,
#'   value = c(1.1, 2.2, 3.3),
#'   group = c("a", "b", "c")
#' )
#' res <- data_names(df, c("Идентификатор", "Значение", "Группа"))
#' res$data_names
#' res$to_num_metrics
data_names <- function(df, rus_names) {
  stopifnot(is.data.frame(df))
  
  eng_names <- colnames(df)
  if (length(rus_names) != length(eng_names)) {
    stop("Длина rus_names не совпадает с числом колонок df")
  }
  
  cat_names    <- rep("dev or method or net or result", ncol(df))
  data_classes <- vapply(df, function(x) class(x)[1], character(1))
  num_flags    <- vapply(df, is.numeric, logical(1))
  num_metrics  <- rep(NA_character_, ncol(df))
  
  dict_tbl <- tibble::tibble(
    eng_names    = eng_names,
    rus_names    = rus_names,
    cat_names    = cat_names,
    data_classes = data_classes,
    num_metrics  = num_metrics,
    is_num       = num_flags
  )
  
  to_num_metrics <- dict_tbl$eng_names[dict_tbl$is_num]
  
  list(
    data_names     = dict_tbl[, c("eng_names", "rus_names",
                                  "cat_names", "data_classes",
                                  "num_metrics")],
    to_num_metrics = to_num_metrics
  )
}