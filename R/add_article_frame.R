#' Append article frame to file
#'
#' @param file_name Way to file
#'
#'
#' @return Append article frame to file. Mostly for .qmd files
#' @export
#'
#' @examples
#' add_article_frame("./article.qmd")

add_article_frame <- function(file_name){
  frame = "## Аннотация

**Введение**

**Цель**

**Материалы и методы**

**Результаты**

**Дисскуссия**

**Заключение**

**Ключевые слова**

## Abstract

**Introduction**

**Purpose**

**Materials and Methods**

**Results**

**Discussion**

**Conclusion**

**Keywords**

## Введение

## Цель

## Материалы и методы

## Результаты

## Дисскуссия

## Заключение
"
  cat(frame,file = file_name,append = T)
}
