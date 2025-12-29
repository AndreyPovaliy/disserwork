#' Append article frame to file
#'
#' @param file_name Way to file
#'
#'
#' @return Append article frame to file. Mostly for .qmd files
#' @export
#'

add_article_frame <- function(file_name){
  frame = "# Название статьи

Ф И.О.^1^, Ф И.О.^1,2^

1 - Учереждение 1, Город, Страна

2 - Учереждение 2, Город, Страна

## Об авторах

ФИО –  должность, Город, Страна.

e-mail: [mail](mailto:mail)

SPIN-код: 0000-0000

ORCID: <https://orcid.org/0000-0000-0000-0000>

## Аннотация

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
