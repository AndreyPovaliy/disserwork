#' Append tesis frame to file
#'
#' @param file_name Way to file
#'
#'
#' @return Append tesis frame to file. Mostly for .qmd files
#' @export
#'
#' @examples
#' add_tesis_frame("./tesis.qmd")

add_tesis_frame <- function(file_name){
  frame = "
*Название*

*Авторы*

*ФИО – аспирант/соискатель кафедры травматологии и ортопедии Медицинский институт/Клиника высоких технологий Санкт-Петербургский государственный университет, врач, Федеральное государственное бюджетное образовательное учреждение высшего образования «Санкт-Петербургский государственный университет»
Россия, 199034, Санкт-Петербург, Университетская наб., д. 7–9
e-mail: mail@domen.ru телефон: +0 000 000-00-00 SPIN-код: 0000-0000 ORCID: https://orcid.org/0000-0000-0000-0000*

# Аннотация

# Abstract

# *Тезис*

**Цель.**

**Материалы и методы.**

**Результаты.**

**Выводы.**

# *Thesis*

**Aim.**

**Materials and methods.**

**Results.**

**Conclusions.**

# Cписок литературы:

# References:

"
  cat(frame,file = file_name,append = T)
}
