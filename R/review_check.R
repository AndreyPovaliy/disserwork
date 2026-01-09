#' Check review chapters
#'
#' @param pages Target pages
#' @param actual_year Actual year poblications
#' @param file_Rmd To target file Rmd
#' @param file_bib To target file bib
#'
#'
#' @return String for table
#' @export
#' @importFrom utils read.csv
#' @import tidytext
#' @import tidyverse
#' @import bib2df bib2df
#'
#' @examples
#' # pages <- 200 # целевое значение страниц
#' # actual_year <- 2015 # уровень актуальности
#' # file_Rmd <- "archive/Text_work/Chapters/full_text/Abus_full.Rmd"
#' # file_bib <- "archive/Text_work/Chapters/full_text/references.bib"
#' 
#' # review_check(pages,actual_year,file_Rmd,file_bib)

review_check <- function(pages,actual_year,file_Rmd,file_bib){
  ## подключение фалов ------------------------------------------------------------
  take_f_txt_keys <- read.csv(file_Rmd,header = F,sep = '\n')
  
  file_content <- readLines(file_Rmd, warn = FALSE)
  
  
  ## work --------------------------------------------------------------------
  
  colnames(take_f_txt_keys) <- c("text")
  # весь текст
  control_full <-take_f_txt_keys %>%
    unnest_tokens(word, text)
  
  # сбор введенных ссылок
  control_filt <- take_f_txt_keys %>%
    unnest_tokens(word, text) %>%
    filter(str_detect(word, regex("^[a-z]+[0-9][0-9][0-9][0-9]$", ignore_case = TRUE))) %>% 
    distinct(word)
  
  
  # создание датафрейма по списку
  bib_dataframe <- bib2df(file_bib)
  bib_dataframe$is_rus <- ifelse(bib_dataframe$LANGID == "ru", 1,0)
  bib_dataframe$ACTUAL <- ifelse(bib_dataframe$YEAR >actual_year, 1,0)
  
  
  
  # показ невведенных ссылок  
  bib_dataframe_out <- bib_dataframe %>%
    filter(!(BIBTEXKEY %in% control_filt$word))
  
  
  if(any(!(bib_dataframe_out$BIBTEXKEY %in% control_full$word))){
    print(bib_dataframe_out$BIBTEXKEY[!(bib_dataframe_out$BIBTEXKEY %in% control_full$word)])
    ffilt <- rbind(control_filt$word, bib_dataframe_out$BIBTEXKEY[bib_dataframe_out$BIBTEXKEY %in% control_full$word])
    
  }else{
    ffilt <- rbind(control_filt$word, bib_dataframe_out$BIBTEXKEY)
  }
  # проверка введенных ссылок
  bib_dataframe <- bib_dataframe %>%
    filter(BIBTEXKEY %in% ffilt)
  
  gg1 <- bib_dataframe %>% 
    ggplot(aes(x = as.numeric(YEAR), fill = as.factor(ACTUAL)))+
    geom_density(alpha = 0.5)+
    scale_x_continuous(breaks = seq(min(bib_dataframe$YEAR),max(bib_dataframe$YEAR),2))+
    scale_fill_viridis_d()+
    labs(
      title = paste("Процент актуальных источников с",actual_year,"года: ", round(sum(bib_dataframe$ACTUAL)/nrow(bib_dataframe)*100,2), "%.",
                    "\nВсего: ", nrow(bib_dataframe),
                    "\nРусскоязычные: ",sum(bib_dataframe$is_rus),
                    "\nИностранные: ",nrow(bib_dataframe) - sum(bib_dataframe$is_rus)
      ),
      x = "Год",
      y = "Частота",
      fill = "Актуальный"
    )
  
  print(gg1)
  
  combined_text <- paste(file_content, collapse = "")
  
  # Подсчитываем количество символов
  num_characters <- nchar(combined_text)
  
  # Выводим результаты:
  print(paste("Неиспользуемые ссылки: ", bib_dataframe_out$BIBTEXKEY[!(bib_dataframe_out$BIBTEXKEY %in% control_full$word)]))
  
  print(paste("Неправильный формат ссылки: ", 
              bib_dataframe$BIBTEXKEY[!(str_detect(bib_dataframe$BIBTEXKEY, "^[a-z]+[0-9][0-9][0-9][0-9]$"))]
  ))
  
  
  
  print(paste("Пропущенный языковой иднтификатор: ", bib_dataframe$BIBTEXKEY[is.na(bib_dataframe$LANGID)]))
  
  print(paste("Дубликаты в списке литературы: ", bib_dataframe$BIBTEXKEY[duplicated(bib_dataframe$BIBTEXKEY)]))
  
  print(paste("Количество страниц:", num_characters/2000))
  print(paste("Осталось до полного объема:", round((1 - num_characters/2000/pages)*100,2),"%"))
  
  print(paste("Процент актуальных источников с",actual_year,"года: ", round(sum(bib_dataframe$ACTUAL)/nrow(bib_dataframe)*100,2), "%.",
              "Всего: ", nrow(bib_dataframe),
              "Русскоязычные: ",sum(bib_dataframe$is_rus),
              "Иностранные: ",nrow(bib_dataframe) - sum(bib_dataframe$is_rus)
  ))
  return(bib_dataframe)
  
}
