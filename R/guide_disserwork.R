#' Create disser frame by heading
#'
#'
#'
#' @return Type instruction to use this package
#' @export


guide_disserwork <- function(){
  answer = readline("Please choose info:
1 - about disser,
2 - about article,
3 - about tesis,
4 - about lec protocol,
5 - about sciense protocol")
  message <- switch(answer,
                  "1" = "1) use commnds quarto create-project my-book --type book,
  NB! For naming you have to use acronym!
2) Run function create_disser_str(\"./\")
3) Edit plan
4) setwd() and Run function create_disser_frame(\"plan.json\"),
5) Please work with quarto
6) To preview: quarto preview.
7) To render: quarto render",
                  "2" = "1) use commnds quarto create-project my-book --type manuscript,
  NB! For naming you have to use art_YOURNAME!
2) Run function add_article_frame(\"index.qmd\")
3) Please work with quarto
4) To preview: quarto preview.
5) To render: quarto render",
                  "3" = "1) use commnds quarto create-project my-book --type manuscript,
  NB! For naming you have to use tesis_YOURNAME!
2) Run function add_tesis_frame.R
3) Please work with quarto
4) To preview: quarto preview.
5) To render: quarto render",
                  "4" = "1) use commnds quarto create-project my-book --type manuscript,
  NB! For naming you have to use lec_prot_YOURNAME!
2) Run function add_lec_protocol_frame(\"index.qmd\")
3) Please work with quarto
4) To preview: quarto preview.
5) To render: quarto render",
                  "5" = "1) use commnds quarto create-project my-book --type manuscript,
  NB! For naming you have to use sc_prot_YOURNAME!
2) Run function add_sc_protocol_frame(\"index.qmd\")
3) Please work with quarto
4) To preview: quarto preview.
5) To render: quarto render",
                  "unknown command")
  cat(message)

}
