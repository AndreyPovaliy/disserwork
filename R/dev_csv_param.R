#' Split CSV by group into separate files
#'
#' @param file_in Path to input CSV file
#' @param folder Output folder for split files
#' @param dev Name of the column to split by (as string)
#'
#' @return Invisibly returns the input data frame
#' @export
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr group_by group_walk
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' dev_csv_param("data.csv", "split", dev = "параметр")
#' }

dev_csv_param <- function(file_in, folder, dev = "параметр") {
  
  df <- readr::read_csv(file_in, show_col_types = FALSE)
  
  if (!dev %in% names(df)) {
    stop("Column '", dev, "' not found in ", file_in)
  }
  
  dir.create(folder, showWarnings = FALSE)
  
  df |>
    dplyr::group_by(.data[[dev]]) |>
    dplyr::group_walk(~ readr::write_csv(
      .x,
      file.path(folder, paste0(
        gsub("[^[:alnum:]_]", "_", .y[[dev]]),
        ".csv"
      ))
    ))
  
  invisible(df)
}