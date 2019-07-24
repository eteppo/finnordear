#' Read and clean a Nordea bank statement pdf-file.
#'
#' \code{read_statement} reads and cleans up a Finnish Nordea bank statement
#' pdf-file and returns a tidy dataset of classes data.frame (base) and "tbl_df" (tibble).
#'
#' @param file A full path to a Finnish Nordea bank statement pdf-file.
#'
#' @return A dataset of balances and transactions of classes data.frame (base) and tbl_df (tibble).
#'
#' @importFrom dplyr "%>%"
#'
#' @export

read_statement <- function(file) {

  pdftools::pdf_text(file) %>%
    stringr::str_split("\n") %>%
    unlist() %>%
    stringr::str_squish() %>%
    dplyr::tibble(line = 1:length(.), text = .) %>%
    # lines with numbers with , and ending with + or -
    dplyr::filter(
      stringr::str_detect(text, ",[0-9]+\\+") |
        stringr::str_detect(text, ",[0-9]+-")
    ) %>%
    tidyr::extract(text, "amount", "([0-9\\.]+,[0-9\\.\\+-]+)", remove = FALSE) %>%
    tidyr::extract(text, "counterparty", "([a-zA-Z]+.+[a-zA-Z]+)", remove = FALSE) %>%
    tidyr::extract(text, "record_date", "(^[0-9][0-9\\.]+[0-9]+)") %>%
    dplyr::filter(!is.na(record_date)) %>%
    dplyr::mutate(type = dplyr::case_when(
      is.na(record_date) ~ "balance",
      stringr::str_detect(record_date, "\\.[0-9]+\\.") ~ "balance",
      TRUE ~ "transaction"
    )) %>%
    dplyr::filter(!(dplyr::row_number() == nrow(.) & type == "transaction")) %>%
    tidyr::separate(record_date, c("day", "month", "year"), fill = "right") %>%
    dplyr::mutate(year = dplyr::if_else(
      condition = is.na(year),
      true = year %>%
        na.omit() %>%
        unique() %>%
        as.numeric() %>%
        max() %>%
        as.character(),
      false = year
    )) %>%
    tidyr::unite(col = record_date, day, month, year, sep = "-") %>%
    dplyr::mutate(
      record_date = lubridate::dmy(record_date),
      amount = amount %>%
        stringr::str_remove_all("\\.") %>%
        stringr::str_replace(",", ".") %>%
        stringr::str_remove("\\+")
    ) %>%
    dplyr::mutate(amount = dplyr::if_else(
      stringr::str_detect(amount, "-"),
      amount %>% stringr::str_remove("-") %>% stringr::str_c("-", .),
      amount
    )) %>%
    dplyr::mutate(
      amount = as.numeric(amount),
      counterparty = dplyr::if_else(counterparty %in% c("Saldo", "Loppusaldo"), "N/A", counterparty)
    ) %>%
    dplyr::select(
      type,
      record_date,
      counterparty,
      amount,
      -line
    ) %>%
    dplyr::arrange(type, record_date)

}
