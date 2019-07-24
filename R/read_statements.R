#' Read and clean many Finnish Nordea bank statement pdf-files at once.
#'
#' This is just a convenient plural version of \code{read_statement}.
#'
#' @param directory A full path to a directory of Finnish Nordea bank statement pdf-files.
#' @return A dataset of transactions and balances of classes data.frame (base) and tbl_df (tibble).
#' @importFrom dplyr "%>%"
#' @export

read_statements <- function(directory) {

    files <- directory %>%
        stringr::str_c("/", base::dir(directory)) %>%
        stringr::str_subset(".pdf")

    # problem : directory can contain duplicate files
    # solution: select unique statement files by calculating MD5 hashes on parsed text
    md5_hashes <- files %>%
        purrr::map_chr(function(x) digest::digest(pdftools::pdf_text(x), algo = "md5"))

    unique_files <- files[!duplicated(md5_hashes)]

    # problem: multiple different statements contain the same transaction
    # solution: remove all duplicated rows
    ## NOTE: valid identical transactions are lost too in this approach, e.g.,
    ## multiple identical purchases on the same day
    statements <- unique_files %>%
        purrr::map_dfr(finnordear::read_statement) %>%
        dplyr::distinct_all()

    return(statements)
}
