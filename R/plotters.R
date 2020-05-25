#' Plot transaction and balance history.
#'
#' @param data A dataframe returned by read_statement*-functions.
#'
#' @return A ggplot-object.
#'
#' @importFrom magrittr "%>%"
#'
#' @export

plot_history <- function(data) {
    data %>%
        dplyr::filter(type == "transaction") %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = record_date, y = amount)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(y = "Amount") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) -> transaction_history

    data %>%
        dplyr::filter(type == "balance") %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = record_date, y = amount)) +
        ggplot2::theme_minimal() +
        ggplot2::labs(y = "Amount", x = "Date") +
        ggplot2::scale_y_continuous(limits = c(0, NA)) -> balance_history

    patchwork::wrap_plots(transaction_history, balance_history, ncol = 1)
}

#' Plot transactions totals by counterparty.
#'
#' @param data A dataframe returned by read_statement*-functions.
#'
#' @return A ggplot-object.
#'
#' @importFrom magrittr "%>%"
#'
#' @export

plot_partytotals <- function(data) {

    data %>%
        dplyr::filter(type == "transaction") %>%
        dplyr::group_by(counterparty) %>%
        dplyr::summarise(
            amount = base::sum(amount, na.rm = TRUE),
            count = dplyr::n()
        ) %>%
        dplyr::mutate(
            absolute_amount = base::abs(amount),
            sign_amount = base::as.character(base::sign(amount))
        ) %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(
            ggplot2::aes(y = forcats::fct_reorder(counterparty, amount), x = absolute_amount, fill = sign_amount),
            stat = "identity"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::scale_x_log10() +
        ggplot2::scale_fill_manual(guide = FALSE, values = c("red", "green")) +
        ggplot2::labs(x = "Absolute amount", y = "Counterparty") -> plot

    return(plot)

}
