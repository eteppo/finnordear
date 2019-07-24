# finnordear

**finnordear** is an R toolkit that makes it much easier and faster to analyze Finnish Nordea bank statements (tiliote) in R. See the current features in the [function reference](https://eteppo.github.io/finnordear/).

1. Get bank statements
---------------

You can download your own data from your Nordea online bank account. You can find statements (Tiliote) under the accounts (Tilit) tab. For now, you'll need to select a range that you're interested in and manually save each pdf-file.

2. Install & load the package
------------

``` r
install.packages("devtools")
devtools::install_github("eteppo/finnordear")
library(finnordear)
```

You probably want to use `tidyverse` too.

``` r
install.packages("tidyverse")
library(tidyverse)
```

3. Read statement files to R
------------

``` r
statements <- read_statements("/path/to/statements/")
```

4. Explore and discover useful patterns
-------------

You could start by visualizing your transaction and balance history.

``` r
statements %>%
    filter(type == "transaction") %>%
    ggplot() +
        geom_point(aes(x = record_date, y = amount)) +
        theme_minimal()
```

``` r
statements %>%
    filter(type == "balance") %>%
    ggplot() +
        geom_point(aes(x = record_date, y = amount)) +
        theme_minimal()
```

Calculate sum over transactions for each counterparty, sort, and explore the table.

``` r
cp_totals <- statements %>%
    filter(type == "transaction") %>%
    group_by(counterparty) %>%
    summarise(
        amount = sum(amount, na.rm = TRUE),
        count = n()
    ) %>%
    arrange(desc(amount))

View(cp_totals)
```

