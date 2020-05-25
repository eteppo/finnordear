# finnordear

**finnordear** contains R functions that make it much easier to analyze Finnish Nordea bank statement (tiliote) pdf-files in R.

**finnordear**-paketti sisältää R-funktioita, joiden avulla suomenkielisten Nordean tiliotteiden analysointi on helpompaa.

### 1. Get bank statements from online bank

Download your own data from your Nordea online bank account. You can find statements (Tiliote) under the accounts (Tilit) tab. For now, you'll need to select a range that you're interested in and manually save each pdf-file into a directory.

### 2. Install & load the package

``` r
install.packages("devtools")
devtools::install_github("eteppo/finnordear")
library(finnordear)
```

You probably want to use `tidyverse` too like we use below.

``` r
install.packages("tidyverse")
library(tidyverse)
```

### 3. Read statement files from a directory to R and inspect.

``` r
statements <- read_statements("/path/to/statements/")
View(statements)
```

### 4. Explore and discover useful patterns

You could start by visualizing your transaction and balance history and transaction sums by counterparty.

``` r
plot_history(statements)
plot_partytotals(statements)
```

