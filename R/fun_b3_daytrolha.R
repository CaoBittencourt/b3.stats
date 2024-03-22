# [SETUP] -----------------------------------------------------------------
# - Packages (temp) ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages (temp)
  , 'dplyr', 'tidyr' #Data wrangling
  , 'vctrs' #Data frame subclasses
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'b3.data' #Tidy financial transactions (temp)
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){

    if(!require(pkg, character.only = T)){

      install.packages(pkg)

    }

    require(pkg, character.only = T)

  }
)

# Activate / install Git packages
Map(
  function(git, profile){

    if(!require(git, character.only = T)){

      install_github(
        paste0(profile, '/', git)
        , upgrade = F
        , force = T
      )

    }

    require(git, character.only = T)

  }
  , git = chr_git
  , profile = names(chr_git)
)

# [FUNCTIONS] --------------------------------------------------------------
# - Calculate daytrolha function ---------------------------------------------------------
fun_b3_daytrolha <- function(df_transactions){

  # arguments validation
  stopifnot(
    "'df_transactions' must be a data frame with the 'df_transactions' subclass." =
      all(
        is.data.frame(df_transactions)
        , any(class(df_transactions) == 'df_transactions')
      )
  )

  # identify daytrolha

  # calculate trolha

  # aggregate trolha by month-year

  # aggregate trolha by year

  # output
  return(list(
    'daytrolha' = list_daytrolha$transactions,
    'daytrolha_month' = list_daytrolha$month,
    'daytrolha_year' = list_daytrolha$year
  ))

}

# [TEST] ------------------------------------------------------------------
# - Test data -------------------------------------------------------------
# b3 financial transactions files
list(
  '/home/Cao/Storage/github/auto.tax/data/2019/transactions_2019.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2020/transactions_2020.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2021/transactions_2021.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2022/transactions_2022.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2023/transactions_2023.xlsx'
) -> list_transactions

# - fun_b3_clean ----------------------------------------------------------
list_transactions %>%
  fun_b3_clean() ->
  list_b3_data

# - fun_b3_daytrolha ------------------------------------------------------
list_b3_data$
  transactions %>%
  filter(stock) %>%
  group_by(
    ticker,
    date,
    type,
    event
  ) %>%
  tally() %>%
  arrange(-n) %>%
  print(n = 100)
