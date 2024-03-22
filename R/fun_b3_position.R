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
# - Calculate position and mean price function ---------------------------------------------------------
fun_b3_position <- function(df_transactions){

  # arguments validation
  stopifnot(
    "'df_transactions' must be a data frame with the 'df_transactions' subclass." =
      all(
        is.data.frame(df_transactions)
        , any(class(df_transactions) == 'df_transactions')
      )
  )

  # output
  return()

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
# - fun_b3_position -------------------------------------------------------
list_b3_data$
  transactions %>%
  group_by(
    ticker,
    cycle
  ) %>%
  mutate(
    position =
      cumsum(qtd)
    , mean_price = if_else(
      position != 0
      , cumsum(qtd * price) /
        cumsum(qtd)
      , lag(mean_price)
    )
      # cumsum(position * price) /
      # cumsum(position)
    , total =
      mean_price *
      position
  ) %>%
  select(
    -type,
    -stock
  ) %>%
  filter(
    ticker == 'FHER3'
    # ticker == 'GSHP3'
    # ticker == 'BIDI4'
    # ticker == 'WEGE3'
  )

list_b3_data$
  transactions %>%
  filter(
    event == 'transferência'
  ) %>%
  print(n = Inf)


