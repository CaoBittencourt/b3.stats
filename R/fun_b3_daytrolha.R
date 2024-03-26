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
  'CaoBittencourt' = 'b3.data', #Tidy financial transactions (temp)
  'CaoBittencourt' = 'b3.stats' #Tidy financial statistics (temp)

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
# NOTE: TO CALCULATE TROLHA MEAN_PRICE IS REQUIRED! ------------------------
# - Identify daytrolha function -------------------------------------------
fun_b3_is_daytrolha <- function(df_transactions){

  # arguments validated in main function

  # daytrolha = buy and sell same asset on the same day
  df_transactions %>%
    group_by(
      date,
      ticker
    ) %>%
    mutate(
      .after = stock
      , daytrolha =
        length(unique(type)) > 1
    ) -> df_transactions

  # output
  return(df_transactions)

}

# - Calculate daytrolha function ---------------------------------------------------------
fun_b3_daytrolha <- function(df_transactions, df_position = NULL){

  # arguments validation
  stopifnot(
    "'df_transactions' must be a data frame with the 'df_transactions' subclass." =
      all(
        is.data.frame(df_transactions)
        , any(class(df_transactions) == 'df_transactions')
      )
  )

  # identify daytrolha
  df_transactions %>%
    fun_b3_is_daytrolha() ->
    df_transactions

  # get mean prices

  # calculate trolha

  # aggregate trolha by month-year

  # aggregate trolha by year

  # aggregate trolha by period

  # output
  return(df_transactions)
  # return(list(
  #   'daytrolha' = list_daytrolha$transactions,
  #   'daytrolha_month' = list_daytrolha$month,
  #   'daytrolha_year' = list_daytrolha$year
  # ))

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

# b3 financial events files
list(
  '/home/Cao/Storage/github/auto.tax/data/2019/events_2019.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2020/events_2020.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2021/events_2021.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2022/events_2022.xlsx',
  '/home/Cao/Storage/github/auto.tax/data/2023/events_2023.xlsx'
) -> list_events

# # b3 financial position files
# list(
#   '/home/Cao/Storage/github/auto.tax/data/2020/position_2020.xlsx',
#   '/home/Cao/Storage/github/auto.tax/data/2021/position_2021.xlsx',
#   '/home/Cao/Storage/github/auto.tax/data/2022/position_2022.xlsx',
#   '/home/Cao/Storage/github/auto.tax/data/2023/position_2023.xlsx'
# ) -> list_position

# - fun_b3_clean ----------------------------------------------------------
fun_b3_clean(
  list_transactions,
  list_events
) -> list_b3_data

# - fun_b3_daytrolha ------------------------------------------------------
list_b3_data$
  transactions %>%
  fun_b3_daytrolha() ->
  df_daytrolha

df_daytrolha %>%
  filter(
    daytrolha
  ) %>%
  print(n = Inf)
