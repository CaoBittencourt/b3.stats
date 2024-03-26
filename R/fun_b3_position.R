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
# - Mean price function ---------------------------------------------------
fun_b3_mean_price <- function(
    dbl_qtd
    , dbl_price
    , dbl_cycle
    , dbl_position =
      cumsum(dbl_qtd)
){

  # arguments validation
  stopifnot(
    "'dbl_qtd', 'dbl_price', 'dbl_cycle', and 'dbl_position' must all be numeric vectors of the same length." =
      all(
        is.numeric(dbl_qtd),
        is.numeric(dbl_price),
        is.numeric(dbl_cycle),
        is.numeric(dbl_position),
        !any(diff(
          length(dbl_qtd),
          length(dbl_price),
          length(dbl_cycle),
          length(dbl_position)
        )) #bug when nrow == 1
      )
  )

  # initialize mean price
  dbl_price[1] -> dbl_mean_price

  if(length(dbl_price) > 1){

    # calculate mean price
    for(t in 2:length(dbl_qtd)){

      # buy
      # or split / event
      # after buying
      if(all(
        dbl_price[t] >= 0,
        dbl_qtd[t] > 0,
        dbl_cycle[t - 1] ==
        dbl_cycle[t]
      )){

        # mean cost of acquisition
        weighted.mean(
          x = c(
            dbl_mean_price[t - 1]
            , dbl_price[t]
          )
          , w = c(
            dbl_position[t - 1]
            , dbl_qtd[t]
          )
        ) -> dbl_mean_price[t]

      }

      # sell
      if(dbl_qtd[t] < 0){

        # selling does not alter mean price
        dbl_mean_price[t - 1] ->
          dbl_mean_price[t]

      }

      # split / event
      # after selling
      # or grouping event
      if(any(
        dbl_cycle[t - 1] !=
        dbl_cycle[t]
        , all(
          dbl_price[t] == 0,
          dbl_qtd[t] > 0,
          dbl_qtd[t - 1] < 0
        )
      )){

        # mean price adjusted by proportion
        dbl_mean_price[t - 1] *
          dbl_position[t - 1] /
          dbl_position[t] ->
          dbl_mean_price[t]

      }

    }

  }

  # output
  return(dbl_mean_price)

}

# - Position function ---------------------------------------------------------
fun_b3_position <- function(df_transfers){

  # arguments validation
  stopifnot(
    "'df_transfers' must be a data frame with the 'df_transfers' subclass." =
      all(
        is.data.frame(df_transfers)
        , any(class(df_transfers) == 'df_transfers')
      )
  )

  # position
  df_transfers %>%
    group_by(
      ticker,
      cycle
    ) %>%
    mutate(
      position =
        cumsum(qtd)
    ) %>%
    ungroup() ->
    df_position

  rm(df_transfers)

  # mean price and value
  df_position %>%
    group_by(
      ticker
    ) %>%
    mutate(
      mean_price =
        fun_b3_mean_price(
          dbl_qtd = qtd,
          dbl_price = price,
          dbl_cycle = cycle,
          dbl_position = position
        )
      , value =
        mean_price *
        position
    ) %>%
    ungroup() ->
    df_position

  # add subclass
  new_data_frame(
    df_position
    , class = c(
      class(df_position)
      , 'df_position'
    )
  ) -> df_position

  # output
  return(df_position)

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

# events to remove
list() -> list_events_remove

# events to add
list() -> list_events_add

# - fun_b3_clean ----------------------------------------------------------
fun_b3_clean(
  list_chr_path_transactions = list_transactions,
  list_chr_path_events = list_events,
  list_chr_path_events_remove =  list_events_remove,
  list_chr_path_events_add = list_events_add
) -> list_b3_data

# - fun_b3_position -------------------------------------------------------
list_b3_data$
  events$
  transfers %>%
  fun_b3_position() ->
  df_position

# - edge cases -------------------------------------------------------
# transactions do not work for identifying daytrolha!
# must use dealings files for daytrolha
# remove single 'atualização' events? (single == with no follow-up events)
# list_b3_data$
#   transactions %>%
#   filter(
#     ticker == 'BIDI4'
#     ticker == 'INFH12'
#     ticker == 'INBR31'
#     ticker == 'INBR32'
#   )
# ticker == 'TIET4' #working
# ticker == 'AESB1' #working
# ticker == 'AESB3' #working
# ticker == 'TAEE11' #working
# ticker == 'TAEE3' #working
# ticker == 'WEGE3' #working
# ticker == 'MGLU3' #'atualização' event bug (position should be 0)
# ticker == 'SAPR3' #working
# ticker == 'SAPR4' #working
# ticker == 'BOVA11' #'atualização' event bug (position should be 0)
# ticker == 'GOLL4' #working
# ticker == 'AZUL4' #working
# ticker == 'INHF12' #'incorporação' event bug
# ticker == 'SOMA3' #'incorporação' price should not be 0
# ticker == 'HGTX3' #'incorporação' bug (position should be 0)
# ticker == 'SLCE3' #working
# ticker == 'TESA3' #this stock was converted into LAND3 (1:3) and position should be 0
# ticker == 'LAND3' #working
# ticker == 'EQTL1' #working
# ticker == 'EQTL3' #working
# ticker == 'EGIE3' #working
# ticker == 'PRIO3' #'atualização' event bug ('atualização' should not be counted as additional stocks, 'atualização' == lag(position))
# ticker == 'BIDI4' #split working but positions should be 0
# ticker == 'GSHP3' #working
# ticker == 'FHER3' #working
# ticker == 'INBR31' #edge case
# ticker == 'INBR32' #working
# ticker == 'SANB11' #edge case 'cisão' event (position should be 0)
