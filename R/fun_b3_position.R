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
# # - Calculate position function -------------------------------------------
# fun_b3_position <- function(df_transfers){
#
#   # arguments validated in main function
#
#   # aggregate position
#   df_transfers %>%
#     group_by(
#       ticker
#       , cycle
#     ) %>%
#     mutate(
#       position =
#         cumsum(qtd)
#     ) %>%
#     ungroup() ->
#     df_transfers
#
#   # output
#   return(df_transfers)
#
# }
#
# # - Calculate mean price function -----------------------------------------
# fun_b3_mean_price <- function(df_position){
#
#   # arguments validated in main function
#
#   # calculate mean price
#   df_position %>%
#     group_by(
#       ticker,
#       cycle
#     ) %>%
#     mutate(
#       mean_price = if_else(
#         position != 0
#         , cumsum(qtd * price) /
#           position
#         , NA
#       )
#     ) %>%
#     ungroup() ->
#     df_position
#
#   # output
#   return(df_position)
#
# }

# - Calculate position and mean price function ---------------------------------------------------------
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

  # mean price
  df_position %>%
    group_by(
      ticker,
      cycle
    ) %>%
    mutate(
      mean_price =
        cumsum(qtd * price * (qtd > 0)) /
        cumsum(qtd * (qtd > 0))
      # position
    ) %>%
    ungroup() ->
    df_position

  df_position %>%
    mutate(
      total =
        position *
        mean_price
    ) -> df_position


  # # value
  # df_position %>%
  #   mutate(
  #     value =
  #       qtd * price
  #   ) -> df_position
  #
  # # total
  # df_position %>%
  #   group_by(
  #     ticker,
  #     cycle
  #   ) %>%
  #   mutate(
  #     total =
  #       cumsum(value)
  #   ) %>%
  #   ungroup() ->
  #   df_position
  #
  # # mean price
  # df_position %>%
  #   group_by(
  #     ticker,
  #     cycle
  #   ) %>%
  #   mutate(
  #     mean_price =
  #       total /
  #       position
  #       # if_else(
  #       #   position != 0
  #       #   , total /
  #       #     position
  #       #   , NA
  #       # )
  #   ) %>%
  #   ungroup() ->
  #   df_position
  #
  # # # mean price
  # # df_position %>%
  # #   group_by(
  # #     ticker,
  # #     cycle
  # #   ) %>%
  # #   mutate(
  # #     mean_price =
  # #       if_else(
  # #         position != 0
  # #         , cumsum(qtd * price) /
  # #           cumsum(qtd)
  # #         , 0
  # #       )
  # #   ) %>%
  # #   ungroup() ->
  # #   df_position
  #
  # # # value
  # # df_position %>%
  # #   mutate(
  # #     value =
  # #       position *
  # #       mean_price
  # #   ) -> df_position

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

# - fun_b3_position -------------------------------------------------------
# transactions do not work for identifying daytrolha!
# must use dealings files for daytrolha
list_b3_data$
  transactions %>%
  group_by(
    ticker
    # , cycle
  ) %>%
  mutate(
    position =
      cumsum(qtd)
    , total = cumsum(
      qtd * price
    )
    # , mean_price =
    #   cumsum(qtd * price) /
    #   cumsum(qtd)
    # , mean_price = if_else(
    #   position != 0
    #   , mean_price
    #   , NA
    # )
    # cumsum(position * price) /
    # cumsum(position)
    # , total =
    #   mean_price *
    #   position
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
  events$
  events %>%
  group_by(
    event
  ) %>%
  tally()

transactional events
assets


list_b3_data$
  transactions %>%
  filter(
    event == 'transferência'
  ) %>%
  print(n = Inf)


list_b3_data$
  transactions %>%
  filter(
    ticker == 'GETT11'
  )

fun_b3_clean_dealings(
  list_dealings
) %>%
  filter(
    ticker == 'FHER3'
    # ticker == 'GSHP3'
    # ticker == 'BIDI4'
    # ticker == 'WEGE3'
  ) %>%
  mutate(
    position =
      cumsum(qtd)
    , total = cumsum(
      qtd * price
    )
    # , mean_price =
    #   cumsum(qtd * price) /
    #   cumsum(qtd)
    # , mean_price = if_else(
    #   position != 0
    #   , mean_price
    #   , NA
    # )
    # cumsum(position * price) /
    # cumsum(position)
    # , total =
    #   mean_price *
    #   position
  )

list_b3_data$
  transactions %>%
  filter(
    ticker == 'FHER3'
  ) %>%
  group_by(date) %>%
  tally()

list_b3_data$
  events$
  events %>%
  filter(
    ticker == 'FHER3'
  ) %>%
  group_by(date) %>%
  tally()

list_b3_data$
  transactions %>%
  filter(
    ticker == 'GSHP3'
  )

list_b3_data$
  events$
  events %>%
  filter(
    ticker == 'GSHP3'
  )

list_b3_data$
  events %>%
  lapply(
    function(df){
      df %>%
        filter(
          # ticker == 'BIDI4'
          str_detect(
            ticker,
            'INB'
          )
        )
    }
  )

list_b3_data$
  events$
  transfers %>%
  filter(
    ticker == 'WEGE3'
    # ticker == 'BIDI4'
    # ticker == 'INBR32'
    # ticker == 'INBR31'
    # ticker == 'GSHP3'
    # ticker == 'FHER3'
    # str_detect(
    #   ticker,
    #   'Tesouro'
    # )
  ) %>%
  fun_b3_position()

# now splits are wrong
