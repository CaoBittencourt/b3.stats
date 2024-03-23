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
# # - Position function ---------------------------------------------------------
# fun_b3_position <- function(df_transfers){
#
#   # arguments validation
#   stopifnot(
#     "'df_transfers' must be a data frame with the 'df_transfers' subclass." =
#       all(
#         is.data.frame(df_transfers)
#         , any(class(df_transfers) == 'df_transfers')
#       )
#   )
#
#   # position
#   df_transfers %>%
#     group_by(
#       ticker,
#       cycle
#     ) %>%
#     mutate(
#       position =
#         cumsum(qtd)
#     ) %>%
#     ungroup() ->
#     df_position
#
#   rm(df_transfers)
#
#   # new cycle whenever position = 0
#   df_position %>%
#     group_by(
#       ticker
#     ) %>%
#     mutate(
#       cycle =
#         cycle +
#         lag(
#           cumsum(position == 0)
#           , default = 0
#         )
#     ) %>%
#     ungroup() ->
#     df_position
#
#   # mean price
#   df_position %>%
#     group_by(
#       ticker,
#       cycle
#     ) %>%
#     mutate(
#       mean_price =
#         # acquisition cost / qtd bought
#         cumsum(qtd * price * (qtd > 0)) /
#         cumsum(qtd * (qtd > 0))
#       # if_else(
#       #   price != 0
#       #   , cumsum(qtd * price * (qtd > 0)) /
#       #     cumsum(qtd * (qtd > 0))
#       #   , (cumsum(qtd * price * (qtd > 0)) +
#       #        cumsum(qtd * (qtd < 0)) *
#       #        cumsum(qtd * price * (qtd > 0)) /
#       #        cumsum(qtd * (qtd > 0))) / position
#       # )
#
#       # cumsum(qtd * price * (qtd > 0)) /
#       # position
#       # , mean_price =
#       #   if_else(
#       #     !is.na(mean_price)
#       #     , mean_price
#       #     , 0
#       #   )
#     ) %>%
#     ungroup() ->
#     df_position
#
#   df_position %>%
#     mutate(
#       total =
#         position *
#         mean_price
#     ) -> df_position
#
#
#   # # value
#   # df_position %>%
#   #   mutate(
#   #     value =
#   #       qtd * price
#   #   ) -> df_position
#   #
#   # # total
#   # df_position %>%
#   #   group_by(
#   #     ticker,
#   #     cycle
#   #   ) %>%
#   #   mutate(
#   #     total =
#   #       cumsum(value)
#   #   ) %>%
#   #   ungroup() ->
#   #   df_position
#   #
#   # # mean price
#   # df_position %>%
#   #   group_by(
#   #     ticker,
#   #     cycle
#   #   ) %>%
#   #   mutate(
#   #     mean_price =
#   #       total /
#   #       position
#   #       # if_else(
#   #       #   position != 0
#   #       #   , total /
#   #       #     position
#   #       #   , NA
#   #       # )
#   #   ) %>%
#   #   ungroup() ->
#   #   df_position
#   #
#   # # # mean price
#   # # df_position %>%
#   # #   group_by(
#   # #     ticker,
#   # #     cycle
#   # #   ) %>%
#   # #   mutate(
#   # #     mean_price =
#   # #       if_else(
#   # #         position != 0
#   # #         , cumsum(qtd * price) /
#   # #           cumsum(qtd)
#   # #         , 0
#   # #       )
#   # #   ) %>%
#   # #   ungroup() ->
#   # #   df_position
#   #
#   # # # value
#   # # df_position %>%
#   # #   mutate(
#   # #     value =
#   # #       position *
#   # #       mean_price
#   # #   ) -> df_position
#
#   # add subclass
#   new_data_frame(
#     df_position
#     , class = c(
#       class(df_position)
#       , 'df_position'
#     )
#   ) -> df_position
#
#   # output
#   return(df_position)
#
# }

# # - Mean price function ---------------------------------------------------
# fun_b3_mean_price <- function(
    #     dbl_qtd
#     , dbl_price
#     , dbl_position =
#       cumsum(dbl_qtd)
# ){
#
#   # arguments validation
#   stopifnot(
#     "'dbl_qtd', 'dbl_price', and 'dbl_position' must all be numeric vectors of the same length." =
#       all(
#         is.numeric(dbl_qtd),
#         is.numeric(dbl_price),
#         is.numeric(dbl_position),
#         length(dbl_qtd) == length(dbl_price),
#         length(dbl_price) == length(dbl_position)
#       )
#   )
#
#   # initialize mean price
#   dbl_price[1] -> dbl_mean_price
#
#   # calculate mean price
#   for(t in 2:length(dbl_qtd)){
#
#     # buy
#     if(all(
#       dbl_price[t] > 0,
#       dbl_qtd[t] > 0
#     )){
#
#       # mean cost of acquisition
#       weighted.mean(
#         x = c(
#           dbl_mean_price[t - 1]
#           , dbl_price[t]
#         )
#         , w = c(
#           dbl_position[t - 1]
#           , dbl_qtd[t]
#         )
#       ) -> dbl_mean_price[t]
#
#     }
#
#     # sell
#     if(all(
#       dbl_price[t] > 0,
#       dbl_qtd[t] < 0
#     )){
#
#       # selling does not alter mean price
#       dbl_mean_price[t - 1] ->
#         dbl_mean_price[t]
#
#     }
#
#     # split / grouping / event
#     if(all(
#       dbl_price[t] == 0,
#       dbl_qtd[t] > 0
#     )){
#
#       # mean price adjusted by proportion
#       (dbl_qtd[t] / dbl_position[t]) *
#         dbl_mean_price[t - 1] ->
#         dbl_mean_price[t]
#
#     }
#
#   }
#
#   # output
#   return(dbl_mean_price)
#
# }

# - Mean price function ---------------------------------------------------
fun_b3_mean_price <- function(
    dbl_qtd
    , dbl_price
    , dbl_position =
      cumsum(dbl_qtd)
){

  # arguments validation
  stopifnot(
    "'dbl_qtd', 'dbl_price', and 'dbl_position' must all be numeric vectors of the same length." =
      all(
        is.numeric(dbl_qtd),
        is.numeric(dbl_price),
        is.numeric(dbl_position),
        length(dbl_qtd) == length(dbl_price),
        length(dbl_price) == length(dbl_position)
      )
  )

  # initialize mean price
  dbl_price[1] -> dbl_mean_price

  # calculate mean price
  for(t in 2:length(dbl_qtd)){

    # buy
    if(all(
      dbl_price[t] >= 0,
      dbl_qtd[t] > 0
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
    if(all(
      dbl_price[t] > 0,
      dbl_qtd[t] < 0
    )){

      # selling does not alter mean price
      dbl_mean_price[t - 1] ->
        dbl_mean_price[t]

    }

    # split / grouping / event
    if(all(
      dbl_price[t] == 0,
      dbl_qtd[t] > 0,
      dbl_qtd[t - 1] < 0
    )){

      # mean price adjusted by proportion
      dbl_mean_price[t - 1] *
        dbl_position[t - 1] /
        dbl_position[t] ->
        dbl_mean_price[t]

    }

  }

  # output
  return(dbl_mean_price)

}

# # - Position function ---------------------------------------------------------
# fun_b3_position <- function(df_transfers){
#
#   # arguments validation
#   stopifnot(
#     "'df_transfers' must be a data frame with the 'df_transfers' subclass." =
#       all(
#         is.data.frame(df_transfers)
#         , any(class(df_transfers) == 'df_transfers')
#       )
#   )
#
#   # position
#   df_transfers %>%
#     group_by(
#       ticker,
#       cycle
#     ) %>%
#     mutate(
#       position =
#         cumsum(qtd)
#     ) %>%
#     ungroup() ->
#     df_position
#
#   rm(df_transfers)
#
#   # # new cycle whenever position = 0
#   # df_position %>%
#   #   group_by(
#   #     ticker
#   #   ) %>%
#   #   mutate(
#   #     cycle =
#   #       cycle +
#   #       lag(
#   #         cumsum(position == 0)
#   #         , default = 0
#   #       )
#   #   ) %>%
#   #   ungroup() ->
#   #   df_position
#   #
#   # # mean price
#   # df_position %>%
#   #   group_by(
#   #     ticker,
#   #     cycle
#   #   ) %>%
#   #   mutate(
#   #     mean_price =
#   #       # acquisition cost / qtd bought
#   #       cumsum(qtd * price * (qtd > 0)) /
#   #       cumsum(qtd * (qtd > 0))
#   #     # if_else(
#   #     #   price != 0
#   #     #   , cumsum(qtd * price * (qtd > 0)) /
#   #     #     cumsum(qtd * (qtd > 0))
#   #     #   , (cumsum(qtd * price * (qtd > 0)) +
#   #     #        cumsum(qtd * (qtd < 0)) *
#   #     #        cumsum(qtd * price * (qtd > 0)) /
#   #     #        cumsum(qtd * (qtd > 0))) / position
#   #     # )
#   #
#   #     # cumsum(qtd * price * (qtd > 0)) /
#   #     # position
#   #     # , mean_price =
#   #     #   if_else(
#   #     #     !is.na(mean_price)
#   #     #     , mean_price
#   #     #     , 0
#   #     #   )
#   #   ) %>%
#   #   ungroup() ->
#   #   df_position
#   #
#   # df_position %>%
#   #   mutate(
#   #     total =
#   #       position *
#   #       mean_price
#   #   ) -> df_position
#
#
#   # # value
#   # df_position %>%
#   #   mutate(
#   #     value =
#   #       qtd * price
#   #   ) -> df_position
#   #
#   # # total
#   # df_position %>%
#   #   group_by(
#   #     ticker,
#   #     cycle
#   #   ) %>%
#   #   mutate(
#   #     total =
#   #       cumsum(value)
#   #   ) %>%
#   #   ungroup() ->
#   #   df_position
#   #
#   # # mean price
#   # df_position %>%
#   #   group_by(
#   #     ticker,
#   #     cycle
#   #   ) %>%
#   #   mutate(
#   #     mean_price =
#   #       total /
#   #       position
#   #       # if_else(
#   #       #   position != 0
#   #       #   , total /
#   #       #     position
#   #       #   , NA
#   #       # )
#   #   ) %>%
#   #   ungroup() ->
#   #   df_position
#   #
#   # # # mean price
#   # # df_position %>%
#   # #   group_by(
#   # #     ticker,
#   # #     cycle
#   # #   ) %>%
#   # #   mutate(
#   # #     mean_price =
#   # #       if_else(
#   # #         position != 0
#   # #         , cumsum(qtd * price) /
#   # #           cumsum(qtd)
#   # #         , 0
#   # #       )
#   # #   ) %>%
#   # #   ungroup() ->
#   # #   df_position
#   #
#   # # # value
#   # # df_position %>%
#   # #   mutate(
#   # #     value =
#   # #       position *
#   # #       mean_price
#   # #   ) -> df_position
#
#   # add subclass
#   new_data_frame(
#     df_position
#     , class = c(
#       class(df_position)
#       , 'df_position'
#     )
#   ) -> df_position
#
#   # output
#   return(df_position)
#
# }

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
# now splits are wrong
# remove single 'atualização' events? (single == with no follow-up events)
# list_b3_data$
#   transactions %>%
#   filter(
#     ticker == 'GETT11'
#   )
# list_b3_data$
#   transactions %>%
#   filter(
#     ticker == 'BIDI4'
#     ticker == 'INBR31'
#     ticker == 'INBR32'
#   )

list_b3_data$
  events$
  transfers %>%
  filter(
    # ticker == 'TIET4' #working
    # ticker == 'AESB1' #working
    # ticker == 'AESB3' #working
    # ticker == 'TAEE11' #working
    # ticker == 'TAEE3' #working
    # ticker == 'WEGE3' #split not working (split after selling)
    # ticker == 'MGLU3' #'atualização' event bug (position should be 0) + split not working (see WEGE3; splits after sell don't work)
    # ticker == 'SAPR3' #working
    # ticker == 'SAPR4' #working
    # ticker == 'BOVA11' #'atualização' event bug (position should be 0)
    # ticker == 'GOLL4' #working
    # ticker == 'AZUL4' #working
    # ticker == 'INHF12' #'incorporação' event bug
    # ticker == 'SOMA3' #'incorporação' price should not be 0
    # ticker == 'HGTX3' #bug (position should be 0)
    # ticker == 'SLCE3' #working
    # ticker == 'TESA3' #this stock was converted into LAND3 (1:3) and position should be 0
    # ticker == 'LAND3' #working
    # ticker == 'EQTL1' #working
    # ticker == 'EQTL3' #working
    # ticker == 'EGIE3' #working
    # ticker == 'PRIO3' #split not working (split after selling) 'atualização' event bug ('atualização' should not be counted as additional stocks, 'atualização' == lag(position))
    ticker == 'BIDI4' #split working but positions should be 0
    # ticker == 'GSHP3' #grouping mean price to be implemented
    # ticker == 'FHER3' #working
    # ticker == 'INBR31' #edge case
    # ticker == 'INBR32' #working
    # ticker == 'GETT11' #working
    # ticker == 'SANB11' #edge case
  ) %>%
  fun_b3_position() %>%
  print(n = Inf)

# list_b3_data$
#   events$
#   transfers$
#   ticker %>%
#   unique()

# dsds --------------------------------------------------------------------
list_b3_data$
  events$
  transfers %>%
  filter(
    ticker == 'WEGE3'
  ) -> dsds

dsds %>%
  select(
    -type,
    -stock,
    -ticker
  ) %>%
  mutate(
    position =
      cumsum(qtd)
    # , value =
    #   case_when(
    #     qtd <= 0 ~ 0,
    #     price == 0 ~ 0,
    #     .default = qtd * price
    #   )
    # , cumval =
    #   cumsum(value)
    # , mean_price =
    #   cumval /
    #   cumsum((qtd >= 0) * qtd)
    # , cumval =
    #   cumval +
    #   (qtd < 0) * qtd * mean_price
  ) %>%
  print(n = Inf)

# qtd > 0, price > 0, no events
# 1. mean_price = 35.2
# 1. value = 10 * 35.2 = 352

# qtd > 0, price > 0, no events
# 2. mean_price = (35.2 * 10 + 37.2 * 5) / 15 = 35.8667
# 2. value = (35.2 * 10 + 37.2 * 5) = 538

# qtd > 0, price > 0, no events
# 3. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4) / 19 = 36.2736
# 3. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4) = 689.2

# qtd > 0, price > 0, no events
# 4. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11) / 30 = 36.8333
# 4. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11) = 1105

# qtd > 0, price > 0, no events
# 5. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 5. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) = 1139.4

# qtd < 0, price > 0, no events
# 6. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 6. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 = 1139.4 - 36.7548 = 1102.645

# qtd < 0, price > 0, no events
# 7. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 7. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 = 1139.4 - 3 * 36.7548 = 1029.136

# qtd < 0, price > 0, no events
# 8. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 8. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 - 4 * 36.7548 = 1139.4 - 7 * 36.7548 = 882.1164

# qtd < 0, price > 0, no events
# 9. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 9. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 - 4 * 36.7548 - 4 * 36.7548 = 1139.4 - 11 * 36.7548 = 735.0972

# qtd < 0, price > 0, no events
# 10. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 10. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 - 4 * 36.7548 - 4 * 36.7548 - 5 * 36.7548 = 1139.4 - 16 * 36.7548 = 551.3232

# qtd < 0, price > 0, no events
# 11. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 11. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 - 4 * 36.7548 - 4 * 36.7548 - 5 * 36.7548 - 1 * 36.7548 = 1139.4 - 17 * 36.7548 = 514.5684

# qtd < 0, price > 0, no events
# 12. mean_price = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 36.7548
# 12. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 - 4 * 36.7548 - 4 * 36.7548 - 5 * 36.7548 - 1 * 36.7548 - 10 * 36.7548 = 1139.4 - 27 * 36.7548 = 147.0204

# qtd > 0, price == 0, desdobro
# 13. mean_price = (4 / 8) * (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) / 31 = 18.3774
# = (qtd / position) * lag(mean_price)
# 13. value = (35.2 * 10 + 37.2 * 5 + 37.8 * 4 + 37.8 * 11 + 34.4 * 1) - 1 * 36.7548 - 2 * 36.7548 - 4 * 36.7548 - 4 * 36.7548 - 5 * 36.7548 - 1 * 36.7548 - 10 * 36.7548 = 1139.4 - 27 * 36.7548 = 147.0204
# value = lag(value)

# qtd < 0, price > 0, no events
# 14. mean_price = lag(mean_price) = 18.3774
# 14. value = lag(value) - mean_price = 147.0204 - 18.3774 = 128.643

# qtd < 0, price > 0, no events
# 15. mean_price = lag(mean_price) = 18.3774
# 15. value = lag(value) - 2 * mean_price = 128.643 - 2 * 18.3774 = 91.8882

# qtd < 0, price > 0, no events
# 16. mean_price = lag(mean_price) = 18.3774
# 16. value = lag(value) - 2 * mean_price = 91.8882 - 2 * 18.3774 = 55.1334

# qtd < 0, price > 0, no events
# 17. mean_price = lag(mean_price) = 18.3774
# 17. value = lag(value) - 1 * mean_price = 55.1334 - 1 * 18.3774 = 36.756

# qtd > 0, price > 0, no events
# 18. mean_price = (18.3774 * 2 + 39.7 * 1) / 3 = 25.4849
# = (lag(mean_price) * lag(position) + qtd * price) / position
# = (lag(mean_price) * lag(position) + qtd * price) / (lag(position) + qtd)
# = weighted.mean(x = c(lag(mean_price), price), w =  c(lag(position), qtd))
# 18. value = lag(value) + 1 * 39.7 = 36.756 + 1 * 39.7 = 76.456

# qtd > 0, price > 0, no events
# 19. mean_price = (18.3774 * 2 + 39.7 * 1 + 26 * 2) / (2 + 1 + 2) = 25.6909
# = (25.4849 * 3 + 26 * 2) / 5
# = (lag(mean_price) * lag(position) + qtd * price) / position
# = (lag(mean_price) * lag(position) + qtd * price) / (lag(position) + qtd)
# = weighted.mean(x = c(lag(mean_price), price), w = c(lag(position), qtd))

# etc

# qtd > 0, price > 0, no events
mean_price = weighted.mean(
  x = c(
    lag(mean_price, default = price)
    , price
  )
  , w = c(
    lag(position, default = qtd)
    , qtd
  )
)

# qtd < 0, price > 0, no events
mean_price = lag(mean_price, default = price)

# qtd > 0, price == 0, desdobro
mean_price = (qtd / position) * lag(mean_price, default = price)

dsds %>%
  mutate(
    position =
      cumsum(qtd)
    , mean_price = case_when(
      # buy
      all(qtd > 0, price > 0) ~ weighted.mean(
        x = c(
          lag(mean_price, default = price)
          , price
        )
        , w = c(
          lag(position, default = qtd)
          , qtd
        )
      ),
      # sell
      all(qtd < 0, price > 0) ~ lag(
        mean_price
        , default = price
      ),
      # event (split or grouping)
      all(qtd > 0, price == 0) ~ lag(
        mean_price
        , default = price
      ) * (qtd / position),
      .default = price
    )
  )

# fun_b3_mean_price <- function(dbl_qtd, dbl_price){
#
#   # position
#   cumsum(dbl_qtd) -> dbl_position
#
#   # default mean price
#   dbl_price -> dbl_mean_price
#
#   # buy
#   if(all(qtd > 0, dbl_price > 0)){
#
#     weighted.mean(
#       x = c(
#         # lag(dbl_mean_price, default = dbl_price)
#         lag(
#           fun_b3_mean_price(
#             dbl_qtd,
#             dbl_price
#           )
#           , default = dbl_price
#         )
#         , dbl_price
#       )
#       , w = c(
#         lag(dbl_position, default = dbl_qtd)
#         , dbl_qtd
#       )
#     ) -> dbl_mean_price
#
#   }
#
#   # sell
#   if(all(dbl_qtd < 0, dbl_price > 0)){
#
#     lag(
#       fun_b3_mean_price(
#         dbl_qtd,
#         dbl_price
#       )
#       , default = dbl_price
#     ) -> dbl_mean_price
#
#     # lag(
#     #   dbl_mean_price
#     #   , default = dbl_price
#     # ) -> dbl_mean_price
#     #
#   }
#
#   # event (split or grouping)
#   if(all(dbl_qtd > 0, dbl_price == 0)){
#
#     (dbl_qtd / dbl_position) *
#       lag(
#         fun_b3_mean_price(
#           dbl_qtd,
#           dbl_price
#         )
#         , default = dbl_price
#       ) -> dbl_mean_price
#
#   }
#
#   # output
#   return(dbl_mean_price)
#
# }

dbl_mean_price <- dsds$price[1]

position =
  cumsum(qtd)
, mean_price = case_when(
  # buy
  all(qtd > 0, price > 0) ~ weighted.mean(
    x = c(
      lag(mean_price, default = price)
      , price
    )
    , w = c(
      lag(position, default = qtd)
      , qtd
    )
  ),
  # sell
  all(qtd < 0, price > 0) ~ lag(
    mean_price
    , default = price
  ),
  # event (split or grouping)
  all(qtd > 0, price == 0) ~ lag(
    mean_price
    , default = price
  ) * (qtd / position),
  .default = price
)
)


fun_b3_mean_price(
  dbl_qtd = dsds$qtd
  , dbl_price = dsds$price
) %>% length()

