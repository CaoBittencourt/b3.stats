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

  # new cycle whenever position = 0
  df_position %>%
    group_by(
      ticker
    ) %>%
    mutate(
      cycle =
        cycle +
        lag(
          cumsum(position == 0)
          , default = 0
        )
    ) %>%
    ungroup() ->
    df_position

  # mean price
  df_position %>%
    group_by(
      ticker,
      cycle
    ) %>%
    mutate(
      mean_price =
        # acquisition cost / qtd bought
        cumsum(qtd * price * (qtd > 0)) /
        cumsum(qtd * (qtd > 0))
      # if_else(
      #   price != 0
      #   , cumsum(qtd * price * (qtd > 0)) /
      #     cumsum(qtd * (qtd > 0))
      #   , (cumsum(qtd * price * (qtd > 0)) +
      #        cumsum(qtd * (qtd < 0)) *
      #        cumsum(qtd * price * (qtd > 0)) /
      #        cumsum(qtd * (qtd > 0))) / position
      # )

      # cumsum(qtd * price * (qtd > 0)) /
      # position
      # , mean_price =
      #   if_else(
      #     !is.na(mean_price)
      #     , mean_price
      #     , 0
      #   )
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
  transfers$
  ticker %>%
  unique()

list_b3_data$
  events$
  transfers %>%
  fun_b3_position() %>%
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
    # ticker == 'BIDI4' #split working but positions should be 0
    # ticker == 'GSHP3' #grouping mean price to be implemented
    # ticker == 'FHER3' #working
    # ticker == 'INBR31' #edge case
    # ticker == 'INBR32' #working
    # ticker == 'GETT11' #working
    # ticker == 'SANB11' #edge case
  ) %>%
  print(n = Inf)
