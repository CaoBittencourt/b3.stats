# [FUNCTIONS] --------------------------------------------------------------
# - Position function ---------------------------------------------------------
fun_b3_position <- function(df_events_transfers){

  # arguments validation
  stopifnot(
    "'df_events_transfers' must be a data frame with the 'df_events_transfers' subclass." =
      all(
        is.data.frame(df_events_transfers)
        , any(class(df_events_transfers) == 'df_events_transfers')
      )
  )

  # position
  # df_events_transfers %>%
  #   group_by(
  #     ticker,
  #     cycle
  #   ) %>%
  #   mutate(
  #     position =
  #       cumsum(qtd)
  #   ) %>%
  #   ungroup() ->
  #   df_position

  df_events_transfers %>%
    group_by(
      ticker,
      cycle
    ) %>%
    mutate(
      position =
        cumsum(qtd)
    ) %>%
    ungroup() %>%
    mutate(
      position =
        if_else(
          is.na(convert)
          | date < convert
          , position
          , 0
        )
    ) -> df_position

  rm(df_events_transfers)

  # set position = 0 for tickers in df_convert$ticker when date equal to convert date

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

  # drop useless columns
  df_position %>%
    select(
      date,
      ticker,
      # new_ticker, #remove later
      stock,
      position,
      mean_price,
      value
    ) -> df_position

  # current position
  df_position %>%
    group_by(
      ticker
    ) %>%
    slice(n()) %>%
    ungroup() %>%
    filter(
      position > 0
    ) -> df_position_now

  # daily position
  df_position %>%
    select(
      date,
      ticker,
      # new_ticker, #remove later
      stock,
      position,
      mean_price,
      value
    ) %>%
    group_by(
      ticker
    ) %>%
    mutate(
      .after = 1
      , reps =
        as.numeric(
          difftime(
            lead(date),
            date
          )
        )
      , reps =
        if_else(
          !is.na(reps)
          , reps
          , 0
        )
    ) %>%
    group_by(
      ticker,
      date
    ) %>%
    slice(
      rep(
        1, first(reps)
      )
    ) %>%
    select(
      -reps
    ) %>%
    ungroup() ->
    df_position_day

  # add subclasses
  new_data_frame(
    df_position
    , class = c(
      class(df_position)
      , 'df_position'
    )
  ) -> df_position

  new_data_frame(
    df_position_now
    , class = c(
      class(df_position_now)
      , 'df_position_now'
    )
  ) -> df_position_now

  new_data_frame(
    df_position_day
    , class = c(
      class(df_position_day)
      , 'df_position_day'
    )
  ) -> df_position_day

  # output
  return(list(
    'position' = df_position,
    'position_now' = df_position_now,
    'position_day' = df_position_day
  ))

}


# # - Position function ---------------------------------------------------------
# fun_b3_position <- function(df_events_transfers){
#
#   # arguments validation
#   stopifnot(
#     "'df_events_transfers' must be a data frame with the 'df_events_transfers' subclass." =
#       all(
#         is.data.frame(df_events_transfers)
#         , any(class(df_events_transfers) == 'df_events_transfers')
#       )
#   )
#
#   # position
#   df_events_transfers %>%
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
#   # rm(df_events_transfers)
#
#   # mean price and value
#   df_position %>%
#     group_by(
#       ticker
#     ) %>%
#     mutate(
#       mean_price =
#         fun_b3_mean_price(
#           dbl_qtd = qtd,
#           dbl_price = price,
#           dbl_cycle = cycle,
#           dbl_position = position
#         )
#       , value =
#         mean_price *
#         position
#     ) %>%
#     ungroup() ->
#     df_position
#
#   # drop useless columns
#   df_position %>%
#     select(
#       date,
#       ticker,
#       stock,
#       position,
#       mean_price,
#       value
#     ) -> df_position
#
#   # add subclasses
#   new_data_frame(
#     df_position
#     , class = c(
#       class(df_position)
#       , 'df_position'
#     )
#   ) -> df_position
#
#   # convert assets
#   fun_b3_convert(
#     df_position
#     , df_events_transfers
#     , df_converted_assets
#   ) -> df_position
#
#   # # drop useless columns
#   # df_position %>%
#   #   select(
#   #     date,
#   #     ticker,
#   #     stock,
#   #     position,
#   #     mean_price,
#   #     value
#   #   ) -> df_position
#
#   # current position
#   df_position %>%
#     group_by(
#       ticker
#     ) %>%
#     slice(n()) %>%
#     ungroup() %>%
#     filter(
#       position > 0
#     ) -> df_position_now
#
#   # daily position
#   df_position %>%
#     select(
#       date,
#       ticker,
#       stock,
#       position,
#       mean_price,
#       value
#     ) %>%
#     group_by(
#       ticker
#     ) %>%
#     mutate(
#       .after = 1
#       , reps =
#         as.numeric(
#           difftime(
#             lead(date),
#             date
#           )
#         )
#       , reps =
#         if_else(
#           !is.na(reps)
#           , reps
#           , 0
#         )
#     ) %>%
#     group_by(
#       ticker,
#       date
#     ) %>%
#     slice(
#       rep(
#         1, first(reps)
#       )
#     ) %>%
#     select(
#       -reps
#     ) %>%
#     ungroup() ->
#     df_position_day
#
#   # add subclasses
#   new_data_frame(
#     df_position
#     , class = c(
#       class(df_position)
#       , 'df_position'
#     )
#   ) -> df_position
#
#   new_data_frame(
#     df_position_now
#     , class = c(
#       class(df_position_now)
#       , 'df_position_now'
#     )
#   ) -> df_position_now
#
#   new_data_frame(
#     df_position_day
#     , class = c(
#       class(df_position_day)
#       , 'df_position_day'
#     )
#   ) -> df_position_day
#
#   # output
#   return(list(
#     'position' = df_position,
#     'position_now' = df_position_now,
#     'position_day' = df_position_day
#   ))
#
# }
#
