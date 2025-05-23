# [FUNCTIONS] --------------------------------------------------------------
# [ ] INBR31 --------------------------------------------------------------
# [ ] is position_day necessary? --------------------------------------------------------------
# - Position function ---------------------------------------------------------
fun_b3_position <- function(df_events_transfers) {
  # arguments validation
  stopifnot(
    "'df_events_transfers' must be a data frame with the 'df_events_transfers' subclass." =
      all(
        is.data.frame(df_events_transfers),
        any(class(df_events_transfers) == "df_events_transfers")
      )
  )

  # position
  df_events_transfers %>%
    group_by(
      ticker,
      cycle
    ) %>%
    mutate(
      position =
        qtd |>
          is.na() |>
          ifelse(0, qtd) |>
          cumsum()
    ) %>%
    ungroup() ->
  df_position

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
          dbl_position = position,
          dbl_prop = prop
        ) *
          active,
      value =
        mean_price *
          position *
          active
    ) |>
    ungroup() ->
  df_position

  # set position = 0 for obsolete tickers
  df_position %>%
    mutate(across(
      .cols = c(position, value),
      .fns = ~ .x * active
      # need to set active = F for all t >= obsolete <date>
    )) -> df_position

  # drop auxiliary rows for converted (new) tickers
  df_position %>%
    filter(!(
      cycle == 0 &
        !is.na(convert)
    )) -> df_position

  # drop useless columns
  df_position %>%
    select(
      date,
      ticker,
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
      position > 0 | year(date) == max(year(date))
    ) -> df_position_now

  # daily position
  df_position %>%
    select(
      date,
      ticker,
      stock,
      position,
      mean_price,
      value
    ) %>%
    group_by(
      ticker
    ) %>%
    mutate(
      .after = 1,
      reps =
        as.numeric(
          difftime(
            lead(date),
            date
          )
        ),
      reps =
        if_else(
          !is.na(reps),
          reps,
          0
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
    df_position,
    class = c(
      class(df_position),
      "df_position"
    )
  ) -> df_position

  new_data_frame(
    df_position_now,
    class = c(
      class(df_position_now),
      "df_position_now"
    )
  ) -> df_position_now

  new_data_frame(
    df_position_day,
    class = c(
      class(df_position_day),
      "df_position_day"
    )
  ) -> df_position_day

  # output
  return(list(
    "position" = df_position,
    "position_now" = df_position_now,
    "position_day" = df_position_day
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
#   rm(df_events_transfers)
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
#   # set position = 0 for converted (old) tickers
#   df_position %>%
#     filter(
#       ticker_convert ==
#         'ticker'
#     ) %>%
#     mutate(
#       date = convert
#     ) %>%
#     select(
#       date,
#       ticker,
#       mean_price
#     ) %>%
#     group_by(
#       ticker
#     ) %>%
#     slice(n()) %>%
#     ungroup() %>%
#     mutate(
#       position = 0
#       , mean_price =
#         mean_price
#       , value = 0
#       , active = F
#     ) %>%
#     bind_rows(
#       df_position
#     ) %>%
#     arrange(
#       date
#     ) %>%
#     fill(
#       c(
#         ticker_convert,
#         cycle,
#         stock,
#         ticker_type
#       )
#     ) -> df_position
#
#   # drop auxiliary rows for converted (new) tickers
#   df_position %>%
#     filter(!(
#       cycle == 0
#       & ticker_convert ==
#         'new_ticker'
#     )) -> df_position
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
