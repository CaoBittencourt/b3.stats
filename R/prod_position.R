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
  df_events_transfers %>%
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

  rm(df_events_transfers)

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
      stock,
      position,
      mean_price,
      value
    ) -> df_position

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
    df_position_day
    , class = c(
      class(df_position_day)
      , 'df_position_day'
    )
  ) -> df_position_day

  # output
  return(list(
    'position' = df_position,
    'position_day' = df_position_day
  ))

}
