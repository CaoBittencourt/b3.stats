# [FUNCTIONS] --------------------------------------------------------------
# - Calculate dividends function ---------------------------------------------------------
fun_b3_dividends <- function(df_events_dividends){

  # arguments validation
  stopifnot(
    "'df_events_dividends' must be a data frame with the 'df_events_dividends' subclass." =
      all(
        is.data.frame(df_events_dividends)
        , any(class(df_events_dividends) == 'df_events_dividends')
      )
  )

  # calculate dividends
  df_events_dividends %>%
    select(
      -type,
      -cycle
    ) %>%
    relocate(
      date,
      ticker,
      stock,
      event,
      qtd,
      price
    ) %>%
    mutate(
      value = qtd * price
    ) -> df_dividends

  rm(df_events_dividends)

  # aggregate dividends by month-year
  df_dividends %>%
    group_by(
      year = year(date),
      month = month(date),
      ticker,
      event
    ) %>%
    reframe(
      value = sum(value)
    ) %>%
    group_by(
      year,
      month,
      ticker
    ) %>%
    mutate(
      total = sum(value)
    ) %>%
    ungroup() ->
    df_dividends_month

  # aggregate dividends by year
  df_dividends %>%
    group_by(
      year = year(date),
      ticker,
      event
    ) %>%
    reframe(
      value = sum(value)
    ) %>%
    group_by(
      year,
      ticker
    ) %>%
    mutate(
      total = sum(value)
    ) %>%
    ungroup() ->
    df_dividends_year

  # aggregate dividends by period
  df_dividends %>%
    group_by(
      ticker,
      event
      ) %>%
    reframe(
      value = sum(value)
    ) %>%
    group_by(
      ticker
    ) %>%
    mutate(
      total = sum(value)
    ) %>%
    ungroup() ->
    df_dividends_period

  # add data frame subclasses
  new_data_frame(
    df_dividends
    , class = c(
      class(df_dividends)
      , 'df_dividends'
    )
  ) -> df_dividends

  new_data_frame(
    df_dividends_month
    , class = c(
      class(df_dividends_month)
      , 'df_dividends_month'
    )
  ) -> df_dividends_month

  new_data_frame(
    df_dividends_year
    , class = c(
      class(df_dividends_year)
      , 'df_dividends_year'
    )
  ) -> df_dividends_year

  new_data_frame(
    df_dividends_period
    , class = c(
      class(df_dividends_period)
      , 'df_dividends_period'
    )
  ) -> df_dividends_period

  # output
  return(list(
    'dividends' = df_dividends,
    'dividends_month' = df_dividends_month,
    'dividends_year' = df_dividends_year,
    'dividends_period' = df_dividends_period,
    'dividends_total' = sum(df_dividends_period$value)
  ))

  # output
  return(df_dividends_agg)

}

