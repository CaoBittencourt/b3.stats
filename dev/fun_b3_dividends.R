# [FUNCTIONS] --------------------------------------------------------------
# - Calculate dividends function ---------------------------------------------------------
fun_b3_dividends <- function(df_dividends_events){

  # # arguments validation
  # stopifnot(
  #   "'df_dividends_events' must be a data frame with the 'df_dividends_events' subclass." =
  #     all(
  #       is.data.frame(df_dividends_events)
  #       , any(class(df_dividends_events) == 'df_dividends_events')
  #     )
  # )

  # must group by type of event!

  # calculate dividends
  df_dividends_events %>%
    mutate(
      dividends =
        qtd * price
    ) -> df_dividends

  rm(df_dividends_events)

  # aggregate dividends by month-year
  df_dividends %>%
    group_by(
      year = year(date),
      month = month(date),
      ticker
    ) %>%
    reframe(
      dividends = sum(dividends)
    ) -> df_dividends_month

  # aggregate dividends by year
  df_dividends %>%
    group_by(
      year = year(date),
      ticker
    ) %>%
    reframe(
      dividends = sum(dividends)
    ) -> df_dividends_year

  # aggregate dividends by period
  df_dividends %>%
    group_by(ticker) %>%
    reframe(
      dividends = sum(dividends)
    ) -> df_dividends_period

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
    'dividends_total' = sum(df_dividends_period$dividends)
  ))

  # output
  return(df_dividends_agg)

}
