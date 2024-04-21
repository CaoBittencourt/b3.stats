# [FUNCTIONS] --------------------------------------------------------------
# - Calculate daytrolha function ---------------------------------------------------------
fun_b3_daytrolha <- function(df_transactions, df_position_day){

  # arguments validation
  stopifnot(
    "'df_transactions' must be a data frame with the 'df_transactions' subclass." =
      all(
        is.data.frame(df_transactions)
        , any(class(df_transactions) == 'df_transactions')
      )
  )

  stopifnot(
    "'df_position_day' must be a data frame with the 'df_position_day' subclass." =
      all(
        is.data.frame(df_position_day)
        , any(class(df_position_day) == 'df_position_day')
      )
  )

  # identify daytrolha
  df_transactions %>%
    fun_b3_is_daytrolha() %>%
    filter(daytrolha) %>%
    filter(qtd < 0) ->
    df_daytrolha

  rm(df_transactions)

  # get mean prices
  df_position_day %>%
    select(
      date,
      ticker,
      mean_price
    ) %>%
    group_by(
      date,
      ticker
    ) %>%
    slice(1) %>%
    ungroup() %>%
    inner_join(
      df_daytrolha
      , by = c(
        'date' = 'date',
        'ticker' = 'ticker'
      )
      , suffix = c('','')
      , relationship =
        'many-to-many'
    ) -> df_daytrolha

  rm(df_position_day)

  # calculate trolha
  df_daytrolha %>%
    relocate(
      date,
      type,
      ticker,
      stock,
      daytrolha,
      qtd,
      price,
      mean_price
    ) %>%
    mutate(
      trolha = -qtd * (
        price - mean_price
      )
    ) -> df_daytrolha

  # aggregate trolha by month-year
  df_daytrolha %>%
    group_by(
      year = year(date),
      month = month(date),
      ticker
    ) %>%
    reframe(
      trolha = sum(trolha)
    ) -> df_daytrolha_month

  # aggregate trolha by year
  df_daytrolha %>%
    group_by(
      year = year(date),
      ticker
    ) %>%
    reframe(
      trolha = sum(trolha)
    ) -> df_daytrolha_year

  # aggregate trolha by period
  df_daytrolha %>%
    group_by(ticker) %>%
    reframe(
      trolha = sum(trolha)
    ) -> df_daytrolha_period

  # add data frame subclasses
  new_data_frame(
    df_daytrolha
    , class = c(
      class(df_daytrolha)
      , 'df_daytrolha'
    )
  ) -> df_daytrolha

  new_data_frame(
    df_daytrolha_month
    , class = c(
      class(df_daytrolha_month)
      , 'df_daytrolha_month'
    )
  ) -> df_daytrolha_month

  new_data_frame(
    df_daytrolha_year
    , class = c(
      class(df_daytrolha_year)
      , 'df_daytrolha_year'
    )
  ) -> df_daytrolha_year

  new_data_frame(
    df_daytrolha_period
    , class = c(
      class(df_daytrolha_period)
      , 'df_daytrolha_period'
    )
  ) -> df_daytrolha_period

  # output
  return(list(
    'daytrolha' = df_daytrolha,
    'daytrolha_month' = df_daytrolha_month,
    'daytrolha_year' = df_daytrolha_year,
    'daytrolha_period' = df_daytrolha_period,
    'daytrolha_total' = sum(df_daytrolha_period$trolha)
  ))

}

