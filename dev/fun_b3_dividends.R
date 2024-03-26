# [FUNCTIONS] --------------------------------------------------------------
# - Calculate dividends function ---------------------------------------------------------
fun_b3_dividends <- function(df_dividends){

  # arguments validation
  stopifnot(
    "'df_dividends' must be a data frame with the 'df_dividends' subclass." =
      all(
        is.data.frame(df_dividends)
        , any(class(df_dividends) == 'df_dividends')
      )
  )

  # aggregate dividends
  df_dividends %>%
    group_by(
      year = year(date),
      ticker,
      event
    ) %>%
    reframe(
      total = sum(
        qtd * price
      )
    ) -> df_dividends_agg

  rm(df_dividends)

  # add data frame subclass
  new_data_frame(
    df_dividends_agg
    , class = c(
      class(df_dividends_agg)
      , 'df_dividends_agg'
    )
  ) -> df_dividends_agg

  # output
  return(df_dividends_agg)

}