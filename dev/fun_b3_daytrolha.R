# [FUNCTIONS] --------------------------------------------------------------
# NOTE: TO CALCULATE TROLHA MEAN_PRICE IS REQUIRED! ------------------------
# - Identify daytrolha function -------------------------------------------
fun_b3_is_daytrolha <- function(df_transactions){

  # arguments validated in main function

  # daytrolha = buy and sell same asset on the same day
  df_transactions %>%
    group_by(
      date,
      ticker
    ) %>%
    mutate(
      .after = stock
      , daytrolha =
        length(unique(type)) > 1
    ) -> df_transactions

  # output
  return(df_transactions)

}

# - Calculate daytrolha function ---------------------------------------------------------
fun_b3_daytrolha <- function(df_transactions, df_position){

  # arguments validation
  stopifnot(
    "'df_transactions' must be a data frame with the 'df_transactions' subclass." =
      all(
        is.data.frame(df_transactions)
        , any(class(df_transactions) == 'df_transactions')
      )
  )

  stopifnot(
    "'df_position' must be a data frame with the 'df_position' subclass." =
      all(
        is.data.frame(df_position)
        , any(class(df_position) == 'df_position')
      )
  )

  # identify daytrolha
  df_transactions %>%
    fun_b3_is_daytrolha() ->
    df_transactions

  # get mean prices
  df_position %>%
    select(

    )


  # calculate trolha

  # aggregate trolha by month-year

  # aggregate trolha by year

  # aggregate trolha by period

  # output
  return(df_transactions)
  # return(list(
  #   'daytrolha' = list_daytrolha$transactions,
  #   'daytrolha_month' = list_daytrolha$month,
  #   'daytrolha_year' = list_daytrolha$year
  # ))

}
