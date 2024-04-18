# [FUNCTIONS] --------------------------------------------------------------
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
    ) %>%
    ungroup() ->
    df_transactions

  # output
  return(df_transactions)

}
