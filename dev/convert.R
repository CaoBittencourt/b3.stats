# [FUNCTIONS] -------------------------------------------------------------
# - convert assets function -----------------------------------------------
fun_b3_convert <- function(
    df_position
    , df_events_transfers
    , df_converted_assets = NULL
){

  # arguments validation

  # convert assets
  if(!is.null(df_converted_assets)){

    # temp var prop
    # df_position$prop <- 1

    df_converted_assets %>%
      inner_join(
        df_events_transfers
      ) %>%

    ... %>%
      right_join(
        df_position
      ) -> df_position

    # mean price
    # value = old's stock total value
    # value = value * prop
    # mean price = value / qtd

    # drop temp vars
    # select(-prop) -> df_position

  }

  rm(df_events_transfers)
  rm(df_converted_assets)

  # output
  return(df_position)

}

# dsds --------------------------------------------------------------------
NULL %>% right_join(
  tibble(diag(3))
)

new_data_frame(
  tibble(diag(3))
  , class = c(
    class(tibble(diag(3)))
    , 'dsds'
  )
) -> dsds

bind_rows(
  dsds
  ,tibble(diag(3))
) %>% class()


tibble(
  # old_ticker = c('HGTX3', 'CMRV11', 'CMRV3'),
  # ticker = rep('SOMA3', 3)
  ticker = c('HGTX3', 'CMRV11', 'CMRV3'),
  new_ticker = rep('SOMA3', 3)
  ,active = F
) %>%
  left_join(
    list_b3_data$
      events$
      transfers
    , by = 'ticker'
    , suffix = c('','.transfers')
    , relationship = 'many-to-many'
  ) %>%
  select(
    -type,
    # -event,
    -ends_with('.transfers')
  )


list_b3_data$events %>% map(~ .x %>% filter(str_detect(ticker, 'CMRV')))
list_b3_data$events %>% map(~ .x %>% filter(str_detect(ticker, 'GMSH')))
list_b3_data$events %>% map(~ .x %>% filter(str_detect(ticker, 'HGTX')))
list_b3_data$events %>% map(~ .x %>% filter(str_detect(ticker, 'SOMA')))

list_position$
  position %>%
  filter(
    str_detect(
      ticker
      , 'HGTX|CMRV|GMSH'
    )
  )

list_position$
  position %>%
  filter(
    str_detect(
      ticker
      , 'BIDI|INBR|INHF'
    )
  )

# resgate é evento financeiro (cash event / other)

# tibble(
#   # old_ticker = c('HGTX3', 'CMRV11', 'CMRV3'),
#   # ticker = rep('SOMA3', 3)
#   ticker = c('HGTX3', 'CMRV11', 'CMRV3'),
#   new_ticker = rep('SOMA3', 3)
#   ,active = F
# ) %>%
#   left_join(
#     list_b3_data$
#       events$
#       transfers
#     , by = 'ticker'
#     , suffix = c('','.transfers')
#     , relationship = 'many-to-many'
#   ) %>%
#   select(
#     -type,
#     -event,
#     -ends_with('.transfers')
#   ) %>%
#   right_join(
#     list_position$
#       position
#   )