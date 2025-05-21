# [FUNCTIONS] --------------------------------------------------------------
# - Mean price function ---------------------------------------------------
fun_b3_mean_price <- function(
    dbl_qtd,
    dbl_price,
    dbl_cycle,
    dbl_position =
      cumsum(dbl_qtd),
    dbl_prop) {
  # arguments validation
  stopifnot(
    "'dbl_qtd', 'dbl_price', 'dbl_cycle', 'dbl_position', and 'dbl_prop' must all be numeric vectors of the same length." =
      all(
        is.numeric(dbl_qtd),
        is.numeric(dbl_price),
        is.numeric(dbl_cycle),
        is.numeric(dbl_position),
        is.numeric(dbl_prop),
        !any(diff(
          length(dbl_qtd),
          length(dbl_price),
          length(dbl_cycle),
          length(dbl_position),
          length(dbl_prop)
        )) # bug when nrow == 1
      )
  )

  # initialize mean price
  dbl_price[1] -> dbl_mean_price

  if (length(dbl_price) > 1) {
    # calculate mean price
    for (t in 2:length(dbl_qtd)) {
      # buy
      # or split / event
      # after buying
      if (all(
        dbl_price[t] >= 0,
        is.na(dbl_qtd[t]) | dbl_qtd[t] > 0,
        dbl_cycle[t - 1] ==
          dbl_cycle[t]
      )) {
        # mean cost of acquisition
        weighted.mean(
          x = c(
            dbl_mean_price[t - 1],
            dbl_price[t]
          ),
          w = c(
            dbl_position[t - 1],
            dbl_qtd[t]
          ),
          na.rm = T
        ) -> dbl_mean_price[t]
      }

      # sell
      if (is.na(dbl_qtd[t]) | dbl_qtd[t] < 0) {
        # selling does not alter mean price
        dbl_mean_price[t - 1] ->
        dbl_mean_price[t]
      }

      # split / event
      # after selling
      # or grouping event
      if (any(
        dbl_cycle[t - 1] !=
          dbl_cycle[t],
        all(
          dbl_price[t] == 0,
          is.na(dbl_qtd[t]) | dbl_qtd[t] > 0,
          dbl_qtd[t - 1] < 0
        )
      )) {
        # mean price adjusted by proportion
        dbl_mean_price[t - 1] *
          dbl_position[t - 1] /
          dbl_position[t] ->
        dbl_mean_price[t]
      }
    }
  }

  # adjust mean price by proportion
  dbl_mean_price *
    dbl_prop ->
  dbl_mean_price

  # output
  return(dbl_mean_price)
}

# # - Mean price function ---------------------------------------------------
# fun_b3_mean_price <- function(
#     dbl_qtd
#     , dbl_price
#     , dbl_cycle
#     , dbl_position =
#       cumsum(dbl_qtd)
# ){
#
#   # arguments validation
#   stopifnot(
#     "'dbl_qtd', 'dbl_price', 'dbl_cycle', and 'dbl_position' must all be numeric vectors of the same length." =
#       all(
#         is.numeric(dbl_qtd),
#         is.numeric(dbl_price),
#         is.numeric(dbl_cycle),
#         is.numeric(dbl_position),
#         !any(diff(
#           length(dbl_qtd),
#           length(dbl_price),
#           length(dbl_cycle),
#           length(dbl_position)
#         )) #bug when nrow == 1
#       )
#   )
#
#   # initialize mean price
#   dbl_price[1] -> dbl_mean_price
#
#   if(length(dbl_price) > 1){
#
#     # calculate mean price
#     for(t in 2:length(dbl_qtd)){
#
#       # buy
#       # or split / event
#       # after buying
#       if(all(
#         dbl_price[t] >= 0,
#         dbl_qtd[t] > 0,
#         dbl_cycle[t - 1] ==
#         dbl_cycle[t]
#       )){
#
#         # mean cost of acquisition
#         weighted.mean(
#           x = c(
#             dbl_mean_price[t - 1]
#             , dbl_price[t]
#           )
#           , w = c(
#             dbl_position[t - 1]
#             , dbl_qtd[t]
#           )
#         ) -> dbl_mean_price[t]
#
#       }
#
#       # sell
#       if(dbl_qtd[t] < 0){
#
#         # selling does not alter mean price
#         dbl_mean_price[t - 1] ->
#           dbl_mean_price[t]
#
#       }
#
#       # split / event
#       # after selling
#       # or grouping event
#       if(any(
#         dbl_cycle[t - 1] !=
#         dbl_cycle[t]
#         , all(
#           dbl_price[t] == 0,
#           dbl_qtd[t] > 0,
#           dbl_qtd[t - 1] < 0
#         )
#       )){
#
#         # mean price adjusted by proportion
#         dbl_mean_price[t - 1] *
#           dbl_position[t - 1] /
#           dbl_position[t] ->
#           dbl_mean_price[t]
#
#       }
#
#     }
#
#   }
#
#   # output
#   return(dbl_mean_price)
#
# }
#
