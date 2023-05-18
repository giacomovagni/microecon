
#' Solve for Y
#' @name demand_curve
#' @param U the Utility
#' @param x the quantity of x
#' @param a the alpha value
#' @param b the beta value
#' @returns A dataframe
#' @examples
#' a = 0.2; b = 0.5; px = 1; py = 3; I = 1000
#' cb_objet = cobbs_douglas_utility(I = I, a = a, b = a, px = px, py = py)
# demand_curve(cb_objet = cb_objet, max_price = 100, y_max = 100, x_max)
#


#
demand_curve = function(cb_objet, max_price = 50, y_max = 50, x_max = 50){

  x = cb_objet$x
  y = cb_objet$y

  px = cb_objet$px
  py = cb_objet$py

  I = cb_objet$I

  # price varies
  px_v = seq(0,max_price, by = 0.1)
  py_v = seq(0,max_price, by = 0.1)
  #

  y_fixed = median(y)
  x_fixed = median(x)

  x_demand = (I - py*y_fixed) / px_v
  y_demand = (I - px*x_fixed) / py_v

  x_d = data.frame(good = "x", price = px_v, demand = x_demand)
  y_d = data.frame(good = "y", price = py_v, demand = y_demand)

  bd = bind_rows(x_d, y_d)
  bd = bd[!is.infinite(bd$demand), ]

  #
  fig =
    bd %>%
    ggplot(aes(demand,price, colour = good)) +
      geom_line() + facet_grid(~good) +
      theme_minimal() +
      scale_color_manual(values = c('red', 'blue4')) +
      xlim(c(min(x_demand),x_max)) +
      ylim(c(min(y_demand),y_max))
  #

  #
  return(list(demand = bd, fig = fig))
}
