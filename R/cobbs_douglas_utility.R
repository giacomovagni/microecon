
############################################################
############################################################

#' Solve for Y
#' @name f_cobbs_y
#' @param U the Utility
#' @param x the quantity of x
#' @param a the alpha value
#' @param b the beta value
#' @returns A dataframe
#' @examples
#' f_cobbs_y(x=0:10,U=5,a=0.5,b=0.5)

#
f_cobbs = function(x=x,y=y,a=a,b=b) (x^a) * (y^b)
# cobbs-douglas solve for y
f_cobbs_y = function(x,U=25,a=a,b=b) (U / x^a)^(1/b)
# maximum number of good x given a price
f_max = function(I,p) I/p
# budget function
f_budget = function(x,px,y,py) x*px + py*y
# budget solve for y
f_budget_y = function(x,px,py, I) (I - x*px)/py
#

############################################################
############################################################


#' Calculate the optimal bundle for two goods (x and y) given a budget constraint.
#' \deqn{U(x,y) = x^a \cdot y^b}
#' @name cobbs_douglas_utility
#' @param I the budget constraint
#' @param px the price of good x
#' @param py the price of good y
#' @param a the alpha value
#' @param b the beta value
#' @returns A list.
#' @examples
#' cobbs_douglas_utility(I=100, a=0.5, b=0.5, px = 1, py = 2)
#' cobbs_douglas_utility(I=1000, a=0.4, b=0.3, px = 1.2, py = 1.4)

# general function to solve a Cobbs-Douglas Utility function #
cobbs_douglas_utility = function(I, a=0.5, b=0.5, px, py){

  # find maximum number of x I can get given their price
  # what if I spent all my money on x
  x_max = f_max(I = I, p = px)
  # find maximum number of x I can get given their price
  # what if I spent all my money on y
  y_max = f_max(I = I, p = py)
  #

  # slope of budget line is px/py
  slope = -(px/py)
  #

  x = 0:x_max
  x[1] = 0.01
  # calculate the values for y that respect the budget #
  y = y_max + (slope * x)
  #

  #
  U = f_cobbs(x,y,a,b)
  max_U = max(U)
  #

  #
  y_Umax = f_cobbs_y(x = x, U = max_U, a = a, b = b)
  #

  #
  df = data.frame(x, y, yU = f_cobbs_y(x, U = max_U, a = a, b = b), max_U) %>% mutate(Budget = x*px+yU*py)
  # we can see that we are over our budget
  head(df) #
  # we retrieve the row which respect our budget
  # because I am using rounded values, we are not exactly at 100, but very close
  w = which.min(df$Budget - I)
  # df[w, ]
  # the optimal combination maximising utility
  # is 23 = x, and 15.5 = y
  # which respect our budget of $100
  #

  #
  optimal_bundle = df[w, ]
  #

  p1 = round(optimal_bundle$max_U,1)
  p2 = round(optimal_bundle$x,1)
  p3 = round(optimal_bundle$y,1)
  plab = paste("Optimal Bundle (", p2, ",", p3, ")", " Max Utility = ", p1, sep = '')

  #
  fig = df %>%
    ggplot(aes(x,y)) +
    geom_line() +
    geom_hline(yintercept = optimal_bundle$y, alpha = 0.6, linetype = 2) +
    geom_vline(xintercept = optimal_bundle$x, alpha = 0.6, linetype = 2) +
    xlim(c(0,x_max+10)) +
    ylim(c(0,y_max+10)) +
    theme_minimal() +
    geom_line(aes(x, yU), col = 'red') +
    annotate(geom = "text", x = -Inf, y = Inf, label = plab, vjust = 2, hjust = 0)

  #
  df = data.frame(x, y, y_Umax)
  #

  fig

  #
  return(list(df = df, x_max = x_max, y_max = y_max, I = I, a = a, b = b, px = px, py = py, slope = slope, x = x, y = y, y_Umax = y_Umax, U=U, optimal_bundle = optimal_bundle, fig = fig))
}
