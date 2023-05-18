
#' Solve for Y
#' @name f_derive
#' @param h the instantaneous rate of change
#' @param x the quantity of good x
#' @param y the quantity of good y
#' @param a the alpha value
#' @param b the beta value
#' @returns A dataframe
#' @examples
#' f_derive(h = 0.01, x = 10, y = 10, a = 0.5, b = 0.5) 
#' MUxy = - f_derive(h = 0.01, x = x, y = y, a = a, b = b) / f_derive(h = 0.01, x = y, y = x, a = b, b = a)

#
f_derive = function(h = 0.01, f = f_cobbs, x, y, a, b) (f(x = x+h, y = y, a = a, b = b) - f(x = x,y = y,a = a, b = b)) / h

# MUxy = - f_derive(h = 0.01, x = x, y = y, a = a, b = b) / f_derive(h = 0.01, x = y, y = x, a = b, b = a)
