
#' Solve for Y
#' @name MU_cb
#' @param x the quantity of x
#' @param a the alpha value
#' @returns numeric
#' @examples
#' cb = cobbs_douglas_utility(I = 100, a = a, b = b, px = 1, py = 1)
#' a = 0.5
#' b = 0.5
#' x = cb$x
#' y = cb$y
#' plot(x, MU_cb(x, 0.5), type = 'l')

#
MU_cb = function(x,a) x^a