
# Giacomo Vagni
# May 2023
# Essex

#
library(tidyverse)
#

############################################################
############################################################

# Varian
# ESTIMATING UTILITY FUNCTIONS 83

#
cobb_douglas_mult = function(a, px, I){
  x = (a / sum(a)) * (I/px)
  U = prod(x^a) # x1^a * x2^b * x3^c * ...
  df = data.frame(goods = 1:length(px), I, a, px, x, U, Budget_respect = sum(px*x))
  return(df)
}
#

# 4 goods #
I = 100
a = c(0.1, 0.1, 0.4, 0.4)
sum(a)
#
px = c(1,1,2,2)
#

#
cobb_douglas_mult(a = a, px = px, I = I)
#

###########################################################################
###########################################################################

# you can go back to 2 goods by removing the extra-goods from the Budget

# let's focus on good x3 and x4
I_new = 100 - ((20*2) + (20*2))
# new budget
I_new
#

###########################################################################
###########################################################################

# x3 and x4
I = I_new
a = c(0.4, 0.4) # keep the same alpha values
px = c(2,2) # keep the same prices
#
cobb_douglas_mult(a = a, px = px, I = I_new)
# we can plot a bivariate plot for x3 and x4
cobbs_douglas_utility(I = I_new, a = 0.4, b = 0.4, px = 2, py = 2)$optimal_bundle
#

