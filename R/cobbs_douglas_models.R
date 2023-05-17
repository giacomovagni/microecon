

#
#model1 = cobbs_douglas_utility(I = 100, a = 0.25, b = 0.75, px = 1, py = 1)
#model2 = cobbs_douglas_utility(I = 100, a = 0.25, b = 0.75, px = 1, py = 2)
#model3 = cobbs_douglas_utility(I = 100, a = 0.25, b = 0.75, px = 2, py = 1)
#model4_policy = cobbs_douglas_utility(I = 200, a = 0.25, b = 0.75, px = 2, py = 3)
#

#
g(model1, model2, model3, ymax = 150, xmax = 120)
#

#
cobbs_douglas_models = function(..., ymax = 120, xmax = 120){

  n = list(...)

  models = lapply(1:length(n), function(i) n[[i]]$df)
  optimal_bundles = lapply(1:length(n), function(i) n[[i]]$optimal_bundle)

  models = bind_rows(models, .id = 'model')
  optimal_bundles = bind_rows(optimal_bundles , .id = 'model')

  U_max = paste("U = ", round(optimal_bundles$max_U, 1))

  # plot #
  fig = models %>%
    ggplot(aes(x,y, colour = model)) +
    geom_line() +
    geom_line(aes(x,y_Umax, colour = model), linetype = 5) +
    ylim(c(0,ymax)) +
    xlim(c(0,xmax)) +
    theme_minimal() +
    annotate(geom = "text", x = optimal_bundles$x, optimal_bundles$y, label = U_max) +
    scale_color_calc() +
    geom_abline(slope = 1, linetype = 2, alpha = 0.4)
  #

  #
  return(list(fig = fig, models = models, optimal_bundles = optimal_bundles))
}
#

