#' Place ggplot legend in corner specified.
lgnd = function(x, y){
  theme(legend.position=c(x, y), legend.justification = c(x, y))
}
