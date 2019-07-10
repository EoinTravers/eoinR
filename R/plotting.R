#' Place ggplot legend in corner specified.
lgnd = function(x, y){
  theme(legend.position=c(x, y), legend.justification = c(x, y))
}

# Some colours
# green = "#4daf4a"
# green2 = "#306E2E"
# orange = "#FFAE00"
# red = "#e41a1c"
# red2 = '#ff5600'
# blue = "#377eb8"

gg_color_hue <- function(n, offset=15) {
  hues = seq(offset, 360+offset, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

mean_sd = function (x, mult = 1) {
  x <- stats::na.omit(x)
  std <- sd(x)
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - std, ymax = mean + std)
}

smooth.pairplot = function(data, ...){
  df = data %>% select(...)
  order = names(df)
  f = function(df, .xvar){
    df %>% gather(yvar, y, -.xvar) %>%
      rename(x=.xvar) %>% mutate(xvar=.xvar) %>%
      select(xvar, yvar, x, y) %>%
      return()
  }
  res = names(df) %>% map_df(f, df=df) %>%
    mutate(xvar = factor(xvar, levels = order),
           yvar = factor(yvar, levels = order))
  g = ggplot(res, aes(x, y)) +
    facet_grid(yvar~xvar, scales='free', margins=F, switch='both', shrink=F) +
    stat_smooth(method='loess') +
    labs(x='', y='') +
    theme_bw()
  return(g)
}
