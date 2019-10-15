#' Place ggplot legend in corner specified.
lgnd = function(x, y){
  theme(legend.position=c(x, y), legend.justification = c(x, y))
}

no_legend = function(){
  theme(legend.position = 'none')
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

coefplot = function(model, se.width=1.96, sort.by=NA,
                    mark.sig=T, name.updater=NA){
  # se.width = 1.96
  # sort.by = NA | 'length' | 'any string'
  co = model %>% summary() %>% coef() %>%
    data.frame() %>% rownames_to_column('term') %>%
    rename(b=Estimate, se=Std..Error) %>%
    mutate(lo = b-se.width*se, hi=b+se.width*se)
  if(!is.na(name.updater)) {
    co = mutate(co,
                term = str_replace_all(term,  name.updater) %>% str_replace_all('\\(Intercept\\)', 'Intercept'))
  }
  names(co)[str_detect(names(co), '^Pr...')] = 'p.val'
  if(!is.na(sort.by)) {
    if(sort.by == 'length') {
      co = co %>%
        mutate(term.length = ifelse(term=='Intercept', 0, str_length(term)  )) %>%
        arrange(term.length)
      levels(co$term) = co[order(co$term.length, co$term) , 'term']
    } else {
      ## Sort by count of whatever the string was.
      co = co %>%
        mutate(term.length = ifelse(term=='Intercept', 0, str_count(term, sort.by))) %>%
        arrange(term.length)
      levels(co$term) = co[order(co$term.length, co$term) , 'term']
    }
  }
  co$i = 1:nrow(co)
  g = ggplot(co, aes(reorder(term, rev(i)), b, ymin=lo, ymax=hi)) +
    geom_hline(yintercept=0, linetype='dashed') +
    labs(x='Term', y='Regression weight') +
    coord_flip()
  if(mark.sig) {
    g = g + geom_point(aes(color=(p.val<.05))) + geom_linerange(aes(color=(p.val<.05))) +
      labs(color='p < .05') +
      scale_color_manual(values=c('black', 'red')) +
      lgnd(1,0)
  } else {
    g = g + geom_point() + geom_linerange()
  }
  g
}

binomial_smooth = function(link='probit', ...){
    geom_smooth(method='glm', method.args = list(family=binomial(link=link)), ...)
}
