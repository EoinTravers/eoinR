

theme_eoin = function(base_size=16){
  theme_classic(base_size = base_size) +
    theme(panel.grid.major = element_line(color='grey', size=.25))
}

no_grid = function(){
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}

theme_eoin_box = function(base_size=16){
  theme_bw(base_size = base_size)
}

#' Place ggplot legend in corner specified.
lgnd = function(x, y){
  theme(legend.position=c(x, y), legend.justification = c(x, y))
}

no_legend = function(){
  theme(legend.position = 'none')
}

no_legend_bg = function(){theme(legend.background = element_blank())}


tilt_x_ticks = function(angle=45, vjust=1, hjust=1){
  theme(axis.text.x = element_text(angle=angle, vjust=vjust, hjust=hjust))
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

pcnt = function(x){
  scales::percent(x, accuracy = 1)
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


column_plot = function(data,
                       scale_by='bounds',
                       colours=c('blue', 'white', 'red')){
  df = data %>%
    mutate_if(~{!is.numeric(.)}, chr_to_ordinal)
  if(scale_by=='bounds') df = mutate_all(df, bound_transform)
  if(scale_by=='z') df = mutate_all(df, z_transform)
  df = df %>%
    select_if(is.numeric) %>%
    mutate(ix = 1:n()) %>%
    gather(x, y, -ix)

    df %>%
        ggplot(aes(x, ix, fill=y)) +
        geom_tile() +
        coord_flip() +
        scale_fill_gradient2(low=colours[1], mid=colours[2], high=colours[3]) +
        theme(legend.position='none')
}


#' Plot matrix as heatmap
#' @description
#' `plot_covariance_matrix()` adds an appropriate `fill_label`
#' `plot_correlation_matrix()` also sets limits to ±1
#'
#' @param mat Matrix to plot
#' @param labeller Function or list used to rename rows/cols
#' @param digits Digits to round values to (default 2)
#' @param limit Limit (±) of colour scale. Defaults to `abs(max(value))`
#' @param fill_label Label to use for colourscale
#' @param fill_gradient Optional custom fill gradient (default blue-white-red)
#'
#' @examples
#' iris %>%
#'   select_if(is.numeric) %>%
#'   cor() %>%
#'   plot_correlation_matrix()
#'
#' @export
plot_matrix = function(mat, labeller=NULL,
                       digits=2, limit=NULL, fill_label=NULL,
                       fill_gradient=NULL) {
  var_order = colnames(mat)
  df = gather_matrix(mat) %>%
    mutate(var1 = factor(var1, levels=var_order),
           var2 = factor(var2, levels=rev(var_order)))
  if(is.null(limit)){
    limit = abs(max(df$value))
  }
  # Handle labels
  if(is.function(labeller)){
    levels(df$var1) = labeller(levels(df$var1))
    levels(df$var2) = labeller(levels(df$var2))
  } else if (is.list(labeller)){
    levels(df$var1) = replace_if_found(levels(df$var1), labeller)
    levels(df$var2) = replace_if_found(levels(df$var2), labeller)
  }
  g = df %>%
    ggplot(aes(var1, var2, fill=value, label=round(value, digits))) +
    geom_tile() +
    geom_label(fill='white') +
    coord_fixed() +
    labs(x='', y='', fill=fill_label)
  # Use default gradient (blue-white-red), or apply custom one.
  if(is.null(fill_gradient)){
    g = g + scale_fill_gradient2(low='blue', mid='white', high='red',
                                 limits=c(-limit, limit))
  } else {
    g = g + fill_gradient
  }
  g
}
#' @rdname plot_matrix
#' @export
plot_covariance_matrix = function(cov_mat, ...){
  plot_matrix(cov_mat, fill_label='(Co)variance', ...)
}

#' @rdname plot_matrix
#' @export
plot_correlation_matrix = function(cor_mat, ...){
  plot_matrix(cor_mat, limit=1, fill_label='ρ', ...)
}

#' @export
plot_pairwise = function(df, smooth='loess'){
  res = gather_pairwise(df)
  g = ggplot(res, aes(x, y)) +
    facet_grid(yvar~xvar, scales='free', margins=F, switch='both', shrink=F)
  if(!is.null(smooth)){
    g = g + stat_smooth(method=smooth)
  }
  g = g +
    geom_point() +
    labs(x='', y='') +
    theme(panel.border = element_rect(colour = "black", fill=NA))
  g
}
