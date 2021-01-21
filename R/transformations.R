# Basic transformations
# TODO: Add documentation

logit = function(p) log(p / (1-p))
invlogit = function(x) exp(x)/(1+exp(x))
reciprocal = function(x) 1/x
prob.to.odds = function (x) x / (1-x)
odds.to.prob = function(x) x / (1+x)
odds.to.risk = function(odds, control.p) {odds / (1 - control.p + (control.p * odds))}

num.cut = function(x, bins){
  cuts = cut(x, bins)
  cuts %>% str_replace_all('[\\(\\[\\)\\]]', '') %>%
      str_split(',') %>% map(as.numeric) %>% map_dbl(mean)
}

edge = function(x, size=.001) {
  xsign = sign(.5-x)
  return (x + size*xsign)
}

z = function(x){scale(x)}
center  = function(x){scale(x, scale=F)}
as.caps = function(x) sapply(x, Hmisc::capitalize)

sem = function(x, na.rm=F){
  sd(x, na.rm=na.rm) / sqrt(length(x))
}

mean.sd = function(x, digits=2){
  # Returns string with "Mean (Standard deviation)" of intput.
  # Nice for APA tables!
  x.mean = mean(x, na.rm=T) %>% round(digits)
  x.sd = sd(x, na.rm=T) %>% round(digits)
  paste(x.mean, ' (', x.sd, ')', sep='') %>% return()
}

mean_quantile = function(x, alpha){
  x <- stats::na.omit(x)
  mean <- mean(x)
  iq = .5 * (1 - alpha)
  q = c(iq, 1-iq)
  quant = quantile(x, q)
  data.frame(y = mean, ymin = quant[1], ymax = quant[2])
}

mean_95 = purrr::partial(mean_quantile, alpha=.95)
mean_68 = purrr::partial(mean_quantile, alpha=.68)

q2.5 = function(x) quantile(x, .025)
q97.5 = function(x) quantile(x, .975)


#' Transform posterior samples (e.g. from arm::sim) to p values using Normal approximation.
p.value.from.samples = function(samps){
  b = mean(samps)
  se = sd(samps)
  z = c(b / se)[[1]]
  p = 2 * pnorm(-abs(z))
  p
}

soft_apply = function(df, fun) {
  nums = sapply(df, is.numeric)
  df[,nums] %>% fun
}

round_df <- function(df, digits) {
  mutate_if(df, is.numeric, funs(round), digits=digits)
}

relative.risk = function(ci){
  # Only for intercept + single term models
  control.p = logit(ci[1,1])
  out = c(
    odds.to.risk(exp(ci[2,1]), control.p),
    odds.to.risk(exp(ci[2,2]), control.p),
    odds.to.risk(exp(ci[2,3]), control.p))
  names(out) = c('estimate', 'lower', 'upper')
  out
}

## Newer stuff
chr_to_ordinal = function(vals){
  # Cooerce strings to ordinal numers
  as.numeric(as.factor(vals))
}

z_transform = function(x){
  cx = (x - mean(x, na.rm = T))
  std = sd(cx, na.rm = T)
  if(std > 0){
    cx = cx / std
  }
  return(cx)
}

bound_transform = function(x, lower=-1, upper=1){
  .low = min(x, na.rm=T)
  .high = max(x, na.rm=T)
  unit_scaled = (x - .low) / (.high-.low)
  (unit_scaled * (upper-lower))  + lower
}

sem = function(x){
  sd(x) / sqrt(length(x))
}




center  = function(x) x - mean(x, na.rm = T)
z = function(x){
  cx = center(x)
  cx / sd(cx, na.rm = T)
}

#' Replace values if a substitute is defined
#'
#' @param x Vector of strings
#' @param sub_list A named list of substitutes
#' @return A modified vector
#'
#' @examples
#' x = c('ugly', 'something else')
#' sub_list = list('ugly'='Pretty', 'Nasty'='Nice')
#' replace_if_found(x, sub_list)
#' @export
replace_if_found = function(x, sub_list){
  .can.replace = x %in% names(sub_list)
  x[.can.replace] = purrr::map_chr(x[.can.replace], ~sub_list[[.]])
  x
}

#' Round numeric columns
#'
#' @param df Input data.frame
#' @param digits Digits to round to
#' @return Rounded data.frame
#' @export
round_df <- function(df, digits) {
  dplyr::mutate_if(df, is.numeric, round, digits=digits)
}


#' Gathers a matrix into a long data frame
#' @export
gather_matrix = function(mat) {
  mat %>%
    data.frame() %>%
    tibble::rownames_to_column('var1') %>%
    gather(var2, value, -var1)
}

#' Gather data into very long data.frame of pairwise comparisons
#'
#' @description
#' Gather data into very long data.frame of pairwise comparisons,
#' with columns c(xvar, yvar, x, y).
#' If input has n rows, m columns, output has n * (m^2 - m) rows (and 4 columns).
#'
#' @param df Data frame to gather
#' @return Data frame with one row per pairwise comparison
#'
#' @export
gather_pairwise = function(df){
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
  res
}
