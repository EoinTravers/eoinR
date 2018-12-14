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

mean.sd = function(x, digits=2){
  # Returns string with "Mean (Standard deviation)" of intput.
  # Nice for APA tables!
  x.mean = mean(x, na.rm=T) %>% round(digits)
  x.sd = sd(x, na.rm=T) %>% round(digits)
  paste(x.mean, ' (', x.sd, ')', sep='') %>% return()
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
