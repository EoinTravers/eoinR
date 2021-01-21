pretty.lme.table = function (model, row.names=NULL, pandoc=TRUE) {
  model.summary = summary(model)
  model.coef = coef(model.summary)
  model.ci = confint(model, method="Wald", parm=names(fixef(model)))
  model.coef = cbind(model.coef, model.ci)
  if (!is.null(row.names)){ row.names(model.coef) = row.names}
  out = round(model.coef, 3)
  if (pandoc){ out = pandoc.table(out, split.tables=Inf)}
  return(out)
}

pretty.t = function(t){
  p = 2*pnorm(-abs(t))
  t = round(t, 3)
  if(p < .001){paste(t, '***') %>% return}
  else if(p < .01){paste(t, '**') %>% return}
  else if(p < .05){paste(t, '*') %>% return}
  else if(p < .1){paste(t, '.') %>% return}
  else return(round(t, 3) %>% as.character )
}

pretty.p = function (p){
  options(scipen=999)
  sapply(p, function(p){
    p = round(p, 3)
    if(p < .001) { return (paste("<.001", "***"))}
    if(p < .01)  { return (paste(substr(p, 2, 5), "**"))}
    if(p < .05)  { return (paste(substr(p, 2, 5), "*"))}
    if(p < .1)   { return (paste(substr(p, 2, 5), "."))}
    else{return (as.character(p))}
  })
}

pretty.stars = function(p){
  sapply(p, function(p){
    p = round(p, 3)
    if(p < .001) { return ("***")}
    if(p < .01)  { return ("**")}
    if(p < .05)  { return ("*")}
    if(p < .1)   { return (".")}
    else{return ('')}
  })
}

pretty.coef = function (coef, p) {
  coef = round(coef, 3)
  if(p < .001){paste(coef, '***') %>% return}
  else if(p < .01){paste(coef, '**') %>% return}
  else if(p < .05){paste(coef, '*') %>% return}
  else if(p < .1){paste(coef, '.') %>% return}
  else return(round(coef, 3) %>% as.character )
}
v.pretty.coef = Vectorize(pretty.coef)

debracket = function(x) {
  gsub('\\(', '', x) %>% gsub('\\)', '', .)
}

get.ci = function(model, level=.95){
  ci = confint(model, level=level, method='Wald') %>%
    data.frame() %>% rownames_to_column('term')
  names(ci) = c('term', 'lower', 'upper')
  b = fixef(model) %>% data.frame() %>% rownames_to_column('term') %>% rename(b='.')
  inner_join(b, ci, by='term') %>% return()
}
