my.ci = function(model, which){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  ci = stats::confint(model, parm=which)
  if(typeof(model)=='S4'){
    b = fixef(model)[which]
  } else { b = coef(model)}
  cbind(beta=b, ci)
}

my.approx.ci = function(model, which){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  co = summary(model)$coefficients %>%
    data.frame %>%
    tibble::rownames_to_column('term') %>%
    filter(term %in% which)
  b = co$Estimate
  se = co$Std..Error
  lower = b - 1.96*se
  upper = b + 1.96*se
  cbind(b, lower, upper)
}

