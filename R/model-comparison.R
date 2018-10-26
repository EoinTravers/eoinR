
stepwise.lmer = function(dv, ivs, ranfx, data) {
  warning('This is an old function, and has not been tested. Use at your own risk.')
  # String, list of strings, string, data.frame
  f0 = paste(dv, '~', ranfx)
  m0 = lmer(f0, data=data, REML=F)
  # Get all higher order terms
  # 	main.fx = ivs
  # 	for (n in 2:length(ivs)){
  # 		interactions = combn(main.fx, n)
  # 		for (i in 1:ncol(interactions)){
  # 			ivs = c(ivs,
  # 					  paste(interactions[,i], collapse=':'))
  # 		}
  # 	}
  chi.sq = c()
  Df = c()
  p.vals = c()
  for (i in 1:length(ivs)){
    f1 = paste(
      dv, '~',
      paste(ivs[1:i], collapse=' + '),
      '+', ranfx)
    m1 = lmer(formula=f1, data=data, REML=F)
    a = anova(m0, m1)
    chi = a$Chisq[2] %>% round(2)
    chi.sq = c(chi.sq, chi)
    d = a[2,7]
    Df = c(Df, d)
    p = round(a[2,8], 3)
    p.vals = c(p.vals, p)
    # For next loop
    m0 <- m1
  }
  out = data.frame(X=chi.sq, DF=Df, p=p.vals)
  rownames(out) = ivs
  return (out)
}

stepwise.glmer = function(dv, ivs, ranfx, data) {
  warning('This is an old function, and has not been tested. Use at your own risk.')
  # String, list of strings, string, data.frame
  f0 = paste(dv, '~', ranfx)
  m0 = glmer(f0, data=data, family=binomial)
  # Get all higher order terms
  # 	main.fx = ivs
  # 	for (n in 2:length(ivs)){
  # 		interactions = combn(main.fx, n)
  # 		for (i in 1:ncol(interactions)){
  # 			ivs = c(ivs,
  # 					  paste(interactions[,i], collapse=':'))
  # 		}
  # 	}
  chi.sq = c()
  Df = c()
  p.vals = c()
  for (i in 1:length(ivs)){
    f1 = paste(
      dv, '~',
      paste(ivs[1:i], collapse=' + '),
      '+', ranfx)
    m1 = glmer(f1, data=data, REML=F, family=binomial)
    a = anova(m0, m1)
    chi = a$Chisq[2] %>% round(2)
    chi.sq = c(chi.sq, chi)
    d = a[2,7]
    Df = c(Df, d)
    p = round(a[2,8], 3)
    p.vals = c(p.vals, p)
    # For next loop
    m0 <- m1
  }
  out = data.frame(X=chi.sq, DF=Df, p=p.vals)
  rownames(out) = ivs
  return (out)
}

lmer.many.dv = function (dvs, f0, f1, data){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  chi.sq = c()
  p.vals = c()
  max.grads = c()
  for (dv in dvs){
    if(length(unique(data[[dv]]))==2){
      m0 = glmer(paste(dv, f0, sep=''),
                 data=data, family=binomial)
      m1 = glmer(paste(dv, f1, sep=''),
                 data=data, family=binomial)
    }
    else {
      m0 = lmer(paste(dv, f0, sep=''),
                data=data,
                REML=F)
      m1 = lmer(paste(dv, f1, sep=''),
                data=data,
                REML=F)
    }
    a = anova(m0, m1)
    chi = a$Chisq[2] %>% round(2)
    chi.sq = c(chi.sq, chi)
    p = round(a[2,8], 4)
    p.vals = c(p.vals, p)
    max.grad = m1@optinfo$derivs %>%
      with(solve(Hessian,gradient)) %>% abs %>% max
    max.grads = c(max.grads, max.grad)

  }
  out = data.frame(X=chi.sq, p=p.vals, max.grad=max.grads)
  rownames(out) = dvs
  return (out)
}

model.names.from.anova.object = function(a){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  # Hack out model names from ANOVA object
  h = attr(a, 'heading')
  h = h[3:length(h)] # First two values are padding
  is.start.line = map_lgl(h, ~stringr::str_detect(., '~'))
  h = h[is.start.line]
  get.group.name = function(s){
    stringr::str_match(s, '.+?:') %>%
      stringr::str_replace(':', '')
  }
  map_chr(h, get.group.name)
}

aic.weights = function(a){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  d = a$AIC - max(a$AIC)
  lik = 1/exp(-.5*d)
  p.best = lik / sum(lik)
  m.names = model.names.from.anova.object(a)
  data.frame(model=m.names, aic=a$AIC, aic.lik=lik, p.best=p.best)
}


lme4.anova.summary = function(a, md=T, row.names=NULL){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  d = data.frame(a)
  if( is.character(row.names)){
    rownames(d) = row.names
  }
  names(d) = c('SS', 'MS', 'DF_1', 'DF_2', 'F', 'p')
  if (md) {
    d = md.table(d)
  }
  return (d)
}

compare.models = function(...){
  warning('This is an old function, and has not been tested. Use at your own risk.')
  model.names = match.call() %>% as.list
  model.names = model.names[2:length(model.names)]
  model.names = map_chr(model.names, toString)
  ## print(model.names)
  a = lme4:::anova.merMod(...)
  a = tidy(a) %>% arrange(term)
  # print(a)
  o = data.frame(
    model=as.array(model.names),
    AIC=a$AIC,
    BIC=a$BIC)  %>%
    mutate(
      AIC.delta = AIC - min(AIC),
      BIC.delta = BIC - min(BIC),
      AIC.ratio = exp(-.5*AIC.delta),
      BIC.ratio = exp(-.5*BIC.delta),
      AIC.prob = AIC.ratio / sum(AIC.ratio),
      BIC.prob = BIC.ratio / sum(BIC.ratio)
    )
  return(o)
}
