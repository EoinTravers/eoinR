#' Print verbal description of lme4::lmer or lme4::glmer model.
#'
#' @param mod The lme4 model to describe.
#'
#' @return None
verbalise_lmm = function(mod, digits = 2, subject_id='subject_nr'){
  stopifnot('merModLmerTest' %in% class(mod)) # Make sure model fit with lmerTest
  s = summary(mod)
  co = s %>% coef %>% round(digits)
  ci = confint(mod, method='Wald', parm='beta_') %>% round(digits)
  vc = s$varcor[[subject_id]]
  subj.sd = attr(vc, 'stddev') %>% round(digits)
  a = car::Anova(mod)
  template_lmm = 'Parameter: %s. B = %.#f CI = [%.#f, %.#f], t(%.1f) = %.3f, p = %.3f, '
  template_lmm = str_replace_all(template_lmm, '#', paste0(digits))
  template_glmm = 'Parameter: %s. B = %.#f CI = [%.#f, %.#f], z = %.3f, p = %.3f, '
  template_glmm = str_replace_all(template_glmm, '#', paste0(digits))
  template_aov = 'χ^2(%i) = %.2f, p = %.3f, '
  result = ''
  for(v in rownames(co)){
    if('t value' %in% colnames(co)){
      # Linear model
      txt = sprintf(template_lmm,
                    v, co[v,'Estimate'],
                    ci[v,1], ci[v, 2],
                    co[v,'df'], co[v,'t value'], co[v,'Pr(>|t|)'])
    } else {
      txt = sprintf(template_glmm,
                    v, co[v,'Estimate'],
                    ci[v,1], ci[v, 2],
                    co[v,'z value'], co[v,'Pr(>|z|)'])
    }
    if(v %in% rownames(a)){
      txt = paste0(txt, sprintf(template_aov, a[v, 'Df'], a[v,'Chisq'], a[v,'Pr(>Chisq)']))
    }
    if(v %in% names(subj.sd)){
      txt = paste0(txt, ' SD across subjects = ', round(subj.sd[v], digits))
    }
    if(result==''){
      result = txt
    } else{
      result = paste0(result, '\n', txt)
    }
  }
  cat(result)
}

verbalise_t_test = function(test) {
  template = 'DV: %s, t(%.2f) = %.3f, p = %.3f'
  dv = test$data.name
  t = test$statistic
  df = test$parameter
  p = test$p.value
  return(sprintf(template, dv, df, t, p))
}

verbalise_lmm_aov = function(a) {
  warning('This function needs to be tested')
  # 	label = attr(a, "heading")
  # 	label[3:length(label)]
  D.aic = diff(a$AIC) %>% round(3)
  DF = a$'Chi Df'[2]
  Chi = a$Chisq[2] %>% round(3)
  p = a$'Pr(>Chisq)'[2] %>% round(3)
  result= paste(#label,
    '$\\\\delta$ AIC = ',
    D.aic,
    ', $\\\\chi^2$ (',
    DF,
    ') = ',
    Chi,
    ', p = ',
    p,
    sep='')
  return(result)
}


lme4_anova_bf = function(m0, m1) {
  b0 = BIC(m0)
  b1 = BIC(m1)
  bf = exp( (b0 - b1)/2 )
  return (bf)
}

pretty.ranef = function(s){
  # Takes model summary as input
  vc = s$varcor
  for(n in names(vc)){
    paste('\nRandom effects by', n, '\n') %>% cat
    v = vc[[n]]
    cat('\nStandard deviations:\n\n')
    st.devs = attr(v, 'stddev')
    md.table(st.devs) %>% cat
    cat('\nCorrelations:\n\n')
    corrs = attr(v, 'correlation')
    corrs %>% md.table %>% cat
  }
}

coef.table = function(m, rnames=NULL, caption=NULL){
  co = summary(m) %>% coef
  co[,1:ncol(co)-1] %<>% round(2) # Round everything but the p values
  co[,3] %<>% round(1)
  co[,ncol(co)] %<>% sapply(pretty.p) # Make the p values pretty
  colnames(co)[ncol(co)] = 'p value'
  if( is.character(rnames)) {
    rownames(co) = rnames
  }
  md.table(co, caption=caption)
}

anova.table = function(m1, m2){
  a = anova(m1, m2) %>% data.frame
  a[2, ncol(a)] %<>% pretty.p
  a$Chisq %<>% round(1) %>% as.character
  colnames(a)[ncol(a)] = 'p value'
  md.table(a)
}

protect = function(x) {
  p = capture.output(x)
  cat(paste('\t## ', p, '\n', sep=''))
}

md.table = function(x, caption=''){
  pander::pandoc.table(x, caption=caption, split.table=Inf, style='rmarkdown')
}


short.t.test = function(t) {
  paste('t(', t$parameter,
        ') = ', round(t$statistic, 3),
        ', p = ', round(t$p.value, 4),
        sep='')}

apa.anova = function(a, terms=NULL){
  tidied = tidy(a)
  error.df = tidied[nrow(tidied),]$df
  if (is.numeric(terms)){
    terms = tidied$term[terms]
  }
  if (is.character(terms)) {
    my.terms = filter(tidied, term %in% terms)
  } else {
    my.terms = tidied[1:(nrow(tidied)-1),]
  }
  o = ''
  for (i in 1:nrow(my.terms)) {
    term = my.terms[i,]
    o %<>% paste(term$term, ': F(', term$df, ', ', error.df,
                 ') = ', round(term$statistic, 3), ', p = ', round(term$p.value, 4),
                 '\n\n', sep='')
  }
  return (o)
}

tidy.cor = function(x,y) {
  cor.test(x,y) %>% tidy %>%
    select(r=estimate, p=p.value) %>%
    mutate(r=round(r, 3), p=round(p, 4))
}

