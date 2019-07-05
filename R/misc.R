#' Within reason, this function finds the denominator d
#' to represent a decimal value f as a fraction 1/d.
#'
#' @param x A decimal float
#'
#' @return d The corresponding denominator
find.denominator = function(x, decimals=2){
  for (d in 1:(10^decimals)){
    numerator = x*d
    delta = abs(numerator - round(numerator))
    precision = 1 / 10^decimals
    #     print(paste(numerator, delta))
    if(delta < precision){return(d)}}}

#' Turns a fraction
smart.odds = function(x, decimals=2){
  denominators = sapply(x, find.denominator, decimals=decimals)
  numerators = x *denominators
  paste(round(numerators, 0), denominators, sep='/')
}

read.txt = function(fn) readChar(fn, file.info(fn)$size)
write.txt = function(x, fn){
  o  = file(fn)
  writeLines(x, o)
  close(o)
}


# https://github.com/dmbates/RePsychLing/blob/master/R/rePCA.R
rePCA <- function(x) UseMethod('rePCA')
rePCA.merMod <- function(x) {
  chfs <- getME(x,"Tlist")  # list of lower Cholesky factors
  nms <- names(chfs)
  unms <- unique(nms)
  names(unms) <- unms
  svals <- function(m) {
    vv <- svd(m,nv=0L)
    names(vv) <- c("sdev","rotation")
    vv$center <- FALSE
    vv$scale <- FALSE
    class(vv) <- "prcomp"
    vv
  }
  structure(lapply(unms,function(m) svals(bdiag(chfs[which(nms == m)]))),
            class="prcomplist")
}

summary.prcomplist <- function(object,...) {
  lapply(object,summary)
}


aggregate.t.test = function(data, iv, dv, groupby, paired=T) {
  data %>%
    group_by_(groupby, iv) %>%
    summarise_(y=paste("mean(", dv, ")")) %>%
    t.test(as.formula(paste('y~', iv)), data=., paired=T)
}

exp.beta = function(m){
  co = summary(m)$coefficients %>% tidy
  .exponent = exp(co$Estimate)
  .reciprocal = 1 / Exponent
  out = cbind(co[1:2], .exponent, .reciprocal, co[3:ncol(co)])
  return(out)
}


marginal.summary = function(d, f, dv, ...) {
  warning('This is an old function, and has not been tested. Use at your own risk.')
  group_cols = unname(dplyr::select_vars(colnames(sub.acc), ...))
  dots <- lapply(group_cols, as.symbol)
  new.dv = substitute(dv) %>% as.character
  cell.means = d %>%
    group_by_(.dots=dots) %>%
    summarise_each_(funs(f), new.dv)
  names(cell.means)[3] = 'val'
  cell.means
  grid.means = spread_(cell.means, group_cols[[2]], 'val')
  names(grid.means)[2:3] %<>% paste0(group_cols[[2]], ': ', .)
  bottom.means = d %>%
    group_by_(.dots=dots[[2]]) %>%
    summarise_each_(funs(f), new.dv)
  names(bottom.means)[2] = 'val'
  grid.means[3,1:3] = c('Mean', bottom.means$val)
  right.means = d %>%
    group_by_(.dots=dots[[1]]) %>%
    summarise_each_(funs(f), new.dv)
  names(right.means)[2] = 'val'
  grid.means$Mean = c(right.means$val, NA)
  grid.means
}

se_of_combined_terms = function(m, terms) {
  warning('This is an old function, and has not been tested. Use at your own risk.')
  v = vcov(m)
  se = sqrt(diag(v))
  result = 0
  for (a in 1:(length(terms))){
    i = terms[a]
    result = result + se[i]
    if (a < length(terms)) {
      for (b in (a+1):(length(terms))) {
        j = terms[b]
        result = result + 2*abs(v[i,j])
      }
    }
  }
  return (result)
}

flat.cor = function(d) {
  warning('This is an old function, and has not been tested. Use at your own risk.')
  c.mat = cor(d)
  nms = colnames(c.mat)
  out.names = c()
  out.values = c()
  for (i in 1:(ncol(c.mat)-1)) {
    for (j in (i+1):ncol(c.mat)) {
      out.names = c(out.names,
                    paste0(nms[i], '.', nms[j]))
      out.values = c(out.values, c.mat[i,j])
    }
  }
  data.frame(comp=out.names, estimate=out.values)
}


flat.cor.p = function(d) {
  d %<>% data.frame
  out = rbind()
  nms = names(d)
  for (i in 1:(ncol(d)-1)) {
    for (j in (i+1):ncol(d)) {
      R =
        out = rbind(
          out,
          cbind(comp=paste0(nms[i], '.', nms[j]),
                glance(cor.test(d[,i], d[,j])))
        )
    }
  }
  data.frame(out)
}

value.counts = function(x){
    tbl = data.frame(table(x))
    tbl = rbind(tbl,
                data.frame(x='NA', Freq=sum(is.na(x))))
    names(tbl) = c('value', 'count')
    return(tbl)
}



#' Fits the psychometric function for response y against variable x,
#' for each cell of the data.
#' 
#' TODO:
#' - MOVE ME TO DIFFERENT FILE
#' - Accept formula input
#' - Generalise to other classes of model.
#' 
#'
#' @param df: The data
#' @param x: The independant variable.
#' @param y: The binary (for now) resposne variable
#' @param ...: Arguments passed to tidyr::nest to divide the data into cells/
#'
#' @return res: Nested dataframe with columns containing the models (m), coefficients (co), and predictions (pred).
#' 
#' Example:
#' resp.models = fit.models.per.cell(data, stimulus, response, -subject.nr, -condition)
#' resp.model.coef = resp.models %>% unnest(coef)
#' resp.models.pred = resp.models %>% unnest(pred)
#' resp.models.m = resp.models %>% unnest(m)
fit.psych.fun.per.cell = function(df, x, y, ...) {
  nest.vars <- enquos(...)
  x = rlang::enquo(x)
  y = rlang::enquo(y)
  xvals = df[rlang::quo_text(x)]
  x.range = seq(min(xvals), max(xvals), length.out = 50)
  x <- rlang::quo_expr(x, warn = TRUE)
  y <- rlang::quo_expr(y, warn = TRUE)
  my.formula = rlang::new_formula(y, x)
  model.func = function(d){ glm(my.formula, data=d, family=binomial('probit')) }
  res = df %>% 
    nest(...) %>%
    mutate(
      m = map(data, model.func),
      co = map(m, broom::tidy),
      pred = map(m, function(model){
        df.new = data.frame(i=1:50)
        df.new[rlang::quo_text(x)] = x.range
        p = predict(model, newdata=df.new, type='link', se.fit=T)
        df.new$pred = pnorm(p$fit)
        df.new$lo = pnorm(p$fit - p$se.fit)
        df.new$hi = pnorm(p$fit + p$se.fit)
        df.new
      })
    )
  return(res)
}

#' Example:
#' f = function(df) lm(confidence ~ rt, data=df)
#' resp.models = fit.models.per.cell(data, f, -subject.nr)
#' resp.models %>% unnest(co)
fit.models.per.cell = function(df, model.func, ...) {
  res = df %>% 
    nest(...) %>%
    mutate(
      m = map(data, model.func),
      model.formula = map(m, ~{. %>% formula() %>% deparse() }),
      co = map(m, broom::tidy),
      aov = map(m, function(mod){
        car::Anova(mod) %>% broom::tidy()
      }))
  return(res)
}
