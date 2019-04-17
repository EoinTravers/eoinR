# # # library(multcomp)
# # library(mixtools)



# find.denominator = function(x, decimals=2){
#   for (i in 1:(10^decimals)){
#     numerator = x*i
#     delta = abs(numerator - round(numerator))
#     precision = 1 / 10^decimals
# #     print(paste(numerator, delta))
#     if(delta < precision){return(i)}}}

# smart.odds = function(x, decimals=2){
#   denominators = sapply(x, find.denominator, decimals=decimals)
#   numerators = x *denominators
#   paste(round(numerators, 0), denominators, sep='/')
# }

# my.ci = function(model, which){
#   ci = stats::confint(model, parm=which)
#   if(typeof(model)=='S4'){
#     b = fixef(model)[which]
#   } else { b = coef(model)}
#   cbind(beta=b, ci)
# }

# my.approx.ci = function(model, which){
# 	co = summary(model)$coefficients %>%
# 		data.frame %>%
# 		tibble::rownames_to_column('term') %>%
# 		filter(term %in% which)
# 	b = co$Estimate
# 	se = co$Std..Error
# 	lower = b - 1.96*se
# 	upper = b + 1.96*se
# 	cbind(b, lower, upper)
# }



# relative.risk = function(ci){
#   # Only for intercept + single term models
#   control.p = logit(ci[1,1])
#   out = c(
#   	odds.to.risk(exp(ci[2,1]), control.p),
#   	odds.to.risk(exp(ci[2,2]), control.p),
#   	odds.to.risk(exp(ci[2,3]), control.p))
#   names(out) = c('estimate', 'lower', 'upper')
#   out
# }



# # Verbalizr

# ## describe.lmer.aov = function(a) {
# ## # 	label = attr(a, "heading")
# ## # 	label[3:length(label)]
# ## 	D.aic = diff(a$AIC) %>% round(3)
# ## 	DF = a$'Chi Df'[2]
# ## 	Chi = a$Chisq[2] %>% round(3)
# ## 	p = a$'Pr(>Chisq)'[2] %>% round(3)
# ## 	paste(#label,
# ## 		  '$\\\\delta$ AIC = ',
# ## 		  D.aic,
# ## 		  ', $\\\\chi^2$ (',
# ## 		  DF,
# ## 		  ') = ',
# ## 		  Chi,
# ## 		  ', p = ',
# ## 		  p,
# ## 		  sep='')
# ## }

# describe.t.test = function(test) {
# 	dv = test$data.name
# 	t = test$statistic %>% round(3)
# 	df = test$parameter
# 	p = test$p.value %>% round(3)
# 	return(paste('\n\n    Dependent variable: ', dv,
# 				 '\n    t(', df, ') = ', t,
# 				 ', p = ', p, '.', sep=''))
# }

# pretty.lme.table = function (model, row.names=NULL, pandoc=TRUE) {
# 	model.summary = summary(model)
# 	model.coef = coef(model.summary)
# 	model.ci = confint(model, method="Wald")
# 	model.coef = cbind(model.coef, model.ci)
# 	if (!is.null(row.names)){ row.names(model.coef) = row.names}
# 	out = round(model.coef, 3)
# 	if (pandoc){ out = pandoc.table(out, split.tables=Inf)}
# 	return(out)
# }

# pretty.t = function(t){
# 	p = 2*pnorm(-abs(t))
# 	t = round(t, 3)
#   	if(p < .001){paste(t, '***') %>% return}
#   	else if(p < .01){paste(t, '**') %>% return}
#   	else if(p < .05){paste(t, '*') %>% return}
#   	else if(p < .1){paste(t, '.') %>% return}
#   	else return(round(t, 3) %>% as.character )
# }

# pretty.p = function (p){
#   	options(scipen=999)
# 	sapply(p, function(p){
# 	  	p = round(p, 3)
# 	  	if(p < .001) { return (paste("<.001", "***"))}
# 	  	if(p < .01)  { return (paste(substr(p, 2, 5), "**"))}
# 	  	if(p < .05)  { return (paste(substr(p, 2, 5), "*"))}
# 	  	if(p < .1)   { return (paste(substr(p, 2, 5), "."))}
# 	  	else{return (as.character(p))}
# 	})
# }

# pretty.stars = function(p){
# 	sapply(p, function(p){
# 		p = round(p, 3)
# 		if(p < .001) { return ("***")}
# 		if(p < .01)  { return ("**")}
# 		if(p < .05)  { return ("*")}
# 		if(p < .1)   { return (".")}
# 		else{return ('')}
# 	})
# }

# pretty.coef = function (coef, p) {
#   coef = round(coef, 3)
#   if(p < .001){paste(coef, '***') %>% return}
#   else if(p < .01){paste(coef, '**') %>% return}
#   else if(p < .05){paste(coef, '*') %>% return}
#   else if(p < .1){paste(coef, '.') %>% return}
#   else return(round(coef, 3) %>% as.character )
# }
# v.pretty.coef = Vectorize(pretty.coef)

# function(ph, e=T, r=T){
#   print(ph %>% summary)
#   ci = confint(ph)
#   if (e) {
#     print ("Exponentials")
#     ci %>% soft.apply(exp) %>% print
#     if (r) {
#       print("Reciprocals")
#       ci %>% soft.apply(exp) %>% soft.apply(reciprocal) %>% print
#     }
#   }
# }

# stepwise.lmer = function(dv, ivs, ranfx, data) {
# 	# String, list of strings, string, data.frame
# 	f0 = paste(dv, '~', ranfx)
# 	m0 = lmer(f0, data=data, REML=F)
# 	# Get all higher order terms
# # 	main.fx = ivs
# # 	for (n in 2:length(ivs)){
# # 		interactions = combn(main.fx, n)
# # 		for (i in 1:ncol(interactions)){
# # 			ivs = c(ivs,
# # 					  paste(interactions[,i], collapse=':'))
# # 		}
# # 	}
# 	chi.sq = c()
# 	Df = c()
# 	p.vals = c()
# 	for (i in 1:length(ivs)){
# 		f1 = paste(
# 			dv, '~',
# 			paste(ivs[1:i], collapse=' + '),
# 			'+', ranfx)
# 		m1 = lmer(formula=f1, data=data, REML=F)
# 		a = anova(m0, m1)
# 		chi = a$Chisq[2] %>% round(2)
# 		chi.sq = c(chi.sq, chi)
# 		d = a[2,7]
# 		Df = c(Df, d)
# 		p = round(a[2,8], 3)
# 		p.vals = c(p.vals, p)
# 		# For next loop
# 		m0 <- m1
# 	}
# 	out = data.frame(X=chi.sq, DF=Df, p=p.vals)
# 	rownames(out) = ivs
# 	return (out)
# }

# stepwise.glmer = function(dv, ivs, ranfx, data) {
# 	# String, list of strings, string, data.frame
# 	f0 = paste(dv, '~', ranfx)
# 	m0 = glmer(f0, data=data, family=binomial)
# 	# Get all higher order terms
# 	# 	main.fx = ivs
# 	# 	for (n in 2:length(ivs)){
# 	# 		interactions = combn(main.fx, n)
# 	# 		for (i in 1:ncol(interactions)){
# 	# 			ivs = c(ivs,
# 	# 					  paste(interactions[,i], collapse=':'))
# 	# 		}
# 	# 	}
# 	chi.sq = c()
# 	Df = c()
# 	p.vals = c()
# 	for (i in 1:length(ivs)){
# 		f1 = paste(
# 			dv, '~',
# 			paste(ivs[1:i], collapse=' + '),
# 			'+', ranfx)
# 		m1 = glmer(f1, data=data, REML=F, family=binomial)
# 		a = anova(m0, m1)
# 		chi = a$Chisq[2] %>% round(2)
# 		chi.sq = c(chi.sq, chi)
# 		d = a[2,7]
# 		Df = c(Df, d)
# 		p = round(a[2,8], 3)
# 		p.vals = c(p.vals, p)
# 		# For next loop
# 		m0 <- m1
# 	}
# 	out = data.frame(X=chi.sq, DF=Df, p=p.vals)
# 	rownames(out) = ivs
# 	return (out)
# }


# #http://stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4
# # max.grad = m1@optinfo$derivs %>%
# # 	with(solve(Hessian,gradient)) %>% abs %>% max

# lmer.many.dv = function (dvs, f0, f1, data){
# 	chi.sq = c()
# 	p.vals = c()
# 	max.grads = c()
# 	for (dv in dvs){
# 		if(length(unique(data[[dv]]))==2){
# 			m0 = glmer(paste(dv, f0, sep=''),
# 					  data=data, family=binomial)
# 			m1 = glmer(paste(dv, f1, sep=''),
# 					  data=data, family=binomial)
# 		}
# 		else {
# 			m0 = lmer(paste(dv, f0, sep=''),
# 					  data=data,
# 					  REML=F)
# 			m1 = lmer(paste(dv, f1, sep=''),
# 					  data=data,
# 					  REML=F)
# 		}
# 		a = anova(m0, m1)
# 		chi = a$Chisq[2] %>% round(2)
# 		chi.sq = c(chi.sq, chi)
# 		p = round(a[2,8], 4)
# 		p.vals = c(p.vals, p)
# 		max.grad = m1@optinfo$derivs %>%
# 			with(solve(Hessian,gradient)) %>% abs %>% max
# 		max.grads = c(max.grads, max.grad)

# 	}
# 	out = data.frame(X=chi.sq, p=p.vals, max.grad=max.grads)
# 	rownames(out) = dvs
# 	return (out)
# }


# aic.weights = function(a){
# 	d = a$AIC - max(a$AIC)
# 	lik = 1/exp(-.5*d)
# 	p.best = lik / sum(lik)
# 	data.frame(model=seq(1,length(d)), aic=a$AIC, aic.lik=lik, p.best=p.best)
# }

# mean.sd = function(x, digits=2){
#   # Returns string with "Mean (Standard deviation)" of intput.
#   # Nice for APA tables!
#   x.mean = mean(x, na.rm=T) %>% round(digits)
# 	x.sd = sd(x, na.rm=T) %>% round(digits)
# 	paste(x.mean, ' (', x.sd, ')', sep='') %>% return()
# }

# # Some colousr
# green = "#4daf4a"
# orange = "#FFAE00"
# red = "#e41a1c"
# blue = "#377eb8"



# round_df <- function(x, digits) {
#   numeric_columns <- sapply(x, class) == 'numeric'
#   x[numeric_columns] <-  round(x[numeric_columns], digits)
#   x
# }
# aggregate.t.test = function(data, iv, dv, groupby, paired=T) {
#   data %>%
#     group_by_(groupby, iv) %>%
#     summarise_(y=paste("mean(", dv, ")")) %>%
#     t.test(as.formula(paste('y~', iv)), data=., paired=T)
# }
# library(pander)
# md.table = function(x, caption=''){
# 	pandoc.table(x, caption=caption, split.table=Inf, style='rmarkdown')
# }

# # You can pass lsmeans to confint
# my.posthoc = function(m, iv, exponent=F, recip=F){
#   ph = summary(
#     lsmeans(m, as.formula(paste('pairwise ~', iv)))
#   )$contrasts
#   print(ph)
#   cat('\n\n')
#   ph$lower = ph$estimate - (1.96*ph$SE)
#   ph$upper = ph$estimate + (1.96*ph$SE)
#   cis = ph %>% dplyr::select(estimate, lower, upper)
#   if(exponent) {
#     cis = exp(cis)
#     cat('Exponentiated values:\n')
#   }
#   if(recip){
#     cis = reciprocal(cis)
#     cat('(Reciprocals)\n')
#   }
#   print(cbind(contrast=ph$contrast, cis))
# }

# z = function(x){scale(x)}
# center  = function(x){scale(x, scale=F)}


# # https://github.com/dmbates/RePsychLing/blob/master/R/rePCA.R
# rePCA <- function(x) UseMethod('rePCA')
# rePCA.merMod <- function(x) {
# 	chfs <- getME(x,"Tlist")  # list of lower Cholesky factors
# 	nms <- names(chfs)
# 	unms <- unique(nms)
# 	names(unms) <- unms
# 	svals <- function(m) {
# 		vv <- svd(m,nv=0L)
# 		names(vv) <- c("sdev","rotation")
# 		vv$center <- FALSE
# 		vv$scale <- FALSE
# 		class(vv) <- "prcomp"
# 		vv
# 	}
# 	structure(lapply(unms,function(m) svals(bdiag(chfs[which(nms == m)]))),
# 			  class="prcomplist")
# }

# summary.prcomplist <- function(object,...) {
# 	lapply(object,summary)
# }

# read.txt = function(fn) readChar(fn, file.info(fn)$size)
# write.txt = function(x, fn){
#   o  = file(fn)
#   writeLines(x, o)
#   close(o)
# }

# as.caps = function(x) sapply(x, Hmisc::capitalize)

# probability.diagram = function(probs, save.as, template.file='template.svg'){
#   prob.string = function(x, digits=2){
#     format(round(x, digits), nsmall=digits) %>% as.character %>% substring(2)}
#   template.keys = c('.A1',      '.a1',        '.B1',      '.b1',        '.C1',      '.c1',
#                     '.A2',      '.a2',        '.B2',      '.b2',        '.C2',      '.c2')
#   template.vals = c(probs[1,1], 1-probs[1,1], probs[1,2], 1-probs[1,2], probs[1,3], 1-probs[1,3],
#                     probs[2,1], 1-probs[2,1], probs[2,2], 1-probs[2,2], probs[2,3], 1-probs[2,3])
#   template.vals = sapply(template.vals, prob.string)

#   plot.template = read.txt('template.svg')
#   for(i in 1:length(template.keys)){
#     old.str = template.keys[i]
#     new.str = template.vals[i]
#     plot.template = sub(old.str, new.str, plot.template)
#   }
#   write.txt(plot.template, save.as)
# }

# pretty.ranef = function(s){
# 	# Takes model summary as input
# 	vc = s$varcor
# 	for(n in names(vc)){
# 		paste('\nRandom effects by', n, '\n') %>% cat
# 		v = vc[[n]]
# 		cat('\nStandard deviations:\n\n')
# 		st.devs = attr(v, 'stddev')
# 		md.table(st.devs) %>% cat
# 		cat('\nCorrelations:\n\n')
# 		corrs = attr(v, 'correlation')
# 		corrs %>% md.table %>% cat
# 	}
# }

# coef.table = function(m, rnames=NULL, caption=NULL){
# 	co = summary(m) %>% coef
# 	co[,1:ncol(co)-1] %<>% round(2) # Round everything but the p values
# 	co[,3] %<>% round(1)
# 	co[,ncol(co)] %<>% sapply(pretty.p) # Make the p values pretty
# 	colnames(co)[ncol(co)] = 'p value'
# 	if( is.character(rnames)) {
# 		rownames(co) = rnames
# 	}
# 	md.table(co, caption=caption)
# }
# anova.table = function(m1, m2){
# 	a = anova(m1, m2) %>% data.frame
# 	a[2, ncol(a)] %<>% pretty.p
# 	a$Chisq %<>% round(1) %>% as.character
# 	colnames(a)[ncol(a)] = 'p value'
# 	md.table(a)
# }

# protect = function(x) {
# 	p = capture.output(x)
# 	cat(paste('\t## ', p, '\n', sep=''))
# }



# #### From GitHub/Cognition/

# # # Some colours
# green = "#4daf4a"
# green2 = "#306E2E"
# orange = "#FFAE00"
# red = "#e41a1c"
# red2 = '#ff5600'
# blue = "#377eb8"


# # library(pander)
# # md.table = function(x) {
# #   pandoc.table(x, split.table=Inf, style='rmarkdown')
# # }

# # mean.sd = function(x, digits=2){
# # 	# Returns string with "Mean (Standard deviation)" of intput.
# # 	# Nice for APA tables!
# # 	x.mean = mean(x, na.rm=T) %>% round(digits)
# # 	x.sd = sd(x, na.rm=T) %>% round(digits)
# # 	paste(x.mean, ' (', x.sd, ')', sep='') %>% return()
# # }


# # this.ci = function(model, logistic=T){
# # 	param.names = names(fixef(model))
# # 	param = param.names[2:length(param.names)] # Don't bother with intercept
# # 	ci = stats::confint(model, parm=param)
# # 	b = fixef(model)[param]
# # 	beta = cbind(beta=b, ci)
# # 	if(logistic){
# # 		out = rbind(beta, exp(beta), 1/exp(beta))
# # 		headings = c('Estimate', 'Exponent', '1/Exponent')
# # 		rownames(out) = do.call(paste, expand.grid(param, ':', headings))
# # 	} else {
# # 		out = rbind(beta, exp(beta), 1/exp(beta))
# # 	}
# # 	return(out)
# # }

# # z = function(x){scale(x)}
# # center  = function(x){scale(x, scale=F)}
# # median.split = function(x) x > median(x)

# # pretty.p = function (p, digits=4){
# # 	p = round(p, digits)
# # 	if(p < .0001) { return (paste("<.0001", "***"))}
# # 	if(p < .001) { return (paste("<.001", "***"))}
# # 	if(p < .01)  { return (paste(substr(p, 2, 5), "**"))}
# # 	if(p < .05)  { return (paste(substr(p, 2, 5), "*"))}
# # 	if(p < .1)   { return (paste(substr(p, 2, 5), "."))}
# # 	else{return (as.character(p))}
# # }

# # coef.table = function(m){
# # 	co = summary(m) %>% coef
# # 	co[,1:ncol(co)-1] %<>% round(3) # Round everything but the p values
# # 	co[,ncol(co)] %<>% sapply(pretty.p) # Make the p values pretty
# # 	colnames(co)[ncol(co)] = 'p value'
# # 	md.table(co)
# # }
# # anova.table = function(m1, m2){
# # 	a = anova(m1, m2) %>% data.frame
# # 	a[2, ncol(a)] %<>% pretty.p
# # 	a$Chisq %<>% round(1) %>% as.character
# # 	colnames(a)[ncol(a)] = 'p value'
# # 	md.table(a)
# # }

# # protect = function(x) {
# # 	p = capture.output(x)
# # 	cat(paste('\t## ', p, '\n', sep=''))
# # }

# # # nlopt optimizer for faster convergence. See https://cran.r-project.org/web/packages/lme4/vignettes/lmerperf.html
# # nlopt <- function(par, fn, lower, upper, control) {
# # 	.nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper,
# # 							  opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
# # 							  			maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
# # 	list(par = res$solution,
# # 		 fval = res$objective,
# # 		 conv = if (res$status > 0) 0 else res$status,
# # 		 message = res$message
# # 	)
# # }


# lme4.anova.summary = function(a, md=T, row.names=NULL){
# 	d = data.frame(a)
# 	if( is.character(row.names)){
# 		rownames(d) = row.names
# 	}
# 	names(d) = c('SS', 'MS', 'DF_1', 'DF_2', 'F', 'p')
# 	if (md) {
# 		d = md.table(d)
# 	}
# 	return (d)
# }

# short.t.test = function(t) {
# 	paste('t(', t$parameter,
# 		  ') = ', round(t$statistic, 3),
# 		  ', p = ', round(t$p.value, 4),
# 		  sep='')}

# apa.anova = function(a, terms=NULL){
# 	tidied = tidy(a)
# 	error.df = tidied[nrow(tidied),]$df
# 	if (is.numeric(terms)){
# 		terms = tidied$term[terms]
# 	}
# 	if (is.character(terms)) {
# 		my.terms = filter(tidied, term %in% terms)
# 	} else {
# 		my.terms = tidied[1:(nrow(tidied)-1),]
# 	}
# 	o = ''
# 	for (i in 1:nrow(my.terms)) {
# 		term = my.terms[i,]
# 		o %<>% paste(term$term, ': F(', term$df, ', ', error.df,
# 					 ') = ', round(term$statistic, 3), ', p = ', round(term$p.value, 4),
# 					 '\n\n', sep='')
# 		}
# 	return (o)
# }

# exp.beta = function(m){
#   co = summary(m)$coefficients %>% tidy
#   Exponent = exp(co$Estimate)
#   Reciprocal = 1 / Exponent
#   out = cbind(co[1:2], Exponent, Reciprocal, co[3:ncol(co)])
#   return(out)
# }

# soft.apply = function(df, fun) {
#   nums = sapply(df, is.numeric)
#   df[,nums] %>% fun
# }

# pretty.ph = function(ph, e=T, r=T){
#   print(ph %>% summary)
#   ci = confint(ph)
#   if (e) {
#     print ("Exponentials")
#     ci %>% soft.apply(exp) %>% print
#     if (r) {
#       print("Reciprocals")
#       ci %>% soft.apply(exp) %>% soft.apply(reciprocal) %>% print
#     }
#   }
# }

# marginal.summary = function(d, f, dv, ...) {
#   group_cols = unname(dplyr::select_vars(colnames(sub.acc), ...))
#   dots <- lapply(group_cols, as.symbol)
#   new.dv = substitute(dv) %>% as.character
#   cell.means = d %>%
#     group_by_(.dots=dots) %>%
#     summarise_each_(funs(f), new.dv)
#   names(cell.means)[3] = 'val'
#   cell.means
#   grid.means = spread_(cell.means, group_cols[[2]], 'val')
#   names(grid.means)[2:3] %<>% paste0(group_cols[[2]], ': ', .)
#   bottom.means = d %>%
#     group_by_(.dots=dots[[2]]) %>%
#     summarise_each_(funs(f), new.dv)
#   names(bottom.means)[2] = 'val'
#   grid.means[3,1:3] = c('Mean', bottom.means$val)
#   right.means = d %>%
#     group_by_(.dots=dots[[1]]) %>%
#     summarise_each_(funs(f), new.dv)
#   names(right.means)[2] = 'val'
#   grid.means$Mean = c(right.means$val, NA)
#   grid.means
# }




# transition_plot = function(data, stage1, stage2) {
#   # stage1 and stage2 as strings, for now.
#   # Should be either boolean or binary.

#   # Geometric locations
#   x0 = 0
#   y0 = 0
#   x1 = 1
#   y1 = 1.5
#   x2 = 2
#   y2 = 3
#   my.arrow = grid::arrow(length = unit(0.05, "npc"), type='closed')
#   my.arrow2 = grid::arrow(length = unit(0.05, "npc"), type='closed', ends='first')
#   xgap = .1
#   ygap = .15

#   # Proportions

#   data = data %>%
#     dplyr::mutate_each_(funs(as.numeric), c(stage1, stage2))
#   p1 = mean(data[[stage1]])
#   p2 = mean( data[data[[stage1]]==1,][[stage2]] )
#   p3 = mean( data[data[[stage1]]==0,][[stage2]] )
#   p1b = (1-p1)
#   p2b = (1-p2)
#   p3b = (1-p3)

#   Ps = c(p1, p1b, p2, p2b, p3, p3b)
#   make.label = function(p){
#     paste0(round(p, 2) * 100, '%')
#   }
#   Labs = purrr::map_chr(Ps, make.label)

#   g = ggplot(data) +
#     # Draw the lines and curves
#     # Start to initially True
#     geom_segment(x=x0, y=y0,
#                  xend=-x1+xgap, yend=y1-ygap,
#                  color='black', arrow=my.arrow) +
#     # Initially True to True
#     geom_segment(x=-x1-xgap, y=y1+ygap,
#                  xend=-x2+xgap, yend=y2-ygap,
#                  color='black', arrow=my.arrow) +
#     # Initially True to False
#     geom_curve(xend=-x1-xgap, yend=y1+ygap,
#                x=x2-2*xgap, y=y2+ygap,
#                color='black',
#                curvature=.5, angle=120, arrow=my.arrow2) +
#     # Start to initially False
#     geom_segment(x=x0, y=y0,
#                  xend=x1-xgap, yend=y1-ygap,
#                  color='black', arrow=my.arrow) +
#     # Initially False to False
#     geom_segment(x=x1+xgap, y=y1+ygap,
#                  xend=x2-xgap, yend=y2-ygap,
#                  color='black', arrow=my.arrow) +
#     # Initially False to True
#     geom_curve(x=x1+xgap, y=y1+ygap,
#                xend=-x2+2*xgap, yend=y2+ygap,
#                color='black',
#                curvature=.5, angle=50, arrow=my.arrow) +
#     # Labels
#     # Start to initially True
#     geom_text(x = -(x0+x1)/2, y= (y0+y1)/2,
#               label=Labs[1], hjust=1.5, vjust=1) +
#     # Start to initially False
#     geom_text(x = (x0+x1)/2, y=(y0+y1)/2,
#               label=Labs[2], hjust=-.5, vjust=1) +
#     # Initially True to True
#     geom_text(x = -(x1+x2)/2, y=(y1+y2)/2, label=Labs[3], hjust=1.5, vjust=1) +
#     # Initially False to False
#     geom_text(x = -(x2*1.5-x1)/2, y=(y1+1.3*y2)/2,
#               label=Labs[4], hjust=.5, vjust=1) +
#     #  Initially False to True
#     geom_text(x = (x2*1.5-x1)/2, y=(y1+1.3*y2)/2,
#               label=Labs[5], hjust=.5, vjust=1) +
#     #  Initially False to False
#     geom_text(x = (x1+x2)/2, y=(y1+y2)/2, label=Labs[6], hjust=-.5, vjust=1) +
#     # General aesthetics
#     coord_fixed() + theme_minimal() +
#     scale_x_continuous(breaks=NULL, limits = c(-3, 3)) +
#     scale_y_continuous(breaks=NULL, limits = c(-1, 5)) +
#     # 'T' and 'F' endpoints (optional)
#     geom_text(x=-x2, y=y2, label='T', size=5, hjust=.5, vjust=0) +
#     geom_text(x=x2, y=y2, label='F', size=5, hjust=.5, vjust=0)
#   print(Labs)
#   return(g)
# }


# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }


# aic.weights = function(a){
# 	d = a$AIC - max(a$AIC)
# 	lik = 1/exp(-.5*d)
# 	p.best = lik / sum(lik)
# 	m.names = model.names.from.anova.object(a)
# 	data.frame(model=m.names, aic=a$AIC, aic.lik=lik, p.best=p.best)
# }

# model.names.from.anova.object = function(a){
# 	# Hack out model names from ANOVA object
# 	h = attr(a, 'heading')
# 	h = h[3:length(h)] # First two values are padding
# 	is.start.line = map_lgl(h, ~stringr::str_detect(., '~'))
# 	h = h[is.start.line]
# 	get.group.name = function(s){
# 		stringr::str_match(s, '.+?:') %>%
# 			stringr::str_replace(':', '')
# 	}
# 	map_chr(h, get.group.name)
# }

# ## compare_models = function(...) {
# ##   # rel.weight = function(x) x / max(x)
# ##   mCall <- match.call(expand.dots = TRUE)
# ##   dots <- list(...)
# ##   .sapply <- function(L, FUN, ...) unlist(lapply(L, FUN, ...))
# ##   modp <- (as.logical(vapply(dots, is, NA, "merMod")) |
# ##              as.logical(vapply(dots, is, NA, "lm")))
# ##   mCall = as.list(mCall)
# ##   mNms <- vapply(mCall[2:length(mCall)],#[c(FALSE, TRUE, modp)],
# ##                 lme4:::safeDeparse, "")
# ##   a = anova(...)
# ##   o = data.frame(
# ##     model=mNms,
# ##     AIC=a$AIC,
# ##     BIC=a$BIC) %>%
# ##     mutate(
# ##       AIC.delta = AIC - min(AIC),
# ##       BIC.delta = BIC - min(BIC),
# ##       AIC.ratio = exp(.5*AIC.delta),
# ##       BIC.ratio = exp(.5*BIC.delta)
# ##     )
# ##   #   mutate(
# ##   #     AICw = Weights(AIC),
# ##   #     BICw = Weights(BIC),
# ##   #     AICd = rel.weight(AICw),
# ##   #     BICd = rel.weight(BICw))
# ##   o[c(1,2,4,6,3,5, 7)]
# ## }
# ## compare_lme = compare_models # Backwards compatibility

# compare.models = function(...){
#     model.names = match.call() %>% as.list
#     model.names = model.names[2:length(model.names)]
#     model.names = map_chr(model.names, toString)
#     ## print(model.names)
#     a = lme4:::anova.merMod(...)
#     a = tidy(a) %>% arrange(term)
#                                         # print(a)
#     o = data.frame(
#         model=as.array(model.names),
#         AIC=a$AIC,
#         BIC=a$BIC)  %>%
#         mutate(
#             AIC.delta = AIC - min(AIC),
#             BIC.delta = BIC - min(BIC),
#             AIC.ratio = exp(-.5*AIC.delta),
#             BIC.ratio = exp(-.5*BIC.delta),
#             AIC.prob = AIC.ratio / sum(AIC.ratio),
#             BIC.prob = BIC.ratio / sum(BIC.ratio)
#         )
#     return(o)
# }
# reciprocal = function(x) 1/x
# prob.to.odds = function (x) x / (1-x)
# odds.to.prob = function(x) x / (1+x)
# odds.to.risk = function(odds, control.p) {odds / (1 - control.p + (control.p * odds))}
# z = function(x){scale(x)}
# center  = function(x){scale(x, scale=F)}
# as.caps = function(x) sapply(x, Hmisc::capitalize)
# read.txt = function(fn) readChar(fn, file.info(fn)$size)
# write.txt = function(x, fn){
#   o  = file(fn)
#   writeLines(x, o)
#   close(o)
# }

# coef.table = function(m, rnames=NULL, caption=NULL){
#   co = summary(m) %>% coef %>% data.frame %>% rownames_to_column('term')
#   co[,2:(ncol(co)-1)] %<>% round(3) # Round everything but the p values
#   co[,ncol(co)] %<>% sapply(pretty.p) %>%
#     gsub('\\*', '\\\\*', .) %>%
#     gsub('<', '\\\\<', .)
#   colnames(co)[ncol(co)] = 'p value'
#   if( is.character(rnames)) {
#     rownames(co) = rnames
#   }
#   data.frame(co)
#   # md.table(co, caption=caption)
# }

# anova.table = function(m1, m2){
#   a = anova(m1, m2) %>% data.frame
#   a[2, ncol(a)] %<>% pretty.p
#   a$Chisq %<>% round(1) %>% as.character
#   colnames(a)[ncol(a)] = 'p value'
#   a
# }

# anova.bf = function(m0, m1) {
#   b0 = BIC(m0)
#   b1 = BIC(m1)
#   bf = exp( (b0 - b1)/2 )
#   return (bf)
# }

# my.ci = function(model, which){
#   ci = stats::confint(model, parm=which)
#   if(typeof(model)=='S4'){
#     b = fixef(model)[which]
#   } else { b = coef(model)}
#   cbind(beta=b, ci)
# }

# my.approx.ci = function(model){
#   co = summary(model)$coefficients
#   b = co[,1]
#   se = co[,2]
#   lower = b - 1.96*se
#   upper = b + 1.96*se
#   cbind(b, lower, upper)
# }


# describe.t.test = function(test) {
#   dv = test$data.name
#   t = test$statistic %>% round(3)
#   df = test$parameter
#   p = test$p.value %>% round(3)
#   return(paste('\n\n    Dependent variable: ', dv,
#                '\n    t(', df, ') = ', t,
#                ', p = ', p, '.', sep=''))
# }

# describe.t.simple = function(test) {
#   t = test$statistic %>% round(3)
#   df = test$parameter
#   p = test$p.value %>% round(3)
#   return(paste('t(', df, ') = ', t,
#                ', p = ', p, '.', sep=''))
# }

# pretty.lme.table = function (model, row.names=NULL, pandoc=TRUE) {
#   model.summary = summary(model)
#   model.coef = coef(model.summary)
#   model.ci = confint(model, method="Wald")
#   model.coef = cbind(model.coef, model.ci)
#   if (!is.null(row.names)){ row.names(model.coef) = row.names}
#   out = round(model.coef, 3)
#   if (pandoc){ out = pandoc.table(out, split.tables=Inf)}
#   return(out)
# }

# pretty.t = function(t){
#   p = 2*pnorm(-abs(t))
#   t = round(t, 3)
#   if(p < .001){paste(t, '***') %>% return}
#   else if(p < .01){paste(t, '**') %>% return}
#   else if(p < .05){paste(t, '*') %>% return}
#   else if(p < .1){paste(t, '.') %>% return}
#   else return(round(t, 3) %>% as.character )
# }

# pretty.p = function (p){
#   options(scipen=999)
#   sapply(p, function(p){
#     p = round(p, 3)
#     if(p < .001) { return (paste("<.001", "***"))}
#     if(p < .01)  { return (paste(substr(p, 2, 5), "**"))}
#     if(p < .05)  { return (paste(substr(p, 2, 5), "*"))}
#     if(p < .1)   { return (paste(substr(p, 2, 5), "."))}
#     else{return (as.character(p))}
#   })
# }

# pretty.coef = function (coef, p) {
#   coef = round(coef, 3)
#   if(p < .001){paste(coef, '***') %>% return}
#   else if(p < .01){paste(coef, '**') %>% return}
#   else if(p < .05){paste(coef, '*') %>% return}
#   else if(p < .1){paste(coef, '.') %>% return}
#   else return(round(coef, 3) %>% as.character )
# }
# v.pretty.coef = Vectorize(pretty.coef)


# mean.sd = function(x, digits=2){
#   # Returns string with "Mean (Standard deviation)" of intput.
#   # Nice for APA tables!
#   x.mean = mean(x, na.rm=T) %>% round(digits)
#   x.sd = sd(x, na.rm=T) %>% round(digits)
#   paste(x.mean, ' (', x.sd, ')', sep='') %>% return()
# }

# # Some colours
# green = "#4daf4a"
# orange = "#FFAE00"
# red = "#e41a1c"
# blue = "#377eb8"



# round_df <- function(x, digits) {
#   numeric_columns <- sapply(x, class) == 'numeric'
#   x[numeric_columns] <-  round(x[numeric_columns], digits)
#   x
# }

# aggregate.t.test = function(data, iv, dv, groupby, paired=T) {
#   data %>%
#     group_by_(groupby, iv) %>%
#     summarise_(y=paste("mean(", dv, ")")) %>%
#     t.test(as.formula(paste('y~', iv)), data=., paired=T)
# }

# md.table = function(x, caption=''){
#   pander::pandoc.table(x, caption=caption, split.table=Inf, style='rmarkdown')
# }





# pretty.ranef = function(s){
#   # Takes model summary as input
#   vc = s$varcor
#   for(n in names(vc)){
#     paste('\nRandom effects by', n, '\n') %>% cat
#     v = vc[[n]]
#     cat('\nStandard deviations:\n\n')
#     st.devs = attr(v, 'stddev')
#     data.frame(st.devs) %>% cat
#     cat('\nCorrelations:\n\n')
#     corrs = attr(v, 'correlation')
#     corrs %>% cat
#   }
# }


# protect = function(x) {
#   p = capture.output(x)
#   cat(paste('\t## ', p, '\n', sep=''))
# }



# #### From GitHub/Cognition/

# # # Some colours
# green = "#4daf4a"
# green2 = "#306E2E"
# orange = "#FFAE00"
# red = "#e41a1c"
# red2 = '#ff5600'
# blue = "#377eb8"


# short.t.test = function(t) {
#   paste('t(', t$parameter,
#         ') = ', round(t$statistic, 3),
#         ', p = ', round(t$p.value, 4),
#         sep='')}

# apa.anova = function(a, terms=NULL){
#   tidied = tidy(a)
#   error.df = tidied[nrow(tidied),]$df
#   if (is.numeric(terms)){
#     terms = tidied$term[terms]
#   }
#   if (is.character(terms)) {
#     my.terms = filter(tidied, term %in% terms)
#   } else {
#     my.terms = tidied[1:(nrow(tidied)-1),]
#   }
#   o = ''
#   for (i in 1:nrow(my.terms)) {
#     term = my.terms[i,]
#     o %<>% paste(term$term, ': F(', term$df, ', ', error.df,
#                  ') = ', round(term$statistic, 3), ', p = ', round(term$p.value, 4),
#                  '\n\n', sep='')
#   }
#   return (o)
# }

# soft.apply = function(df, fun) {
#   nums = sapply(df, is.numeric)
#   df[,nums] %>% fun
# }

# se_of_combined_terms = function(m, terms) {
#   v = vcov(m)
#   se = sqrt(diag(v))
#   result = 0
#   for (a in 1:(length(terms))){
#     i = terms[a]
#     result = result + se[i]
#     if (a < length(terms)) {
#       for (b in (a+1):(length(terms))) {
#         j = terms[b]
#         result = result + 2*abs(v[i,j])
#       }
#     }
#   }
#   return (result)
# }

# edge = function(x) {
#   xsign = sign(.5-x)
#   return (x + .001*xsign)
# }

# flat.cor = function(d) {
#   c.mat = cor(d)
#   nms = colnames(c.mat)
#   out.names = c()
#   out.values = c()
#   for (i in 1:(ncol(c.mat)-1)) {
#     for (j in (i+1):ncol(c.mat)) {
#       out.names = c(out.names,
#                     paste0(nms[i], '.', nms[j]))
#       out.values = c(out.values, c.mat[i,j])
#     }
#   }
#   data.frame(comp=out.names, estimate=out.values)
# }


# flat.cor.p = function(d) {
#   d %<>% data.frame
#   out = rbind()
#   nms = names(d)
#   for (i in 1:(ncol(d)-1)) {
#     for (j in (i+1):ncol(d)) {
#       R =
#       out = rbind(
#         out,
#         cbind(comp=paste0(nms[i], '.', nms[j]),
#               glance(cor.test(d[,i], d[,j])))
#       )
#     }
#   }
#   data.frame(out)
# }

# debracket = function(x) {
#   gsub('\\(', '', x) %>% gsub('\\)', '', .)
# }

# tidy.cor = function(x,y) {
#   cor.test(x,y) %>% tidy %>%
#     select(r=estimate, p=p.value) %>%
#     mutate(r=round(r, 3), p=round(p, 4))
# }

# lgnd = function(x, y){
#   theme(legend.position=c(x, y), legend.justification = c(x, y))
# }

# report.lmm = function(mod, digits = 2){
#   s = summary(mod)
#   co = coef(s) %>% round(digits)
#   ci = confint(mod, method='Wald', parm='beta_') %>% round(digits)
#   vc = s$varcor$subject_nr
#   subj.sd = attr(vc, 'stddev') %>% round(digits)
#   for(v in rownames(co)){
#     # txt = paste0('Parameter: ', v, ' B = ', co[v,'Estimate'],
#     #              ', CI = ')
#     #              )
#     #              %s. B = %f CI = [%f, %f], t(%.1f) = %.3f, p = %.3f.')
#     template = 'Parameter: %s. B = %.#f CI = [%.#f, %.#f], t(%.1f) = %.3f, p = %.3f, '
#     template = str_replace_all(template, '#', paste0(digits))
#     txt = sprintf(template,
#                   v, co[v,'Estimate'],
#                   ci[v,1], ci[v, 2],
#                   co[v,'df'], co[v,'t value'], co[v,'Pr(>|t|)'])
#     if(v %in% names(subj.sd)){
#       txt = paste0(txt, ' SD across subjects = ', round(subj.sd[v], digits))
#     }
#     print(txt)
#   }
# }

