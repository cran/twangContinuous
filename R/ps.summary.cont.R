ps.summary.cont <- function(x, t, w, sampw = NULL, get.wcor = TRUE,
                            get.tstat = FALSE,
                            #na.action = c("level", "exclude", "lowest")[1],
                            collapse.by.var = FALSE, fillNAs = FALSE){

	# isFactor <- is.factor(x)
	#
	# if(isFactor){
	# 	if((sum(is.na(x)) > 0) && (na.action %in% c("level","lowest"))){
	# 		x <- factor(x, levels = c(levels(x), "<NA>"))
	# 		x[is.na(x)] <- "<NA>"
	# 	}
	#
	# }

  if(is.null(sampw)) sampw <- rep(1, length(x))

	# design <- svydesign(ids =~ 1, weights = ~w,
	#                     data = data.frame(x=x,t=t,w=w,
	#                                      sampw = sampw, miss = is.na(x)))
	# designSW <- svydesign(ids =~ 1, weights = ~sampw,
	#                       data = data.frame(x=x,t=t,w=w,
	#                                         sampw = sampw, miss = is.na(x)))

	# if(na.action == "exclude") design <- subset(design, !is.na(x))
	ret <- NULL


	if(get.wcor){
		#if(isFactor){
		  ret <- apply(x, 2, wcor, x = x, y = t, wts = w)

		#}
		# else{
		# 	design.t <- subset(design, t==1)
		#
		# 	if(multinomATE) design.c <- designSW
		# 	else design.c <- subset(design, t==0)
		#
		# 	m.t <- svymean(~x, design.t, na.rm = TRUE)
		# 	m.c <- svymean(~x, design.c, na.rm = TRUE)
		# 	sd.t <- sqrt(svyvar(~x, design.t, na.rm = TRUE))
		# 	sd.c <- sqrt(svyvar(~x, design.c, na.rm = TRUE))
		#
		# 	if((estimand == "ATE")) sd.denom <- sqrt(svyvar(~x, designSW, na.rm = TRUE))
		# 	else sd.denom <- sd.t
		#
		# 	t.n <- summary(svyglm(x~t, design))$coefficients[2,3:4]
		# 	b.n <- ifelse(sd.denom == 0.0, NA, (m.t - m.c)/sd.denom)
		#
		# 	ret <- cbind(m.t, sd.t, m.c, sd.c, b.n, t.n[1], t.n[2])
		# 	colnames(ret) <- c("tx.mn","tx.sd","ct.mn", "ct.sd","std.eff.sz","stat","p")
		# 	if((sum(is.na(x))>0) && (na.action == "level")){
		# 		m.t <- svymean(~is.na(x), design.t, na.rm = TRUE)[2]
		# 		m.c <- svymean(~is.na(x), design.c, na.rm = TRUE)[2]
		#
		# 		sd.t <- sqrt(m.t*(1-m.t))
		# 		sd.c <- sqrt(m.c * (1-m.c))
		#
		# 		test <- try(summary(svyglm(is.na(x)~t,family=quasibinomial,design)), silent=TRUE)
		# 		if(class(test)[1] != "try-error") t.n <- test$coefficients[2,3:4]
		# 		else t.n <- c(NA,NA)
		#
		# 		if(estimand == "ATE"){
		# 			m.SW <- svymean(~is.na(x), designSW, na.rm = TRUE)[2]
		# 			sd.p <- sqrt(m.SW * (1 - m.SW))
		# 			b.n <- ifelse(sd.p == 0, NA, (m.t - m.c)/sd.p)
		# 		}
		# 		else b.n <- ifelse(sd.t==0.0, NA, (m.t-m.c)/sd.t)
		#
		# 		ret <- rbind(ret, c(m.t, sd.t, m.c, sd.c, b.n, t.n))
		#	 }
		# }
	}

	# if(get.tstat){
	# 	work <- design$variables
	# 	if(!isFactor){
	# 		if(na.action == "lowest") work$x[is.na(work$x)] <- min(work$x, na.rm = TRUE) - 1
	# 		if(na.action == "level") work <- subset(work, !is.na(x))
	# 	}
	# 	work$w[work$t == 1] <- with(subset(work, t==1), w/sum(w))
	#
	#
	# 	work$w[work$t == 0] <- with(subset(work, t == 0), -w/sum(w))
	#
	# 	if(!isFactor){
	# 		ess <- with(work, sapply(split(w,t), function(w){sum(w)^2/sum(w^2)}))
	# 		ind <- order(work$x)
	# 		cumv <- abs(cumsum(work$w[ind]))
	# 		cumv <- cumv[diff(work$x[ind]) != 0]
	# 		ks <- ifelse(length(cumv) > 0, max(cumv), 0)
	# 	}
	# 	else ks <- abs(sapply(split(work$w, work$x), sum))
	#
	#
	# 	if(isFactor){
	# 			if(sum(ks>0)<=1){ # deal with factors with some empty levels
	# 				ks[1:length(ks)]   <- 0  # preserves names(ks)
	# 				pval <- 1
	# 				}
	# 				else{
	# 					pval <- try(as.numeric(svychisq(~x+t,design=design)$p.value), silent=TRUE)
	# 					if(class(pval)[1] == "try-error"){
	# 						pval <- NA
	# 						}
	# 				}
	# 		}
	#
	# 	else pval <- 1- .C("psmirnov2x", p = as.double(ks), as.integer(ess[2]), as.integer(ess[1]), PACKAGE = "twang")$p
	#
	#
	# 	if((sum(is.na(design$variables$x)) > 0) && (na.action == "level") && !isFactor){
	# 		work <- design$variables
	# 		work$w[work$t == 1] <- with(subset(work, t==1), w/sum(w))
	# 		if(multinomATE){
	# 			work$w[work$t == 0] <- 0
	# 			work$w <- work$w - work$sampw/sum(work$sampw)
	# 		}
	# 		else{
	# 			work$w[work$t == 0] <- with(subset(work, t == 0), -w/sum(w))
	# 		}
	#
	# 		ks <- c(ks, abs(sum(with(subset(work, is.na(x)), sapply(split(w,t), sum)))))
	# 		pval <- c(pval, as.numeric(svychisq(~miss + t, design = design)$p.value))
	# 	}
	#
	# 	ret <- cbind(ret, ks, ks.pval = pval)
	# 	}

	ret <- data.frame(ret)
	# if(isFactor & fillNAs) {
	# 	ret$stat[is.na(ret$stat)] <- ret$stat[1]
	# 	ret$p[is.na(ret$p)] <- ret$p[1]
	# 	}
	# if(!isFactor) rownames(ret) <- c("","<NA>")[1:nrow(ret)]
	return(ret)

}
