#' Fit a linear model
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class) a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#'
#' @param data an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which lm is called.
#'
#' @param subset an optional vector specifying a subset of observations to be used in the fitting process.
#'
#' @export



mylm <- function(formula, data, subset=NULL) {
  if(!is.null(subset)){
    if(!is.numeric(subset)) stop("Non numeric vector input please correct")
    if(max(abs(subset))>nrow(data)) stop("Subset stated is larger than the input dataset, please correct")
    data <- data[subset,]
  } 
  
  if (any(!(all.vars(formula) %in% names(data)))) stop(paste(paste(all.vars(formula)[!(all.vars(formula) %in% names(data))], collapse = " and "), "are named in formula but not present in data, please check formula"))
  
  data <- data[all.vars(formula)]
  if(any(!(sapply(data, class) %in% c("numeric","factor", "interger")))) stop(paste(paste(names(data)[!(sapply(data, class) %in% c("numeric","factor", "interger"))], collapse = " and "), "are not numeric, integer or factor variables, please reconsider"))
  
  if(sum(rowSums(is.na(data)) != 0) != 0) warning(paste(sum(rowSums(is.na(data)) != 0), "data points contain at least 1 null value and so will not be considered"))
  data <- data[rowSums(is.na(data))== 0,]
  
  if(nrow(data)  <= 2 & nrow(data)) stop("2 or fewer data points, technique not suitable")
  if(nrow(data)  >= 2 & nrow(data) <= 5) warning("2 to 5 data points, technique may not be suitable")
  
  
  yname <- as.character(formula[[2]])
  predictor_data <- data[setdiff(all.vars(formula), yname)]
  
  
  
  
  factored_data <- predictor_data[names(predictor_data)[sapply(predictor_data, class) %in% c("factor")]]
  
  if(any(unlist(lapply(factored_data, function(x)all(duplicated(x)[-1L]))))) stop("catagorical with only 1 factor detected, please remove and try again")
  rm(factored_data)
  
  
  if(any(sapply(predictor_data[names(predictor_data)[sapply(predictor_data, class) %in% c("numeric", "interger")]], sd) == 0)) stop("numeric predictor variable with 0 variance detected, please remove and try again")
  
  cormat <- cor(predictor_data[names(predictor_data)[sapply(predictor_data, class) %in% c("numeric", "interger")]])
  diag(cormat) <- 0
  cormat[is.na(cormat)] <- 0
  if(any(cormat >= 0.99999999, na.rm = T)) stop("Perfect corrolation detected between predictor variables, can not compute")
  rm(cormat)
  rm(predictor_data)
  
  
  yvec <- data[,yname]
  xmat <- model.matrix(formula, data=data)
  df.residual <- nrow(xmat)-ncol(xmat)


  
  
  xxinv <- solve(t(xmat)%*%xmat)
  coef <- as.vector(xxinv%*%t(xmat)%*%yvec)
  names(coef) <- colnames(xmat)
  yfit <- as.vector(xmat%*%coef)
  residuals <- yvec-yfit
  sigma <- sqrt(sum(residuals^2)/df.residual)
  vcov <- sigma^2*xxinv

  mylmobject <- list(call=match.call(),
                     formula=formula, data=data, yname=yname,
                     coef=coef, sigma=sigma, vcov=vcov,
                     npar=ncol(xmat), df.residual=df.residual,
                     residuals=residuals,
                     fitted.values=yfit)

  class(mylmobject) <- "mylm"
  return(mylmobject)
}

#' Codes for marking p-values
#'
#' @param pvals Vector of p-values
#'
#' @export
signif_codes <- function(pvals) {
  s <- ifelse(pvals>0.1, " ",
              ifelse(pvals>0.05, ".",
                     ifelse(pvals>0.01, "*",
                            ifelse(pvals>0.001, "**","***"))))
  return(s)
}


#' Summary method for mylm
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
summary.mylm <- function(object,...) {
  mylmobj_summary <- object
  class(mylmobj_summary) <- "summary.mylm"
  return(mylmobj_summary)
}

#' Print method for mylm
#'
#' @param x object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
print.mylm <- function(x,...) {
  with(x, {
    cat("Linear model mylm object\n")
    cat("\nCall:\n")
    print(call)
    cat("\nCoefficients:\n")
    print(coef)
    cat("\n")
  })
  invisible()
}

#' Print method for summary(mylm)
#'
#' @param x object of class "summary.mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
print.summary.mylm <- function(x,...) {
  with(x, {
    cat("Linear model mylm object\n")
    cat("\nCall:\n")
    print(call)
    se <- sqrt(diag(vcov))
    n <- nrow(data)
    coeftab <- data.frame(Estimate=coef,
                          "Std. Error"=se,
                          "t value"=coef/se,
                          pvals=2*pt(abs(coef/se), df.residual, lower.tail=FALSE))
    coeftab$s <- signif_codes(coeftab$pvals)
    names(coeftab) <- c("Estimate","Std. Error", "t value", "Pr(>|t|)"," ")
    rss <- sum(residuals^2)
    tss <- sum((data[,yname]-mean(data[,yname]))^2)
    r.squared <- 1 - rss/tss
    adj.r.squared <- 1 - (1-r.squared)*(n-1)/(df.residual)
    fstat <- ((tss-rss)/(npar-1))/(rss/df.residual)
    pvalue <- pf(fstat, npar-1, df.residual, lower.tail=FALSE)
    cat("\nCoefficients:\n")
    print(coeftab)
    cat("---\n")
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    cat("\n")
    cat("Residual standard error: ",sigma," on ",df.residual," degrees of freedom\n")
    cat("Multiple R-squared: "); cat(r.squared); cat(" ");
    cat("Adjusted R-squared: "); cat(adj.r.squared); cat("\n")#	Adjusted R-squared:  0.2261
    cat("F-statistic: "); cat(fstat); cat(" on "); cat(npar-1); cat(" and "); cat(df.residual);
    cat(" DF, p-value "); cat(pvalue); cat("\n")#,25.25 on 1 and 82 DF,  p-value: 2.906e-06

    cat("\nVariance Covariance matrix:\n")
    print(vcov)
    cat("\n")
  })
  invisible()
}

#' Variance covariance matricx for parameters
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
vcov.mylm <- function(object,...) {
  with(object, {
    return(vcov)
  })
}

#' Confidence intervals for parameters
#'
#' @param object object of class "mylm"
#'
#' @param parm A specification of which parameters are to be given confidence intervals, either a vector of numbers or a vector of names. If missing, all parameters are considered.
#'
#' @param level The confidence level required (default = 0.95).
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
confint.mylm <- function(object, parm=NULL, level=0.95,...) {
  with(object, {
    se <- sqrt(diag(vcov))
    tval <- qt(1-(1-level)/2, df=df.residual, lower.tail=TRUE)
    retval <- cbind(coef-tval*se, coef+tval*se)
    dimnames(retval)[[2]] <- sprintf("%f %%", c(0,100)+c(1,-1)*100*(1-level)/2)
    if(!is.null(parm)) retval <- retval[parm,]
    return(retval)
  })
}


#' Fitted values
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
fitted.mylm <- function(object,...) {
  with(object, {
    return(fitted.values)
  })
}

#' Residuals
#'
#' @param object object of class "mylm"
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
residuals.mylm <- function(object,...) {
  with(object, {
    return(residuals)
  })
}

#' Predicted values
#'
#' @param object object of class "mylm"
#'
#' @param newdata An optional data frame in which to look for variables with which to predict. If omitted, the fitted values are used.
#'
#' @param se.fit A switch indicating if standard errors are required (default = FALSE).
#'
#' @param interval Type of interval calculation, can be "none", "confidence" or "prediction". Can be abbreviated.
#'
#' @param level The confidence level required (default = 0.95).
#' 
#' @param ... additional arguments to be passed to methods
#'
#' @export
predict.mylm <- function(object, newdata=NULL, se.fit=FALSE,
                         interval=c("none", "confidence", "prediction"),
                         level=0.95,...) {
  with(object, {
    if(is.null(newdata)) newdata <- object$data
    xmat.new <- model.matrix(object$formula, data=newdata)
    fitval <- as.vector(xmat.new%*%object$coef)
    retval <- list(fit=fitval)
    if(se.fit || interval!="none") {
       se.val <- sqrt(diag(xmat.new%*%vcov%*%t(xmat.new)))
       if(se.fit) retval$se.fit <- se.val
    }
    zval <- qnorm(1-(1-level)/2)
    if(interval[1]=="confidence") {
      retval$fit <- cbind(fitval, fitval-zval*se.val, fitval+zval*se.val)
      dimnames(retval$fit)[[2]] <- c("fit","lwr","upr")
    } else if(interval[1]=="prediction") {
      pred.err <- sqrt(sigma^2 + se.val^2)
      retval$fit <- cbind(fitval, fitval-zval*pred.err, fitval+zval*pred.err)
      dimnames(retval$fit)[[2]] <- c("fit","lwr","upr")
    }
    retval$df <- df.residual
    retval$residual.scale <- sigma
    return(retval)
  })
}

#' Plotting
#'
#' @param x object of class "mylm"
#'
#' @param ... additional arguments to be passed to methods, such as graphical parameters such as \code{par}.
#'
#' @export
plot.mylm <- function(x, ...) {
  with(x, {
    plot(data[,yname], fitted.values,
         xlab="Observed", ylab="Fitted",
         ...)
    abline(a=0, b=1, ...)
  })
  invisible()
}

