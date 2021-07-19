#' Fit a linear model
#'
#' @export
mylm <- function(formula, data, subset=NULL) {

  if(!is.null(subset)) data <- data[subset,]

  yname <- as.character(formula[[2]])
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
#' @param mylmobj mylm object
#'
#' @export
summary.mylm <- function(mylmobj) {
  mylmobj_summary <- mylmobj
  class(mylmobj_summary) <- "summary.mylm"
  return(mylmobj_summary)
}

#' Print method for mylm
#'
#' @param mylmobj mylm object
#'
#' @export
print.mylm <- function(mylmobj) {
  with(mylmobj, {
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
#' @param mylmobj_summary Summary object
#'
#' @export
print.summary.mylm <- function(mylmobj_summary) {
  with(mylmobj_summary, {
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
#' @export
vcov.mylm <- function(mylmobj) {
  with(mylmobj, {
    return(vcov)
  })
}

#' Confidence intervals for parameters
#'
#' @export
confint.mylm <- function(mylmobj, parm=NULL, level=0.95) {
  with(mylmobj, {
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
#' @export
fitted.mylm <- function(mylmobj) {
  with(mylmobj, {
    return(fitted.values)
  })
}

#' Residuals
#'
#' @export
residuals.mylm <- function(mylmobj) {
  with(mylmobj, {
    return(residuals)
  })
}

#' Predicted values
#'
#' @export
predict.mylm <- function(mylmobj, newdata=NULL, se.fit=FALSE,
                         interval=c("none", "confidence", "prediction"),
                         level=0.95) {
  with(mylmobj, {
    if(is.null(newdata)) newdata <- mylmobj$data
    xmat.new <- model.matrix(mylmobj$formula, data=newdata)
    fitval <- as.vector(xmat.new%*%mylmobj$coef)
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

#' @export
plot.mylm <- function(mylmobj, ...) {
  with(mylmobj, {
    plot(data[,yname], fitted.values,
         xlab="Observed", ylab="Fitted",
         ...)
    abline(a=0, b=1, ...)
  })
  invisible()
}

