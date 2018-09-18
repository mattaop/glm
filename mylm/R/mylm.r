
# Select Build, Build and reload to build and lode into the R-session.

mylm <- function(formula, data = list(), contrasts = NULL, ...){
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  X  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
  Y  <- model.response(mf)
  terms <- attr(mf, "terms")


  # Add code here to calculate coefficients, residuals, fitted values, etc...
  # and store the results in the list est
  betahat = solve(t(X) %*% X) %*% t(X) %*% Y
  H = X%*%solve((t(X)%*%X))%*%t(X)
  residuals = (diag(nrow(H))-H)%*%Y
  sigma2 <- sum((Y - X%*%betahat)^2) / (nrow(X) - ncol(X))
  std <- sqrt(diag(solve(crossprod(X))) * sigma2)
  t_value <- betahat/std
  for(i in 0:(ncol(X))){
    C = c(ncol(X))
    C[i] = 1
    c_jj = solve(t(X)%*%X)
    p_value <- t(betahat)*solve(sigma2*c_jj)%*%betahat
  }

  est <- list(terms = terms, model = mf, betahat = betahat, residuals=residuals, std = std, t_value=t_value, p_value=p_value)

  # Store call and formula used
  est$call <- match.call()
  est$formula <- formula

  # Set class name. This is very important!
  class(est) <- 'mylm'

  # Return the object with all results
  return(est)
}

print.mylm <- function(object, ...){
  # Code here is used when print(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat('Call:\n')
  print.default(object$call)
  cat('\nCoefficients:\n')
  print.default(t(object$betahat), digits=5)
  #format
}

summary.mylm <- function(object, ...){
  # Code here is used when summary(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat('Call:\n')
  print.default(object$call)
  cat('\nResiduals:\n')
  cat('Min: ')
  print.default(min(object$residuals), digits=5)
  cat('1Q: ')
  cat('Median: ')
  print.default(median(object$residuals), digits=4)
  cat('3Q: ')
  cat('Max: ')
  print.default(max(object$residuals), digits=5)
  cat('\nCoefficients:\n')

  # Formating
  cat('Estimate')
  print.default(object$betahat, digits=5)
  cat('\nStd. Error')
  print.default(object$std, digits = 4)
  cat('\nt-value')
  print.default(object$t_value, digits=4)
  cat('\nPr(>|t|)')
  print.default(object$p_value)

}

plot.mylm <- function(object, ...){
  # Code here is used when plot(object) is used on objects of class "mylm"

}



# This part is optional! You do not have to implement anova
anova.mylm <- function(object, ...){
  # Code here is used when anova(object) is used on objects of class "mylm"

  # Components to test
  comp <- attr(object$terms, "term.labels")

  # Name of response
  response <- deparse(object$terms[[2]])

  # Fit the sequence of models
  txtFormula <- paste(response, "~", sep = "")
  model <- list()
  for(numComp in 1:length(comp)){
    if(numComp == 1){
      txtFormula <- paste(txtFormula, comp[numComp])
    }
    else{
      txtFormula <- paste(txtFormula, comp[numComp], sep = "+")
    }
    formula <- formula(txtFormula)
    model[[numComp]] <- lm(formula = formula, data = object$model)
  }

  # Print Analysis of Variance Table
  cat('Analysis of Variance Table\n')
  cat(c('Response: ', response, '\n'), sep = '')
  cat('          Df  Sum sq X2 value Pr(>X2)\n')
  for(numComp in 1:length(comp)){
    # Add code to print the line for each model tested
  }

  return(model)

}
