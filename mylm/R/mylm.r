
# Select Build, Build and reload to build and lode into the R-session.

mylm <- function(formula, data = list(), contrasts = NULL, ...){
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  X  <- model.matrix(attr(mf, "terms"), data = mf, contrasts.arg = contrasts)
  Y  <- model.response(mf)
  terms <- attr(mf, "terms")

  # Add code here to calculate coefficients, residuals, fitted values, etc...
  # and store the results in the list est
  betahat <- solve(t(X) %*% X) %*% t(X) %*% Y
  fitted_values <- X%*%betahat
  H <- X%*%solve((t(X)%*%X))%*%t(X)
  residuals <- (diag(nrow(H))-H)%*%Y
  mse <- sum((Y - fitted_values)^2)/(nrow(X))
  Cov <- mse*solve(t(X)%*%X)
  sigma2 <- sum((Y - fitted_values)^2) / (nrow(X) - ncol(X))
  std <- sqrt(diag(solve(crossprod(X))) * sigma2)
  z_value <- betahat/std
  p_value <- pnorm(-abs(betahat)/std)  * 2
  p_value2 <- 2*exp(-(abs(z_value)^2)/2)/(sqrt(2*pi)*abs(z_value))
  chi_squared <- sum((Y-fitted_values)^2/fitted_values)


  # R_squared
  sse <- sum(residuals ^ 2)  ## residual sum of squares
  sst <- sum((Y - mean(Y)) ^ 2)  ## total sum of squares
  R_squared <- 1 - sse/sst
  n <- length(Y)
  k <- length(betahat)-1
  adj_R_squared <- 1-(1-R_squared)*(n-1)/(n-k-1)

  est <- list(terms = terms, model = mf, betahat = betahat, fitted = fitted_values, residuals=residuals, std = t(std), z_value=z_value, p_value=p_value, p_value2=p_value2, covariance_matrix=Cov, R_squared=R_squared, adj_R_squared=adj_R_squared, sse=sse, sst=sst)


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
  print.default(t(object$std), digits = 4)
  cat('\nz-value')
  print.default(object$z_value, digits=4)
  cat('\nPr(>|z|)')
  print.default(object$p_value)
  print.default(object$p_value2)

  # R_squared
  cat('\nR-squared:')
  print.default(object$R_squared)
  cat('Adjusted R-squared:')
  print.default(object$adj_R_squared)
  cat('\n\n')

}

plot.mylm <- function(object, ...){
  # Code here is used when plot(object) is used on objects of class "mylm"
  plot(object$fitted, object$residuals, ylab="Residuals", xlab="Fitted", main="Residual vs Fitted")

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
