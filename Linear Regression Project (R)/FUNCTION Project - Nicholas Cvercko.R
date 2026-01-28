linear <- function(x, outcome, predictors, na.rm = TRUE) {
  
  o <- outcome
  p <- predictors
  
  if (na.rm) {x <- na.omit(x)}
  
  
  output.error <- c('Incorrect object input for the argument x, for this function it must be the data for the linear regression model, containing both the outcome variable and predictor variable values either a matrix or a dataframe object in R.',
                    'Incorrect object input for the argument outcome, for this function it must be the identifier for the desired outcome, this could be a character object of the column name or a numeric value of the column index in the data frame or matrix',
                    'Incorrect object input for the argument ppredictors, for this function it must be the identifiers for the desired predictor variables, this could be a character list of the column names or numeric values of the column indeces in the data frame or matrix')
  error <- c()
  if ((!is.matrix(x) & !is.data.frame(x)) |
      (!is.character(o) & !is.numeric(o)) |
      (!is.character(p) & !is.numeric(p))){
    if(!is.matrix(x) & !is.data.frame(x)) {
      error <- append(error, output.error[1], length(error))
    }
    if ((!is.character(o) & !is.numeric(o))){
      error <- append(error, output.error[2], length(error))
    }
    if (!is.character(p) & !is.numeric(p)){
      error <- append(error, output.error[3], length(error))
    }
    r.error <- list(error)
    names(r.error) <- c('The function encountered the following errors')
    return(r.error)
  }
  
  if (is.character(o)){
    if (!(o %in% colnames(x))){
      stop('The input for argument outcome is not an existing column name in the input for argument x')
    }
  }
  if (is.numeric(o)) {
    if (o > ncol(x)) {
      stop('The numeric input for the argument outcome is outside of the column indices of the input for the argument of x')
    }
    else if (o < 0) {
      stop('The numeric input for the argument outcome is outside of the column indices of the input for the argument of x')
    }
  }
  
  if (is.character(p)){
    p
    co <- colnames(x)
    wrong <- c()
    for (i in c(1:length(p))) {
      if (!(p[i] %in% co)) {wrong <- append(wrong, p[i], after = length(wrong))}
    }
    if (!is.null(wrong)) {
      if (length(wrong) == 1) {stop('The variable ', wrong ,' is not in x')}
      else {return(c('The following inputs for arguments for predictors are not column names in the argument input for x: ', wrong))}  
    }
  }
  if (is.numeric(p)) {
    if (max(p) > ncol(x)) {
      stop('At least one of the numeric inputs for the argument predictors is outside of the column indices of the input for the argument of x')
    }
    else if (min(p) < 0){
      stop('At least one of the numeric inputs for the argument predictors is outside of the column indices of the input for the argument of x')
    }
  }
  
  if ((is.data.frame(x) | is.matrix(x)) & 
      (is.character(o) | is.numeric(o)) & 
      (is.character(p) | is.numeric(p))){
    if (is.data.frame(x)) {
      m <- as.matrix(x[p])
      y <- as.matrix(x[o])
      x0 <- as.matrix(rep(1, nrow(x)))
      mx <- cbind(x0, m)
    }else if (is.matrix(x)){
      d <- c()
      for (i in p){
        a = which(colnames(x) == i)
        d <- append(d, a, after = length(d))
      }
      if (is.character(p)) {m <- x[ ,d]
      } else { 
        m <- x[ ,p]
      }
      if(is.numeric(o)) {
        y <- x[ ,o] 
      } else {y <- x[ ,which(colnames(x) == o)]
      } 
      x0 <- as.matrix(rep(1, nrow(x)))
      mx <- cbind(x0, m)
    }
    
    b <- round((solve(t(mx) %*% mx) %*% t(mx) %*% y), digits = 8)
    
    RSS <- (t(y) %*% y) - (t(b) %*% t(mx) %*% y)
    MSE <- RSS/(length(y) - (ncol(mx) - 1) - 1)
    
    SSreg <- t(b) %*% t(mx) %*% y - length(y) * mean(y)^2
    SYY <- t(y) %*% y - length(y) * mean(y)^2
    
    inv.XtX <- solve(t(mx) %*% mx)
    dg <- diag(inv.XtX)
    
    
    ses <- round(sqrt(MSE %*% dg), digits = 8)
    t.ses <- t(ses)
    
    R2 <- SSreg/SYY
    
    rownames(b)[1] <- 'Intercept'
    f <- cbind(b, t.ses)
    t <- f[ ,1]/f[ ,2]
    p <- 2 * pt(abs(t), nrow(mx) - length(p) - 1, lower.tail = FALSE)
    f <- cbind(b, t.ses, round(t, digits = 8), round(p, digits = 8))
    colnames(f) <- c('Parameter estimate', 'Standard Error', 't-value', 'Pr(>|t|)')
    
    r <- list(f, R2[1])
    
    
    names(r) <- c('Parameter Characteristics', 
                  'R-Squared Goodness of Fit Measure')
    
    return(r)
  }
}