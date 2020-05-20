#'
#' @title Computes a lm
#' @description This function calculates a lm
#' @details Test lm
#' @param formula for the lm
#' @param data used for lm
#' @return parts required for lm
#' @author Stefan B., Daniel S.
#' @export
#'
lmTestDS = function (formula = NULL, data = NULL)
{
  if (! is.null(data)) {
    dataDF = eval(parse(text = data))
  } else {
    stop("Data is missing")
  }
  X = model.matrix(formula, data = dataDF)
  y = dataDF[[all.vars(formula)[1]]]

  return (list(xtx_inv = solve(t(X) %*% X), xty = t(X) %*% y))
}

