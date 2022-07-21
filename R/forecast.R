#' Conditional Expectation
#'
#' Returns the expected number of transactions for a given forecast
#' history
#'
#' @param r shape parameter of gamma distribution
#' @param alpha scale parameter of gamma distribution
#' @param a beta function shape parameter
#' @param b beta function shape parameter
#'
#' @returns  \preformatted{
#'  Inner function does the work of estimating the parameters of the bg/nbd
#'  model. Returns the forecasted number of transactions.
#'  x: repeat purchases
#'  t_x: time of last repeat
#'  T_: calibration period
#'  t: forecast window}
#'
#' @export
#'
forecast <- function(r=0.2426707, alpha=4.4159851, a=0.7928716, b=2.4247084) {

  ex_yt <- function(x, t_x, T_, t) {

    f21 <- sum(.gh(r, alpha, a, b, x, T_, t))

    ex_trans <- (a + b + x - 1) / (a - 1) * (1 - ((alpha + T_) / (alpha + T_ + t)) ^ (r + x) * f21) /
      (1 + ifelse(x > 0, 1, 0) * a / (b + x - 1) * ((alpha + T_) / (alpha + t_x)) ^ (r + x))

    ex_trans

  }

}


#' Gaussian hypergeometric function
#'
#' @keywords internal
#'
#' @param r shape parameter of gamma distribution
#' @param alpha scale parameter of gamma distribution
#' @param a beta function shape parameter
#' @param b beta function shape parameter
#' @param x repeat purchases
#' @param T_ calibration period
#' @param t forecast window
#'
.gh <- function(r, alpha, a, b, x, T_, t) {

  g_a <- r + x
  g_b <- b + x
  g_c <- a + b + x - 1
  g_z <- t / (alpha + T_ + t)

  terms <- list()
  terms[[1]] <- 1
  idx <- 2
  while(terms[[idx-1]] > 0) {

    terms[[idx]] <- terms[[idx-1]] * (g_a + (idx - 1) - 1 ) * (g_b + (idx - 1) - 1) /
      ((g_c + (idx - 1) - 1) * (idx - 1)) * g_z

    idx <- idx + 1
  }

  unlist(terms)

}

