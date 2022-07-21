#' Beta Geometric/Negative Binomial Distribution Model
#'
#' A functional implementation of the bg/nbd model for rfm analysis.
#' Implemented from the paper: “Counting Your Customers” the Easy Way: An Alternative to the Pareto/NBD Model
#' \url{http://www.brucehardie.com/papers/bgnbd_2004-04-20.pdf}
#' Functions are adapted from:
#' \url{http://www.brucehardie.com/notes/004/bgnbd_spreadsheet_note.pdf}
#'
#' Outer function that sets the environment.
#'
#' @param rfm_data \preformatted{A data frame of rfm formatted data. Columns should refer to:
#'   x: repeat purchases
#'   t_x: time of last repeat
#'   T_: calibration period}
#' @param estimate Is this for individual log likelihood contributions or for estimating the
#'   parameters of the distribution?
#' @return \preformatted{
#'  Inner function does the work of estimating the parameters of the bg/nbd
#'  model. Returns either the individual log likelihood contributions or
#'  negative log likelihood for estimating the bg/nbd parameters.
#'  r: shape parameter of gamma distribution
#'  alpha: scale parameter of gamma distribution
#'  a: beta function shape parameter
#'  b: beta function shape parameter}
#'
#' @export
bg_nbd <- function(rfm_data, estimate=FALSE) {

  bg_nbd_param <- function(r=1, alpha=1, a=1, b=1) {

    valid_cols <- c('x', 't_x', 'T_')

    if(!any(valid_cols %in% colnames(rfm_data))) {

      stop('Valid columns are x, t_x, and T_')

    }

    x <- rfm_data$x
    t_x <- rfm_data$t_x
    T_ <- rfm_data$T_

    if(estimate) {
      -sum(.a1(x, r, alpha) + .a2(x, a, b) + log(exp(.a3(x, T_, r, alpha)) + ifelse(x > 0, exp(.a4(x, t_x, r, alpha, a, b)), 0)))
    } else {
      .a1(x, r, alpha) + .a2(x, a, b) + log(exp(.a3(x, T_, r, alpha)) + ifelse(x > 0, exp(.a4(x, t_x, r, alpha, a, b)), 0))
    }

  }

}

.a1 <- function(x, r, alpha) {
  log(gamma(r+x)) - log(gamma(r)) + r * log(alpha)
}

.a2 <- function(x, a, b) {
  log(gamma(a+b)) + log(gamma(b+x)) - log(gamma(b)) - log(gamma(a+b+x))
}

.a3 <- function(x, T_, r, alpha) {
  -1 * (r+x) * log(alpha+T_)
}

.a4 <- function(x, t_x, r, alpha, a, b) {
  ifelse(x > 0, log(a) - log(b+x-1) - (r+x) * log(alpha+t_x), 0)
}
