#' My Confidence Interval
#'
#' @param x single sample x
#'
#' @return a 95% confidence interval for the mean from a single sample x
#' @export
#'
#' @examples x = rnorm(30, mean=10, sd=12); myci(x)
myci = function(x) {
  t.test(x, conf.level = 0.95)$conf.int
}
