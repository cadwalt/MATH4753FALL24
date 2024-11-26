#' My N Curve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a y value
#'
#' @return the curve, the shaded area between the curve and x-axis from -infinity to x=a, and the area
#' @export
#'
#' @examples myncurve(mu = 0, sigma = 1, a = 2)
myncurve = function(mu, sigma, a){
  graphics::curve(stats::dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), main = paste("Normal Curve: mean =", mu, ", sd =", sigma), ylab = "Density", xlab = "X")

  xcurve = seq(mu-3*sigma, a, length=1000)
  ycurve = stats::dnorm(xcurve, mean = mu, sd = sigma)
  graphics::polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col = "cyan")

  area = stats::pnorm(a, mean=mu, sd = sigma)
  x=NULL
  graphics::text(x=a, y=.02, paste("Area =", round(area, 4), sep = ""), pos = 4)

  return(list(mu = mu, sigma = sigma, area=area))
}
