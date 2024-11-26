#' N Tickets
#'
#' @param N number of seats in flight
#' @param gamma probability of overbooking
#' @param p probability a passenger will show
#'
#' @return prints a named list containing nd, nc, N, p and gamma - where nd is calculated using the discrete distribution and nc is the same calculated with normal approximation. Also creates discrete and continuous plots of Objective function vs n
#' @export
#'
#' @examples nTickets(400, 0.02, 0.95)
nTickets = function(N, gamma, p) {
  target_value <- N
  target_prob <- 1 - gamma
  x <- N

  #find number of tickets to sell using discrete distribution
  fnd <- function(nd) {
    1-gamma-stats::pbinom(N, nd, p)
  }
  nd <- N -1 + which.min(abs(fnd(N:floor(N*1.1))))
  nd

  #find number of tickets to sell using normal approximation
  q <- 1 - p
  fn <- function(nc) {
    prob_nc <- stats::pnorm(N + 0.5, mean = nc * p, sd = sqrt(nc * p * q))
    abs(prob_nc - target_prob)
  }
  nc <- stats::optimize(fn, interval = c(N, N+20))$minimum #use optimize to find minimum nc

  #print named list containing nd, nc, N, p, and gamma
  namedlist = list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)
  print(namedlist)

  #create discrete plot of objective function vs n
  plot(N:floor(N*1.1), fnd(N:floor(N*1.1)), lwd = 2, xlab = "n", ylab = "Objective", main = paste("Objective vs n to find optimal tickets sold (", nd, ") \ngamma = ", gamma, " N = ", N, " discrete", sep=""))
  graphics::abline(v=nd, col = "red", lwd=2, h=0)

  #create continuous plot of objective function vs n
  fn <- function(nc) {
    prob_nc <- 1-gamma-stats::pnorm(N + 0.5, mean = nc * p, sd = sqrt(nc * p * q))

  }
  graphics::curve(fn, xlim = c(N,N*1.1), ylim=c(0,1), col = "black", lwd = 2, xlab = "n", ylab = "Objective", main = paste("Objective vs n to find optimal tickets sold (", nc, ") \ngamma = ", gamma, " N = ", N, " continuous", sep=""))
  graphics::abline(v=nc, col="blue", lwd=2, h=0)
}
