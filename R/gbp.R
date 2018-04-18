#' Finding the target room size in generalized birthday problem
#'
#' @name gbp
#' @param k Integer. k persons shared birthday.
#' @param p Integer. Room size to be tested.
#' @param B_l Lower bound of tested room size.
#' @param B_u Upper bound of upper room size.
#' @param threshold A threshold to keep while loop running for binary search.
#' @param print Set TRUE if you want to check the result while simulating. Default is FALSE.
#' @param recor TRUE if you want to output the room tested in simulation. Default is FALSE.
#' @return Room size
#' @examples
#'# Simple example with k=2
#'system.time({
#'y <- gbp(k=2, p=0.5)
#'})
#'y
#'
#'# You can also track the simulation process
#'y <- gbp(k=3, p=0.5, print=TRUE)
#'
#'# Or output the tested room sizes
#'y <- gbp(k=3, p=0.5, recor=TRUE)
#'y

library(Rcpp)

gbp <- function(k=3, p=.5, B_l=1000, B_u=20000, threshold=.001, print=FALSE, recor=FALSE) {
  replicate <- B_l             # For one room size, do 1000 replciates
  i <- k                       # Tested room size; start from k persons in a room
  x <- 1                       # Counter for while loop
  result <- 0                  # An object for temporarily saved result
  L <- 0                       # Lower bound for binary search
  R <- k^4+100+sample(-k:k, 1) # Upper bound for binary search
  record <- NULL               # A record for which room size has been tested

  while (abs(result - p) >= threshold) {
    # Printed record
    if (print == TRUE) cat(paste0('L', L, '\tR', R, '\t'))

    # Tested room size
    i <- ceiling((L+R)/2)

    # Run simulation in C++
    result <- simulateC(k=k, room=i, replicate = replicate)
    
    # Increase replicate when approaching target room size
    replicate <- floor(-B_u/(1+exp(-.03* ((R-L)-300))))+(B_u+B_l)
    
    # Binary search algorithm:
    # Decrease upper bound if simulated probability is less than p
    # Increase lower bound if simulated probability is greater than p
    if (result >= p) R <- i
    if (result < p) L <- i
    record[x] <- i       # Keep a record on every tested room size
    x <- x + 1           # Counter

    # Print out each room size tested
    if (print == TRUE) cat(paste0('Room size = ', i, '\tReplicate = ', replicate, '\n'))

    # Stop the loop if trapped at local value
    if (sum(duplicated(tail(record,5))) == 2 | L == R) break
  }
  # Answer
  ans <- R
  if (print == TRUE) cat(paste0('Room size = ', ans, '\n'))
  if (recor == TRUE) {
    return(list(ans=ans, record=record))
  } else if (recor == FALSE) return(ans)
}
