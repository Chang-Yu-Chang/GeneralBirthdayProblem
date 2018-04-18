#' A function for simulation of generalized birthday problem
#' 
#' @name simulateC
#' @param k Integer. k persons shared birthday.
#' @param room Integer. Room size to be tested.
#' @param repliate Integer. Replicate size.
#' @return A floating value indicating the probability of at least k persons sharing birthday in a room
#' @examples
#'system.time({
#'p <- simulateC(k=2, roomSize=23, replicate=1e7)
#'})
#'p
#'
#'

simulateC <- function(k=2, room=23, replicate=1e5) {
  .Call(`_GeneralBirthdayProblem_simulateC`, k=k, room=room, replicate=replicate)
}



