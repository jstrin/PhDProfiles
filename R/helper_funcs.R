#' Helper Functions
#'
#' \code{int_breaks_y} a helper function that returns a vector of integers to create
#' integer-only breaks for continuosu y axis starting at 0
#'
#'
#' @param var a continuous variable
#'
#' @return a vector of integers
#'
#'
#'@export


int_breaks_y <- function(var){
  unique(floor(pretty( seq(0,max(var) + 1 ) * 1.1 ) ) )
}


#' \code{int_breaks_x} a helper function that returns a vector of integers to create
#' integer-only breaks for continuous x axis
#'
#' @param var a continuous variable
#'
#' @return a vector of integers
#'
#'
#'@export
int_breaks_x <- function(var){
  unique(floor(pretty(seq(var[1], var[2]+1) )))
  }
