# ---------------------------------------------------------
# Project     : Coursera Data Science: R Programming
# Sub Project : Week 3 - Programming Assignment 2
# Task        : Clojure
# Author      : Georg Maubach
# Created     : 2016-12-10
# Updated     : 2016-12-10
# ---------------------------------------------------------
# Motto: Explicit is good!
# ---------------------------------------------------------

# [ Functions ] -------------------------------------------

makeCacheMatrix <- function(m = matrix())
#----------------------------------------------------------
#'
#' @title makeCacheMatrix
#'
#' @description
#' \code{function_name} This function creates a special 
#' "matrix" object that can cache its inverse.
#'
#' @usage
#'   my_matrix <- makeCacheMatrix(m = matrix())
#'
#' @details
#'   Creates a list of getting and setting functions for
#'   the matrix as well as the setting and getting
#'   functions for the inverse of the matrix.
#'   @section Caching
#'   Caching is done with objects with the function.
#'
#' @param m (object of class matrix)
#'   m is a square invertible matrix.
#'
#' @return Returns a list with getter and setter functions.
#'
#' @author Georg Maubach
#' 
#----------------------------------------------------------
{
  matrix_inverse <- NULL
  
  set_matrix <- function(y) 
  {
    matrix_data    <<- y
    matrix_inverse <<- NULL
  }
  
  get_matrix <- function()
  {
    return(m)
  }
  
  set_inverse <- function(inversed_matrix)
  {
    matrix_inverse <<- inversed_matrix
  }
  
  get_inverse <- function()
  {
    return(matrix_inverse)
  }
  
  return(
    list(
      set_matrix = set_matrix,
      get_matrix = get_matrix,
      set_inverse = set_inverse,
      get_inverse = get_inverse)
  )
}

cacheSolve <- function(FUN, ...)
#----------------------------------------------------------
#'
#' @title cacheSolve
#'
#' @description
#' \code{function_name} This function returns an inverted
#' matrix either from cache or if it is not present there
#' fresh calculated.
#'
#' @usage
#'  my_cache_solve <- cacheSolve(FUN = my_cache_matrix)
#'
#' @details
#'    Returns an inverted matrix either from cache or if
#'    it is not present there fresh calculated.
#'    Inverting is done using solve().
#'
#' @param m (matrix list)
#'   FUN is a list containing getter and setter functions
#'   for a matrix cache function.
#'
#' @return Returns the inverse of a matrix.
#'
#' @author Georg Maubach
#' 
#----------------------------------------------------------
{
  inverse <- FUN$get_inverse()
  
  if(!is.null(inverse))
  {
    message("getting cached inverse")
    return(inverse)
  } else 
  {
    message("calculating inverse from data and caching it")
    data <- FUN$get_matrix()
    i <- solve(data, ...)
    FUN$set_inverse(i)
    return(i)
  }
}

# [ Tests ] -----------------------------------------------
my_matrix <- matrix(data = c(1, 2, 3, 4),
                    nrow = 2,
                    ncol = 2)

my_cached_matrix <- makeCacheMatrix(m = my_matrix)

for (i in c(1, 2, 3))
{
  my_inverse <- cacheSolve(FUN = my_cached_matrix)
}

# EOF .

