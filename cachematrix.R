## Edited by Brandon Hu 
## Date: 24/08/2014
## Description: Contains 2 functions
## makeCacheMatrix: setter and getter methods of the cached matrix
## cacheSolve: return the cached inverse matrix

## Set the value of the matrix
## Get the value of the matrix 
## Set the value of the inverse matrix (solve)
## Get the value of the inverse matrix (solve)
makeCacheMatrix <- function(x = matrix()) {
    m_list <- NULL
    set <- function(m_data) {
      x <<- m_data #assign matrix data value to an obj by lexical scoping
      m_list <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m_list <<- solve
    getSolve <- function() m_list 
    list(set=set,get=get,setSolve=setSolve,getSolve=getSolve) #returns a list
}

## Return the cached stored inverse matrix 
## Else store the inversed matrix in cache
cacheSolve <- function(x, ...) {
  m_list <- x$getSolve()
  if (!is.null(m_list)) { #if list not null, return the inverse matrix stored in cache 
      message("Retrieving cached data ...")
      return(m_list) # return a matrix that is the inverse of 'x'
  }
  else {
    data <- x$get()
    m_list <-  solve(data, ...)
    x$setSolve(m_list)
    m_list
    message("Store the data in cache.")
  }
}
