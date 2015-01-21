## Here we have two functions, created in a way that utilizes R's unique lexical
## scoping. Essentialy, "makeCacheMatrix" produces an output of 4 unique functions,
## all the while storing some matrix of interest within the body of the main function,
## i.e. a different environment than the current environment. Further, "cacheSolve"
## pulls from this list of functions and goes through an initial check, to see if the 
## matrix inverse of interest is already cached, and ultimately produces this inverse
## if necessary by running through the four functions produced by "makeCacheMatrix."


## The four functions produced in "makeCacheMatrix"'s output are meant to set the
## value of a matrix, get that matrix, set the matrix inverse value, and 
## finally get that matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <-- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Initially, a matrix inverse check occurs to see if the inverse can already be found.
## If failed, "cacheSolve" will then "get" the matrix of interest, take its inverse
## using an R function "solve," "set" this inverse matrix and the "get" this inverse
## matrix as its final output!

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
