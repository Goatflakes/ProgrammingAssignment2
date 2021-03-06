## cachematrix.R: Implements a matrix abstract data type which
##   1. is assumed to be invertible
##   2. computes its inverse when needed, and then caches it until
##      the matrix changes
## To create the matrix use makeCacheMatrix() optionally passing the
## intial matrix.
##
## To get the inverse, use cacheSolve() passing the return value of
## makeCacheMatrix.
##
## The matrix must have been intialised with an invertible matrix,
## either via an argument to the makeCacheMatrix() call, or a later
## call to myMatrix$set(matrix), where myMatrix is the return value of
## makeCacheMatrix() and matrix is an invertible matrix

## Return a cached matrix, which is really a list, containing
## functions to:
##   1. set the value of the matrix (NB: matrix must be invertible)
##   2. get the value of the matrix
##   3. set the value of the matrix inverse. For cacheSolve()
##   4. get the value of the matrix inverse. Again for cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  # Note that x stores the matrix and inv stores its inverse,
  # if it has been calculated yet.
  
  # Each call to makeCacheMatrix makes a new environment and new
  # x and inv that can be accessed by the <<- operator in functions
  # that are enclosed in makeCacheMatrix(). These functions are called
  # closures because they have an enclosing environment which is
  # accessible to them.

  # This is because <<- searches its parent environment.

  # inv == NULL means the inverse hasn't been calculated yet
  inv <- NULL
  
  # set() is a function that sets the matrix x
  # it also sets inv to NULL to indicate the inverse hasn't been
  # calculated yet.
  
  # set() is supposed to be used by the user, by calling
  # myMatrix$set(), where myMatrix is the return value of the
  # makeCacheMatrix() function.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get() is a function that simply returns x
  # it is used both by the user and the internal functions
  # It is called by myMatrix$get(), where myMatrix is the return
  # value of the makeCacheMatrix() function.
  get <- function() x

  setInv <- function(inverse) inv <<- inverse

  # getInv() is a function that returns the inverse inv

  # getInv() is used only by the internal functions. If you want the
  # get the inverse, call cacheSolve(myMatrix) instead, where
  # myMatrix is the return value of makeCacheMatrix()
  getInv <- function() inv
  
  # Return a list of all the accessor functions. Note that we don't
  # need add inv or x to the list, because the functions are closures
  # which can see x and inv in this environment & set them with the
  # <<- operator.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Given a list x returned by makeCacheMatrix(), determine if the
## inverse of the contained matrix has been computed.
## If so, return it. If not, compute it, store it and return it.

cacheSolve <- function(x, ...) {
  # determine if the matrix has been inverted yet
  inv <- x$getInv()
  if(!is.null(inv)) {
    # if so print a message
    message("getting cached data")
    # and return the precomputed inverse
    return(inv)
  }
  
  # if not, first get the stored matrix
  matrix <- x$get()
  # then invert it
  inv <- solve(matrix)
  # then store the inverse
  x$setInv(inv)
  # then return the inverse
  inv
}
