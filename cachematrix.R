
## this function returns a list containing 4 functions : set, get,
## setInv, getInv

makeCacheMatrix <- function(x = matrix()) {
  storeInv <- NULL # to store inverted result
  # set a matrix to object created by makeCacheMatrix function
  
  set <- function(y) {
    x <<- y
    storeInv <<- NULL # initialises storeInv to null
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) storeInv <<- inv # setInv sets the inversed matrix
  getInv <- function() storeInv # getInv to return the inversed matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
## this function inverses the matrix got from makeCacheMatrix
## checks if inversion result is there and returns the inverted matrix.
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  final <- x$getInv() # get the inversed matrix from object x
  if(!is.null(final)) { # if the inversion result present
    message("getting cached data")
    return(final) # return the calculated inversion as final
  }
  getdata <- x$get() # if not, we get the matrix object
  final <- solve(getdata)
  x$setInv(final) # setting the solved result to final
  final # return final solved reult
}
