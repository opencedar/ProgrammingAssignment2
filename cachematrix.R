
## makeCacheMatrix ingests a matrix, and also allows the other function cacheSolve to call the 
##variables. It is important to note that the variables x and i are "cached" in an external 
## environment (this is why there is an <<-). 
## To make this possible (external calling) each function is made as a list at the end.

makeCacheMatrix <- function(x = matrix()) {
  i = NULL #initiates i (the inverse) as null
  set <- function(y) { ##the set function stores the matrix and the inverse in the external environment
    x <<- y
    i <<- NULL
  }
  get <- function() x #Gets the original matrix
  setinv <- function(solution) i <<- solution #puts the solution into the external variable
  getinv <- function() i #gets the matrix inverse out
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
#creates a list of the functions for calling externally (from cacheSolve)
}

#-------------------------------------------------------------------------


## Cachesolve computes, caches, and returns matrix inverse OR
#returns the previously cached matrix if i in not null

cacheSolve <- function(x, ...) {
  i = x$getinv() #gets the inverse out of the external environment, if not null, uses and returns
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } #otherwise get the data fresh, solve it, set it, and return it.
  data <- x$get() 
  i <- solve(data, ...)
  x$setinv(i)
  i
}


#-----------------------------------------------------------------------------------
#TESTS------------UNCOMMENT TO USE
#Testing steps to ensure the program is working
#amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#amatrix$get()         # Returns original matrix
#cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
#amatrix$getinv()  # Returns matrix inverse
#cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
#amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
#cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
#amatrix$get()         # Returns matrix
#amatrix$getinv()  # Returns matrix inverse
