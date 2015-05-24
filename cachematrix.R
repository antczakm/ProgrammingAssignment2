## Functions below are used to cache inverse of matrix not to recalculate it if matrix did not change


## Function makeCacheMatrix creates matrix object with 4 methods
## -get matrix/its inverse from the cache
## -set matrix/its inverse in the cache 

makeCacheMatrix <- function(M = matrix()) {
      InvM <- NULL
      setM <- function(N) {
              M <<- N
              InvM <<- NULL
      }
      getM <- function() M
      setInvM <- function(solve) InvM <<- solve
      getInvM <- function() InvM
      list(setM = setM, getM = getM,
      setInvM = setInvM,
      getInvM = getInvM)
}


## Function cacheSolve retrieves matrix inverse from the cache if it was already calculated and computes it if not

cacheSolve <- function(M, ...) {
	InvM <- M$getInvM()
      if(!is.null(InvM)) {
              message("getting cached data")
              return(InvM)
      }
	else {
     		data <- M$getM()
     		InvM <- solve(data, ...)
      	M$setInvM(InvM)
	}
      InvM
}
