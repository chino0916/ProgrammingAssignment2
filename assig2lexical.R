makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmean <- function(mean) invertion <<- mean
  getmean <- function() inv
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  mat <- x&get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  invertion

  }