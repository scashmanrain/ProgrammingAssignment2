## this pair of functions work together to take a matrix and calculate its inverse.
## If the inverse has already been calcuated- that result is returned rather than recalculating.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x     #returns original vector of data
  setinv <- function(minv) minv <<- minv  #inv passed in from parent...set to local.
  getinv <- function() minv  #calculates the mean of orig vector
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  ## if m already exists - return it and exit
  if(!is.null(minv)) {  
    message("getting cached data")
    return(minv)
  }
  
  ## otherwise calcuate m
  data <- x$get()   #retrieve original input matrix
  minv <- solve(data, ...)  #calculate the inverse matrix
  x$setinv(minv)    #set the inverse in cached environment
  minv
}

#z=rbind(c(1, -1/4), c(-1/4, 1))  
# z=rbind(c(4, 1), c(-3, 1)) 
# m1<-makeCacheMatrix(z)
# cacheSolve(m1)

