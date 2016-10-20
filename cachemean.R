# z<-c(2,3,4,5)
# m1<-makeVector(z)
# cachemean(m1)

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x     #returns original vector of data
  #setmean <- function(mean) m <<- mean 
  setmean <- function(xbar) m <<- xbar 
  getmean <- function() m  #calculates the mean of orig vector
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  ## if m already exists - return it and exit
  if(!is.null(m)) {  
    message("getting cached data")
    return(m)
  }
  
  ## otherwise calcuate m
  data <- x$get()   #retrieve original input
  m <- mean(data, ...)  #calculate the mean
  x$setmean(m)    #set the mean in cached environment
  m
}