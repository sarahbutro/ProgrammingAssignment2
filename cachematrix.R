## These functions work together to create and store the inverse of a matrix 
## so that it does not need to be computed every time the function is run

##First function creates a list of functions to be accessed by the second calling function
##the argument is a matrix

createlist <- function(x = matrix()) {  
  i <- NULL  #creates a null variable that will be the inverse 
  get <- function() {as.matrix(x)} #function that will return the value of x (the original matrix)
  setinverse <- function(inverse) {i <<- inverse} #function that will be accessed by cacheinverse to store i
  getinverse <- function() {i} #function that will return i, accessed by cacheinverse after i is stored
  list(get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

#Second function takes the list from the first and uses it to produce and store or retrieve the inverse
#the argument is the result of the first function
#this function fist checks to see if there is already a value in getverse, if so, it returns a message and the value
#if not, it recalls the data (matrix) stored in get, finds its inverse, assigns this value to getinverse
#and prints the inverse. 

cacheinverse <-function(x, ...) { 
  i <- x$getinverse() 
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  data <- x$get() 
  i <- solve(data, ...) 
  x$setinverse(i) 
  i
}
