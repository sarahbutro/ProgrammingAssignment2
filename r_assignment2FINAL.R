##First function creates a list of the matrix inputed as function argument
#and places for the inverse to be set and retrieved from in a later function

createlist <- function(x = matrix()) {  #calls for the argument to be a matrix
  i <- NULL  #creates a null variable that will be the inverse 
  get <- function() {as.matrix(x)} #creates a function that will return the value of x (the original matrix)
  setinverse <- function(inverse) {i <<- inverse} #creates a function that will be accessed by cachemean to store i
  getinverse <- function() {i} #creates a function that will return i, accessed by cachemean after i is stored
  list(get = get, #creates a list of the internal functions for any other calling function to access
       setinverse = setinverse,
       getinverse = getinverse)
}

#Second function takes the list from the first and uses it to produce and store or retrieve the inverse
cacheinverse <-function(x, ...) { #arguement is the result of the first function
  i <- x$getinverse() 
  if(!is.null(i)) { #checks to see if there is already a value in getverse
    message("getting cached data") #if so returns this message and the value
    return(i)
  }
  data <- x$get() #if not, recalls the data (the matrix) stored in get
  i <- solve(data, ...) #and finds it's inverse
  x$setinverse(i) #runs the inverse through the setinverse function created in createlist
                  #this assigns this value to getinverse as well
                  #so in the future, getinverse will not be null
  i #prints the result
}
cacheinverse(createlist(x))
