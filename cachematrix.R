## Functions that first creates an empty variable (m) , second 
## creates four functions in a list to evaluate if the matrix  has been cached then
## and third in case to be the matrix cached, shows the current MAtrix. In case 
## the Matrix is not evaluated, execute the command solve to generate the Mˆ-1


## makeCacheMatrix is a function that creates two objects (x and m) which
##are assigned to be evaluated in another environment. X is given by the user and m
## is created to evaluate the existence of the matrix in the parent enviroment.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CAcheSolve is a function that receives x from the user, then evaluate if m is in
## the current environment. If is affirmative, print a message warning there is
## already a matrix and showing the current matrix. 
##If not, execute the inverse of the Matrix and displays the result. The value
## will be in the parent environment for further evaluations.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Already a Matrix, showing result:")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

