
## This function creates a matrix and stores its inverse in the memory. It's necesary to assign makeCacheMatrix to
## a variable
makeCacheMatrix <- function(x = matrix()) {
  
  #First, it sets invers as "NULL"
  matrixInv <- NULL
  
  ## Here, it defines the function that  sets the matrix with the value of "y" and its inverse to NULL.
  ## This function uses the operator "<<-" because both variables, x and matrixInv were defined in the parent
  ## enviroment of this function
  set <- function(y){
    x <<-y
    matrixInv <<- NULL
  }

  ##Here it defines the function that returns the vactual value of the matrix
  get<- function() x
  
  ##Here it defines the function that calculate the inverse of the matrix and set it to the variable matrixInv
  ## It uses the special operator because that variable is in a diferent enviroment
  setsolve <- function(solve) matrixInv <<- solve
  
  ##Here it defines the function that returns the inverse of the matrix that was previously calculated
  getsolve <- function()matrixInv
  
  
  ##In order to get the values of the functions, we define a list where its elements has the same names as the
  ##functions. This will make easier to get the values
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## cacheSolve evaluates if the inverse of the matrix was previously calculated. otherwise it calculates it.

cacheSolve <- function(x, ...) {
  
  ##Here it is trying to get the value of matrixInv of the previous function
  matrixInv <- x$getsolve()
  if(!is.null(matrixInv)){
    message("getting cached data")
    return(matrixInv)
  }
  ## since there's no value asigned to matrixInv, it calculates, getting first the value of x and then calling the
  ## function setsolve to assign the calculated value to matrixInv
  data <- x$get()
  matrixInv <- solve(data,...)
  x$setsolve(matrixInv)
  matrixInv
  
}

