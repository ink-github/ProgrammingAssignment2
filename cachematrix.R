##This script contains: 
## 1. MakeCacheMatrix function 
## 2. cacheSOlve function 
## 3. Test Examples for reviewer's convenience

## First, we create a function which creates the cache.
## Then, we create a function with a nested if statement 
## that allows us to retrieve a calculated value from the cache
## if such a value exists. 
## Otherwise, it calculates the inverse afresh.

## How to use these functions?...
## 1. create the matrix object that you want the inverse of
## 2. call the makeCacheMatrix function with the matrix as argument 
## 3. call the cacheSolve function with the above as argument

## makeCacheMatrix Function. Creates a cache. Detailed steps below.  
makeCacheMatrix <- function(x = numeric()) { #creating function makeCacheMatrix 
  m <- NULL                                  #initializing variable m as NULL 
  set <- function(y) {                       #calling function on y and assigning it to set
    x <<- y                                  #value of y assigned to x in a separate environment
    m <<- NULL                               #value of m assigned to be NULL in a separate environment
  }
  get <- function() x                        #the get function get returns the value x  
  setinverse <- function(solve) m <<- solve  #the setinverse function solves to return m to which solve is assigned in a separate environment 
  getinverse <- function() m                 #the getinverse function returns value m 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)              #returns list vector with the elemnts efined above
}

## cacheSolve function. Retrives value from cache if available, or computes inverse afresh.Detailed steps below.
cacheSolve <- function(x, ...) {             #creating function cacheSolve
  m <- x$getinverse()                        #getinverse is called and assigned to m
  if(!is.null(m)) {                          #checking if inverse has already been calculated (i.e. is m not NULL?)
    message("Getting cached data!")          #providing message for user as retrieval occurs
    return(m)                                #printing the inverse 
  }
  data <- x$get()                            #fetching data if no value is available
  m <- solve(data, ...)                      # Return a matrix that is the inverse of the data fed 
  x$setinverse(m)                            # setinverse is given new value 
  m
}

##TEST EXAMPLE 1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)

## TEST RESULT 1
#       [,1] [,2]
#[1,]    6    8
#[2,]    2    4

##TEST EXAMPLE 2
m2 <- matrix(c(3, 6, 12, 4, 6 ,7, 7, 4, 15), nrow = 3, ncol = 3)
myMatrix_object <- makeCacheMatrix(m2)
cacheSolve(myMatrix_object)

#TEST RESULT 2
#          [,1]        [,2]       [,3]
#[1,] -0.3229167  0.05729167  0.1354167
#[2,]  0.2187500  0.20312500 -0.1562500
#[3,]  0.1562500 -0.14062500  0.0312500

