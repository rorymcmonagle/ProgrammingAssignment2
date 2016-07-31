

## This function:
## 1) stores matrix inserted as the argument of the function as the "get" sub function
## 2) provides a location to store the inverse of the matrix, in "getinverse" sub function
## 3) stores the inverse of the matrix internally in the function, once inverse of matrix has been computed in "cacheSolve"


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL                                     ## Inverse of matrix not yet computed so "inv" is NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                           ## "get" object stores matrix
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv                  ## "getinverse" object stores inverse of matrix, which is not yet computed, so is NULL
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

##EXAMPLE: (PART 1)
## m <-matrix(c(7,2,1,0,3,-1,-3,4,-2),3,3)
## > m
##      [,1] [,2] [,3]
## [1,]    7    0   -3
## [2,]    2    3    4
## [3,]    1   -1   -2

## m1 <-  makeCacheMatrix(m)

## m1$get()    <------------"retrieves m"
##      [,1] [,2] [,3]
## [1,]    7    0   -3
## [2,]    2    3    4
## [3,]    1   -1   -2

## m1$getinverse()   <------------"retrieves inverse of m"
## NULL            <----------------EMPTY - inverse of m not yet computed
## NEED TO RUN cacheSolve 

##----------------------------------------------------------


## This function first checks if the inverse of the matrix exists

cacheSolve <- function(x, ...) {

inv <- x$getinverse()                     ## <---------assigning variable inv to contents of "getinverse" 
if(!is.null(inv)) {                       ## <----------if inv is not NULL(i.e. inverse of matrix already computed) return inv, 
  message("retrieving cached data")       ## which is the inverse of the matrix
  return(inv)
}
data <- x$get()                            ## <-----------if inv is NULL, inverse has not been computed, so compute now
inv <- solve(data, ...)
x$setinverse(inv)
inv
}

##EXAMPLE: (continuing from PART 1 above)
## To find inverse of m, run cacheSolve on m1 created above:
## > cacheSolve(m1)
##      [,1] [,2] [,3]
## [1,]   -2    3    9
## [2,]    8  -11  -34
## [3,]   -5    7   21

## THIS IS THE INVERSE OF m

## You can check that the inverse has now been cached:
## > m1$getinverse()
##      [,1] [,2] [,3]
## [1,]   -2    3    9
## [2,]    8  -11  -34
## [3,]   -5    7   21

## Now when you run cacheSolve again, it does not compute the inverse, but retrieves it from your cache:
## > cacheSolve(m1)
## retrieving cached data         <--------note message telling user data (matrix inverse) is retrieving from cache
##      [,1] [,2] [,3]
## [1,]   -2    3    9
## [2,]    8  -11  -34
## [3,]   -5    7   21
