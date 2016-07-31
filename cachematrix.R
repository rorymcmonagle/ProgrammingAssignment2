## Put comments here that give an overall description of what your
## functions do

## This function:
## 1) stores matrix inserted as the argument of the function as the "get" sub function
## 2) provides a location to store the inverse of the matrix, in "getinv" sub function
## 3) stores the inverse of the matrix internally in the function, once inverse of matrix has been computed in "cacheSolve"

##EXAMPLE:
## m <-matrix(c(7,2,1,0,3,-1,-3,4,-2),3,3)
## > m
##      [,1] [,2] [,3]
## [1,]    7    0   -3
## [2,]    2    3    4
## [3,]    1   -1   -2

## m1 <-  makeCacheMatrix(m)

## m1$get()    <------------"stores m"
##      [,1] [,2] [,3]
## [1,]    7    0   -3
## [2,]    2    3    4
## [3,]    1   -1   -2

## m1$getinverse()   <------------"stores inverse of m"
## NULL            <----------------EMPTY - inverse of m not yet computed



makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


}


## This function first checks if the inverse of the matrix exists

cacheSolve <- function(x, ...) {
        function(x,...){
i <- x$getinverse()                     ## <---------assigning variable i to contents of "getinverse" 
if(!is.null(i)) {                       ## <----------if i is not NULL return i, which is the inverse of the matrix
  message("retrieving cached data")
  return(i)
}
m <- x$get()                            ## <-----------if i is NULL, 
i <- solve(m, ...)
x$setinverse(i)
i
}
}
