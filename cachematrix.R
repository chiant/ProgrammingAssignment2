## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. The following two functions are designed to cache the 
## inverse of a matrix.


## The first function, `makeCachMatrix` creates a special "matrix", 
## which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the inverse of the matrix 
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then the `cachesolve` 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached matrix")
        }
        else{
                data <- x$get()
                m <- solve(data, ...)
                x$setinv(m)
        }
        return(m)
        ## Return a matrix that is the inverse of 'x'
}

## Examples
## m1<-matrix(c(1,2,3,4),nrow=2,ncol=2)
## m2<-matrix(c(1,2,3,4,1,6,1,8,9),nrow=3,ncol=3)
## cm<-makeCacheMatrix(m1)
## cm$get()
## cacheSolve(cm)
## cm$getinv()
## cm$set(m2)
## cacheSolve(cm)

