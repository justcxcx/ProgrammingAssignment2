## Below are two functions that are used to create a special 
## object that stores the numeric matrix and cache the inverse 
## of the matrix

## The function  makeCacheMatrix creates a special "matrix"
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        inv <- NULL
        set <- function(y){
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The function cacheSolve calcuates the inverse of the special
## "matrix" created by the above function. It first checks to
## see if the inverse has been calcuated. If so, it gets the inverse
## from the cache directly.Otherwise, it calcuates the inverse and store
## the inverse in the cache via the function set_inverse()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inverse(inv)
        inv
}
