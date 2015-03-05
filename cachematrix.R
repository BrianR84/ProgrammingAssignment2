# Below are a pair of functions that calculate the inverse of a matrix and cache
# the results to reduce repeated computations of the inverse of an unchanged
# matrix.

# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse matrix to NULL
    inv <- NULL
    
    # Function to set the value of a matrix
    # Inverse matrix is set back to null as matrix is being updated
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to retrieve the matrix
    get <- function () x
    
    # Function to set the value of the inverse matrix
    setInv <- function(inverse) inv <<- inverse
    
    # Function to retrieve the inverse matrix
    getInv <- function() inv
    
    # Returns a list which contains the above functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not 
# changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Retrieve cached inverse
    inv <- x$getInv()
    
    # Return the cached inverse if it exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Retrieve matrix for inverse calculation in the case of no cached inverse
    data <- x$get()
    
    # Calculate the inverse of the matrix
    inv <- solve(data, ...)
    
    # Set the calculated inverse matrix in the cache
    x$setInv(inv)
    
    # Return a matrix that is the inverse of 'x'
    inv
}