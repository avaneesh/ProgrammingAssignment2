# Here I provide a way to cache inverse of a matrix
# makeCacheMatrix() creates a special 'matrix' such that it bundles
# getters and setters to provided caching of inverse of a matrix

# Another function, cacheSolve() will use this 'special matrix' in a manner
# such that cached value is looked at first.

# This helps reduce computation when inverse is needed to be computed frequently


# Creates 'CachedMatrix' that is capable of caching the result of Inverse
# Input: Any matrix, but assumption is that is square and invetible
# Returns: List of functions such that 'cached' operations can be acted upon
makeCacheMatrix <- function(m = matrix()) {
    # Variable to store inverse at this function's scope
    inverse <- NULL
    
    setMatrix <- function(x) {
        m <<- x
        inverse <<- NULL
    }
    
    getMatrix <- function() m
    
    # When setInverse is called, we want to save it in parent's environment
    # This will be returned when 'cached' value is required
    setInverse <- function(newInverse) inverse <<- newInverse
    
    # Return the 'cached' inserve, 
    # For getInverse(), its a free variable, so scope will be resolved to
    # where its defined, hence we get 'inverse' defined in 'makeCacheMatrix()'
    getInverse <- function() inverse

    # Return list of functions such that caller can make use of 
    # cache result of inverse.
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Gets cached inverse if available.
# Input: matrix created using makeCacheMatrix()
# Returns: Inverse of matrix.
# Returns Error if matrix was not invertible.
cacheSolve <- function(my_matrix) {
    
    # First check if cache exists 
    cachedInverse <- my_matrix$getInverse()
    if(!is.null(cachedInverse)){
        message("Getting cached inverse!!")
        # Returned cached value of inverse
        return(cachedInverse)
    }
    
    # Cache of inverse does not exist.
    # Let's "solve" that, by using solve()    
    matrixData <- my_matrix$getMatrix()
    # Get "actual" matrix and run solve() on it
    computedInverse <- solve(matrixData)
    
    # Cache newly computed Inverse,
    # so that future calls can take advantage of cache
    my_matrix$setInverse(computedInverse)
    
    # Retun newly computed inverse
    computedInverse
}
