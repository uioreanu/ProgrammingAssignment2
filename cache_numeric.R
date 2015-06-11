#########
# creating a functions container, then 
# cache the output of the solve() function
# Usage example below
#########

#########
## makeCacheMatrix creates a list of functions to set an object, retrieve it,
## but also set and get a generalised cached attached value (mean, solve, max etc)
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setCache <- function(cacheValue) cache <<- cacheValue
        getCache <- function() cache
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}


#########
## cacheSolve checks to see if the object x has a cached property already and returns it,
## or else calculates the cached property, "saves" it into the object then returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getCache()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setCache(cache)
        cache
}


#  # create the functions container
#  Matrix <- makeCacheMatrix()
#
#  # attach an invertible 2 x 2 matrix 
#  Matrix$set(rbind(c(1, -1/4), c(-1/4, 1)))
#
#  # retrieve the inverse and trigger caching
#  cacheSolve(Matrix)
#  #....
#
#  # a second and all further calls will retrieve the result from the cache
#  cacheSolve(Matrix)
#  #getting cached data
#  #....

