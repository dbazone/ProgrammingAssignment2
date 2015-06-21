## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverted_matrix <- NULL
	
	
	set <- function(new_matrix) {
		x <<- new_matrix
		inverted_matrix <<- NULL
	}
	
	get <- function() x
	
	## setting the value in cache
	set_invertible_matrix <- function(z)  inverted_matrix <<- z
	
	## getting the value from cache
	get_invertible_matrix <- function () inverted_matrix
	
	list(set = set, get = get,
		set_invertible_matrix = set_invertible_matrix,
		get_invertible_matrix = get_invertible_matrix)
		
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		##try to get the matrix from cache first
		inv_matrix<-x$get_invertible_matrix()
		
		##if the value is available in cache then return cached matrix
		if (!is.null(inv_matrix)){
			message("getting cached data")
			return (inv_matrix)
		}
		
		##invert the matrix for the first time
		data <-x$get()
		inv_matrix <- solve(data)
		
		##setting the inverted matrix in cache
		x$set_invertible_matrix(inv_matrix)
		
		inv_matrix
		
}
