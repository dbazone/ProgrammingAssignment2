## The objective of this script is to cache the values which invloves matrix inversion
## value would be return from the cached if the input matrix is not changed


## this function provides the service of caching; calling program can 
## set the input matrix return the inverted values
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


## this function actually calls the cachematrix function it first checks if the inverse value is already present in the memory 
## if not then it will inverse the matrix for the first time and the subsequent run for the same matrix will return the inverted 
## data from the cache

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



## > original_matrix <- matrix(rnorm(25),5,5)
##************************************************************************************
## > original_matrix
##            [,1]       [,2]      [,3]       [,4]       [,5]
##[1,] -0.4015762 -0.3888382  1.036893  1.2814553  0.7011080
##[2,]  1.7116445  0.3187455 -1.897801 -1.0403066  0.8210861
##[3,] -1.3045834  1.4978945 -1.067493 -1.0006155  0.2479922
##[4,]  0.4071550 -1.6320133  2.149779 -0.1424232 -2.1639016
##[5,] -0.1516166 -1.2265478 -1.336543  0.9742509 -0.2321613
##
##************************************************************************************
## > cache_function = makeCacheMatrix(original_matrix)
##
##$set
##function (new_matrix) 
##{
##    x <<- new_matrix
##    inverted_matrix <<- NULL
##}
##<environment: 0x0000000017908020>

##$get
##function () 
##x
##<environment: 0x0000000017908020>
##
##$set_invertible_matrix
##function (z) 
##inverted_matrix <<- z
##<environment: 0x0000000017908020>
##
##$get_invertible_matrix
##function () 
##inverted_matrix
##<environment: 0x0000000017908020>
##
##
##************************************************************************************
## >cacheSolve(cache_function)
##           [,1]      [,2]       [,3]       [,4]       [,5]
##[1,] -1.2462633 -0.536290 -1.2701947 -0.7160912 -0.3426629
##[2,] -2.4053178 -1.613102 -1.7822796 -1.5299176 -0.6128669
##[3,]  0.5197684  0.168924  0.1818929  0.2828665 -0.2751197
##[4,] -1.9786091 -1.552352 -1.8564237 -1.4274135 -0.1439939
##[5,]  2.2261989  1.385688  1.4080886  0.9319661  0.1339024
##************************************************************************************
## > cacheSolve(cache_function)
##getting cached data
##           [,1]      [,2]       [,3]       [,4]       [,5]
##[1,] -1.2462633 -0.536290 -1.2701947 -0.7160912 -0.3426629
##[2,] -2.4053178 -1.613102 -1.7822796 -1.5299176 -0.6128669
##[3,]  0.5197684  0.168924  0.1818929  0.2828665 -0.2751197
##[4,] -1.9786091 -1.552352 -1.8564237 -1.4274135 -0.1439939
##[5,]  2.2261989  1.385688  1.4080886  0.9319661  0.1339024