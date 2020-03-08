## makeCacheMatrix function have four functions
##set_matrix() to set new matrix to 'x' for  finding inverse
##get_matrix() to access the matrix for which inverse has to be calculated
##set_inverse() setting the inverse of a matrix to matrix_inverse
##get_inverse() for accessing the inverse

## makeCacheMatrix will return a list of matrices 

makeCacheMatrix <- function(x = matrix())
  {  if(nrow(x)!=ncol(x))
         print("Not a Square matrix :Can't find inverse")
           
        
    else
   {
  
     matrix_inverse<-NULL
     set_matrix<-function(y)
     { x<<-y
       matrix_inverse<<-NULL
     }
    get_matrix<-function() x
    set_inverse<- function(mat)
    { matrix_inverse<<-mat}
    get_inverse<- function()  
    {  
      matrix_inverse
      }

    list(set_matrix = set_matrix,get_matrix=get_matrix,set_inverse=set_inverse,get_inverse=get_inverse)
  }
}

#cacheSolve function for returning inversee of the matrix

cacheSolve <- function(x, ...) 
{
        
 matrix_inv<-x$get_inverse() #assigning inverse of matrix by calling gey_inverse
 if(!is.null(matrix_inv))  #if condition, to check whether matrix empty or no for returning matrix 
  {  
   print("getting cached matrix")
       return(matrix_inv)
   }
 mat<-x$get_matrix()       #accessing the matrix for which inverse is tobe calculated
 if(det(mat)!=0)      #checking is matrix singular or nonsingular
 {  
   inv_mat<-solve(mat)
   x$set_inverse(inv_mat)
  }
 else
   print("0 determinant found :Matrix inverse not possible")
 
 matrix_inv<-x$get_inverse() #accessing inverse of the matrix
 matrix_inv
}

