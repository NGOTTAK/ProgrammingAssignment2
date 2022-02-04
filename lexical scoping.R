##Put comments here that give an overall description of what your
## functions do
 
## Write a short comment describing this function
## This function takes a matrix object as argument 
## and returns a list object containing four functions
## these functions having their own environment. whenever the function would be invoked
##,these functions would be copied as well.
## within these functions, set function uses "<<-" operator that assigns
## argument value to x object in parent environment(which can be environment 
## within makecachematrix function).use of lexical scoping is done to advantage
## by putting x object outside the parenthesis in get() function.This is so because
## value of x is now retrieved from parent environment. same is done in getinv
## function. as for setinv, <<- operator assign the input argument to value
##of i in parent environment.

> library(MASS)
> makecachematrix<-function(m=matrix()){
+     inv<-NULL
+     set<-function(y){
+         m<-y
+         inv<-NULL
+     }
+     get<-function()m
+     setinv<-function(inverse)inv<<-inverse
+     getinv<-function(){
+         inver<-ginv(m)
+         inver%*%m
+     }
+     list(set=set , get=get,
+          setinv=setinv,
+          getinv=getinv)
+ }
> ## Write a short comment describing this function
## this function takes the makecachematrix 
## as input argument which further passes list of 4 functions as 
## input arguments. the "getinv"function is invoked first
## to see if cached value in makecachematrix is null or not null. if it is not null,
## the cached inverse can be returned to parent environment. if that is not the
## case, then it retrieves setinv function and return a inversed matrix.

> 
> cachesolve<-function(m,...)
+ {
+     inv<-m$getinv()
+     if(!is.null(inv)){
+         message("getting cached data !")
+         return(inv)
+     }
+     data<-x$get()
+     inv<-solve(data,....)
+     m$setinv(inv)
+     inv ## returns a matrice that is the inverse of "m"
+     
+ }
> f<-makecachematrix(matrix(1:8,2,4))
> f$get()
     [,1] [,2] [,3] [,4]
[1,]    1    3    5    7
[2,]    2    4    6    8
> f$getinv()
     [,1] [,2] [,3] [,4]
[1,]  0.7  0.4  0.1 -0.2
[2,]  0.4  0.3  0.2  0.1
[3,]  0.1  0.2  0.3  0.4
[4,] -0.2  0.1  0.4  0.7
> cachesolve(f)
getting cached data !
     [,1] [,2] [,3] [,4]
[1,]  0.7  0.4  0.1 -0.2
[2,]  0.4  0.3  0.2  0.1
[3,]  0.1  0.2  0.3  0.4
[4,] -0.2  0.1  0.4  0.7
> 