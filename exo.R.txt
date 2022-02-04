
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
> #
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