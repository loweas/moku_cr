# DUMMIER 

# This function takes a numeric vector, eg. c(1,2,3), and returns a matrix with
# the correspondigly dummy variables, eg. c([1,0,0],[0,1,0],[0,0,1])

dummier <- function (vec, name="D.")
{

# This function takes a numeric vector, eg. c(1,2,3), and returns a matrix with
# the correspondigly dummy variables, eg. c([1,0,0],[0,1,0],[0,0,1])

	a<-matrix()
	names <- vector()

	vec<-factor(vec)
	a<-diag(nlevels(vec))[vec,]
	
	if(length(name) == 1) { names <-paste(name,levels(vec),sep="") }		# user provides one word name and then paste variable value, e.g. w1, w2, w3 ..
	if(length(name) > 1)  { names <-name }					# full list of names provided, e.g. jan, feb, mar, ...

	if (length(names)!= ncol(a)) { print("Warning: labes have a different dimension then number of colums") }		

	colnames(a)<-names
		

	return(a)
}