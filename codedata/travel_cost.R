###############################
##### TRAVEL COST PACKAGE #####
###############################

# 2 functions to calculate welfare measures from travel cost models

# site.closure = WTP to avoid the closure of one of more sites
# quality.change = WTP arising from changes in attributes of one or more sites

######################
#### SITE.CLOSURE ####
######################

## This function returns the WTP for site closure for one single choice
## occastion for a vector of respondents

## The sites which are closed are the same for all respondents

## INPUT:
## beta = parameters of the multisite travel cost
## cost = position of the cost parameter, default = 1
## X = matrix of explanatory variables (needs to be in the right order)
## siteid = id for the sites
## pid = individual ids (respondents, grid squares, postcodes etc.)
## vec = vector of site ids which are closed !!

## OUTPUT: A daframe with:
##  respondent ID, WTP and the total probability of 
##  going to the sites that are close
##  formula is based on site visit probability, formula 8.47 in Haab and McConnell 2002


site.closure <- function(beta, X, siteid, pid, vec, cost =1)
{
	eq <- X %*% beta
	num = exp(eq)

	U <- data.frame(pid, siteid, num)
	
	den <- tapply(num, pid, sum)	
	idt <- as.numeric(names(den))
	den <- data.frame(pid = idt,den)
	
	U <- merge(U,den, all = TRUE)

	U$prob <- U$num / U$den
	
	U$out <- U$siteid %in% vec

	Pout <- tapply(U$prob * U$out, U$pid, sum)		## sum of the probabilities for all the sites that close
	
	WTP <- log(1-Pout)/beta[cost]
	WTP <- data.frame(id = idt, WTP)
	
	out <- WTP
	out$prob <- Pout
	
	return(out)

}


########################
#### QUALITY.CHANGE ####
########################

## This function returns the WTP for quality changes
## to series of sites for a set of respondents

## INPUT:
## beta = parameters of the multisite travel cost
## cost = position of the cost parameter, default = 1
## X0 = matrix of explanatory variables in the baseline (needs to be in the right order)
## X1 = matrix of explanatory variables in the sceanario (needs to be in the right order)
## pid = individual ids (respondents, grid squares, postcodes etc.)

## OUTPUT:
## A dataframe with the respondent ID and the WTP

quality.change <- function(beta, X0, X1, pid, cost =1)
{

### logsum for the baseline (X0)

	eq <- X0 %*% beta
	num = exp(eq)

	U <- data.frame(pid, num)
	
	den <- tapply(num, pid, sum)	
	idt <- as.numeric(names(den))
	denU0 <- data.frame(pid = idt,den)
	denU0$logsum <- log(denU0$den)

### logsum for the scenario (X1)
	
	eq <- X1 %*% beta
	num = exp(eq)

	U <- data.frame(pid, num)
	
	den <- tapply(num, pid, sum)	
	idt <- as.numeric(names(den))
	denU1 <- data.frame(pid = idt,den)
	denU1$logsum <- log(denU1$den)

##	
	WTP <- -(denU1$logsum - denU0$logsum)/beta[cost]
	WTP <- data.frame(id = idt, WTP)

	return(WTP)

}




