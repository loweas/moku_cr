##################
#### GRIDPLOT ####
##################


gridplot <- function(var,x,y, breaks = 5, colori=c("gray90","black"), pch = 16, size = 0.2, legend = FALSE, text.legend ="Legend", ...)
{

### This function plots points with different x an y coordinates
### with different colors based on the value of var

### returns also a list with the colors and the breaks

### default size = 0.2 so small points
### colors can have more than 2 values
### default number of breaks = 5 in the 25% quantile position (4 colors)

### pch = 16 is full dot as default

	if (length(breaks) == 1)			## calculate breaks if no default
	{
		breaks <- quantile(var, probs = seq(0,1,length=breaks))
	}

	colorlist <- colorRampPalette(colori)(length(breaks)-1)
	
	plot(x[var >= breaks[1]], y[var >= breaks[1]], cex=size, pch=pch, col = colorlist[1], ...)		

	for(i in 2:length(colorlist))
	{
		points(x[var >= breaks[i]], y[var >= breaks[i]], col =colorlist[i], cex=size, pch = pch)

	}

	points(x[var > max(breaks)], y[var > max(breaks)], col ="white", cex=size, pch = pch)	## do not plot all values above the max
	
	out <- list(breaks = breaks, colors = colorlist)

	if(legend == TRUE)
	{

		labels = character(length(colorlist))

		for(i in 1:length(labels))
		{
			labels[i] <- paste(round(breaks[i],2), round(breaks[i+1],2), sep = " - ")

		}

		legend("topright",
		legend = labels,
		col = colorlist,
		pch = 15, pt.cex = 1.5, 
		title = text.legend)

#		legend("topright",				## REMOVING THIS COMMENTS ADDS A LEGEND TITLE OF BIGGER SIZE
#		legend = character(length(colorlist)),
#		cex = 1.2, 
#		title = text.legend, bty ='n', 
#		title.adj=-1)					## ADJUST HERE FOR POSITIONING THE TITLE IN THE CORRECT SPOT
	
	}

# "topright",c(" "," "),title="Legend",cex=0.6, bty='n', title.adj=0.15

#	return(out)			## make it not a comment if you want to print always the colors and breaks

### EXAMPLES ###

#
#
# gridplot(var=forc$temp,x=forc$x,y=forc$y, colori=c("gray90","black") )
#
#
# si <- gridplot(var=forc$temp,x=forc$x,y=forc$y, colori=c("yellow","red", "blue"),
# 	   breaks = quantile(forc$temp, probs = seq(0, 1, 0.15)) )
#
#
# gridplot(var=forc$temp,x=forc$x,y=forc$y, colori=c("yellow","green","red","blue"),
# 	   breaks = c(0, 10, 12, 14, 20))
#

}


