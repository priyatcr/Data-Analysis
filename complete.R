complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
nobs <- 0
numcomp <- data.frame(id=NA, nobs=NA)
j <- 1
#access each file in the directory, add "0" or "00" as needed
for (i in seq_along(id)){
	charid <- as.character(id[i])
  	len_charid <- nchar(charid)
  	if (len_charid ==1){
    
      	charid <- paste("00", charid,sep="")}
      	else if (len_charid ==2){ 
        	charid <- paste("0", charid,sep="")
      }
  	filext <- "csv"
  	filename <- paste(charid, filext, sep = ".")
  	filepath <- paste(directory, filename, sep = "/")
  	monitordata <- read.csv(filepath, header=TRUE)
	good <- complete.cases(monitordata) # complete cases logical vector
	gooddata <- monitordata[good,] # get complete cases rows 
	nobs[i] <- nrow(gooddata) # no. of good cases
	numcomp[j,] <- c(id[i], nobs[i]) # output format
	j <- j+1
	}	

	return (numcomp)	

}