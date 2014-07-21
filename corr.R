corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
corr_result <- vector(mode = "numeric")
filelist <- list.files(directory)
for (i in filelist){
	filename <- paste(directory,i, sep="/")
	monitordata <- read.csv(filename, header=TRUE)
	good <- complete.cases(monitordata) # complete cases vector
	gooddata <- monitordata[good,] # complete cases data
	nobs <- nrow(gooddata) # number of complete observations
  #logic for correlation based on threshold and number of complete observations
	if (nobs > threshold){
		corr_sulnit <- cor(gooddata$sulfate, gooddata$nitrate) 
		corr_result <- append(corr_result, corr_sulnit)
	}
}
	return (corr_result)
}