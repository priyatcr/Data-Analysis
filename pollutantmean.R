pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    sulf_data <- data.frame()
    nitr_data <- data.frame()
    
    #access each file in the directory, add "0" or "00" as needed
    for (i in seq_along(id)){
        charid <- as.character(id[i])
        print(charid)
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
        if (pollutant =="sulfate"){
            sulf_data <- rbind(sulf_data, monitordata[,2])   
        }
        else{ 
            nitr_data <- rbind(nitr_data, monitordata[,3])
        }
    }
    if (pollutant == "sulfate"){
        #sulf_data <- as.vector(sulf_data)
        bad <- is.na(sulf_data)
        means <- (mean(sulf_data[!bad]))
    }
    else {
        #nitr_data <- as.vector(nitr_data)
        bad <- is.na(nitr_data)
        means <- (mean(nitr_data[!bad]))
    }
    return (means)    
}