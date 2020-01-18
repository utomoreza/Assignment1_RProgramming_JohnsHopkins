pollutantmean <- function(directory, pollutant, id = 1:332) {
    # 'directory' is a character vector of length 1 indicating the location
    # of the csv files.
    
    # The 'pollutant' is a character vector of length 1 indicating the name
    # of the pollutant for which we will calculate the mean; either 'sulfate'
    # or 'nitrate'.
    
    # 'id' is an integer vector indicating the monitor ID numbers to be used.
    
    # Return the mean of the pollutant across all monitors list in the 'id'
    # vector (ignoring NA values).
    # NOTE: Do not round the result!
    
    # Examples:
    # pollutantmean("specdata", "sulfate", 1:10)
    ## [1] 4.064128
    # pollutantmean("specdata", "nitrate", 70:72)
    ## [1] 1.706047
    # pollutantmean("specdata", "nitrate", c(23,101,254,314))
    ## [1] 1.28083311
    
    # declare initial variable as a buffer
    buffer <- c(0)
    
    # declare initial iteration to be used in for loop
    iterate <- 1:length(id)
    
    # change id data type to numerical vector
    id_vctr <- as.numeric(id)
    
    # loop the function all the way of csv files desired
    for(i in iterate){
        # set format for the index so that it's in 3 digits, eg "001"
        id_idx <- as.character(sprintf("%03d", id_vctr[i]))
        
        # create csv file path to be used
        loc <- paste(as.character(directory),'/',id_idx,'.csv', sep = "")
        
        # calling the csv file
        csv <- read.csv(file = loc)
        
        # subset the pollutant type
        pollut <- csv[,pollutant]
        
        # save the pollutant observations in buffer variable temporarily
        buffer <- c(buffer, pollut)
    }
    
    # calculate mean of the pollutant observations (by excluding the first idx of buffer var)
    avg <- mean(buffer[2:length(buffer)], na.rm = T)
    
    # return the value
    return(avg)
}