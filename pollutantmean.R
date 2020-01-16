pollutantmean <- function(directory, pollutant, id) {
    # 'directory' is a character vector of length 1 indicating the location
    # of the csv files.
    
    # the 'pollutant' is a character vector of length 1 indicating the name
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
    # pollutantmean("specdata", "nitrate", 23)
    ## [1] 1.280833
}