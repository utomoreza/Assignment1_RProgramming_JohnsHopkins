complete <- function(directory, pollutant = "both", id = 1:332){
    # 'directory' is a character vector of length 1 indicating the location
    # of the csv files.
    
    # 'pollutant' is a character vector of either length 1 or 2 indicating
    # which number of complete cases of the pollutant to be shown or that of
    # both (sulfate and nitrate: default value) to be shown.
    
    # 'id' is an integer vector indicating the monitor ID numbers to be used.
    
    # Return a data frame of the form, for example:
    ## id   sulfate nitrate
    ## 1    117     122
    ## 2    1041    1051
    ## ...
    # where 'id' is the monitor ID number, and both 'sulfate' and 'nitrate'
    # show the number of complete cases for each pollutant.
    
    # Examples:
    # complete("specdata", 1)
    ##   id nobs
    ## 1  1  117
    
    # complete("specdata", c(2, 4, 8, 10, 12))
    ##   id nobs
    ## 1  2 1041
    ## 2  4  474
    ## 3  8  192
    ## 4 10  148
    ## 5 12   96
    
    # complete("specdata", 30:25)
    ##   id nobs
    ## 1 30  932
    ## 2 29  711
    ## 3 28  475
    ## 4 27  338
    ## 5 26  586
    ## 6 25  463
    
    # complete("specdata", 3)
    ##   id nobs
    ## 1  3  243
    
    # declare initial iteration to be used in for loop
    iterate <- 1:length(id)
    
    # change id data type to numerical vector
    id_vctr <- as.numeric(id)
    
    # examine whether we want to show nobs of sulfate, nitrate, or both
    if (pollutant == "both") {
        pollut <- c('sulfate','nitrate')
        
        # declare initial variable as a buffer
        nobs1 <- c(0)
        nobs2 <- c(0)
        
        # loop the function all the way of csv files desired
        for (i in iterate) {
            # set format for the index so that it's in 3 digits, eg "001"
            id_idx <- as.character(sprintf("%03d", id_vctr[i]))
            
            # create csv file path to be used
            loc <- paste(as.character(directory),'/',id_idx,'.csv', sep = "")
            
            # calling the csv file
            csv <- read.csv(file = loc)
            
            # save both complete observations (not NA) in buffer variable temporarily
            pollut_buffer1 <- sum(!is.na(csv[,pollut[1]]))
            pollut_buffer2 <- sum(!is.na(csv[,pollut[2]]))
            
            # combine nobs from each csv file in one file
            nobs1 <- c(nobs1,pollut_buffer1)
            nobs2 <- c(nobs2,pollut_buffer2)
            
            # getting each pollutant's name for visualising
            nobs1_name <- pollut[1]
            nobs2_name <- pollut[2]
        }
        
        # create data frame to show the results
        completedf <- data.frame(id_vctr, nobs1[2:length(nobs1)], nobs2[2:length(nobs2)])
        
        # rename each columns accordingly
        colnames(completedf) <- c("id",nobs1_name,nobs2_name)
        
        # return the value
        return(completedf)
        
    # if pollutant selected is not both
    } else {
        # if pollutant selected is sulfate
        if (pollutant == "sulfate"){
            pollut <- pollutant }
        
        # if pollutant selected is nitrate
        else if (pollutant == "nitrate"){
            pollut <- pollutant }
        
        # if pollutant argument entered is illegal
        else {
            stop('Pollutant selected not found. You must choose either "sulfate" to show sulfate
             pollutant, "nitrate" to show nitrate pollutant, or "both" to show both.') }
        
        # declare initial variable as a buffer
        nobs <- c(0)
        
        # loop the function all the way of csv files desired
        for (i in iterate) {
            # set format for the index so that it's in 3 digits, eg "001"
            id_idx <- as.character(sprintf("%03d", id_vctr[i]))
            
            # create csv file path to be used
            loc <- paste(as.character(directory),'/',id_idx,'.csv', sep = "")
            
            # calling the csv file
            csv <- read.csv(file = loc)
            
            # save the complete observations (not NA) in buffer variable temporarily
            pollut_buffer <- sum(!is.na(csv[,pollut]))
            
            # combine nobs from each csv file in one file
            nobs <- c(nobs,pollut_buffer)
            
            # getting the pollutant's name for visualising
            nobs_name <- paste("nobs of", pollut, sep = " ")
        }
        # create data frame to show the results
        completedf <- data.frame(id_vctr, nobs[2:length(nobs)])
        
        # rename each columns accordingly
        colnames(completedf) <- c("id",nobs_name)
        
        # return the value
        return(completedf)
        }
}