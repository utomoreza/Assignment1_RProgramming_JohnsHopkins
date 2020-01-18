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
    
    iterate <- 1:length(id)
    
    id_vctr <- as.numeric(id)
    
    if (pollutant == "both") {
        pollut <- c('sulfate','nitrate')
        
        nobs1 <- c(0)
        nobs2 <- c(0)
        # browser()
        for (i in iterate) {
            id_idx <- as.character(sprintf("%03d", id_vctr[i]))
            # browser()
            loc <- paste(as.character(directory),'/',id_idx,'.csv', sep = "")
            # browser()
            csv <- read.csv(file = loc)
            # browser()
            pollut_buffer1 <- sum(!is.na(csv[,pollut[1]]))
            pollut_buffer2 <- sum(!is.na(csv[,pollut[2]]))
            # browser()
            nobs1 <- c(nobs1,pollut_buffer1)
            nobs2 <- c(nobs2,pollut_buffer2)
            
            nobs1_name <- pollut[1]
            nobs2_name <- pollut[2]
            # browser()
        }
        
        completedf <- data.frame(id_vctr, nobs1[2:length(nobs1)], nobs2[2:length(nobs2)])
        colnames(completedf) <- c("id",nobs1_name,nobs2_name)
        browser()
        return(completedf)
        
    } else {
        if (pollutant == "sulfate"){
            pollut <- pollutant }
        else if (pollutant == "nitrate"){
            pollut <- pollutant }
        else {
            stop('Pollutant selected not found. You must choose either "sulfate" to show sulfate
             pollutant, "nitrate" to show nitrate pollutant, or "both" to show both.') }
        
        nobs <- c(0)
        browser()
        for (i in iterate) {
            id_idx <- as.character(sprintf("%03d", id_vctr[i]))
            browser()
            loc <- paste(as.character(directory),'/',id_idx,'.csv', sep = "")
            browser()
            csv <- read.csv(file = loc)
            
            pollut_buffer <- sum(!is.na(csv[,pollut]))
            browser()
            nobs <- c(nobs,pollut_buffer)
            browser()
            nobs_name <- paste("nobs of", pollut, sep = " ")
        }
        
        completedf <- data.frame(id_vctr, nobs[2:length(nobs)])
        colnames(completedf) <- c("id",nobs_name)
        browser()
        return(completedf)
        }
}