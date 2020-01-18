corr <- function(directory, threshold = 0){
    # 'directory' is a character vector of length 1 indicating the location
    # of the csv files.
    
    # 'threshold' is a numeric vector of length 1 indicating the number of
    # completely observed observations (on all variables) required to
    # compute the correlation between nitrate and sulfate. The default is 0.
    
    # Return a numeric vector of correlations
    # NOTE: Do not round the results!
    
    # Examples:
    # cr <- corr("specdata", 150)
    # head(cr)
    ## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
    # summary(cr)
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313
    
    # cr <- corr("specdata", 400)
    # head(cr)
    ## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
    # summary(cr)
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313
    
    # cr <- corr("specdata", 5000)
    # summary(cr)
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## 
    # length(cr)
    ## [1] 0
    
    # cr <- corr("specdata")
    # summary(cr)
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000
    # length(cr)
    ## [1] 323
    
    # declare id variable as the number (and order) of csv files available
    id = 1:332
    
    # declare initial iteration to be used in for loop
    iterate <- 1:length(id)
    
    # change id data type to numerical vector
    id_vctr <- as.numeric(id)
    
    # declare initial variable as a buffer
    cor2 <- NA
    
    # declare the pollutants available
    pollutant <- c("sulfate","nitrate")
    
    # loop the function all the way of csv files desired
    for (i in iterate) {
        # set format for the index so that it's in 3 digits, eg "001"
        id_idx <- as.character(sprintf("%03d", id_vctr[i]))
        
        # create csv file path to be used
        loc <- paste(as.character(directory),'/',id_idx,'.csv', sep = "")
        
        # calling the csv file
        csv <- read.csv(file = loc)
        
        # save both complete observations (not NA) in buffer variable temporarily
        pollut_buffer1 <- sum(!is.na(csv[,pollutant[1]]))
        pollut_buffer2 <- sum(!is.na(csv[,pollutant[2]]))
        
        # examine whether number of non-NA rows in sulfate col is shorter than
        # or equal to that in nitrate col
        if (pollut_buffer1 <= pollut_buffer2) {
            # create x and y variables as non-NA sulfate and nitrate cols
            x <- csv[!is.na(csv[,pollutant[1]]), pollutant[1]]
            y <- csv[!is.na(csv[,pollutant[1]]), pollutant[2]]
            
            # examine whether number of non-NA rows is greater than or
            # equal to threshold argument
            if (pollut_buffer1 >= threshold) {
                # if yes, calculate correlation between sulfate and nitrate
                cor1 <- cor(x = x, y = y)
            
            # if no, assign as NA
            } else {
                cor1 <- NA
            }
            
        # if number of non-NA rows in sulfate col is longer than that in
        # nitrate col
        } else {
            # create x and y variables as non-NA sulfate and nitrate cols
            x <- csv[!is.na(csv[,pollutant[2]]), pollutant[1]]
            y <- csv[!is.na(csv[,pollutant[2]]), pollutant[2]]
            
            # examine whether number of non-NA rows is greater than or
            # equal to threshold argument
            if (pollut_buffer2 >= threshold) {
                # if yes, calculate correlation between sulfate and nitrate
                cor1 <- cor(x = x, y = y)
                
            # if no, assign as NA
            } else {
                cor1 <- NA
            }
        }
        # collect correlation value from each csv file
        cor2 <- c(cor2, cor1)
        
    }
    # get all correlation results without any NA value and assign
    # the data type of the final results as numeric
    output <- as.numeric(cor2[!is.na(cor2)])
    
    # return the value
    return(output)
}