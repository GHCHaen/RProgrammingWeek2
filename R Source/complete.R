complete <- function(directory, id = 1:332)
{
    ## 'directory is a character vector of length 1 indicating
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
    
    ## Validate data path
    pm_DataDirectory <- paste(getwd(), "/", directory, sep = "")
    if(!dir.exists(pm_DataDirectory))
    {
        stop(paste("The directory indicated: [", directory,
                   "] does not exist in the current working directory."))
    }
    
    ## Create an empty data frame with 2 columns, id and nobs
    pm_Data <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                        c("id", "nobs"))
    for(pm_InputFile in list.files(pm_DataDirectory))
    {
        ## Check if pm_InputFile is one of the desired monitors
        ## ^0* to exclude preceding 0's for coercing into integer
        ## *(.*)\\..*$ matches everything before last . in file name
        pm_FileId <- sub("^0*(.*)\\..*$", "\\1", pm_InputFile)
        if(!pm_FileId %in% id)
        {
            ## File is not in the list, go to next file.
            next()
        }
        ## Read records from file and filter to only complete cases
        pm_FileData <- read.csv(paste(pm_DataDirectory, "/",
                                      pm_InputFile, sep =""))
        pm_FileData <- pm_FileData[complete.cases(pm_FileData), ]
        
        ## Append new row with the id and number of complete cases
        pm_Data <- rbind(pm_Data, as.data.frame(list(id=pm_FileId,
                                                     nobs=nrow(pm_FileData))))
    }
    
    ## Return the data
    pm_Data
}