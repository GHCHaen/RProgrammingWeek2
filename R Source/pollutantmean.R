pollutantmean <- function(directory, pollutant, id = 1:332)
{
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    ## Validate data path
    pm_DataDirectory <- paste(getwd(), "/", directory, sep = "")
    if(!dir.exists(pm_DataDirectory))
    {
        stop(paste("The directory indicated: [", directory,
                   "] does not exist in the current working directory."))
    }
    
    ## Create a data frame from the contents of the files in 'directory'
    pm_Data <- data.frame()
    for(pm_InputFile in list.files(pm_DataDirectory))
    {
        ## Check if pm_InputFile is one of the desired monitors
        ## ^0* to exclude preceding 0's for coercing into integer
        ## *(.*)\\..*$ matches everything before last . in file name
        if(!sub("^0*(.*)\\..*$", "\\1", pm_InputFile) %in% id)
        {
            ## File is not in the list, go to next file.
            next()
        }
        
        pm_Data <- rbind(pm_Data, read.csv(paste(pm_DataDirectory, "/",
                                                 pm_InputFile, sep ="")))
    }
    
    ## Verify 'pollutant' value provided exists in data set
    if(!pollutant %in% colnames(pm_Data))
    {
        stop(paste("The pollutant value provided: [", pollutant,
                   "] does not exist in the data set."))
    }
    
    ## Get non-NA values for provided 'pollutant' from data set
    pm_GoodPollutantValues <- pm_Data[[pollutant]][!is.na(pm_Data[[pollutant]])]
    
    ## Return mean of pm_GoodPollutantValues
    mean(pm_GoodPollutantValues)
}