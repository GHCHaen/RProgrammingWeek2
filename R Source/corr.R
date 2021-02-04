corr <- function(directory, threshold = 0)
{
    ## 'directory is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    ## Validate data path
    pm_DataDirectory <- paste(getwd(), "/", directory, sep = "")
    if(!dir.exists(pm_DataDirectory))
    {
        stop(paste("The directory indicated: [", directory,
                   "] does not exist in the current working directory."))
    }
    
    ## Create a vector of correlations to return
    pm_Correlations <- numeric()
    
    for(pm_InputFile in list.files(pm_DataDirectory))
    {
        ## Check if pm_InputFile meets the threshold
        ## ^0* to exclude preceding 0's for coercing into integer
        ## *(.*)\\..*$ matches everything before last . in file name
        pm_FileId <- sub("^0*(.*)\\..*$", "\\1", pm_InputFile)
        if(complete(directory, pm_FileId)$nobs > threshold)
        {
            pm_FileData <- read.csv(paste(pm_DataDirectory, "/",
                                          pm_InputFile, sep =""))
            pm_FileData <- pm_FileData[complete.cases(pm_FileData), ]
            
            pm_Correlations <- c(pm_Correlations, cor(pm_FileData$sulfate,
                                                      pm_FileData$nitrate))
        }
    }
    
    pm_Correlations
}