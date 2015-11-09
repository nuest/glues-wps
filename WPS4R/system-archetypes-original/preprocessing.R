# main preprocessing function
sampleAndCombineData <- function(inputFiles, size, type) {
    myLog("Loading data from ", length(inputFiles), " datasets")
    
    rasterList <- list()
    
    for (currentFile in inputFiles) {
        fileName <- paste0(dataPath, "/", currentFile)
        # testing: fileName <- paste0(dataPath, "/", "crop2005")
        myLog("Processing ", fileName, " - file exists: ", file.exists(fileName))
        
        raster <- raster(fileName)
        myLog("Current raster: ", toString(capture.output(summary(raster))))
        
        # add raster to the raster list
        rasterList[[length(rasterList)+1]] <- raster 
        
        # if this is the first raster, run the sampling
        if(!exists("theDF")) {
            first <- FALSE
            myLog("Running sampling based on raster ", names(raster), " from file ",
                  fileName)
            
            # sample data
            # the aim of this step is twofold:
            #   1) reduce the numberof datapoints for the analysis
            #   2) reduce the problem of spatial auto correlation
            
            spdf <- as(raster, "SpatialPixelsDataFrame")
            summary(spdf)
            sampelPixels <- spsample(spdf, n = size, type = type)
            samplePoints <- as(sampelPixels, "SpatialPoints")
            theDF <- cbind(coordinates(samplePoints))
        }
        
        rasterSampled <- extract(raster, samplePoints) 
        theDF <- cbind(theDF, rasterSampled)
    }
    
    # convert to data.frame
    theDF <- as.data.frame(theDF)
    names(theDF) <- c("x", "y", files)
    myLog("Created data frame with size ", toString(dim(theDF)), " and variables ",
          toString(names(theDF)))
    
    return(theDF)
}