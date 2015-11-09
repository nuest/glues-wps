################################################################################
#
# System Archetypes Analysis Script for WPS4R
#
# Steps:
#   1) prepare input data for further analysis in R
#   2) cleaning (no NAs) and standardizing data
#   3) use self-organzing maps for land use classification
#
# Script authors: sven.lautenbach@ufz.de, tomas.vaclavik@ufz.de
# WPS editor: d.nuest@52north.org
#
################################################################################

################################################################################
# WPS4R metadata and resources
# wps.des: id = glues.systemarchetypes, title = SOM for land use system archetypes,
# abstract = "Open land use analysis: process loads input files, then preprocesses them and creates a classification based on a self-organizing map algorithm";

# wps.resource: Input_Data_v2;

# wps.metadata: title = GLUES story page 'Global map provides new insights into land use', href = http://geoportal-glues.ufz.de/stories/landsystemarchetypes.html;
# wps.metadata: title = GLUES story page 'Open analysis of global land use', href = http://geoportal-glues.ufz.de/stories/openanalysis.html;
# wps.metadata: title = Journal paper: Mapping global land system archetypes, href = http://www.sciencedirect.com/science/article/pii/S0959378013001532;
# wps.metadata: title = Output dataset in GLUES catalogue (dataset), href = http://catalog-glues.ufz.de/terraCatalog/Query/ShowCSWInfo.do?fileIdentifier=glues:ufz:metadata:dataset:landsystemarchetypes;
# wps.metadata: title = Output dataset in GLUES catalogue (service), href = http://catalog-glues.ufz.de/terraCatalog/Query/ShowCSWInfo.do?fileIdentifier=e9d85591-9ed1-49a1-8251-571f727ae433;

################################################################################
# required packages
require(sp)
require(raster)
require(kohonen)
require(vegan)
require(maptools)

################################################################################
# functions and importing of externalized functions
myLog <- function(...) {
    cat(paste0("[glues.systemarchetypes] ", Sys.time(), " | ", ..., "\n"))
}

# wps.import: basic-plots.R;
# wps.import: advanced-plots.R;
# wps.import: preprocessing.R;


################################################################################
# use this for testing
#wps.off;
#setwd(tempdir())
setwd("C:\\Users\\Daniel\\Documents\\52N-Projects\\2014_GLUES\\WPS4R\\System-Archetypes")
#wps.on;



################################################################################
# 1) Preprocessing
################################################################################

myLog("#### Start preprocessing (1/3) ... ")
myLog("Workspace: ", getwd())

# wps.in: id = dataPath, type = string,
# title = the path to the data layers, minOccurs = 0, maxOccurs = 1,
# abstract = "the path to the data layers, must be a local server path",
# value = Input_Data_v2;
#wps.off;
dataPath <- "Input_Data_v2"
#wps.on;

# the order of the files matters, since the conversion to SpatialPixelsDataFrame
# to do the sampling does not work for all raster layers...
files <- c(#"crop2005",
           #"cropdif50",
           #"grass2005",
           #"grassdif50",
           "nfert",
           "irrigation",
           "totsederosion",
           "y_wheat",
           "y_maize",
           "y_rice",
           "yg_wheat",
           "yg_maize",
           "yg_rice",
           "tpi_agr",
           "hanpp",
           "bio1",
           "bio2",
           "bio12",
           "bio15",
           "bio21",
           "temp_anom",
           "ndvi_mean",
           "ndvi_sd",
           "soil_orgc",
           "spec_rich",
           "gdp",
           "agr_gdp",
           "gcs_agr",
           "pop_glds00ag",
           "popdif50",
           "polstability",
           "cropdif50",
           "crop2005",
           "grass2005",
           "grassdif50",
           "accessibility")
myLog("Input datasets: ", toString(files))

# wps.in: id = sampleSize, type = integer, minOccurs = 0, maxOccurs = 1,
# title = size of the sampling,
# abstract = the number of sampling cells that are used in spsample,
# value = 1000000;
# wps.in: id = samplingType, type = string, minOccurs = 0, maxOccurs = 1,
# title = sampling strategy,
# abstract = "strategy of the sampling used in spsample (e.g. random, regular, 
# stratified, nonaligned, hexagonal, clusted or Fibonacci)",
# value = regular;
#wps.off;
sampleSize <- 100 # 1*10^6 # 100
samplingType <- "regular"
#wps.on;

myLog("Sample size: ", sampleSize, " | sampling type: ", samplingType)

#wps.off;
#testRaster <- raster(paste0(dataPath, files[10]))
#testRaster
#summary(testRaster)
#as(testRaster, "SpatialPixelsDataFrame")
# DN: ERROR, happens with several rasters I tested...
# Error in as.factor(f[[i]][v[, i]]) : 
#Fehler bei der Auswertung des Argumentes 'x' bei der Methodenauswahl
#für Funktion 'as.factor': Error in `[.data.frame`(f[[i]], v[, i]) : undefined columns selected
#wps.on;


# do the sampling
data <- sampleAndCombineData(files, sampleSize, samplingType)

# save statistics and data to files
sampleBaseDataStatistics <- "sample-data_statistics.txt"
capture.output(summary(data), file = sampleBaseDataStatistics)
# wps.out: sampleBaseDataStatistics, type = text, 
# title = statistics of dataset,
# abstract = the output of a summary operation on the created data.frame;
myLog("Saved summary statistics in file ", sampleBaseDataStatistics)
sampledBaseData <- paste0("inputData_", as.integer(sampleSize), "_",
                          samplingType, ".Rdata")
save(data, file = sampledBaseData)
# wps.out: sampledBaseData, type = rdata, title = sampled datasets,
# abstract = an R data.frame with the sampled input data for further analysis;
myLog("Saved sampled data in file ", sampledBaseData, " in directory ", getwd(),
      " | file size: ",	file.info(sampledBaseData)$size / (1024*1024), " MB")

myLog("#### Done with preprocessing (1/3)!")



################################################################################
# 2) Cleaning and standardization
################################################################################

myLog("#### Start standardization (2/3) ... ")
myLog("Workspace content: ", toString(ls()))
myLog("Input data size for standardization (# of observations, # of variables): ", toString(dim(data)))

# remove NAs first
notNA <- complete.cases(data)
#sum(nona)
data.clean <- data[notNA,]
myLog("Cleaned data size (used complete.cases to remove NAs): ",
			toString(dim(data.clean)))

# save cleaned data
cleanedDataFile <- paste0("data_cleaned.Rdata")
save(data.clean, file = cleanedDataFile)
myLog("Saved cleaned data in file ", cleanedDataFile, " in directory ", getwd(),
			" | file size: ",	file.info(cleanedDataFile)$size / (1024*1024)," MB")


# normalise the input data (not the coordinates which will not be used in the cluster analysis)

# wps.in: id = standardizationMethod, type = string,
# title = standardization method, minOccurs = 0, maxOccurs = 1,
# abstract = method for the function decostand from the package vegan,
# value = standardize;
#wps.off;
standardizationMethod <- "standardize"
#wps.on;

myLog("Normalizing input data (decostand) ... ")
data.norm <- decostand(data.clean[,-c(1:2)], standardizationMethod)
#dim(data.norm)
myLog("Done with decostand.")

# sort the variable
data.norm <- data.norm[c("crop2005", "cropdif50", "grass2005", "grassdif50", 
                         "nfert", "irrigation", "totsederosion", 
                         "y_wheat", "y_maize", "y_rice", "yg_wheat", "yg_maize", "yg_rice", 
                         "tpi_agr", "hanpp", "bio1", "bio2", "bio12", "bio15", "bio21", "temp_anom", 
                         "ndvi_mean", "ndvi_sd", "soil_orgc", "spec_rich", 
                         "gdp", "agr_gdp", "gcs_agr", "pop_glds00ag", "popdif50", 
                         "polstability", "accessibility")]
myLog("Variables (sorted): ", toString(names(data.norm)))

output.normalizedDataSummary <- "normalized-data_statistics.txt"
capture.output(summary(data.norm), file = output.normalizedDataSummary)
# wps.out: output.normalizedDataSummary, type = text, 
# title = statistics of dataset,
# abstract = the output of a summary operation on the normalized data.frame;
myLog("Saved summary statistics of normalized data in file ",
      output.normalizedDataSummary, " in ", getwd())

output.normalizedInputData <- paste0("data_normalized.Rdata")
save(data.norm, file = output.normalizedInputData)
# wps.out: output.normalizedInputData, type = rdata, title = normalized input datasets,
# abstract = an R data.frame with the input data with normalized variables which are 
# based on the cleaned input data;
myLog("Saved normalized data in file ", output.normalizedInputData, " in directory ",
			getwd(), " | file size: ",
			file.info(output.normalizedInputData)$size / (1024*1024), " MB")



################################################################################
# 3) Self-organizing map
################################################################################

myLog("#### Start som (3/3) ... ")
myLog("Workspace content: ", toString(ls()))

# wps.in: id = somGridTopology, type = string,
# title = grid topology for som, abstract = the topology of the self-organizing 
# map which can be either 'rectangular' or 'hexagonal', 
# minOccurs = 0, maxOccurs = 1, value = hexagonal;
#wps.off;
somGridTopology <- "hexagonal"
#wps.on;

# wps.in: id = somGridDim, type = string,
# title = "grid dimenstions for som in the format 'xdim,ydim'",
# abstract = "the grid dimensions for the self-organizing map in a comma 
# separated format: 'xdim,ydim' This detemines the number of classes in the SOM!",
# minOccurs = 0, maxOccurs = 1, value = "3,4";
#wps.off;
somGridDim <- "3,4"
#wps.on;

xdim <- unlist(strsplit(x = somGridDim, split = ","))[[1]]
ydim <- unlist(strsplit(x = somGridDim, split = ","))[[2]]
#topology <- list(xdim = xdim, ydim = ydim, topo = somGridTopology)
topologies <- list(list(xdim = xdim, ydim = ydim, topo = somGridTopology))

#topologies <- list(list(xdim = 3, ydim = 4, topo = somGridTopology),
#									 list(xdim = 4, ydim = 4, topo = somGridTopology),
#									 list(xdim = 5, ydim = 5, topo = somGridTopology))
myLog("Topologies: ", toString(topologies))

outputFilePrefix <- "sysarch-som_"

# wps.in: id = somIterations, type = integer,
# title = the number of loops calculating the SOM,
# abstract = "how often is the SOM analysis repeated?",
# value = 1, minOccurs = 0, maxOccurs = 1;
#wps.off;
somIterations <- 1 # 5
#wps.on;

# NOTE: only the last created files (last run loop) will be returned from a WPS process
for(topology in topologies) {
    myLog("Running som with topology: xdim = ", topology[[1]], ", ydim = ",
				topology[[2]], ", topo = ", topo = topology[[3]])
    .topoString <- paste0(collapse = "_", topology)
    
    for(run in 1:somIterations) {
      	myLog("Running ", run, "/", somIterations, " for ", toString(topology))
      	
      	som.grid <- somgrid(xdim = topology[[1]], ydim = topology[[2]], 
      											topo = topology[[3]])
      	myLog("som grid summary: ", toString(capture.output(summary(som.grid))))
      	
      	myLog("Starting som algorithm... current memory state: \n\tmemory.size = ",
      				memory.size(), " (max: ", memory.size(TRUE), "); memory.limit = ",
      				memory.limit())
        som.result <- som(data = as.matrix(data.norm), grid = som.grid)
      	myLog("Calculated som: ", toString(capture.output(summary(som.result))))
        #som.hc <- cutree(hclust(dist(som.result$codes)), ncl)
      	
      	myLog("SOM output created ", length(unique(som.result$unit.classif)), " classes.")
        
        # map the clusters back to the feature space
        # QUESTION: Why are we not using the normalized data for the reverse mapping?
        systemArchetypesData <- data.clean
        systemArchetypesData$som.unit <- som.result$unit.classif # save the classified values
        # map the distances to corresponding SOM codebook vector
        systemArchetypesData$som.distance <- som.result$distances
        .sampleSize <- as.integer(dim(systemArchetypesData)[[1]])
        .sampleRunString <- paste0("_sample-", .sampleSize, "_run-", run)
        
        # create code vectors table
        # wps.out: output.codeVector, type = text/csv, title = code vectors,
        # abstract = an comma-seperated values table with the code vectors of the  
        # SOM classifications;
        codeVectors <- som.result$codes
        output.codeVector <- paste0(outputFilePrefix, "codes_", .topoString, .sampleRunString, ".csv" )
        write.table(codeVectors, file = output.codeVector, sep=";", row.names = FALSE)
      	myLog("Saved code vectors file ", output.codeVector, " in ", getwd())
        
        # create output data.frame
        # wps.out: output.data, type = rdata, title = output datasets,
        # abstract = an R data.frame with the sample input data and the calculated 
        # classifications for each cell and distance to the code vector;
        output.data <- paste0(outputFilePrefix, .topoString, .sampleRunString, ".Rdata")
        save(systemArchetypesData, codeVectors, file = output.data)
        myLog("Saved sampled data (feature space) in file ", output.data, " in directory ", getwd(),
              " | file size: ",    file.info(output.data)$size / (1024*1024), " MB")
        
      	# create pdf
        # wps.out: output.plots, type = pdf, title = ouput datasets,
        # abstract = an R data.frame with the sample input data and the calculated 
        # classifications for each cell and distance to the code vector;
        output.plots <- paste0(outputFilePrefix, .topoString, .sampleRunString, ".pdf")
        #createPDF(output.plots, data, som.result, systemArchetypesData)
        
        ## create shapefile
        ## wps.out: output.shapefile, type = shp, title = ouput datasets,
        ## abstract = an R data.frame with the sample input data and the calculated 
        ## classifications for each cell and distance to the code vector;
        #output.shapefile <- paste0(outputFilePrefix, .topoString, .sampleRunString)
        #createShapefile(systemArchetypesData, output.shapefile)
    } # end run loop 
} # end topologies loop

# TODO? define multiple outputs - right now only the final loop run is accessible

myLog("Output files:\n\t\t", # output.shapefile, " (shp)\n\t\t",
      output.codeVector, " (csv code vectors)\n\t\t",
      output.plots, " (plots)\n\t\t",
      output.data, " (Rdata)")
myLog("#### Done with som (3/4)")

################################################################################
# 4) Advanced plots
################################################################################

# map process output
# wps.out: output.map, type = png, title = map of LSAs,
# abstract = a global thematic map displaying the land use system archetypes;
output.map = "lsa-map.png"

# distance/quality map output
# wps.out: output.distancemap, type = png, title = som distance map,
# abstract = a global map displaying the distance of each raster cell to the code
# vector on a logarithmic scale to estimate representativeness;
output.distancemap = "lsa-som-distancemap.png"

# create statistics plot
# wps.out: output.statistics.plot, type = png, title = system archetypes
# statistics,
# abstract = barplots of the codebook vectors displaying the combination of
# normalized variable values that best characterize each land system archetype;
output.statistics.plot = "lsa-codebook-barplots.png"

pal <- createPaletteAndLabels(dim(codeVectors)[[1]])
mapData <- preparePlotData(systemArchetypesData)
saveMaps(plotData = mapData, paletteAndLabels = pal, lsaMapName = output.map,
         distanceMapName = output.distancemap)
savePlots(vectors = codeVectors, data = systemArchetypesData, paletteAndLabels = pal,
          lsaBarplotFilename = output.statistics.plot)


myLog("#### Done with plots (4/4)")
