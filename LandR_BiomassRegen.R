
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "LandR_BiomassRegen",
  description = "Post-disturbance biomass regeneration module for LandR. Simulates ost-fire mortality, regeneration and serotiny
  as part of the same event - all occurring sequentially immeadiately after fire. Mortality assumed to be 100%, serotiny and regeneration
  algorythms taken from LANDIS-II Biomass Succession extension, v.?",
  keywords = c("biomass regeneration", "LandR", "disturbance", "mortality", "vegetation succession", "vegetation model"),
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandR_BiomassRegen.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".crsUsed", "character", "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
                    NA, NA, "CRS to be used. Defaults to the biomassMap projection"),
    defineParameter(name = "calibrate", class = "logical", default = FALSE, desc = "Do calibration? Defaults to FALSE"),
    defineParameter(name = "fireInitialTime", class = "numeric", default = 2L, 
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter(name = "fireTimestep", class = "numeric", default = 2L,
                    desc = "The number of time units between successive fire events in a fire module")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "species", objectClass = "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput(objectName = "cohortData", objectClass = "data.table",
                  desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                  succession time step"),
    expectsInput(objectName = "sufficientLight", objectClass = "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput(objectName = "speciesEcoregion", objectClass = "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput(objectName = "ecoregionMap", objectClass = "RasterLayer",
                 desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table",
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.gis"),
    expectsInput(objectName = "rstCurrentBurn", objectClass = "RasterLayer",
                 desc = "Binary raster of fire spread"),
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer",
                 desc = "updated community map at each succession time step"),
    expectsInput(objectName = "inactivePixelIndex", objectClass = "logical",
                  desc = "internal use. Keeps track of which pixels are inactive"),
    expectsInput(objectName = "shpStudySubRegion", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Sub study area with fire return interval attribute. 
                 Defaults to a shapefile in Southwestern Alberta, Canada", sourceURL = ""),
    expectsInput(objectName = "shpStudyRegionFull", objectClass = "SpatialPolygonsDataFrame",
                 desc = "this shape file contains two informaton: Full study area with fire return interval attribute.
                 Defaults to a shapefile in Southwestern Alberta, Canada", sourceURL = "")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "cohortData", objectClass = "data.table",
                  desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                  succession time step"),
    createsOutput(objectName = "pixelGroupMap", objectClass = "RasterLayer", 
                  desc = "updated community map at each succession time step"),
    createsOutput(objectName = "rstCurrentBurn", objectClass = "list", 
                  desc = "List of rasters of fire spread"),
    createsOutput(objectName = "burnLoci", objectClass = "numeric", desc = "Fire pixel IDs"), 
    createsOutput(objectName = "postFireRegenSummary", objectClass = "data.table", 
                  desc = "summary table of species post-fire regeneration"),
    createsOutput(objectName = "postFirePixel", objectClass = "numeric",
                  desc = "Pixels that were affected by fire"),
    createsOutput(objectName = "lastFireYear", objectClass = "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput(objectName = "firePixelTable", objectClass = "data.table",
                  desc = "table with pixels IDs that had fire and their corresponding pixel groups")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.LandR_BiomassRegen = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      ## schedule events
      if(!is.null(sim$rstCurrentBurn)){ # anything related to fire disturbance
        sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                             "LandR_BiomassRegen", "fireDisturbance", eventPriority = 3)
      }
    },
    fireDisturbance = {
      sim <- FireDisturbance(sim)
      
      if(!is.null(sim$rstCurrentBurn)) {   
        sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                             "LandR_BiomassRegen", "fireDisturbance",
                             eventPriority = 3)
      }
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  return(invisible(sim))
}

## Fire disturbance regeneration event
FireDisturbance = function(sim) {
  # the presence of valid fire can cause three processes:
  # 1. remove species cohorts from the pixels that have been affected.
  # 2. initiate the post-fire regeneration
  # 3. change of cohortdata and pixelgroup map
  # may be a supplemenatary function is needed to convert non-logical map
  # to a logical map
  postFireReproData <- data.table(pixelGroup = integer(), ecoregionGroup = numeric(),
                                  speciesCode = numeric(), pixelIndex = numeric())
  if(P(sim)$calibrate){
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(),
                                           species = character(),
                                           numberOfRegen = numeric())
  }
  
  if (!is.null(sim$rstCurrentBurn)) { # anything related to fire disturbance
     if (extent(sim$rstCurrentBurn) != extent(sim$pixelGroupMap)) {
      sim$rstCurrentBurn <- raster::crop(sim$rstCurrentBurn, extent(sim$pixelGroupMap))
    }
  }
  
  ## extract burn pixel indices/groups and remve potentially innactive pixels
  sim$burnLoci <- which(sim$rstCurrentBurn[] == 1)
  if (length(sim$inactivePixelIndex) > 0) {
    sim$burnLoci <- sim$burnLoci[!(sim$burnLoci %in% sim$inactivePixelIndex)] # this is to prevent avaluating the pixels that are inactive
  }
  firePixelTable <- data.table(cbind(pixelIndex = sim$burnLoci,
                                     pixelGroup = getValues(sim$pixelGroupMap)[sim$burnLoci]))
  burnPixelGroup <- unique(firePixelTable$pixelGroup)
  
  ## reclassify pixel groups as burnt (0L) 
  sim$pixelGroupMap[sim$burnLoci] <- 0L # 0 is the fire burnt pixels without regenerations
  burnedcohortData <- sim$cohortData[pixelGroup %in% burnPixelGroup]
  set(burnedcohortData, ,c("B", "mortality", "aNPPAct"), NULL)
  #   set(burnedcohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedcohortData, speciesCode)
  tempspecies <- sim$species[postfireregen == "serotiny",
                             .(speciesCode, postfireregen)]
  serotinyAssessCohortData <- burnedcohortData[tempspecies, nomatch = 0][, postfireregen := NULL]
  
  rm(tempspecies)
  if (NROW(serotinyAssessCohortData) > 0) {
    # assess potential serotiny reg
    serotinyAssessCohortData <- setkey(serotinyAssessCohortData, speciesCode)[sim$species[,.(speciesCode, sexualmature)],
                                                                              nomatch = 0]
    newCohortData <- serotinyAssessCohortData[age >= sexualmature] %>% # NOTE should be in mortalityFromDisturbance module or event
      unique(., by = c("pixelGroup", "speciesCode"))
    set(newCohortData, ,"sexualmature", NULL)
    # select the pixels that have potential serotiny regeneration and assess them
    serotinyPixelTable <- firePixelTable[pixelGroup %in% unique(newCohortData$pixelGroup)]
    
    # from now on the regeneration process is assessed for each potential pixel
    setkey(serotinyPixelTable, pixelGroup)
    setkey(newCohortData, pixelGroup)
    newCohortData <- serotinyPixelTable[newCohortData, nomatch = 0, allow.cartesian = TRUE]
    
    # light check
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][,siteShade := 0]
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                     newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
    set(newCohortData, ,c("shadetolerance", "siteShade", "lightProb"), NULL)
    specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
    specieseco_current <- specieseco_current[year == max(specieseco_current$year),
                                             .(ecoregionGroup, speciesCode, establishprob)]
    newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)[specieseco_current, nomatch = 0]
    newCohortData <- newCohortData[(runif(nrow(newCohortData), 0, 1)) %<<% establishprob][, establishprob := NULL]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    if (NROW(newCohortData) > 0) {
      newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)] #
      if(P(sim)$calibrate){
        serotinyRegenSummary <- newCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode]
        serotinyRegenSummary <- serotinyRegenSummary[,.(year = time(sim), regenMode = "Serotiny",
                                                        speciesCode, numberOfRegen)]
        serotinyRegenSummary <- setkey(serotinyRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        serotinyRegenSummary[, ':='(speciesCode = species, species = NULL)]
        setnames(serotinyRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, serotinyRegenSummary))
      }
      serotinyPixel <- unique(newCohortData$pixelIndex) # save the pixel index for resprouting assessment use,
      # i.e., removing these pixel from assessing resprouting
      postFireReproData <- rbindlist(list(postFireReproData, newCohortData))
    } else {
      serotinyPixel <- NULL
    }
    rm(newCohortData)
  } else {
    serotinyPixel <- NULL
  }
  
  #############################################################
  #############################################################
  # from now on, starting assessing resprouting reproduction:
  # basically same thing as serotiny
  # remove the pixels that had successful serotiny regeneration
  if (is.null(serotinyPixel)) {
    resproutingPixelTable <- setkey(firePixelTable, pixelGroup)
  } else {
    resproutingPixelTable <- setkey(data.table(dplyr::anti_join(firePixelTable,
                                                                data.table(cbind(pixelIndex = serotinyPixel)),
                                                                by = "pixelIndex")),
                                    pixelGroup)
  }
  setkey(burnedcohortData, speciesCode)
  species_temp <- sim$species[postfireregen == "resprout",
                              .(speciesCode, postfireregen,
                                resproutage_min, resproutage_max, resproutprob)]
  resproutingAssessCohortData <- burnedcohortData[species_temp, nomatch = 0][age >= resproutage_min & age <= resproutage_max]
  set(resproutingAssessCohortData, ,c("resproutage_min", "resproutage_max", "postfireregen", "age"), NULL)
  rm(species_temp)
  if (NROW(resproutingAssessCohortData) > 0) {
    resproutingAssessCohortData <- unique(resproutingAssessCohortData, by = c("pixelGroup", "speciesCode"))
    setkey(resproutingAssessCohortData, pixelGroup)
    newCohortData <- resproutingPixelTable[resproutingAssessCohortData, nomatch = 0, allow.cartesian = TRUE]
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][, siteShade := 0]
    
    # Light check
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                     newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1),]
    newCohortData <- newCohortData[runif(nrow(newCohortData), 0, 1) %<<% newCohortData$resproutprob]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    set(newCohortData, ,c("resproutprob", "shadetolerance", "siteShade", "lightProb"), NULL)
    # remove all columns that were used temporarily here
    if (NROW(newCohortData) > 0) {
      newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)]#
      if(P(sim)$calibrate){
        resproutRegenSummary <- newCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode]
        resproutRegenSummary <- resproutRegenSummary[,.(year = time(sim), regenMode = "Resprout",
                                                        speciesCode, numberOfRegen)]
        resproutRegenSummary <- setkey(resproutRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        resproutRegenSummary[,':='(speciesCode = species, species = NULL)]
        setnames(resproutRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, resproutRegenSummary))
      }
      postFireReproData <- rbindlist(list(postFireReproData, newCohortData))
      postFirePixel <- c(serotinyPixel, unique(newCohortData$pixelIndex))
      sim$postFirePixel <- postFirePixel # send it to a sim object
      rm(newCohortData)
    } else{
      sim$postFirePixel <- serotinyPixel
      postFireReproData <- postFireReproData
    }
  } else {
    postFireReproData <- postFireReproData
    sim$postFirePixel <- serotinyPixel
  }
  if (NROW(postFireReproData) > 0) {
    maxPixelGroup <- as.integer(maxValue(sim$pixelGroupMap))
    if (!is.null(sim$postFirePixel)) {
      sim$pixelGroupMap[sim$postFirePixel] <- maxPixelGroup +
        as.integer(as.factor(sim$ecoregionMap[sim$postFirePixel]))
      postFireReproData[, pixelGroup := maxPixelGroup +
                          as.integer(as.factor(postFireReproData$ecoregionGroup))]
    }
    sim$cohortData[,sumB := sum(B, na.rm = TRUE), by = pixelGroup]
    addnewcohort <- addNewCohorts(postFireReproData, sim$cohortData, sim$pixelGroupMap,
                                  time = round(time(sim)), speciesEcoregion = sim$speciesEcoregion)
    sim$cohortData <- addnewcohort$cohortData
    sim$pixelGroupMap <- setValues(addnewcohort$pixelGroupMap, as.integer(addnewcohort$pixelGroupMap[]))
  }
  sim$lastFireYear <- time(sim)
  sim$firePixelTable <- firePixelTable
  return(invisible(sim))
}

## ---------------------------------------------------------------------------
## INPUT OBJECTS

.inputObjects <- function(sim) {
  dPath <- dataPath(sim)

  if (!suppliedElsewhere("shpStudyRegionFull", sim)) {
    message("'shpStudyRegionFull' was not provided by user. Using a polygon in Southwestern Alberta, Canada")
    
    canadaMap <- Cache(getData, 'GADM', country = 'CAN', level = 1, path = asPath(dPath),
                       cacheRepo = getPaths()$cachePath, quick = FALSE) 
    smallPolygonCoords = list(coords = data.frame(x = c(-115.9022,-114.9815,-114.3677,-113.4470,-113.5084,-114.4291,-115.3498,-116.4547,-117.1298,-117.3140), 
                                                  y = c(50.45516,50.45516,50.51654,50.51654,51.62139,52.72624,52.54210,52.48072,52.11243,51.25310)))
    
    sim$shpStudyRegionFull <- SpatialPolygons(list(Polygons(list(Polygon(smallPolygonCoords$coords)), ID = "swAB_polygon")),
                                              proj4string = crs(canadaMap))
    
    ## use CRS of biomassMap
    sim$shpStudyRegionFull <- spTransform(sim$shpStudyRegionFull,
                                          CRSobj = P(sim)$.crsUsed)
    
  }
  
  if (!suppliedElsewhere("shpStudySubRegion", sim)) {
    message("'shpStudySubRegion' was not provided by user. Using the same as 'shpStudyRegionFull'")
    sim$shpStudySubRegion <- sim$shpStudyRegionFull
  }
  
  if (!identical(P(sim)$.crsUsed, crs(sim$shpStudyRegionFull))) {
    sim$shpStudyRegionFull <- spTransform(sim$shpStudyRegionFull, P(sim)$.crsUsed) #faster without Cache
  }
  
  if (!identical(P(sim)$.crsUsed, crs(sim$shpStudySubRegion))) {
    sim$shpStudySubRegion <- spTransform(sim$shpStudySubRegion, P(sim)$.crsUsed) #faster without Cache
  }
  
  ## get LANDISII main input table where species and light requirements tables come from
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim))) {
    maxcol <- 7L
    for (i in 1:2) {
      mainInput <- Cache(prepInputs,
                         extractURL("sufficientLight"),
                         targetFile = "biomass-succession_test.txt", 
                         destinationPath = dPath, 
                         fun = "utils::read.table", 
                         fill = TRUE,  #purge = 2,
                         sep = "",
                         header = FALSE,
                         col.names = c(paste("col",1:maxcol, sep = "")),
                         blank.lines.skip = TRUE,
                         stringsAsFactors = FALSE)
      maxcol1 <- max(count.fields(file.path(dPath, "biomass-succession_test.txt"), sep = "")) 
      if (identical(maxcol1,maxcol)) break
    }
    
    mainInput <- data.table(mainInput)
    mainInput <- mainInput[col1 != ">>",]
  }
  
  ## read species txt and convert it to data table
  if (!suppliedElsewhere("species", sim)) {
    maxcol <- 13#max(count.fields(file.path(dPath, "species.txt"), sep = ""))
    species <- Cache(prepInputs, 
                     url = extractURL("species"), 
                     targetFile = "species.txt", 
                     destinationPath = dPath, 
                     fun = "utils::read.table", 
                     fill = TRUE, row.names = NULL, #purge = 7,
                     sep = "",
                     header = FALSE,
                     blank.lines.skip = TRUE,
                     col.names = c(paste("col",1:maxcol, sep = "")),
                     stringsAsFactors = FALSE)
    species <- data.table(species[,1:11])
    species <- species[col1!= "LandisData",]
    species <- species[col1!= ">>",]
    colNames <- c("species", "longevity", "sexualmature", "shadetolerance", 
                  "firetolerance", "seeddistance_eff", "seeddistance_max", 
                  "resproutprob", "resproutage_min", "resproutage_max",
                  "postfireregen")
    names(species) <- colNames
    species[,':='(seeddistance_eff = gsub(",", "", seeddistance_eff),
                  seeddistance_max = gsub(",", "", seeddistance_max))]
    # change all columns to integer
    species <- species[, lapply(.SD, as.integer), .SDcols = names(species)[-c(1,NCOL(species))], by = "species,postfireregen"]
    setcolorder(species, colNames)
    
    # get additional species traits
    speciesAddon <- mainInput
    startRow <- which(speciesAddon$col1 == "SpeciesParameters")
    speciesAddon <- speciesAddon[(startRow+1):(startRow+nrow(species)),1:6, with = FALSE]
    names(speciesAddon) <- c("species", "leaflongevity", "wooddecayrate",
                             "mortalityshape", "growthcurve", "leafLignin")
    speciesAddon[, ':='(leaflongevity = as.numeric(leaflongevity),
                        wooddecayrate = as.numeric(wooddecayrate),
                        mortalityshape = as.numeric(mortalityshape),
                        growthcurve = as.numeric(growthcurve),
                        leafLignin = as.numeric(leafLignin))]
    
    species <- setkey(species, species)[setkey(speciesAddon, species), nomatch = 0]
    
    ## rename species for compatibility across modules (Xxxx_xxx)
    species$species1 <- as.character(substring(species$species, 1, 4))
    species$species2 <- as.character(substring(species$species, 5, 7))
    species[, ':='(species = paste0(toupper(substring(species1, 1, 1)), substring(species1, 2, 4), "_",
                                    species2))]
    
    species[, ':='(species1 = NULL, species2 = NULL)]
    
    sim$species <- species
    rm(maxcol)
  }
  
  
  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    sufficientLight <- mainInput %>%
      data.frame
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow+1):(startRow+5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]
    
    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight)
  }
  
  #  # input species ecoregion dynamics table
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    speciesEcoregion <- Cache(prepInputs, 
                              url = extractURL("speciesEcoregion"),
                              fun = "utils::read.table", 
                              destinationPath = dPath, 
                              targetFile = "biomass-succession-dynamic-inputs_test.txt",
                              fill = TRUE, 
                              sep = "",
                              header = FALSE,
                              blank.lines.skip = TRUE,
                              stringsAsFactors = FALSE)
    maxcol <- max(count.fields(file.path(dPath, "biomass-succession-dynamic-inputs_test.txt"), 
                               sep = ""))
    colnames(speciesEcoregion) <- paste("col",1:maxcol, sep = "")
    speciesEcoregion <- data.table(speciesEcoregion)
    speciesEcoregion <- speciesEcoregion[col1 != "LandisData",]
    speciesEcoregion <- speciesEcoregion[col1 != ">>",]
    keepColNames <- c("year", "ecoregion", "species", "establishprob", "maxANPP", "maxB")
    names(speciesEcoregion)[1:6] <- keepColNames
    speciesEcoregion <- speciesEcoregion[, keepColNames, with = FALSE]
    integerCols <- c("year", "establishprob", "maxANPP", "maxB")
    speciesEcoregion[, (integerCols) := lapply(.SD, as.integer), .SDcols = integerCols]
    
    ## rename species for compatibility across modules (Xxxx_xxx)
    speciesEcoregion$species1 <- as.character(substring(speciesEcoregion$species, 1, 4))
    speciesEcoregion$species2 <- as.character(substring(speciesEcoregion$species, 5, 7))
    speciesEcoregion[, ':='(species = paste0(toupper(substring(species1, 1, 1)), substring(species1, 2, 4), "_",
                                             species2))]
    
    speciesEcoregion[, ':='(species1 = NULL, species2 = NULL)]
    
    sim$speciesEcoregion <- speciesEcoregion
    rm(maxcol)
  }
  
  ## load ecoregion map
  if (!suppliedElsewhere("ecoregionMap", sim )) {
    ## LANDIS-II demo data:
    
    # sim$ecoregionMap <- Cache(prepInputs,
    #                           url = extractURL("ecoregionMap"),
    #                           destinationPath = dPath,
    #                           targetFile = "ecoregions.gis",
    #                           fun = "raster::raster")
    
    ## Dummy version with spatial location in Canada
    ras <- projectExtent(sim$shpStudySubRegion, crs = sim$shpStudySubRegion)
    res(ras) = 250
    ecoregionMap <- rasterize(sim$shpStudySubRegion, ras)
    
    ecoregionMap[!is.na(getValues(ecoregionMap))][] <- sample(ecoregion$mapcode, 
                                                              size = sum(!is.na(getValues(ecoregionMap))), 
                                                              replace = TRUE) 
    sim$ecoregionMap <- ecoregionMap
  }
  
  
  return(invisible(sim))
}

