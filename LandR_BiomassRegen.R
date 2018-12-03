# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "LandR_BiomassRegen",
  description = "Post-disturbance biomass regeneration module for LandR. Simulates post-fire mortality, regeneration and serotiny as part of the same event - all occurring sequentially immeadiately after fire. Mortality assumed to be 100%, serotiny and regeneration algorithms taken from LANDIS-II Biomass Succession extension, v3.6.1",
  keywords = c("biomass regeneration", "LandR", "disturbance", "mortality", "vegetation succession", "vegetation model"),
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandR_BiomassRegen.Rmd"),
  reqdPkgs = list("data.table", "raster", ## TODO: update package list!
                  "PredictiveEcology/pemisc@development"),
  parameters = rbind(
    defineParameter("calibrate", "logical", FALSE, desc = "Do calibration? Defaults to FALSE"),
    defineParameter("fireInitialTime", "numeric", 2L,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", 2L,
                    desc = "The number of time units between successive fire events in a fire module")
  ),
  inputObjects = bind_rows(
    expectsInput("cohortData", "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step"),
    expectsInput("ecoregionMap", "RasterLayer",
                 desc = "ecoregion map that has mapcodes match ecoregion table and speciesEcoregion table",
                 sourceURL = "https://github.com/LANDIS-II-Foundation/Extensions-Succession/raw/master/biomass-succession-archive/trunk/tests/v6.0-2.0/ecoregions.gis"),
    expectsInput("inactivePixelIndex", "logical",
                 desc = "internal use. Keeps track of which pixels are inactive"),
    expectsInput("pixelGroupMap", "RasterLayer",
                 desc = "updated community map at each succession time step"),
    expectsInput("rstCurrentBurn", "RasterLayer",
                 desc = "Binary raster of fire spread"),
    expectsInput("species", "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput("studyArea", "SpatialPolygonsDataFrame",
                 desc = paste("multipolygon to use as the study area,",
                              "with attribute LTHFC describing the fire return interval.",
                              "Defaults to a square shapefile in Southwestern Alberta, Canada."),
                 sourceURL = ""),
    expectsInput("studyAreaLarge", "SpatialPolygonsDataFrame",
                 desc = paste("multipolygon (larger area than studyArea) to use for parameter estimation,",
                              "with attribute LTHFC describing the fire return interval.",
                              "Defaults to a square shapefile in Southwestern Alberta, Canada."),
                 sourceURL = ""),
    expectsInput("sufficientLight", "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt")
  ),
  outputObjects = bind_rows(
    createsOutput("burnLoci", "numeric", desc = "Fire pixel IDs"),
    createsOutput("cohortData", "data.table",
                  desc = paste("age cohort-biomass table hooked to pixel group map",
                               "by pixelGroupIndex at succession time step")),
    createsOutput("firePixelTable", "data.table",
                  desc = "table with pixels IDs that had fire and their corresponding pixel groups"),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "updated community map at each succession time step"),
    createsOutput("postFirePixel", "numeric",
                  desc = "Pixels that were affected by fire"),
    createsOutput("postFireRegenSummary", "data.table",
                  desc = "summary table of species post-fire regeneration"),
    createsOutput("rstCurrentBurn", "rasterLayer",
                  desc = "Binary raster of fire spread")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.LandR_BiomassRegen <- function(sim, eventTime, eventType) {
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
FireDisturbance <- function(sim) {
  # the presence of valid fire can cause three processes:
  # 1. remove species cohorts from the pixels that have been affected.
  # 2. initiate the post-fire regeneration
  # 3. change of cohortdata and pixelgroup map
  # may be a supplemenatary function is needed to convert non-logical map
  # to a logical map

  ## make a copy of pixelGroupMap
  pixelGroupMap <- sim$pixelGroupMap

  postFireReproData <- data.table(pixelGroup = integer(), ecoregionGroup = numeric(),
                                  speciesCode = numeric(), pixelIndex = numeric())
  if(P(sim)$calibrate){
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(),
                                           species = character(),
                                           numberOfRegen = numeric())
  }

  if (!is.null(sim$rstCurrentBurn)) { # anything related to fire disturbance
    if (extent(sim$rstCurrentBurn) != extent(pixelGroupMap)) {
      sim$rstCurrentBurn <- raster::crop(sim$rstCurrentBurn, extent(pixelGroupMap))
    }
  }

  ## extract burn pixel indices/groups and remove potentially innactive pixels
  sim$burnLoci <- which(!is.na(getValues(sim$rstCurrentBurn)))
  if (length(sim$inactivePixelIndex) > 0) {
    sim$burnLoci <- sim$burnLoci[!(sim$burnLoci %in% sim$inactivePixelIndex)] # this is to prevent avaluating the pixels that are inactive
  }
  firePixelTable <- data.table(cbind(pixelIndex = sim$burnLoci,
                                     pixelGroup = getValues(pixelGroupMap)[sim$burnLoci]))
  burnPixelGroup <- unique(firePixelTable$pixelGroup)

  ## reclassify pixel groups as burnt (0L)
  pixelGroupMap[sim$burnLoci] <- 0L

  ## make table spp/ecoregionGroup/age in burnt pixels
  burnedcohortData <- sim$cohortData[pixelGroup %in% burnPixelGroup]
  set(burnedcohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)
  #   set(burnedcohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedcohortData, speciesCode)

  ## subset spp with serotiny
  tempspecies <- sim$species[postfireregen == "serotiny", .(speciesCode, postfireregen)]

  ## join tables to make a serotiny table
  serotinyAssessCohortData <- burnedcohortData[tempspecies, nomatch = 0][, postfireregen := NULL]

  rm(tempspecies)
  if (NROW(serotinyAssessCohortData) > 0) {
    ## assess potential serotiny reg: add sexual maturity to the table and compare w/ age
    ## as long as one cohort is sexually mature, serotiny is activated
    serotinyAssessCohortData <- setkey(serotinyAssessCohortData, speciesCode)[sim$species[,.(speciesCode, sexualmature)],
                                                                              nomatch = 0]
    newCohortData <- serotinyAssessCohortData[age >= sexualmature] %>% # NOTE should be in mortalityFromDisturbance module or event
      unique(., by = c("pixelGroup", "speciesCode"))
    set(newCohortData, NULL, "sexualmature", NULL)

    ## select the pixels that have potential serotiny regeneration and assess them
    serotinyPixelTable <- firePixelTable[pixelGroup %in% unique(newCohortData$pixelGroup)]

    ## from now on the regeneration process is assessed for each potential pixel
    setkey(serotinyPixelTable, pixelGroup)
    setkey(newCohortData, pixelGroup)
    newCohortData <- serotinyPixelTable[newCohortData, nomatch = 0, allow.cartesian = TRUE] ## join table to add pixels

    ## light check: add shade tolerance to table and set shade to 0 (100% mortality.)
    ## the get survival probs and subset survivors with runif
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][, siteShade := 0]
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                     newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1)]  ## subset survivors
    set(newCohortData, NULL, c("shadetolerance", "siteShade", "lightProb"), NULL)   ## clean table again

    ## get establishment probs and subset species that establish with runif
    specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
    specieseco_current <- specieseco_current[year == max(specieseco_current$year),
                                             .(ecoregionGroup, speciesCode, establishprob)]
    newCohortData <- setkey(newCohortData, ecoregionGroup, speciesCode)[specieseco_current, nomatch = 0]  ## join table to add probs
    newCohortData <- newCohortData[establishprob  %>>% runif(nrow(newCohortData), 0, 1)][, establishprob := NULL]

    ## only need one cohort per spp per pixel survives/establishes
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))

    if (NROW(newCohortData) > 0) {
      ## rm age
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
      ## append table to (yet empty) postFireReproData
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
  # remove the pixels that had successful serotiny regeneration (antijoin)

  ## make a table of pixels where resprouting occurs.
  if (is.null(serotinyPixel)) {
    resproutingPixelTable <- setkey(firePixelTable, pixelGroup)
  } else {
    ## should be done by pixel and species
    resproutingPixelTable <- setkey(data.table(dplyr::anti_join(firePixelTable,
                                                                data.table(cbind(pixelIndex = serotinyPixel)),
                                                                by = "pixelIndex")),
                                    pixelGroup)
  }

  ## assess whether reprouting can occur in burnt pixels
  setkey(burnedcohortData, speciesCode)
  species_temp <- sim$species[postfireregen == "resprout",
                              .(speciesCode, postfireregen,
                                resproutage_min, resproutage_max, resproutprob)]
  resproutingAssessCohortData <- burnedcohortData[species_temp, nomatch = 0][age >= resproutage_min & age <= resproutage_max]
  set(resproutingAssessCohortData, NULL, c("resproutage_min", "resproutage_max", "postfireregen", "age"), NULL)
  rm(species_temp)

  if (NROW(resproutingAssessCohortData) > 0) {
    ## assess potential resprouting reg: add reprout probability, siteShade/tolerance to the table and assess who resprouts
    ## as long as one cohort can resprout, resprouting is activated
    resproutingAssessCohortData <- unique(resproutingAssessCohortData, by = c("pixelGroup", "speciesCode"))
    setkey(resproutingAssessCohortData, pixelGroup)

    ## make new table joing resprouters with burnt pixels
    newCohortData <- resproutingPixelTable[resproutingAssessCohortData, nomatch = 0, allow.cartesian = TRUE]

    ## light check: add shade tolerance to table and set shade to 0 (100% mortality.)
    ## the get survival probs and subset survivors with runif
    newCohortData <- setkey(newCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
                                                        nomatch = 0][, siteShade := 0]
    newCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                     newCohortData)
    newCohortData <- newCohortData[lightProb %>>% runif(nrow(newCohortData), 0, 1)]
    newCohortData <- newCohortData[newCohortData$resproutprob %>>% runif(nrow(newCohortData), 0, 1)]
    newCohortData <- unique(newCohortData, by = c("pixelIndex", "speciesCode"))
    set(newCohortData, NULL, c("resproutprob", "shadetolerance", "siteShade", "lightProb"), NULL)

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
      ## append resprouters to the table
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

  ## add new cohorts to pixels where serotiny/regeneration were activated
  if (NROW(postFireReproData) > 0) {
    maxPixelGroup <- as.integer(maxValue(pixelGroupMap))

    ## redo post-fire pixel groups by adding the maxPixelGroup to their ecoregioMap values
    if (!is.null(sim$postFirePixel)) {
      pixelGroupMap[sim$postFirePixel] <- maxPixelGroup +
        as.integer(as.factor(sim$ecoregionMap[sim$postFirePixel]))
      postFireReproData[, pixelGroup := maxPixelGroup +
                          as.integer(as.factor(postFireReproData$ecoregionGroup))]
    }

    ## regenerate biomass in pixels that have serotiny/resprouting
    sim$cohortData[, sumB := sum(B, na.rm = TRUE), by = pixelGroup]
    addnewcohort <- addNewCohorts(postFireReproData, sim$cohortData, pixelGroupMap,
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
  
  if (!suppliedElsewhere("studyArea", sim)) {
    message("'studyArea' was not provided by user. Using a polygon in Southwestern Alberta, Canada")

    canadaMap <- Cache(getData, 'GADM', country = 'CAN', level = 1, path = asPath(dPath),
                       cacheRepo = getPaths()$cachePath, quick = FALSE)
    smallPolygonCoords = list(
      coords = data.frame(x = c(-115.9022, -114.9815, -114.3677, -113.4470, -113.5084,
                                -114.4291, -115.3498, -116.4547, -117.1298, -117.3140),
                          y = c(50.45516, 50.45516, 50.51654, 50.51654, 51.62139,
                                52.72624, 52.54210, 52.48072, 52.11243, 51.25310))
    )

    sim$studyArea <- SpatialPolygons(list(Polygons(list(Polygon(smallPolygonCoords$coords)), ID = "swAB_polygon")),
                                              proj4string = crs(canadaMap))
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    message("'studyAreaLarge' was not provided by user. Using the same as 'studyArea'")
    sim$studyAreaLarge <- sim$studyArea
  }

  ## get LANDISII main input table where species and light requirements tables come from
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim))) {
    mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL in pemisc
  }

  ## read species txt and convert it to data table
  if (!suppliedElsewhere("species", sim)) {
    sim$species <- prepInputsSpecies(url = extractURL("species"), dPath, cacheTags)
  }

  ## make light requirements table
  if (!suppliedElsewhere("sufficientLight", sim)) {
    sufficientLight <- data.frame(mainInput)
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
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
    ras <- projectExtent(sim$studyArea, crs = sim$studyArea)
    res(ras) <- 250
    ecoregionMap <- rasterize(sim$studyArea, ras)

    ecoregionMap[!is.na(getValues(ecoregionMap))][] <- sample(ecoregion$mapcode,
                                                              size = sum(!is.na(getValues(ecoregionMap))),
                                                              replace = TRUE)
    sim$ecoregionMap <- ecoregionMap
  }

  return(invisible(sim))
}

