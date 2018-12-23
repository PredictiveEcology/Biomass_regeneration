# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "Biomass_regeneration",
  description = "Post-disturbance biomass regeneration module for LandR. Simulates post-fire mortality, regeneration and serotiny as part of the same event - all occurring sequentially immeadiately after fire. Mortality assumed to be 100%, serotiny and regeneration algorithms taken from LANDIS-II Biomass Succession extension, v3.6.1",
  keywords = c("biomass regeneration", "LandR", "disturbance", "mortality", "vegetation succession", "vegetation model"),
  authors = person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "Biomass_regeneration.Rmd"),
  reqdPkgs = list("crayon", "data.table", "raster", ## TODO: update package list!
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/pemisc@development"),
  parameters = rbind(
    defineParameter("calibrate", "logical", FALSE, desc = "Do calibration? Defaults to FALSE"),
    defineParameter("fireInitialTime", "numeric", 2L,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", 2L,
                    desc = "The number of time units between successive fire events in a fire module"),
    defineParameter("successionTimestep", "numeric", 10L,
                    desc = "The number of time units between successive seed dispersal events, the 'LANDIS succession time step'")
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
                 desc = "Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned"),
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
    createsOutput("cohortData", "data.table",
                  desc = paste("age cohort-biomass table hooked to pixel group map",
                               "by pixelGroupIndex at succession time step")),
    createsOutput("firePixelTable", "data.table",
                  desc = "table with pixels IDs that had fire and their corresponding pixel groups"),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "updated community map at each succession time step"),
    createsOutput("serotinyResproutSuccessPixels", "numeric",
                  desc = "Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of burnLoci"),
    createsOutput("postFireRegenSummary", "data.table",
                  desc = "summary table of species post-fire regeneration")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.Biomass_regeneration <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      ## schedule events
      if(!is.null(sim$rstCurrentBurn)){ # anything related to fire disturbance
        sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                             "Biomass_regeneration", "fireDisturbance",
                             eventPriority = 3)
      } else {
        message(crayon::green("The Biomass_regeneration module should be loaded after a fire module ",
                "because it assumes that sim$rstCurrentBurn exists. Currently, it does ",
                "not. If this is not correct, please load this module after a fire module."))
      }
    },
    fireDisturbance = {
      sim <- FireDisturbance(sim)

      if(!is.null(sim$rstCurrentBurn)) {
        sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                             "Biomass_regeneration", "fireDisturbance",
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
  if (isTRUE(getOption("LandR.assertions"))) {
    if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = c("pixelGroup", "speciesCode", "age", "B"))))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }

  }

  postFireNewCohortData <- data.table(pixelGroup = integer(), ecoregionGroup = numeric(),
                                  speciesCode = numeric(), pixelIndex = integer())
  if(P(sim)$calibrate){
    sim$postFireRegenSummary <- data.table(year = numeric(),
                                           regenMode = character(),
                                           species = character(),
                                           numberOfRegen = numeric())
  }

  # if (!is.null(sim$rstCurrentBurn)) { # anything related to fire disturbance
  #   if (extent(sim$rstCurrentBurn) != extent(pixelGroupMap)) {
  #     sim$rstCurrentBurn <- raster::crop(sim$rstCurrentBurn, extent(pixelGroupMap))
  #   }
  # }

  ## extract burn pixel indices/groups and remove potentially innactive pixels
  burnLoci <- which(getValues(sim$rstCurrentBurn) > 0)
  if (length(sim$inactivePixelIndex) > 0) {
    # These can burn other vegetation (grassland, wetland)
    burnLoci <- burnLoci[!(burnLoci %in% sim$inactivePixelIndex)] # this is to prevent avaluating the pixels that are inactive
  }
  firePixelTable <- data.table(cbind(pixelIndex = as.integer(burnLoci),
                                     pixelGroup = as.integer(getValues(sim$pixelGroupMap)[burnLoci])))
  burnPixelGroup <- unique(firePixelTable$pixelGroup)

  ## make table spp/ecoregionGroup/age in burnt pixels
  burnedcohortData <- sim$cohortData[pixelGroup %in% burnPixelGroup]
  set(burnedcohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)
  #   set(burnedcohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedcohortData, speciesCode)

  ## subset spp with serotiny
  tempspecies <- sim$species[postfireregen == "serotiny", .(speciesCode, postfireregen)]

  ## join tables to make a serotiny table
  serotinyCohortData <- burnedcohortData[tempspecies, nomatch = 0][, postfireregen := NULL]

  rm(tempspecies)
  if (NROW(serotinyCohortData) > 0) {
    ## assess potential serotiny reg: add sexual maturity to the table and compare w/ age
    ## as long as one cohort is sexually mature, serotiny is activated
    serotinyCohortData <- serotinyCohortData[sim$species[, .(speciesCode, sexualmature)],
                                                         on = "speciesCode", nomatch = 0]
    #serotinyCohortData <- setkey(serotinyCohortData, speciesCode)[sim$species[,.(speciesCode, sexualmature)],
    #                                                                          nomatch = 0]
    serotinyCohortData <- serotinyCohortData[age >= sexualmature] %>% # NOTE should be in mortalityFromDisturbance module or event
      unique(., by = c("pixelGroup", "speciesCode"))
    set(serotinyCohortData, NULL, "sexualmature", NULL)

    ## select the pixels that have potential serotiny regeneration and assess them
    serotinyPixelTable <- firePixelTable[pixelGroup %in% unique(serotinyCohortData$pixelGroup)]

    ## from now on the regeneration process is assessed for each potential pixel
    #setkey(serotinyPixelTable, pixelGroup)
    #setkey(serotinyCohortData, pixelGroup)
    serotinyCohortData <- serotinyPixelTable[serotinyCohortData, nomatch = 0, on = "pixelGroup"] ## join table to add pixels

    ## light check: add shade tolerance to table and set shade to 0 (100% mortality.)
    ## the get survival probs and subset survivors with runif
    serotinyCohortData <- serotinyCohortData[sim$species[, .(speciesCode, shadetolerance)], nomatch = 0, on = "speciesCode"]
    serotinyCohortData[, siteShade := 0]
    # serotinyCohortData <- setkey(serotinyCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
    #                                                     nomatch = 0][, siteShade := 0]
    serotinyCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                     serotinyCohortData)
    serotinyCohortData <- serotinyCohortData[lightProb %>>% runif(nrow(serotinyCohortData), 0, 1)]  ## subset survivors
    set(serotinyCohortData, NULL, c("shadetolerance", "siteShade", "lightProb"), NULL)   ## clean table again

    ## get establishment probs and subset species that establish with runif
    specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
    specieseco_current <- specieseco_current[year == max(specieseco_current$year),
                                             .(ecoregionGroup, speciesCode, establishprob)]
    serotinyCohortData <- serotinyCohortData[specieseco_current, on = c("ecoregionGroup", "speciesCode"), nomatch = 0]
    #serotinyCohortData <- setkey(serotinyCohortData, ecoregionGroup, speciesCode)[specieseco_current, nomatch = 0]  ## join table to add probs
    serotinyCohortData <- serotinyCohortData[runif(nrow(serotinyCohortData), 0, 1) %<<% establishprob][, establishprob := NULL]

    ## only need one cohort per spp per pixel survives/establishes
    serotinyCohortData <- unique(serotinyCohortData, by = c("pixelIndex", "speciesCode"))

    if (NROW(serotinyCohortData) > 0) {
      ## rm age
      serotinyCohortData <- serotinyCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)] #
      serotinyCohortData[, type := "serotiny"]
      if(P(sim)$calibrate){
        serotinyRegenSummary <- serotinyCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode]
        serotinyRegenSummary <- serotinyRegenSummary[,.(year = time(sim), regenMode = "Serotiny",
                                                        speciesCode, numberOfRegen)]
        serotinyRegenSummary <- setkey(serotinyRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        serotinyRegenSummary[, ':='(speciesCode = species, species = NULL)]
        setnames(serotinyRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, serotinyRegenSummary))
      }
      serotinyPixel <- unique(serotinyCohortData$pixelIndex) # save the pixel index for resprouting assessment use,
      # i.e., removing these pixel from assessing resprouting
      ## append table to (yet empty) postFireNewCohortData
      postFireNewCohortData <- rbindlist(list(postFireNewCohortData, serotinyCohortData), fill = TRUE)
    } else {
      serotinyPixel <- NULL
    }
    rm(serotinyCohortData)
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
    availableToResprout <- burnedcohortData[0,]
  } else {
    # Replacing here -- ELiot -- THis was removing entire pixels that had successful serotiny -- now only species-pixel combos are removed
    ## should be done by pixel and species -- Eliot: it works ok now because there are no serotinous species that are resprouters
    full <- firePixelTable[unique(burnedcohortData, by = c("pixelGroup", "speciesCode")), on = "pixelGroup"] #

    # anti join to remove species-pixels that had successful serotiny
    availableToResprout <- full[!postFireNewCohortData, on = c("pixelIndex", "speciesCode")]
  }

  ## assess whether reprouting can occur in burnt pixels
  species_temp <- sim$species[postfireregen == "resprout",
                              .(speciesCode, postfireregen,
                                resproutage_min, resproutage_max, resproutprob)]

  resproutingCohortData <- availableToResprout[species_temp, nomatch = 0, on = "speciesCode"]
  resproutingCohortData <- resproutingCohortData[age >= resproutage_min & age <= resproutage_max]
  # Eliot: remove next line --- seems wrong to use burnedcohortData, rather than availableToResprout
  #resproutingAssessCohortData <- burnedcohortData[species_temp, nomatch = 0][age >= resproutage_min & age <= resproutage_max]
  set(resproutingCohortData, NULL, c("resproutage_min", "resproutage_max", "postfireregen", "age"), NULL)
  rm(species_temp)

  if (NROW(resproutingCohortData) > 0) {
    ## assess potential resprouting reg: add reprout probability, siteShade/tolerance to the table and assess who resprouts
    ## as long as one cohort can resprout, resprouting is activated
    #resproutingAssessCohortData <- unique(resproutingAssessCohortData, by = c("pixelGroup", "speciesCode"))
    #setkey(resproutingAssessCohortData, pixelGroup)

    ## make new table joing resprouters with burnt pixels
    #newCohortData <- resproutingPixelTable[resproutingAssessCohortData, nomatch = 0, allow.cartesian = TRUE]

    ## light check: add shade tolerance to table and set shade to 0 (100% mortality.)
    ## the get survival probs and subset survivors with runif
    resproutingCohortData <- resproutingCohortData[sim$species[, .(speciesCode, shadetolerance)],
                                                   nomatch = 0, on = "speciesCode"]
    resproutingCohortData[,siteShade := 0]
    # resproutingCohortData <- setkey(resproutingCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
    #                                                     nomatch = 0][, siteShade := 0]
    resproutingCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                     resproutingCohortData)

    resproutingCohortData <- resproutingCohortData[lightProb %>>% runif(nrow(resproutingCohortData), 0, 1)]
    resproutingCohortData <- resproutingCohortData[resproutprob %>>% runif(nrow(resproutingCohortData), 0, 1)]

    resproutingCohortData <- unique(resproutingCohortData, by = c("pixelIndex", "speciesCode"))
    set(resproutingCohortData, NULL, c("resproutprob", "shadetolerance", "siteShade", "lightProb"), NULL)

    # remove all columns that were used temporarily here
    if (NROW(resproutingCohortData) > 0) {
      resproutingCohortData <- resproutingCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)]#
      resproutingCohortData[, type := "resprouting"]
      if(P(sim)$calibrate){
        resproutRegenSummary <- resproutingCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode]
        resproutRegenSummary <- resproutRegenSummary[,.(year = time(sim), regenMode = "Resprout",
                                                        speciesCode, numberOfRegen)]
        resproutRegenSummary <- setkey(resproutRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        resproutRegenSummary[,':='(speciesCode = species, species = NULL)]
        setnames(resproutRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, resproutRegenSummary))
      }
      ## append resprouters to the table
      postFireNewCohortData <- rbindlist(list(postFireNewCohortData, resproutingCohortData), fill = TRUE)
      postFireNewCohortData[, type := factor(type)]
      serotinyResproutSuccessPixels <- c(serotinyPixel, unique(resproutingCohortData$pixelIndex))
      sim$serotinyResproutSuccessPixels <- serotinyResproutSuccessPixels # send it to a sim object
      rm(resproutingCohortData)
    } else{
      sim$serotinyResproutSuccessPixels <- serotinyPixel
      postFireNewCohortData <- postFireNewCohortData
    }
  } else {
    postFireNewCohortData <- postFireNewCohortData
    sim$serotinyResproutSuccessPixels <- serotinyPixel
  }

  ## add new cohorts to pixels where serotiny/regeneration were activated
  if (NROW(postFireNewCohortData) > 0) {
    ## redo post-fire pixel groups by adding the maxPixelGroup to their ecoregioMap values
    if (!is.null(sim$serotinyResproutSuccessPixels)) {

      # Add new cohorts to BOTH the sim$cohortData and sim$pixelGroupMap
      ## reclassify pixel groups as burnt (0L)
      outs <- updateCohortData(newCohortData = postFireNewCohortData,
                         cohortData = sim$cohortData,
                         pixelGroupMap = sim$pixelGroupMap,
                         time = round(time(sim)),
                         speciesEcoregion = sim$speciesEcoregion,
                         firePixelTable = firePixelTable,
                         successionTimestep = P(sim)$successionTimestep)
      sim$cohortData <- outs$cohortData
      sim$pixelGroupMap <- outs$pixelGroupMap
      ##########################################################
      # rm missing cohorts (i.e., those pixelGroups that are gone due to the fire/firePixelTable)
      ##########################################################
      #sim$cohortData <- rmMissingCohorts(sim$cohortData, sim$pixelGroupMap, firePixelTable)
      if (isTRUE(getOption("LandR.assertions"))) {
        testCohortData(sim$cohortData, sim$pixelGroupMap, sim = sim)
      }
    }
  }
  if (NROW(unique(sim$cohortData[pixelGroup == 43467]$ecoregionGroup))>1) stop()

  sim$lastFireYear <- time(sim)
  sim$firePixelTable <- firePixelTable
  return(invisible(sim))
}

## ---------------------------------------------------------------------------
## INPUT OBJECTS

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("studyArea", sim)) {
    message("'studyArea' was not provided by user. Using a polygon in Southwestern Alberta, Canada")

    sim$studyArea <- randomStudyArea(seed = 1234)
  }

  if (!suppliedElsewhere("studyAreaLarge", sim)) {
    message("'studyAreaLarge' was not provided by user. Using the same as 'studyArea'")
    sim$studyAreaLarge <- sim$studyArea
  }

  ## get LANDISII main input table where species and light requirements tables come from
  if (!suppliedElsewhere("sufficientLight", sim) |
      (!suppliedElsewhere("species", sim))) {
    mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL
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
    sim$speciesEcoregion <- prepInputsSpeciesEcoregion(url = extractURL("speciesEcoregion"),
                                                       dPath = dPath, cacheTags = cacheTags)
  }

  ## load ecoregion map
  if (!suppliedElsewhere("ecoregionMap", sim )) {
    ## LANDIS-II demo data:

    ## TODO: restore the demo data version with prepInputs:
    # sim$ecoregionMap <- Cache(prepInputs,
    #                           url = extractURL("ecoregionMap"),
    #                           destinationPath = dPath,
    #                           targetFile = "ecoregions.gis",
    #                           fun = "raster::raster")

    ## Dummy version with spatial location in Canada
    ras <- projectExtent(sim$studyArea, crs = sim$studyArea)
    res(ras) <- 250 ## TODO: don't hardcode this; get from rasterToMatch?
    ecoregionMap <- rasterize(sim$studyArea, ras) ## TODO: use fasterize

    ecoregionMap[!is.na(getValues(ecoregionMap))][] <- sample(ecoregion$mapcode,
                                                              size = sum(!is.na(getValues(ecoregionMap))),
                                                              replace = TRUE)
    sim$ecoregionMap <- ecoregionMap
  }

  return(invisible(sim))
}

