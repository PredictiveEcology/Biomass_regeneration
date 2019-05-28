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
  version = list(SpaDES.core = "0.2.3.9009", Biomass_regeneration = "0.1.0"),
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
    defineParameter("fireInitialTime", "numeric", 1L,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", 1L,
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
    expectsInput("sufficientLight", "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadeness",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt"),
    expectsInput("treedFirePixelTableSinceLastDisp", "data.table",
                 desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred")
  ),
  outputObjects = bind_rows(
    createsOutput("cohortData", "data.table",
                  desc = paste("age cohort-biomass table hooked to pixel group map",
                               "by pixelGroupIndex at succession time step")),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("pixelGroupMap", "RasterLayer",
                  desc = "updated community map at each succession time step"),
    createsOutput("serotinyResproutSuccessPixels", "numeric",
                  desc = "Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of treedBurnLoci"),
    createsOutput("postFireRegenSummary", "data.table",
                  desc = "summary table of species post-fire regeneration"),
    createsOutput("treedFirePixelTableSinceLastDisp", "data.table",
                  desc = "3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred")
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
      sim <- scheduleEvent(sim, P(sim)$fireInitialTime,
                           "Biomass_regeneration", "fireDisturbance",
                           eventPriority = 3)
    },
    fireDisturbance = {
      if(!is.null(sim$rstCurrentBurn)) {
        sim <- FireDisturbance(sim)
      } else {
        message(crayon::red("The Biomass_regeneration module is expecting sim$rstCurrentBurn; ",
                              " Currently, it does not exist."))
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep,
                           "Biomass_regeneration", "fireDisturbance",
                           eventPriority = 3)
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
FireDisturbance <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {
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
  # if (time(sim) >= 11) {
  #   browser()
  #   aaaa <<- 1
  # }

  postFirePixelCohortData <- sim$cohortData[0,]
  postFirePixelCohortData[, `:=`(pixelIndex = integer(),
                                 age = NULL, B = NULL, mortality = NULL,
                                 aNPPAct = NULL, sumB = NULL)]
  # postFirePixelCohortData <- data.table(pixelGroup = integer(), ecoregionGroup = numeric(),
  #                                 speciesCode = numeric(), pixelIndex = integer())
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

  ## extract burn pixel indices/groups and remove potentially inactive pixels
  burnedLoci <- which(getValues(sim$rstCurrentBurn) > 0)
  treedBurnLoci <- if (length(sim$inactivePixelIndex) > 0) {
    # These can burn other vegetation (grassland, wetland)
    burnedLoci[!(burnedLoci %in% sim$inactivePixelIndex)] # this is to prevent avaluating the pixels that are inactive
  } else {
    burnedLoci
  }
  treedFirePixelTableSinceLastDisp <-
    data.table(pixelIndex = as.integer(treedBurnLoci),
               pixelGroup = as.integer(getValues(sim$pixelGroupMap)[treedBurnLoci]),
               burnTime = time(sim))
  sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(getValues(sim$pixelGroupMap))[pixelIndex]]
  # append previous year's
  treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
                                                     treedFirePixelTableSinceLastDisp))

  ## make table spp/ecoregionGroup/age in burnt pixels
  burnedcohortData <- sim$cohortData[pixelGroup %in% unique(treedFirePixelTableSinceLastDisp$pixelGroup)]
  set(burnedcohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)
  #   set(burnedcohortData, ,c("sumB", "siteShade"), 0) # assume the fire burns all cohorts on site
  setkey(burnedcohortData, speciesCode)

  ## subset spp with serotiny
  tempspecies <- sim$species[postfireregen == "serotiny", .(speciesCode, postfireregen)]

  ## join tables to make a serotiny table
  serotinyPixelCohortData <- burnedcohortData[tempspecies, nomatch = 0][, postfireregen := NULL]

  rm(tempspecies)
  if (NROW(serotinyPixelCohortData) > 0) {
    ## assess potential serotiny reg: add sexual maturity to the table and compare w/ age
    ## as long as one cohort is sexually mature, serotiny is activated
    serotinyPixelCohortData <- serotinyPixelCohortData[sim$species[, .(speciesCode, sexualmature)],
                                                       on = "speciesCode", nomatch = 0]
    #serotinyPixelCohortData <- setkey(serotinyPixelCohortData, speciesCode)[sim$species[,.(speciesCode, sexualmature)],
    #                                                                          nomatch = 0]
    serotinyPixelCohortData <- serotinyPixelCohortData[age >= sexualmature] %>% # NOTE should be in mortalityFromDisturbance module or event
      unique(., by = c("pixelGroup", "speciesCode"))
    set(serotinyPixelCohortData, NULL, "sexualmature", NULL)

    ## select the pixels that have potential serotiny regeneration and assess them
    serotinyPixelTable <- treedFirePixelTableSinceLastDisp[pixelGroup %in% unique(serotinyPixelCohortData$pixelGroup)]

    ## from now on the regeneration process is assessed for each potential pixel
    #setkey(serotinyPixelTable, pixelGroup)
    #setkey(serotinyPixelCohortData, pixelGroup)
    serotinyPixelCohortData <- serotinyPixelTable[serotinyPixelCohortData, allow.cartesian = TRUE,
                                                  nomatch = 0, on = "pixelGroup"] ## join table to add pixels

    ## light check: add shade tolerance to table and set shade to 0 (100% mortality.)
    ## the get survival probs and subset survivors with runif
    serotinyPixelCohortData <- serotinyPixelCohortData[sim$species[, .(speciesCode, shadetolerance)],
                                                       nomatch = 0, on = "speciesCode"]
    serotinyPixelCohortData[, siteShade := 0]
    # serotinyPixelCohortData <- setkey(serotinyPixelCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
    #                                                     nomatch = 0][, siteShade := 0]
    serotinyPixelCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                               serotinyPixelCohortData)
    serotinyPixelCohortData <- serotinyPixelCohortData[lightProb %>>% runif(nrow(serotinyPixelCohortData), 0, 1)]  ## subset survivors
    set(serotinyPixelCohortData, NULL, c("shadetolerance", "siteShade", "lightProb"), NULL)   ## clean table again

    ## get establishment probs and subset species that establish with runif
    specieseco_current <- sim$speciesEcoregion[year <= round(time(sim))]
    specieseco_current <- specieseco_current[year == max(specieseco_current$year),
                                             .(ecoregionGroup, speciesCode, establishprob)]
    serotinyPixelCohortData <- serotinyPixelCohortData[specieseco_current, on = c("ecoregionGroup", "speciesCode"), nomatch = 0]
    #serotinyPixelCohortData <- setkey(serotinyPixelCohortData, ecoregionGroup, speciesCode)[specieseco_current, nomatch = 0]  ## join table to add probs
    serotinyPixelCohortData <- serotinyPixelCohortData[runif(nrow(serotinyPixelCohortData), 0, 1) %<<% establishprob][, establishprob := NULL]

    ## only need one cohort per spp per pixel survives/establishes
    serotinyPixelCohortData <- unique(serotinyPixelCohortData, by = c("pixelIndex", "speciesCode"))

    if (NROW(serotinyPixelCohortData) > 0) {
      ## rm age
      serotinyPixelCohortData <- serotinyPixelCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)] #
      serotinyPixelCohortData[, type := "serotiny"]
      if(P(sim)$calibrate){
        serotinyRegenSummary <- serotinyPixelCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode]
        serotinyRegenSummary <- serotinyRegenSummary[,.(year = time(sim), regenMode = "Serotiny",
                                                        speciesCode, numberOfRegen)]
        serotinyRegenSummary <- setkey(serotinyRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        serotinyRegenSummary[, ':='(speciesCode = species, species = NULL)]
        setnames(serotinyRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, serotinyRegenSummary))
      }
      serotinyPixel <- unique(serotinyPixelCohortData$pixelIndex) # save the pixel index for resprouting assessment use,
      # i.e., removing these pixel from assessing resprouting
      ## append table to (yet empty) postFirePixelCohortData
      postFirePixelCohortData <- rbindlist(list(postFirePixelCohortData, serotinyPixelCohortData), fill = TRUE)
    } else {
      serotinyPixel <- NULL
    }
    rm(serotinyPixelCohortData)
  } else {
    serotinyPixel <- NULL
  }

  #############################################################
  #############################################################
  # from now on, starting assessing resprouting reproduction:
  # basically same thing as serotiny

  ## make a table of pixels where resprouting occurs.
  if (is.null(serotinyPixel)) {
    resproutingPixelTable <- setkey(treedFirePixelTableSinceLastDisp, pixelGroup)
    availableToResprout <- burnedcohortData[0,]
  } else {
    # Replacing here -- ELiot -- THis was removing entire pixels that had successful serotiny -- now only species-pixel combos are removed
    ## should be done by pixel and species -- Eliot: it works ok now because there are no serotinous species that are resprouters
    full <- treedFirePixelTableSinceLastDisp[unique(burnedcohortData, by = c("pixelGroup", "speciesCode")),
                                             on = "pixelGroup", allow.cartesian = TRUE] #

    # anti join to remove species-pixels that had successful serotiny
    availableToResprout <- full[!postFirePixelCohortData, on = c("pixelIndex", "speciesCode")]
  }

  ## assess whether reprouting can occur in burnt pixels
  species_temp <- sim$species[postfireregen == "resprout",
                              .(speciesCode, postfireregen,
                                resproutage_min, resproutage_max, resproutprob)]

  resproutingPixelCohortData <- availableToResprout[species_temp, nomatch = 0, on = "speciesCode"]
  resproutingPixelCohortData <- resproutingPixelCohortData[age >= resproutage_min & age <= resproutage_max]
  # Eliot: remove next line --- seems wrong to use burnedcohortData, rather than availableToResprout
  #resproutingAssessCohortData <- burnedcohortData[species_temp, nomatch = 0][age >= resproutage_min & age <= resproutage_max]
  set(resproutingPixelCohortData, NULL, c("resproutage_min", "resproutage_max", "postfireregen", "age"), NULL)
  rm(species_temp)

  if (NROW(resproutingPixelCohortData) > 0) {
    ## assess potential resprouting reg: add reprout probability, siteShade/tolerance to the table and assess who resprouts
    ## as long as one cohort can resprout, resprouting is activated
    #resproutingAssessCohortData <- unique(resproutingAssessCohortData, by = c("pixelGroup", "speciesCode"))
    #setkey(resproutingAssessCohortData, pixelGroup)

    ## make new table joing resprouters with burnt pixels
    #newPixelCohortData <- resproutingPixelTable[resproutingAssessCohortData, nomatch = 0, allow.cartesian = TRUE]

    ## light check: add shade tolerance to table and set shade to 0 (100% mortality.)
    ## the get survival probs and subset survivors with runif
    resproutingPixelCohortData <- resproutingPixelCohortData[sim$species[, .(speciesCode, shadetolerance)],
                                                             nomatch = 0, on = "speciesCode"]
    resproutingPixelCohortData[,siteShade := 0]
    # resproutingPixelCohortData <- setkey(resproutingPixelCohortData, speciesCode)[sim$species[,.(speciesCode, shadetolerance)],
    #                                                     nomatch = 0][, siteShade := 0]
    resproutingPixelCohortData <- assignLightProb(sufficientLight = sim$sufficientLight,
                                                  resproutingPixelCohortData)

    resproutingPixelCohortData <- resproutingPixelCohortData[lightProb %>>% runif(nrow(resproutingPixelCohortData), 0, 1)]
    resproutingPixelCohortData <- resproutingPixelCohortData[resproutprob %>>% runif(nrow(resproutingPixelCohortData), 0, 1)]

    resproutingPixelCohortData <- unique(resproutingPixelCohortData, by = c("pixelIndex", "speciesCode"))
    set(resproutingPixelCohortData, NULL, c("resproutprob", "shadetolerance", "siteShade", "lightProb"), NULL)

    # remove all columns that were used temporarily here
    if (NROW(resproutingPixelCohortData) > 0) {
      resproutingPixelCohortData <- resproutingPixelCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, pixelIndex)]#
      resproutingPixelCohortData[, type := "resprouting"]
      if(P(sim)$calibrate){
        resproutRegenSummary <- resproutingPixelCohortData[,.(numberOfRegen = length(pixelIndex)), by = speciesCode]
        resproutRegenSummary <- resproutRegenSummary[,.(year = time(sim), regenMode = "Resprout",
                                                        speciesCode, numberOfRegen)]
        resproutRegenSummary <- setkey(resproutRegenSummary, speciesCode)[sim$species[,.(species, speciesCode)],
                                                                          nomatch = 0]
        resproutRegenSummary[,':='(speciesCode = species, species = NULL)]
        setnames(resproutRegenSummary, "speciesCode", "species")
        sim$postFireRegenSummary <- rbindlist(list(sim$postFireRegenSummary, resproutRegenSummary))
      }
      ## append resprouters to the table
      postFirePixelCohortData <- rbindlist(list(postFirePixelCohortData, resproutingPixelCohortData), fill = TRUE)
      postFirePixelCohortData[, type := factor(type)]
      serotinyResproutSuccessPixels <- c(serotinyPixel, unique(resproutingPixelCohortData$pixelIndex))
      sim$serotinyResproutSuccessPixels <- serotinyResproutSuccessPixels # send it to a sim object
      rm(resproutingPixelCohortData)
    } else{
      sim$serotinyResproutSuccessPixels <- serotinyPixel
    }
  } else {
    sim$serotinyResproutSuccessPixels <- serotinyPixel
  }

  ## add new cohorts to pixels where serotiny/regeneration were activated
  if (NROW(postFirePixelCohortData) > 0) {
    ## redo post-fire pixel groups by adding the maxPixelGroup to their ecoregioMap values
    if (!is.null(sim$serotinyResproutSuccessPixels)) {

      # Add new cohorts to BOTH the sim$cohortData and sim$pixelGroupMap
      ## reclassify pixel groups as burnt (0L)
      if (verbose > 0)
        message(blue("Post serotiny and resprouting"))

      outs <- updateCohortData(newPixelCohortData = postFirePixelCohortData,
                               cohortData = sim$cohortData,
                               pixelGroupMap = sim$pixelGroupMap,
                               time = round(time(sim)),
                               speciesEcoregion = sim$speciesEcoregion,
                               treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                               successionTimestep = P(sim)$successionTimestep)

      sim$cohortData <- outs$cohortData
      sim$pixelGroupMap <- outs$pixelGroupMap
      sim$pixelGroupMap[] <- as.integer(sim$pixelGroupMap[])
      ##########################################################
      # rm missing cohorts (i.e., those pixelGroups that are gone due to the fire/treedFirePixelTableSinceLastDisp)
      ##########################################################
    }
  }

  sim$lastFireYear <- time(sim)
  sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableSinceLastDisp
  return(invisible(sim))
}

## ---------------------------------------------------------------------------
## INPUT OBJECTS

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (suppliedElsewhere(object = "scfmReturnInterval", sim = sim, where = "sim")) {
    if (P(sim)$fireTimestep != sim$scfmReturnInterval){
      sim@params$Biomass_regeneration$fireTimestep <- sim$scfmReturnInterval
      message(paste0("Biomass_regeneration detected 'scfm' fire model. Setting fireTimestep to ", 
                     sim$scfmReturnInterval, 
                     " to match it."))
      }
    } else {
      if(is.null(P(sim)$fireTimestep)){
        P(sim)$fireTimestep <- 1L
        message("fireTimestep is 'NULL'. Setting to 1 unit of time")
      }
    }
  
  if (!suppliedElsewhere("studyArea", sim)) {
    message("'studyArea' was not provided by user. Using a polygon in Southwestern Alberta, Canada")

    sim$studyArea <- randomStudyArea(seed = 1234)
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

  if (!suppliedElsewhere(sim$treedFirePixelTableSinceLastDisp)) {
    sim$treedFirePixelTableSinceLastDisp <- data.table(pixelIndex = integer(), pixelGroup = integer(),
                                                       burnTime = numeric())
  }

  return(invisible(sim))
}
