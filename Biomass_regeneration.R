defineModule(sim, list(
  name = "Biomass_regeneration",
  description = paste(
    "Post-disturbance biomass regeneration module for LandR. Simulates post-fire mortality,",
    "regeneration and serotiny as part of the same event - all occurring sequentially immeadiately after fire.",
    "Mortality assumed to be 100%, serotiny and regeneration algorithms taken from LANDIS-II Biomass Succession extension, v3.2.1"
  ),
  keywords = c("biomass regeneration", "LandR", "disturbance", "mortality", "vegetation succession", "vegetation model"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Yong", "Luo", email = "yluo1@lakeheadu.ca", role = "aut"),
    person("Ceres", "Barros", email = "cbarros@mail.ubc.ca", role = "aut"),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(Biomass_regeneration = "1.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  loadOrder = list(after = "Biomass_core"),
  documentation = list("README.md", "Biomass_regeneration.Rmd"),
  reqdPkgs = list("crayon", "data.table", "terra", ## TODO: update package list!
                  "PredictiveEcology/LandR@development (>= 1.1.5.9016)",
                  "PredictiveEcology/pemisc@development"),
  parameters = rbind(
    defineParameter("calibrate", "logical", FALSE, NA, NA, desc = "Do calibration? Defaults to FALSE"),
    defineParameter("cohortDefinitionCols", "character", LandR::cohortDefinitionCols(), NA, NA,
                    desc = "columns in cohortData that determine unique cohorts"),
    defineParameter("fireInitialTime", "numeric", start(sim, "year") + 1, NA, NA,
                    desc = "The event time that the first fire disturbance event occurs"),
    defineParameter("fireTimestep", "numeric", 1, NA, NA,
                    desc = "The number of time units between successive fire events in a fire module"),
    defineParameter("initialB", "numeric", 10, 1, NA,
                    desc = paste("initial biomass values of new age-1 cohorts.",
                                 "If `NA` or `NULL`, initial biomass will be calculated as in LANDIS-II Biomass Suc. Extension",
                                 "(see Scheller and Miranda, 2015 or `?LandR::.initiateNewCohorts`)")),
    defineParameter("successionTimestep", "numeric", 10L, NA, NA, "defines the simulation time step, default is 10 years"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim, "year"), NA, NA,
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA,
                    desc = paste("Should this entire module be run with caching activated?",
                                 "This is generally intended for data-type modules, where stochasticity and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput("cohortData", "data.table",
                 desc = paste("age cohort-biomass table hooked to pixel group map by
                              `pixelGroupIndex` at succession time step")),
    expectsInput("inactivePixelIndex", "logical",
                 desc = "internal use. Keeps track of which pixels are inactive"),
    expectsInput("pixelGroupMap", "SpatRaster",
                 desc = "updated community map at each succession time step"),
    expectsInput("rasterToMatch", "SpatRaster",
                 desc = "a raster of the `studyArea`."),
    expectsInput("rstCurrentBurn", "SpatRaster",
                 desc = "Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned"),
    expectsInput("species", "data.table",
                 desc = paste("A table of invariant species traits with the following trait colums:",
                              "'Name', 'Longevity', 'Sexual Maturity', 'Shade Tol.', 'Fire Tol.'",
                              "'Seed Dispersal Dist Effective', 'Seed Dispersal Dist Maximum'",
                              "'Vegetative Reprod Prob', 'Sprout Age Min', 'Sprout Age Max'",
                              "'Post-Fire Regen'"),
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt"),
    expectsInput("sufficientLight", "data.frame",
                 desc = "table defining how the species with different shade tolerance respond to stand shadiness",
                 sourceURL = paste0("https://raw.githubusercontent.com/LANDIS-II-Foundation/",
                                    "Extensions-Succession/master/biomass-succession-archive/",
                                    "trunk/tests/v6.0-2.0/biomass-succession_test.txt")),
    expectsInput("treedFirePixelTableSinceLastDisp", "data.table",
                 desc = paste(
                   "Each row represents a forested pixel that was burned up to and including this year,",
                   "since last dispersal event, with its corresponding `pixelGroup` and time it occurred.",
                   "With columns: `pixelIndex`, `pixelGroup`, and `burnTime`."
                   ))
  ),
  outputObjects = bindrows(
    createsOutput("cohortData", "data.table",
                  desc = paste("age cohort-biomass table hooked to pixel group map",
                               "by `pixelGroupIndex` at succession time step")),
    createsOutput("lastFireYear", "numeric",
                  desc = "Year of the most recent fire year"),
    createsOutput("pixelGroupMap", "SpatRaster",
                  desc = "updated community map at each succession time step"),
    createsOutput("serotinyResproutSuccessPixels", "numeric",
                  desc = "Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of `treedBurnLoci`."),
    createsOutput("postFireRegenSummary", "data.table",
                  desc = "summary table of species post-fire regeneration"),
    createsOutput("severityBMap", "SpatRaster",
                  desc = "A map of fire severity, as in the amount of post-fire mortality (biomass loss)"),
    createsOutput("severityData", "data.table",
                  desc = paste("A data.table of pixel fire severity, as in the amount of post-fire mortality (biomass loss).",
                               "May also have severity class used to calculate mortality.")),
    createsOutput("treedFirePixelTableSinceLastDisp", "data.table",
                  desc = paste(
                    "Each row represents a forested pixel that was burned up to and including this year,",
                    "since last dispersal event, with its corresponding `pixelGroup` and time it occurred.",
                    "with columns: `pixelIndex`, `pixelGroup`, and `burnTime`."))
  )
))

## event types
#   - type `init` is required for initialization

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
      if (!is.null(sim$rstCurrentBurn)) {
        sim <- FireDisturbance(sim)
      } else {
        message(crayon::red(
          paste0("The Biomass_regeneration module is expecting sim$rstCurrentBurn;\n",
                 "Currently, it does not exist, so no regeneration will happen.")
        ))
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
  ## check parameters
  if (is.na(P(sim)$fireInitialTime)) {
    stop(paste("Please provide a value for `P(sim)$fireInitialTime`.",
               "It should match the first year of fire."))
  }
  if (is.na(P(sim)$fireTimestep)) {
    stop(paste("Please provide a value for `P(sim)$fireTimestep`.",
               "It should match the fire time step (fire frequency)."))
  }

  paramCheckOtherMods(sim, "initialB", ifSetButDifferent = "warning")

  return(invisible(sim))
}

## Fire disturbance regeneration event
FireDisturbance <- function(sim, verbose = getOption("LandR.verbose", TRUE)) {
  ## the presence of valid fire can cause three processes:
  ## 1. remove species cohorts from the pixels that have been affected.
  ## 2. initiate the post-fire regeneration
  ## 3. change of cohortdata and pixelgroup map
  ## maybe a supplementary function is needed to convert non-logical map to a logical map
  if (isTRUE(getOption("LandR.assertions"))) {
    if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = P(sim)$cohortDefinitionCols)))) {
      stop("cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }
  }

  postFirePixelCohortData <- sim$cohortData[0, ]
  postFirePixelCohortData[, `:=`(pixelIndex = integer(0),
                                 age = NULL, B = NULL, mortality = NULL,
                                 aNPPAct = NULL)]

  ## In some cases sumB exists, but not always -- we want to remove it too here.
  if (isTRUE("sumB" %in% colnames(postFirePixelCohortData))) {
    set(postFirePixelCohortData, NULL, "sumB", NULL)
  }

  if (P(sim)$calibrate && is.null(sim$postFireRegenSummary)) {  ## don't overwrite
    sim$postFireRegenSummary <- data.table(year = numeric(0),
                                           regenMode = character(0),
                                           species = character(0),
                                           numberOfRegen = numeric(0))
  }

  ## extract burn pixel indices/groups and remove potentially inactive pixels
  burnedLoci <- which(as.vector(sim$rstCurrentBurn[]) > 0)
  treedBurnLoci <- if (length(sim$inactivePixelIndex) > 0) {
    # These can burn other vegetation (grassland, wetland)
    burnedLoci[!(burnedLoci %in% sim$inactivePixelIndex)] # this is to prevent evaluating the pixels that are inactive
  } else {
    burnedLoci
  }
  treedFirePixelTableSinceLastDisp <- if (length(treedBurnLoci) > 0) {
    data.table(pixelIndex = as.integer(treedBurnLoci),
               pixelGroup = as.integer(as.vector(sim$pixelGroupMap)[treedBurnLoci]),
               burnTime = time(sim))
  } else {
    data.table(pixelIndex = integer(0),
               pixelGroup = integer(0),
               burnTime = numeric(0))
  }

  ## TODO: Ceres: maybe this should come at the end, lest we introduce pixelGroups that burned
  ## in previous years, but aren't currently burning
  # sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(as.vector(sim$pixelGroupMap[]))[pixelIndex]]
  ## append previous year's
  # treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
  #                                                    treedFirePixelTableSinceLastDisp))

  ## make table spp/ecoregionGroup/age in burnt pixels
  burnedPixelCohortData <- sim$cohortData[pixelGroup %in% unique(treedFirePixelTableSinceLastDisp$pixelGroup)]
  set(burnedPixelCohortData, NULL, c("B", "mortality", "aNPPAct"), NULL)

  ## select the pixels that have burned survivors and assess them
  burnedPixelTable <- treedFirePixelTableSinceLastDisp[pixelGroup %in% unique(burnedPixelCohortData$pixelGroup)]
  ## expand table to pixels
  burnedPixelCohortData <- burnedPixelTable[burnedPixelCohortData, allow.cartesian = TRUE,
                                            nomatch = 0, on = "pixelGroup"]

  ## CALCULATE SEVERITY -----------------------------
  ## add biomass-based severity to severityData
  severityData <- calcSeverityB(sim$cohortData, burnedPixelCohortData)

  ## make severity map
  severityBMap <- setValues(sim$rasterToMatch, rep(NA, ncell(sim$rasterToMatch)))
  severityBMap[severityData$pixelIndex] <- severityData$severityB

  ## export DT and map
  sim$severityBMap <- severityBMap
  sim$severityData <- severityData

  ## CALCULATE SIDE SHADE -----------------------------
  # assume the fire burns all cohorts on site - siteShade calc is no longer part of serotiny.
  # sumB is not actually necessary, but added for consistency w/ Biomass_regenerationPM
  set(burnedPixelCohortData, NULL, c("sumB", "siteShade"), 0)
  setkey(burnedPixelCohortData, speciesCode)

  ## DO SEROTINY -----------------------------
  ## assess potential serotiny reg: add sexual maturity to the table and compare w/ age
  ## as long as one cohort is sexually mature, serotiny is activated
  serotinyOutputs <- doSerotiny(burnedPixelCohortData = burnedPixelCohortData,
                                species = sim$species, currentTime = time(sim),
                                treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                                sufficientLight = sim$sufficientLight,
                                speciesEcoregion = sim$speciesEcoregion,
                                calibrate = P(sim)$calibrate,
                                postFirePixelCohortData = postFirePixelCohortData,
                                postFireRegenSummary = sim$postFireRegenSummary)

  postFirePixelCohortData <- serotinyOutputs$postFirePixelCohortData
  serotinyPixel <- serotinyOutputs$serotinyPixel

  if (!is.null(serotinyOutputs$postFireRegenSummary)) {
    sim$postFireRegenSummary <- serotinyOutputs$postFireRegenSummary
  }

  rm(serotinyOutputs)

  ## DO RESPROUTING --------------------------
  ## assess resprouting reproduction:
  ## basically same thing as serotiny
  resproutingOutputs <- doResprouting(serotinyPixel = serotinyPixel,
                                      treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                                      burnedPixelCohortData = burnedPixelCohortData,
                                      postFirePixelCohortData = postFirePixelCohortData,
                                      currentTime = time(sim), species = sim$species,
                                      sufficientLight = sim$sufficientLight,
                                      calibrate = P(sim)$calibrate,
                                      postFireRegenSummary = sim$postFireRegenSummary)

  postFirePixelCohortData <- resproutingOutputs$postFirePixelCohortData
  sim$serotinyResproutSuccessPixels <- resproutingOutputs$serotinyResproutSuccessPixels

  if (!is.null(resproutingOutputs$postFireRegenSummary)) {
    sim$postFireRegenSummary <- resproutingOutputs$postFireRegenSummary
  }

  rm(resproutingOutputs)

  ## ADD NEW COHORTS -----------------------------
  ## add new cohorts to pixels where serotiny/regeneration were activated
  if (NROW(postFirePixelCohortData) > 0) {
    ## redo post-fire pixel groups by adding the maxPixelGroup to their ecoregioMap values
    if (!is.null(sim$serotinyResproutSuccessPixels)) {

      ## Add new cohorts to BOTH the sim$cohortData and sim$pixelGroupMap
      ## reclassify pixel groups as burnt (0L)
      if (verbose > 0) {
        message(blue("Post serotiny and resprouting"))
      }
      outs <- updateCohortData(newPixelCohortData = postFirePixelCohortData,
                               cohortData = sim$cohortData,
                               pixelGroupMap = sim$pixelGroupMap,
                               currentTime = round(time(sim)),
                               speciesEcoregion = sim$speciesEcoregion,
                               cohortDefinitionCols = P(sim)$cohortDefinitionCols,
                               treedFirePixelTableSinceLastDisp = treedFirePixelTableSinceLastDisp,
                               initialB = P(sim)$initialB,
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
  ## TODO: Ceres potential bug fix. Move this from beginning to here.
  sim$treedFirePixelTableSinceLastDisp[, pixelGroup := as.integer(as.vector(sim$pixelGroupMap[]))[pixelIndex]]
  # append previous year's
  treedFirePixelTableSinceLastDisp <- rbindlist(list(sim$treedFirePixelTableSinceLastDisp,
                                                     treedFirePixelTableSinceLastDisp))

  sim$treedFirePixelTableSinceLastDisp <- treedFirePixelTableSinceLastDisp
  return(invisible(sim))
}

## ---------------------------------------------------------------------------
## INPUT OBJECTS

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

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
    sufficientLight <- data.frame(mainInput, stringsAsFactors = FALSE)
    startRow <- which(sufficientLight$col1 == "SufficientLight")
    sufficientLight <- sufficientLight[(startRow + 1):(startRow + 5), 1:7]
    sufficientLight <- data.table(sufficientLight)
    sufficientLight <- sufficientLight[, lapply(.SD, function(x) as.numeric(x))]

    names(sufficientLight) <- c("speciesshadetolerance",
                                "X0", "X1", "X2", "X3", "X4", "X5")
    sim$sufficientLight <- data.frame(sufficientLight, stringsAsFactors = FALSE)
  }

  #  # input species ecoregion dynamics table
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    sim$speciesEcoregion <- prepInputsSpeciesEcoregion(url = extractURL("speciesEcoregion"),
                                                       dPath = dPath, cacheTags = cacheTags)
  }

  if (!suppliedElsewhere("treedFirePixelTableSinceLastDisp", sim)) {
    sim$treedFirePixelTableSinceLastDisp <- data.table(
      pixelIndex = integer(), pixelGroup = integer(), burnTime = numeric()
    )
  }

  return(invisible(sim))
}
