## -------------------------------------
## LandR FUNCTIONS
## -------------------------------------

assignLightProb <- function(sufficientLight, newCohortData) {
  newCohortData[ , lightProb := sufficientLight[cbind(shadetolerance, siteShade + 2)]]
}

## -------------------------------------

addNewCohorts <- function(newCohortData, cohortData, pixelGroupMap, time, speciesEcoregion) {
  # this function is for 1) adding new cohort data into cohortdata
  # 2) assign initial biomass and age for new cohort
  # 3) assign the new pixelgroup to the pixels that have new cohort
  # 4) update the pixelgroup map
  # newCohortData must have the original pixelgroup, regenerated species and pixelindex
  # it also would be better if it has the collums of cohortData plus pixelindex
  newCohortData$pixelGroup <- getValues(pixelGroupMap)[newCohortData$pixelIndex]
  set(newCohortData, , "temppixelGroup", as.integer(as.factor(newCohortData$pixelGroup)))
  set(newCohortData, , "speciesposition", 2^(newCohortData$speciesCode))
  # newCohortDataExtra is used to connect the original pixelGroup to the newPixelGroup
  newCohortDataExtra <- newCohortData[, .(community = sum(speciesposition),
                                          pixelGroup = mean(pixelGroup),
                                          temppixelGroup = mean(temppixelGroup)), by = pixelIndex]
  set(newCohortData, , c("temppixelGroup", "speciesposition"), NULL)
  set(newCohortDataExtra, , "community",
      as.integer(as.factor(newCohortDataExtra$community)))
  if (max(newCohortDataExtra$community) > max(newCohortDataExtra$temppixelGroup)) {
    set(newCohortDataExtra, ,  "community",
        newCohortDataExtra$community + max(newCohortDataExtra$community)*newCohortDataExtra$temppixelGroup)
  } else {
    set(newCohortDataExtra, , "community",
        newCohortDataExtra$temppixelGroup + max(newCohortDataExtra$temppixelGroup)*newCohortDataExtra$community)
  }
  maxPixelGroup <- max(max(cohortData$pixelGroup), maxValue(pixelGroupMap))
  set(newCohortDataExtra, ,  "newpixelGroup",
      as.integer(as.factor(newCohortDataExtra$community)) + maxPixelGroup)
  set(newCohortDataExtra, , c("community", "temppixelGroup"), NULL)
  setkey(newCohortData, pixelIndex)
  setkey(newCohortDataExtra, pixelIndex)
  newCohortData <- newCohortData[,pixelGroup:=NULL][newCohortDataExtra][,pixelIndex := NULL]
  newCohortData <- unique(newCohortData, by = c("newpixelGroup", "speciesCode"))
  sumTable <- cohortData[, .(pixelGroup,sumB)] %>%
    unique(, by = c("pixelGroup"))
  newCohortData <- dplyr::left_join(newCohortData, sumTable, by = "pixelGroup") %>% data.table
  newCohortData[is.na(sumB),sumB:=0]
  set(cohortData, ,"sumB", NULL)
  set(newCohortData, , "pixelGroup", newCohortData$newpixelGroup)
  set(newCohortData, , c("newpixelGroup"), NULL)
  specieseco_current <- speciesEcoregion[year <= time]
  specieseco_current <- setkey(specieseco_current[year == max(specieseco_current$year),
                                                  .(speciesCode, maxANPP,
                                                    maxB, ecoregionGroup)],
                               speciesCode, ecoregionGroup)
  specieseco_current[, maxB_eco:=max(maxB), by = ecoregionGroup]
  newCohortData <- setkey(newCohortData, speciesCode, ecoregionGroup)[specieseco_current, nomatch=0]
  set(newCohortData, , "age", 1)
  set(newCohortData, ,"B",
      as.integer(pmax(1, newCohortData$maxANPP*exp(-1.6*newCohortData$sumB/newCohortData$maxB_eco))))
  set(newCohortData, ,"B", as.integer(pmin(newCohortData$maxANPP, newCohortData$B)))
  
  newCohortData <- newCohortData[,.(pixelGroup, ecoregionGroup, speciesCode, age, B,
                                    mortality = 0, aNPPAct = 0)]
  newCohortDataExtra2 <- unique(newCohortDataExtra, by = c("pixelGroup", "newpixelGroup"))
  # newCohortDataExtra2 is further simplified form
  # identify which pixelGroups in cohortData have new regeneration
  existingData <- cohortData[pixelGroup %in% unique(newCohortDataExtra2$pixelGroup)]
  setkey(newCohortDataExtra2, pixelGroup)
  setkey(existingData, pixelGroup)
  existingData <- existingData[newCohortDataExtra2, allow.cartesian = TRUE]
  existingData <- existingData[!is.na(ecoregionGroup)]
  set(existingData, ,"pixelGroup", existingData$newpixelGroup)
  set(existingData, ,c("pixelIndex", "newpixelGroup"), NULL)
  existingData <- unique(existingData, by = c("pixelGroup", "speciesCode", "age"))
  rm(newCohortDataExtra2)
  cohortData <- setkey(rbindlist(list(cohortData, newCohortData, existingData)),
                       pixelGroup, speciesCode, age)
  pixelGroupMap[as.integer(newCohortDataExtra$pixelIndex)] <- newCohortDataExtra$newpixelGroup
  
  cohortData <- cohortData[pixelGroup %in% unique(getValues(pixelGroupMap)),]
  pixelGroupMap_new <- pixelGroupMap
  
  temppixelIndex11 <- which(!(getValues(pixelGroupMap) %in% c(0, -1)))
  pgmTemp <- getValues(pixelGroupMap)[temppixelIndex11]
  pixelGroupMap_new[temppixelIndex11] <- as.integer(as.factor(pgmTemp))
  pixelGroupConnection <- data.table(pixelGroup = pgmTemp,
                                     newPixelGroup = getValues(pixelGroupMap_new)[temppixelIndex11]) %>%
    unique(by = "pixelGroup")
  setkey(pixelGroupConnection, pixelGroup)
  setkey(cohortData, pixelGroup)
  cohortData <- cohortData[pixelGroupConnection, nomatch = 0]
  set(cohortData, , "pixelGroup", cohortData$newPixelGroup)
  set(cohortData, , "newPixelGroup", NULL)
  pixelGroupMap <- setValues(pixelGroupMap_new, as.integer(pixelGroupMap_new[]))
  return(list(cohortData = cohortData,pixelGroupMap = pixelGroupMap))
}