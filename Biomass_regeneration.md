---
title: "Biomass_regeneration"
author: "Eliot McIntire, Alex M Chubaty, Ceres Barros"
date: "11 May 2021"
output:
  html_document:
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Biomass_regeneration is a SpaDES module that simulates post-disturbance regeneration mechanisms for Biomass_core.
As such, this module is mostly based on the post-disturbance regeneration mechanisms present in LANDIS-II Biomass Succession v3.2.1 extension (see [LANDIS-II Biomass Succession v3.2 User Guide](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v3.2%20User%20Guide.docx) and [Scheller and Mladenoff (2004)](https://pdfs.semanticscholar.org/4d38/d0be6b292eccd444af399775d37a757d1967.pdf).
At the moment, the Biomass_regeneration module only simulates post-fire disturbance effects on forest species, by simulating post-fire mortality and activating serotiny or resprouting mechanisms for each species, depending on their traits (i.e. ability to resprout and/or germinate from seeds, serotiny, following fire).
Post-fire mortality behaves in a stand-replacing fashion, i.e. should a pixel be within a fire perimeter (determined by a fire raster) all cohorts see their biomasses set to 0.

As for post-fire regeneration, the module first evaluates whether any species present prior to fire are serotinous.
If so, these species will germinate depending on light conditions and their shade tolerance, and depending on their (seed) establishment probability (i.e. germination success) in that pixel.
The module then evaluates if any species present before fire are capable of resprouting.
If so the model growth these species depending, again, on light conditions and their shade tolerance, and on their resprouting probability (i.e. resprouting success).
For any given species in any given pixel, only serotiny or resprouting can occur.
Hence, species that are capable of both will only resprout if serotiny was not activated.

In LANDIS-II, resprouting could never occur in a given pixel if serotiny was activated for one or more species.
According to the manual:

> If serotiny (only possible immediately following a fire) is triggered for one or more species, then neither resprouting nor seeding will occur.
> Serotiny is given precedence over resprouting as it typically has a higher threshold for success than resprouting.
> This slightly favors serotinous species when mixed with species able to resprout following a fire.

([LANDIS-II Biomass Succession v3.2 User Guide](https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v3.2%20User%20Guide.docx))

This is no longer the case in Biomass_regeneration, where both serotinity and resprouting can occur in the same pixel, although not for the same species.
We feel that this is more realistic ecologically, as resprouters will typically regenerate faster  after a fire, often shading serotinous species and creating interesting successional feedbacks (e.g. light-loving serotinous species having to "wait" for canopy gaps to germinate).

## General flow of Biomass_regeneration processes - fire disturbances only

1. Removal of biomass in disturbed, i.e. burnt, pixels
2. Activation of serotiny for serotinous species present before the fire
3. Activation of resprouting for resprouter species present before the fire and for which serotiny was not activated
4. Establishment/growth of species for which serotiny or resprouting were activated

# Usage


```r
library(SpaDES)

setPaths()
getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 10)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list("Biomass_regeneration")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
```

# Parameters

Provide a summary of user-visible parameters.


|paramName            |paramClass |default      |min |max |paramDesc                                                                                                                                                                                                   |
|:--------------------|:----------|:------------|:---|:---|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|calibrate            |logical    |FALSE        |NA  |NA  |Do calibration? Defaults to FALSE                                                                                                                                                                           |
|cohortDefinitionCols |character  |pixelGro.... |NA  |NA  |columns in cohortData that determine unique cohorts                                                                                                                                                         |
|fireInitialTime      |numeric    |1            |NA  |NA  |The event time that the first fire disturbance event occurs                                                                                                                                                 |
|fireTimestep         |numeric    |1            |NA  |NA  |The number of time units between successive fire events in a fire module                                                                                                                                    |
|initialB             |numeric    |10           |1   |NA  |initial biomass values of new age-1 cohorts. If `NA` or `NULL`, initial biomass will be calculated as in LANDIS-II Biomass Suc. Extension (see Scheller and Miranda, 2015 or `?LandR::.initiateNewCohorts`) |
|successionTimestep   |numeric    |10           |NA  |NA  |defines the simulation time step, default is 10 years                                                                                                                                                       |
|.plots               |character  |screen       |NA  |NA  |Used by Plots function, which can be optionally used here                                                                                                                                                   |
|.plotInitialTime     |numeric    |0            |NA  |NA  |This describes the simulation time at which the first plot event should occur                                                                                                                               |
|.plotInterval        |numeric    |NA           |NA  |NA  |This describes the simulation time interval between plot events                                                                                                                                             |
|.saveInitialTime     |numeric    |NA           |NA  |NA  |This describes the simulation time at which the first save event should occur                                                                                                                               |
|.saveInterval        |numeric    |NA           |NA  |NA  |This describes the simulation time interval between save events                                                                                                                                             |
|.useCache            |character  |.inputOb.... |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant                                                    |

# Events

Describe what happens for each event type.

# Data dependencies

## Input data


|objectName                       |objectClass |desc                                                                                                                                                                                                                    |sourceURL                                                                                                                                                                      |
|:--------------------------------|:-----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|cohortData                       |data.table  |age cohort-biomass table hooked to pixel group map by `pixelGroupIndex` at succession time step                                                                                                                         |NA                                                                                                                                                                             |
|inactivePixelIndex               |logical     |internal use. Keeps track of which pixels are inactive                                                                                                                                                                  |NA                                                                                                                                                                             |
|pixelGroupMap                    |RasterLayer |updated community map at each succession time step                                                                                                                                                                      |NA                                                                                                                                                                             |
|rasterToMatch                    |RasterLayer |a raster of the `studyArea`.                                                                                                                                                                                            |NA                                                                                                                                                                             |
|rstCurrentBurn                   |RasterLayer |Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned                                                                                                                                                       |NA                                                                                                                                                                             |
|species                          |data.table  |a table that has species traits such as longevity...                                                                                                                                                                    |https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt                                |
|speciesEcoregion                 |data.table  |table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time                                                                                                                      |https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt |
|sufficientLight                  |data.frame  |table defining how the species with different shade tolerance respond to stand shadiness                                                                                                                                |https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt                |
|treedFirePixelTableSinceLastDisp |data.table  |3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred |NA                                                                                                                                                                             |

## Output data

Description of the module outputs.


|objectName                       |objectClass |desc                                                                                                                                                                                                                    |
|:--------------------------------|:-----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|cohortData                       |data.table  |age cohort-biomass table hooked to pixel group map by pixelGroupIndex at succession time step                                                                                                                           |
|lastFireYear                     |numeric     |Year of the most recent fire year                                                                                                                                                                                       |
|pixelGroupMap                    |RasterLayer |updated community map at each succession time step                                                                                                                                                                      |
|serotinyResproutSuccessPixels    |numeric     |Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of treedBurnLoci                                                                                                                |
|postFireRegenSummary             |data.table  |summary table of species post-fire regeneration                                                                                                                                                                         |
|severityBMap                     |RasterLayer |A map of fire severity, as in the amount of post-fire mortality (biomass loss)                                                                                                                                          |
|severityData                     |data.table  |A data.table of pixel fire severity, as in the amount of post-fire mortality (biomass loss). May also have severity class used to calculate mortality.                                                                  |
|treedFirePixelTableSinceLastDisp |data.table  |3 columns: pixelIndex, pixelGroup, and burnTime. Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding pixelGroup and time it occurred |

# Links to other modules

Primarily used with the LandR Biomass suite of modules, namely [Biomass_core](https://github.com/PredictiveEcology/Biomass_core).

## Getting help

- <https://gitter.im/PredictiveEcology/LandR_Biomass>

