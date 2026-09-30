---
title: "LandR _Biomass_regeneration_ Manual"
subtitle: "v.1.0.1.9000"
date: "Last updated: 2026-09-30"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  bibliography: citations/references_Biomass_regeneration.bib
link-citations: true
always_allow_html: true
---

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:Biomass-regeneration) *Biomass_regeneration*



#### Authors:

Eliot J B McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut, cre], Yong Luo <yluo1@lakeheadu.ca> [aut], Ceres Barros <cbarros@mail.ubc.ca> [aut], Alex M. Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

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

### General flow of Biomass_regeneration processes - fire disturbances only

1. Removal of biomass in disturbed, i.e. burnt, pixels
2. Activation of serotiny for serotinous species present before the fire
3. Activation of resprouting for resprouter species present before the fire and for which serotiny was not activated
4. Establishment/growth of species for which serotiny or resprouting were activated

### Module inputs and parameters

Table \@ref(tab:moduleInputs-Biomass-regeneration) shows the full list of module inputs.

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-Biomass-regeneration)(\#tab:moduleInputs-Biomass-regeneration)List of (ref:Biomass-regeneration) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> age cohort-biomass table hooked to pixel group map by `pixelGroupIndex` at succession time step </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> inactivePixelIndex </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> internal use. Keeps track of which pixels are inactive </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> updated community map at each succession time step </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> a raster of the `studyArea`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rstCurrentBurn </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> Binary raster of fires, 1 meaning 'burned', 0 or NA is non-burned </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> species </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A table of invariant species traits with the following trait colums: 'Name', 'Longevity', 'Sexual Maturity', 'Shade Tol.', 'Fire Tol.' 'Seed Dispersal Dist Effective', 'Seed Dispersal Dist Maximum' 'Vegetative Reprod Prob', 'Sprout Age Min', 'Sprout Age Max' 'Post-Fire Regen' </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> speciesEcoregion </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sufficientLight </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> table defining how the species with different shade tolerance respond to stand shadiness </td>
   <td style="text-align:left;"> https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession_test.txt </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treedFirePixelTableSinceLastDisp </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding `pixelGroup` and time it occurred. With columns: `pixelIndex`, `pixelGroup`, and `burnTime`. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-Biomass-regeneration)):

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-Biomass-regeneration)(\#tab:moduleParams-Biomass-regeneration)List of (ref:Biomass-regeneration) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> calibrate </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Do calibration? Defaults to FALSE </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortDefinitionCols </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> pixelGro.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> columns in cohortData that determine unique cohorts </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The event time that the first fire disturbance event occurs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireTimestep </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> The number of time units between successive fire events in a fire module </td>
  </tr>
  <tr>
   <td style="text-align:left;"> initialB </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> initial biomass values of new age-1 cohorts. If `NA` or `NULL`, initial biomass will be calculated as in LANDIS-II Biomass Suc. Extension (see Scheller and Miranda, 2015 or `?LandR::.initiateNewCohorts`) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> successionTimestep </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> defines the simulation time step, default is 10 years </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> screen </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used by Plots function, which can be optionally used here </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time at which the first plot event should occur </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between plot events </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time at which the first save event should occur </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> .inputOb.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant </td>
  </tr>
</tbody>
</table>

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-Biomass-regeneration)).

<table class="table" style="margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-Biomass-regeneration)(\#tab:moduleOutputs-Biomass-regeneration)List of (ref:Biomass-regeneration) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> age cohort-biomass table hooked to pixel group map by `pixelGroupIndex` at succession time step </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lastFireYear </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Year of the most recent fire year </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> updated community map at each succession time step </td>
  </tr>
  <tr>
   <td style="text-align:left;"> serotinyResproutSuccessPixels </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Pixels that were successfully regenerated via serotiny or resprouting. This is a subset of `treedBurnLoci`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> postFireRegenSummary </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> summary table of species post-fire regeneration </td>
  </tr>
  <tr>
   <td style="text-align:left;"> severityBMap </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> A map of fire severity, as in the amount of post-fire mortality (biomass loss) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> severityData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> A data.table of pixel fire severity, as in the amount of post-fire mortality (biomass loss). May also have severity class used to calculate mortality. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> treedFirePixelTableSinceLastDisp </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Each row represents a forested pixel that was burned up to and including this year, since last dispersal event, with its corresponding `pixelGroup` and time it occurred. with columns: `pixelIndex`, `pixelGroup`, and `burnTime`. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Primarily used with the LandR Biomass suite of modules, namely [Biomass_core](https://github.com/PredictiveEcology/Biomass_core).

## Getting help

- <https://gitter.im/PredictiveEcology/LandR_Biomass>
