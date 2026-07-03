Known issues: <https://github.com/PredictiveEcology/Biomass_regeneration/issues>

# Biomass_regeneration 1.0.1 (2026-06-02)

* aligned cohort definitions with Biomass_core and LandR: added `loadOrder = list(after = "Biomass_core")` to metadata, and switched `cohortDefinitionCols` to `LandR::cohortDefinitionCols()` (first matching Biomass_core's "complete" set `pixelGroup`/`speciesCode`/`age`/`ecoregionGroup`/`B`), then adopting the newer LandR convention that no longer uses `ecoregionGroup` or `B` to define cohorts; requires `PredictiveEcology/LandR@development (>= 1.1.5.9016)`.
* use `inputPath` instead of `dataPath` per latest module best practices.
* metadata and formatting cleanup, including `# nolint` annotations and typo fixes.
* documentation/CI maintenance: reformatted and rebuilt `Biomass_regeneration.Rmd` for use in the manual; updated the `render-module-rmd` GitHub Actions workflow and added a markdown badge.

# Biomass_regeneration 1.0.0 (2023-09-22)

* terra migration: removed `raster::getValues()` calls in favour of terra-compatible `as.vector(rstObj[])`.
* metadata cleanup (including tidied input descriptions from PR #18); added the Ecology Letters CSL file and rebuilt `Biomass_regeneration.Rmd`.
