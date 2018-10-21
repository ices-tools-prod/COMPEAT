# OSPAR Eutrophication Assessment Tool

Common Procedure for the Identification of the Eutrophication Status of the OSPAR Maritime Area
  a) The Screening Procedure
  b) The Comprehensive Procedure
  
https://www.ospar.org/work-areas/hasec/eutrophication/common-procedure

The assessment tool is based on assessment units which are defined by OSPAR contracting parties and each assessment unit can have multiple indicators. Each indicator can have different temporal (months) and spatial (depths) coverage and reference values within the different assessment units

Indicator results can be calculated either from raw data reported to ICES or indicator results can be reported directly. 

## Assessment Units
  * Germany
  * Sweden

## Nutrients
  * Parameters
    * Dissolved Inorganic Nitrogen - DIN [NO3-N + NO2-N + NH4-N]
    * Dissolved Inorganic Phosphorus - DIP [PO4_P]
  * Unit
    * umol/l
  * Aggregation of dissolved nutrients
    * Depth: <= 10 m
    * Period: Winter
      * Sweden (December - February)
    * Arithmetric mean of mean of mean by station, assessment unit and period per year - ThreeStageAnnualMean

## Chlorophyll a
  * Parameters
    * Chlorophyll a
  * Units
    * ug/l
  * Aggregation
    * Depth: <= 10 m
    * Period: Summer
      * Sweden (June - September)
    * Arithmetric mean of mean of mean by station, assessment unit and period per year - ThreeStageAnnualMean

## Dissolved Oxygen
  * Parameters
    * Dissolved Oxygen
  * Units
    * ml/l
  * Aggregation
    * Depth: <= 10 m above seafloor
    * Period: July - October
    * 1-, 5- and 10 percentile by station and cluster per year
    
- Salinity normalisation
