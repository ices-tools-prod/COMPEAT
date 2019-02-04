# OSPAR Common Procedure Eutrophication Assessment Tool

OSPAR Common Procedure for the Identification of the Eutrophication Status of the OSPAR Maritime Area

- The Screening Procedure
- The Comprehensive Procedure
  
<https://www.ospar.org/work-areas/hasec/eutrophication/common-procedure>

Assessment units are defined by OSPAR contracting parties. An assessment unit can have multiple indicators. Each indicator can have different temporal (months) and spatial (depths) coverage and reference values within the different assessment units

Indicator results can be calculated either from raw data reported to ICES or indicator results can be reported directly. 

### Assessment Units

- Sweden
- Germany
- Netherlands

### Indicators

- Nutrients
  - Parameters
    - Dissolved Inorganic Nitrogen - DIN [NO3-N + NO2-N + NH4-N]
    - Dissolved Inorganic Phosphorus - DIP [PO4-P]
  - Unit
    - umol/l
  - Aggregation
    - Depth: <= 10 m
    - Period: Winter
      - Sweden: December - February
      - Germany: **September - February**
      - Netherlands: November - February
    - Metric: Arithmetric mean by station, year, period per assessment unit
    - Normalisation: If the indicator concentration has a significant relation to salinity e.g. above the 95% confidence level (p<0.05) then the indicator is normalised to a given reference salinity e.g. ES_normalised = ES_observed + A * (S_reference - S_observed), where ES is the indicator concentration and S is the salinity and A is the regression slope (currently the reference salinity is set to the mean salinity for the assessment unit in question)

- Chlorophyll a
  - Parameters
    - Chlorophyll a
  - Units
    - ug/l
  - Aggregation
    - Depth: <= 10 m
    - Period: Growing
      - Sweden: June - August
      - Germany: **March - October**
      - Netherlands: March - September
    - Metric: Arithmetric mean by station, year, period per assessment unit

- Dissolved Oxygen
  - Parameters
    - Dissolved Oxygen
  - Units
    - mg/l
  - Aggregation
    - Depth:
      - Sweden: <= 10 % from the bottom
      - Germany: bottom
      - Netherlands: <= 3 m from the bottom
    - Period: Annual
      - Sweden: August - October
      - Germany: March - October
      - Netherlands: **January - December**
    - Metric: Arithmetric minimum by station, year, period per assessment unit
