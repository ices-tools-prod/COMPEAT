# OSPAR Common Procedure Eutrophication Assessment Tool

OSPAR Common Procedure for the Identification of the Eutrophication Status of the OSPAR Maritime Area

- The Screening Procedure
- The Comprehensive Procedure
  
<https://www.ospar.org/work-areas/hasec/eutrophication/common-procedure>

Assessment units are defined by OSPAR contracting parties. An assessment unit can have multiple indicators. Each indicator can have different temporal (months) and spatial (depths) coverage and reference values within the different assessment units.

Indicator results can be calculated either from raw data reported to ICES or indicator results can be reported directly. 

### Shared background assessments

Selecting an assessment period loads cached results from its `data/<period>/output`
folder only when an `assessment.complete` marker and all required indicator,
assessment, station and shapefile outputs exist. Otherwise, the app runs the assessment in a `callr`
background process. Sessions requesting the same period share that process and
check for completion every five seconds. Jobs continue when a user disconnects;
results are loaded only after the process finishes successfully. Failures are
reported to waiting users, and selecting the period again retries the assessment.

Job tracking is shared within one Shiny R process, not across multiple workers or
app instances. Jobs do not survive an app process shutdown, but completed output
files remain available for subsequent sessions. The completion marker is
published by an atomic rename after the assessment script succeeds and its
required outputs are checked. It is removed before regeneration; failed or
interrupted runs cannot produce a reusable cache. Existing output folders without
a marker are recalculated once. This checks completion and file presence, not
file contents or changes to inputs.

Assessment selectors across tabs track the requested period. Selecting a period
clears previous results immediately. Maps and tables remain blank, station and
indicator selectors clear, and downloads are disabled until that assessment
completes successfully or its completed cache is loaded. A failed request leaves
results blank rather than restoring an older period. The app-wide status identifies
the requested and displayed period. Selecting the empty option clears both.

Assessment selectors are locked across all tabs during generation and while
results (including cached results) are loading. They unlock after the browser
reports that its reactive updates are idle. Late selection events are rejected
and the selector is restored to the active request. Generation failures unlock
the selectors so another period can be selected.

Run the shared-job regression tests from the repository root with
`Rscript tests/test-shared-assessments.R`. They use temporary fixture data rather
than running a full assessment.

### Assessment Units

### Indicators

- Dissolved Inorganic Nitrogen (DIN)
- Dissolved Inorganic Phosphorus (DIP)
- Chlorophyll a (CPHL)
- Oxygen Deficiency (DOXY)
- Total Nitrogen (TN)
- Total Phosphorus (TP)
- Secchi Depth (SECC)
- Dissolved Inorganic Nitrogen:Dissolved Inorganic Phosphorus (DIN:DIP)
- Total Nitrogen:Total Phosphorus (TN:TP)

### Abbreviations

- ES = Eutrophication Status
- SD = Standard Deviation
- N = Number of observations
- NM = Number of unique Months
- NMP = Number of potential Months
- SE = Standards Error
- CI = Confidense Interval
- ET = Eutrophication Target / Threshold
- ACDEV = Acceptable Deviation
- BEST = Reference Condition
- EQR = Ecological Quality Ratio
- EQR_HG = Ecological Quality Ratio High/Good boundary
- EQR_GM = Ecological Quality Ratio Good/Moderate boundary
- EQR_MP = Ecological Quality Ratio Moderate/Poor boundary
- EQR_PB = Ecological Quality Ratio Poor/Bad boundary
- EQRS = Ecological Quality Ratio Scaled
- GTC = General Temporal Confidence
- GTC_HM = General Temporal Confidence High/Moderate boundary
- GTC_ML = General Temporal Confidence Moderate/Low boundary
- STC = Specific Temporal Confidence
- STC_HM = Specific Temporal Confidence High/Moderate boundary
- STC_ML = Specific Temporal Confidence Moderate/Low boundary
- TC = Temporal Confidence
- GSC = General Spatial Confidence
- GSC_HM = General Spatial Confidence High/Moderate boundary
- GSC_ML = General Spatial Confidence Moderate/Low boundary
- SSC = Specific Spatial Confidence
- SSC_HM = Specific Spatial Confidence High/Moderate boundary
- SSC_ML = Specific Spatial Confidence Moderate/Low boundary
- SC = Spatial Confidence
- C = Confidence
- NPA = None Problem Area
- PA = Problem Area
- AC = Accuracy Confidence
- ACC = Accuracy Confidence Class
- IW = Indicator Weight
