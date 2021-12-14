#Aggregating grid scale oxygen results to assessment areas.
#Start from wk3 (annual indicator results), run COMPEAT.R first  

####Method 1. For each year, averaging eutrophication state from grid cells across the whole assessment area, weighting each grid cell by its area. (Update TGCOMP8 - nobody was in favour of this method so I have commented it out)
# str(wk3) # year is called Period
# #subset to just oxygen
 indicatorID = 4
 oxy <- wk3[IndicatorID == indicatorID,]
# oxy_avg <- oxy[, .(N_OBS = sum(N), N_GRIDS = .N, ES = weighted.mean(ES, GridArea), SD = weighted.mean(SD, GridArea)), .(IndicatorID, UnitID, Period, Name, Code, Parameters, Units, DepthMin, DepthMax, Metric, Response, ACDEV, ET, BEST, EQR_HG, EQR_GM, EQR_MP, EQR_PB, UnitArea)] #include confidences?
# 
# 
# # Calculate Ecological Quality Ratio (ERQ)
# oxy_avg[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]
# 
# # Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
# oxy_avg[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
# oxy_avg[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
# oxy_avg[, EQR_PB := 2 * EQR_GM - 0.95]
# oxy_avg[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]
# 
# # Calculate Ecological Quality Ratio Scaled (EQRS)
# oxy_avg[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
#                      ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
#                             ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
#                                    ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
#                                           (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]
# 
# oxy_avg[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
#                            ifelse(EQRS >= 0.6, "Good",
#                                   ifelse(EQRS >= 0.4, "Moderate",
#                                          ifelse(EQRS >= 0.2, "Poor","Bad"))))]
# 
# fwrite(oxy_avg, file = file.path(outputPath, "Annual_Oxy_area_by_grid.csv"))
# 
# #Average results to assessment period
# 
# oxy_avg_period <- oxy_avg[, .(Period = min(Period) * 10000 + max(Period), ES = mean(ES), SD = sd(ES), NYears = .N, N_OBS = sum(N_OBS)), .(IndicatorID, UnitID)]
# 
# # Add Year Count where STC = 100 --> NSTC100
# #oxy_avg_period <- oxy_avg[STC == 100, .(NSTC100 = .N), .(IndicatorID, UnitID)][oxy_avg_period, on = .(IndicatorID, UnitID)]
# 
# # Adjust Specific Spatial Confidence if number of years where STC = 100 is at least half of the number of years with measurements
# #oxy_avg_period[, STC := ifelse(!is.na(NSTC100) & NSTC100 >= N/2, 100, STC)]
# 
# # Combine with indicator and indicator unit configuration tables
# oxy_avg_period2 <- indicators[indicatorUnits[oxy_avg_period]]
# 
# #-------------------------------------------------------------------------------
# # Confidence Assessment
# # ------------------------------------------------------------------------------
# 
# # Calculate Temporal Confidence averaging General and Specific Temporal Confidence 
# #oxy_avg_period2 <- oxy_avg_period2[, TC := (GTC + STC) / 2]
# 
# #oxy_avg_period2[, TC_Class := ifelse(TC >= 75, "High", ifelse(TC >= 50, "Moderate", "Low"))]
# 
# # Calculate Spatial Confidence averaging General and Specific Spatial Confidence 
# #oxy_avg_period2 <- oxy_avg_period2[, SC := (GSC + SSC) / 2]
# 
# #oxy_avg_period2[, SC_Class := ifelse(SC >= 75, "High", ifelse(SC >= 50, "Moderate", "Low"))]
# 
# # Standard Error - using number of years in the assessment period and the associated standard deviation
# #oxy_avg_period2[, SE := SD / sqrt(N)]
# 
# # Accuracy Confidence for Non-Problem Area
# #oxy_avg_period2[, AC_NPA := ifelse(Response == 1, pnorm(ET, ES, SD), pnorm(ES, ET, SD))]
# 
# # Standard Error - using number of observations behind the annual mean - to be used in Accuracy Confidence Calculation!!!
# #oxy_avg_period2[, AC_SE := SD / sqrt(N_OBS)]
# 
# # Accuracy Confidence for Non-Problem Area
# #oxy_avg_period2[, AC_NPA := ifelse(Response == 1, pnorm(ET, ES, AC_SE), pnorm(ES, ET, AC_SE))]
# 
# # Accuracy Confidence for Problem Area
# #oxy_avg_period2[, AC_PA := 1 - AC_NPA]
# 
# # Accuracy Confidence Area Class - Not sure what this should be used for?
# #oxy_avg_period2[, ACAC := ifelse(AC_NPA > 0.5, "NPA", ifelse(AC_NPA < 0.5, "PA", "PPA"))]
# 
# # Accuracy Confidence
# #oxy_avg_period2[, AC := ifelse(AC_NPA > AC_PA, AC_NPA, AC_PA)]
# 
# # Accuracy Confidence Class
# #oxy_avg_period2[, ACC := ifelse(AC > 0.9, 100, ifelse(AC < 0.7, 0, 50))]
# 
# #oxy_avg_period2[, ACC_Class := ifelse(ACC >= 75, "High", ifelse(ACC >= 50, "Moderate", "Low"))]
# 
# # Calculate Overall Confidence
# #oxy_avg_period2 <- oxy_avg_period2[, C := (TC + SC + ACC) / 3]
# 
# #oxy_avg_period2[, C_Class := ifelse(C >= 75, "High", ifelse(C >= 50, "Moderate", "Low"))]
# 
# # ------------------------------------------------------------------------------
# 
# # Calculate (BEST)
# oxy_avg_period2[, BEST := ifelse(Response == 1, ET / (1 + ACDEV / 100), ET / (1 - ACDEV / 100))]
# 
# # Calculate Ecological Quality Ratio (ERQ)
# oxy_avg_period2[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]
# 
# # Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
# oxy_avg_period2[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
# oxy_avg_period2[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
# oxy_avg_period2[, EQR_PB := 2 * EQR_GM - 0.95]
# oxy_avg_period2[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]
# 
# # Calculate Ecological Quality Ratio Scaled (EQRS)
# oxy_avg_period2[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
#                      ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
#                             ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
#                                    ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
#                                           (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]
# 
# oxy_avg_period2[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
#                            ifelse(EQRS >= 0.6, "Good",
#                                   ifelse(EQRS >= 0.4, "Moderate",
#                                          ifelse(EQRS >= 0.2, "Poor","Bad"))))]
# 
# fwrite(oxy_avg_period2, file = file.path(outputPath, "Period_Oxy_area_by_grid.csv"))
# 
# 
#plots
i=4
indicatorID <- indicators[i, IndicatorID]
indicatorCode <- indicators[i, Code]
indicatorName <- indicators[i, Name]
indicatorYearMin <- indicators[i, YearMin]
indicatorYearMax <- indicators[i, YearMax]
indicatorMonthMin <- indicators[i, MonthMin]
indicatorMonthMax <- indicators[i, MonthMax]
indicatorDepthMin <- indicators[i, DepthMin]
indicatorDepthMax <- indicators[i, DepthMax]
indicatorYearMin <- indicators[i, YearMin]
indicatorMetric <- indicators[i, Metric]
EQRS_Class_colors <- c("#3BB300", "#99FF66", "#FFCABF", "#FF8066", "#FF0000")
EQRS_Class_limits <- c("High", "Good", "Moderate", "Poor", "Bad")
EQRS_Class_labels <- c(">= 0.8 - 1.0 (High)", ">= 0.6 - 0.8 (Good)", ">= 0.4 - 0.6 (Moderate)", ">= 0.2 - 0.4 (Poor)", ">= 0.0 - 0.2 (Bad)")
# 
# wk <- oxy_avg_period2[IndicatorID == indicatorID] %>% setkey(UnitID)
# 
# wk <- merge(units, wk, all.x = TRUE)  
# 
# # Status map (EQRS)
# title <- paste0("Eutrophication Status (average of grid cell results) ", indicatorYearMin, "-", indicatorYearMax)
# subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
# subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
# subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
# subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
# fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_EQRS_gridagg", ".png"))
# 
# ggplot(wk) +
#   labs(title = title , subtitle = subtitle) +
#   geom_sf(aes(fill = EQRS_Class)) +
#   scale_fill_manual(name = "EQRS", values = EQRS_Class_colors, limits = EQRS_Class_limits, labels = EQRS_Class_labels)
# ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)

####Method 2. Calculate percentage of the assessment area that fails, based only on the grid cells with data.

str(oxy)
#categorise grid cells into pass or fail
oxy[, EQRS_pass_fail := ifelse(EQRS >= 0.6, "Pass", "Fail")]
#sum grid area by pass/fail ###GRID AREA IS NOT CORRECT< WHERE DOES IT COME FROM??
oxy_area_pass <- oxy[, .(GridArea = sum(GridArea), N_GRIDS = .N),.(IndicatorID, UnitID, Period, Name, Code, Parameters, Units, DepthMin, DepthMax, Metric, UnitArea, EQRS_pass_fail)]
#cast gridarea on pass/fail
oxy_percentage <- dcast(oxy_area_pass, IndicatorID + UnitID + Period + Name + Code + Parameters + Units + DepthMin + DepthMax + Metric + UnitArea ~ EQRS_pass_fail, value.var = c("GridArea","N_GRIDS"))
#replace NAs with 0s
oxy_percentage[is.na(GridArea_Fail), GridArea_Fail:= 0]
oxy_percentage[is.na(GridArea_Pass), GridArea_Pass:= 0]
oxy_percentage[is.na(N_GRIDS_Fail), N_GRIDS_Fail:= 0]
oxy_percentage[is.na(N_GRIDS_Pass), N_GRIDS_Pass:= 0]
oxy_percentage[, PercentFail:= GridArea_Fail/(GridArea_Fail+GridArea_Pass)*100]
oxy_percentage[, PercentFailWholeArea:= GridArea_Fail/(UnitArea)*100]

oxy_percentage[, TenPC:= ifelse(PercentFail >= 10, "Fail", "Pass")]
oxy_percentage[, TenPCWholeArea:= ifelse(PercentFailWholeArea >= 10, "Fail", "Pass")]
oxy_percentage[, N_GRIDS:= N_GRIDS_Fail+N_GRIDS_Pass]


fwrite(oxy_percentage, file = file.path(outputPath, "Annual_percent_grid.csv"))


oxy_percentage_period <- oxy_percentage[, .(Period = min(Period) * 10000 + max(Period), PercentFail = mean(PercentFail), PercentFailWholeArea = mean(PercentFailWholeArea), NYears = .N, AVG_N_GRIDS = mean(N_GRIDS)), .(IndicatorID, UnitID)]
oxy_percentage_period[, TenPC:= ifelse(PercentFail >= 10, "Fail", "Pass")]
oxy_percentage_period[, TenPCWholeArea:= ifelse(PercentFailWholeArea >= 10, "Fail", "Pass")]


fwrite(oxy_percentage_period, file = file.path(outputPath, "Period_percent_grid.csv"))


wk <- oxy_percentage_period[IndicatorID == indicatorID] %>% setkey(UnitID)

wk <- merge(units, wk, all.x = TRUE)  

# Map percentage of area with data
title <- paste0("Percentage of grid cells with data falling below good status ", indicatorYearMin, "-", indicatorYearMax)
subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_percent_fail", ".png"))

ggplot(wk) +
  labs(title = title , subtitle = subtitle) +
  geom_sf(aes(fill = PercentFail))
ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)

title <- paste0(">= 10 % of grid cells with data falling below good status ", indicatorYearMin, "-", indicatorYearMax)
subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_percent_fail_classified", ".png"))

ggplot(wk) +
  labs(title = title , subtitle = subtitle) +
  geom_sf(aes(fill = TenPC))
ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)

#Map percentage of whole area
title <- paste0("Percentage of grid cells falling below good status ", indicatorYearMin, "-", indicatorYearMax)
subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_percent_fail_wholearea", ".png"))

ggplot(wk) +
  labs(title = title , subtitle = subtitle) +
  geom_sf(aes(fill = PercentFailWholeArea))
ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)

title <- paste0(">= 10 % of grid cells falling below good status ", indicatorYearMin, "-", indicatorYearMax)
subtitle <- paste0(indicatorName, " (", indicatorCode, ")", "\n")
subtitle <- paste0(subtitle, "Months: ", indicatorMonthMin, "-", indicatorMonthMax, ", ")
subtitle <- paste0(subtitle, "Depths: ", indicatorDepthMin, "-", indicatorDepthMax, ", ")
subtitle <- paste0(subtitle, "Metric: ", indicatorMetric)
fileName <- gsub(":", "", paste0("Assessment_Indicator_Map_", indicatorCode, "_percent_fail_classified_wholearea", ".png"))

ggplot(wk) +
  labs(title = title , subtitle = subtitle) +
  geom_sf(aes(fill = TenPCWholeArea))
ggsave(file.path(outputPath, fileName), width = 12, height = 9, dpi = 300)

View(st_drop_geometry(units))


