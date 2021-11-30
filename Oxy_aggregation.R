#Aggregating grid scale oxygen results to assessment areas.
#Start from wk3 (annual indicator results)

#Method 1. For each year, averaging eutrophication state from grid cells accross the whole assessment area, weighting each grid cell by its area.
str(wk3) # year is called Period
#subset to just oxygen
oxy <- wk3[IndicatorID==4,]
oxy_avg <- oxy[, .(N_OBS = sum(N), N_GRIDS = .N, ES = weighted.mean(ES, GridArea), SD = weighted.mean(SD, GridArea)), .(IndicatorID, UnitID, Period, Name, Code, Parameters, Units, DepthMin, DepthMax, Metric, Response, ACDEV, ET, BEST, EQR_HG, EQR_GM, EQR_MP, EQR_PB, UnitArea)]


# Calculate Ecological Quality Ratio (ERQ)
oxy_avg[, EQR := ifelse(Response == 1, ifelse(BEST > ES, 1, BEST / ES), ifelse(ES > BEST, 1, ES / BEST))]

# Calculate Ecological Quality Ratio Boundaries (ERQ_HG/GM/MP/PB)
oxy_avg[, EQR_GM := ifelse(Response == 1, 1 / (1 + ACDEV / 100), 1 - ACDEV / 100)]
oxy_avg[, EQR_HG := 0.5 * 0.95 + 0.5 * EQR_GM]
oxy_avg[, EQR_PB := 2 * EQR_GM - 0.95]
oxy_avg[, EQR_MP := 0.5 * EQR_GM + 0.5 * EQR_PB]

# Calculate Ecological Quality Ratio Scaled (EQRS)
oxy_avg[, EQRS := ifelse(EQR <= EQR_PB, (EQR - 0) * (0.2 - 0) / (EQR_PB - 0) + 0,
                     ifelse(EQR <= EQR_MP, (EQR - EQR_PB) * (0.4 - 0.2) / (EQR_MP - EQR_PB) + 0.2,
                            ifelse(EQR <= EQR_GM, (EQR - EQR_MP) * (0.6 - 0.4) / (EQR_GM - EQR_MP) + 0.4,
                                   ifelse(EQR <= EQR_HG, (EQR - EQR_GM) * (0.8 - 0.6) / (EQR_HG - EQR_GM) + 0.6,
                                          (EQR - EQR_HG) * (1 - 0.8) / (1 - EQR_HG) + 0.8))))]

oxy_avg[, EQRS_Class := ifelse(EQRS >= 0.8, "High",
                           ifelse(EQRS >= 0.6, "Good",
                                  ifelse(EQRS >= 0.4, "Moderate",
                                         ifelse(EQRS >= 0.2, "Poor","Bad"))))]

wk4 <- wk3[, .(Period = min(Period) * 10000 + max(Period), ES = mean(ES), SD = sd(ES), N = .N, N_OBS = sum(N), GTC = mean(GTC), STC = mean(STC)), .(IndicatorID, UnitID, GridID)]

#Method 2. Calculate percentage of the assessment area that fails, based only on the grid cells with data.

str(oxy)
#categorise grid cells into pass or fail
oxy[, EQRS_pass_fail := ifelse(EQRS >= 0.6, "Pass", "Fail")]
#sum grid area by pass/fail
oxy_area_pass <- oxy[, .(GridArea = sum(GridArea), N_GRIDS = .N),.(IndicatorID, UnitID, Period, Name, Code, Parameters, Units, DepthMin, DepthMax, Metric, UnitArea, EQRS_pass_fail)]
#cast gridarea on pass/fail
oxy_percentage <- dcast(oxy_area_pass, IndicatorID + UnitID + Period + Name + Code + Parameters + Units + DepthMin + DepthMax + Metric + UnitArea ~ EQRS_pass_fail, value.var = "GridArea")
#replace NAs with 0s
oxy_percentage[is.na(Fail), Fail:= 0]
oxy_percentage[is.na(Pass), Pass:= 0]

oxy_percentage[, PercentFail:= Fail/(Fail+Pass)*100]
