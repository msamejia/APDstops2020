#Census Data for "Policing and Race in Austin"

library(tidyverse)
library(tidycensus)
library(dplyr)


#2020 ACS 5-year Census Tract: Hispanic by Race (2020_ACS5_Hisp_by_Race)

ACS5_2020_race_medinc <- get_acs(
  variables = c(White = "B03002_003",
                BlackAfAm = "B03002_004",
                AIAN = "B03002_005",
                Asian = "B03002_006",
                NHPI = "B03002_007",
                Other = "B03002_008",
                Twoplus = "B03002_009",
                HispLat = "B03002_012",
                medinc = "B19013_001"),
  geography = "tract",
  state = 48,
  county = c("Travis", "Hays", "Williamson"),
  year = 2020,
  survey = "acs5",
  output = "wide"
)

car_commuters <- get_acs(
  variables = c(car_commuters = "B08015_001"),
  geography = "tract",
  state = 48,
  county = c("Travis", "Hays", "Williamson"),
  year = 2020,
  survey = "acs5",
  output = "wide"
)

#APD Racial Profiling Data

#Note: Headers were edited in Excel to shorten and remove spaces; the 
#Yes/No "search" column was also updated to just be 1=Y, 0=N.

#Open CSV: APD_Racial_Profiling_2020 <- read.csv("C:/[*PATH*]/2020_Racial_
#                                                Profiling_RP_dataset.csv")


APD_Racial_Profiling_2020 <- mutate(APD_Racial_Profiling_2020,
                                    X_coord = X / 100000,
                                    Y_coord = (Y / 1000000) * -1
)

APD_Racial_Profiling_2020 <- select(APD_Racial_Profiling_2020,
                                    -c(X, Y)
)


#.csv exports

#write.csv(ACS5_2020_race_medinc, "C:/[*PATH*]/ACS5_2020_race_medinc.csv",
#          row.names = FALSE)

#write.csv(APD_Racial_Profiling_2020, "C:/[*PATH*]/APD_Racial_Profiling_2020
#          .csv", row.names = FALSE)

#write.csv(car_commuters, "C:/Users/msam9/OneDrive - The University of Texas at 
Austin/Documents/_Summer 2022/Independent Study with Karner/Policing 
and Race in Austin/Data/vehicles_commuting.csv")



#APD stops analysis with census tract GEOID attached

#Open CSV: APD_Stops_GEOID <- read.csv("C:/[*PATH*]/APDMotorvehicleStops_GEOID.csv")

APD_Stops_GEOID_sml <- select(APD_Stops_GEOID, OID_, Match_addr, USER_stop_key, 
                              USER_type, USER_TCOLE_sex, 
                              USER_TCOLE_race_ethnicity, USER_stop_reason, 
                              USER_search, USER_TCOLE_search_reason, 
                              USER_TCOLE_search_found, USER_TCOLE_stop_result, 
                              USER_TCOLE_arrest_reason, USER_council_district, 
                              USER_county, USER_custody, USER_sector, 
                              USER_standardized_race, USER_stop_date, 
                              USER_stop_time, totalpop, GEOID)

race_stops_GEOID <- APD_Stops_GEOID_sml %>%
  group_by(GEOID, USER_standardized_race) %>%
  summarise(
    race_sum = sum(USER_standardized_race)
  )

library(pivottabler)
pt <- PivotTable$new()
pt$addData(APD_Stops_GEOID_sml)
pt$addColumnDataGroups("USER_standardized_race")
pt$addRowDataGroups("GEOID")
pt$defineCalculation(calculationName="RacebyGEOID", summariseExpression="n()")
pt$evaluatePivot()
pt$renderPivot()
pt

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data", 
                         topRowNumber=1, leftMostColumnNumber=1, applyStyles=FALSE)
saveWorkbook(wb, file="C:/Users/msam9/OneDrive - The University of Texas at Austin/Documents/_Summer 2022/Independent Study with Karner/Policing and Race in Austin/Data/Stops_Race_GEOID.xlsx", overwrite = TRUE)

#Stop to Station Distance Analysis

Stop2station <- read.csv("C:/Users/msam9/OneDrive - The University of Texas at Austin/Documents/_Summer 2022/Independent Study with Karner/Policing and Race in Austin/Data/Stop to Station Near.csv", header = TRUE)

Stop2station_summary <- Stop2station %>%
  group_by(NEAR_FID) %>%
  dplyr::summarise(
    count = n(),
    avg_dist = mean(NEAR_DIST, na.rm = TRUE)
  )

#write.csv(Stop2station_summary, "C:/Users/msam9/OneDrive - The University of Texas at Austin/Documents/_Summer 2022/Independent Study with Karner/Policing and Race in Austin/Data/Stops2Stations_summary.csv")

#Stop Results by Race by Tract

by_stop_result <- group_by(Stop2station, GEOID, USER_TCOLE_race_ethnicity, 
                           USER_TCOLE_stop_result)



