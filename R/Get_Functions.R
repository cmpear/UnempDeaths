#' GetCountyDeaths
#' @description Retrieves and cleans CDC data on deaths per county from inst/extdata/Deaths_by_County folder
#' @return Returns cleaned dataset of deaths by county
#' @export
#' @example data <- UnempDeaths::GetCountyDeaths()
#' @importFrom tidyr %>%
#' @importFrom readr read_tsv
#' @importFrom dplyr mutate
#' @importFrom dplyr transmute
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr rename
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
GetCountyDeaths <- function( ){
  path <- system.file('extdata', 'Deaths_by_County', package = 'UnempDeaths')
  paths <- list.files(path = path)
  rValue <- NULL
  for (p in paths){
    rValue <- readr::read_tsv(paste0(path,'/',p), col_types = 'cccccccnnc' ) %>%
      bind_rows(rValue)
  }
  rValue %>%
    dplyr::transmute("ID" = .data$`County Code`,
                     "YEAR" = .data$Year,
                     "INTENT" = .data$`Injury Intent`,
                     "DEATHS" = .data$Deaths) %>%
    dplyr::mutate("LONG_ID" = paste(.data$ID, .data$YEAR) ) %>%
    dplyr::filter( .data$INTENT %in% c('Suicide','Homicide', 'Unintentional', 'Undetermined') ) %>%
    tidyr::pivot_wider( names_from = .data$INTENT, values_from = .data$DEATHS) %>%
    dplyr::rename("SUICIDE" = .data$Suicide,
                  "HOMICIDE" = .data$Homicide,
                  "UNINTENTIONAL" = .data$Unintentional,
                  "UNDETERMINED" = .data$Undetermined) %>%
    dplyr::group_by(.data$LONG_ID) %>%
    dplyr::summarize("SUICIDE" = sum(.data$SUICIDE),
                     "HOMICIDE" = sum(.data$HOMICIDE),
                     "UNINTENTIONAL" = sum(.data$UNINTENTIONAL),
                     "UNDETERMINED" = sum(.data$UNDETERMINED) ) %>%
    dplyr::ungroup() %>%
    return()
}

#' GetIncome
#' @description Retrives income data for 2016, census data
#' @return returns 2016 income data
#' @export
#' @example data <- UnempDeaths::GetIncome()
#' @importFrom tidyr %>%
#' @importFrom readr read_csv
#' @importFrom rlang .data
GetIncome <- function(){
# cbg_b19.csv, the census data, was 281.7 MB, so I simply saved the data.  Kept old code in comments for reference.
#  path <- system.file('extdata','cbg_b19.csv ', package = 'UnempDeaths')
  path <- system.file('extdata','CleanedIncome.csv ', package = 'UnempDeaths')
  readr::read_csv(file = path) %>%
    # dplyr::select("census_block_group", "B19313m1") %>%
    # dplyr::rename("ID" = .data$census_block_group) %>%
    # dplyr::mutate("ID" = substring(.data$ID, 1, 5) ) %>%
    # dplyr::mutate("B19313m1" = as.numeric(.data$B19313m1) ) %>%
    # dplyr::group_by(.data$ID) %>%
    # dplyr::summarize("AGG_INCOME" = sum(.data$B19313m1, na.rm = TRUE) ) %>%
    # dplyr::ungroup() %>%
    return()
}

#' GetCountyUnemployment
#' @description Retrives census figures for unemployment data /inst/extdata/Unemployment_tsv
#' @return a cleaned dataframe of unemployment data
#' @export
#' @example data <- UnempDeaths::GetCountyUnemployment()
#' @importFrom tidyr %>%
#' @importFrom readr read_tsv
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
GetCountyUnemployment <- function(){
  #  path = 'C:/Users/Christopher/Desktop/Data/Cold_Deaths_Project/Unemployment'
  path <- system.file('extdata','Unemployment_tsv', package = 'UnempDeaths' )
#  path = 'C:/Users/Christopher/Desktop/Data/Cold_Deaths_Project/Unemployment_tsv'
  paths <- list.files(path = path)
  rValue <- NULL
  for (p in paths){
    rValue <- readr::read_tsv(paste0(path,'/',p), col_types = 'ccccnnnnnn'  ) %>%
      dplyr::bind_rows(rValue)
  }
  return(rValue)
}

#' GetCountyGPS
#' @description returns a dataset of county-FIPS codes wtih GPS coordinates.
#' @return a dataset with GPS coordinates for county-FIPS
#' @export
#' @example data <- UnempDeaths::GetCountyGPS
#' @importFrom tidyr %>%
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
GetCountyGPS <- function(){
  path <- system.file('extdata','cbg_geographic_data.csv', package = 'UnempDeaths')
  read_csv(path, col_types = 'cnnnn') %>%
    dplyr::mutate("ID" = substring(.data$census_block_group, 1, 5) ) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::summarize("LAND"  = sum(.data$amount_land),
                     "WATER" = sum(.data$amount_water),
                     "LAT"   = mean(.data$latitude),
                     "LONG"  = mean(.data$longitude) ) %>%
    dplyr::mutate("LAND"  = .data$LAND / 1000000 / 1.6^2,
                  "WATER" = .data$WATER / 1000000 / 1.6^2) %>%  #from meters to km
    dplyr::ungroup() %>%
    return()
}

#' GetCountyPop
#' @description returns a cleaned dataset of populations by county from 2000 to 2018
#' @return a cleaned dataset of populations by county
#' @export
#' @example data <- UnempDeaths::GetCoutnyPop
#' @importFrom tidyr %>%
#' @importFrom stringr str_pad
#' @importFrom stringr str_replace
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr full_join
#' @importFrom rlang .data
GetCountyPop <- function(){
  path <- system.file('extdata','cPop10.csv', package = 'UnempDeaths')
  pop10 <- readr::read_csv(path,
                    col_type = 'dddcccciiiiiiiiiiii', skip = 1,
                    col_names = c('SUMLEV', 'REGION', 'DIVISION','STATE','COUNTY','STNAME','CITY','Y2010',
                                  'B2010','E2010','Y2011','Y2012','Y2013','Y2014','Y2015','Y2016','Y2017','Y2018',
                                  'Y2019' )) %>%
    dplyr::select( - "B2010", - "E2010" ) %>%
    dplyr::mutate("STATE" = stringr::str_pad(.data$STATE, 2, side = 'left', pad = '0'),
                  "COUNTY" = stringr::str_pad(.data$COUNTY, 3, side = 'left', pad = '0') ) %>%
    dplyr::mutate("ID" = paste0(.data$STATE, .data$COUNTY) )

  path <- system.file('extdata','cPop00.csv', package = 'UnempDeaths')
  readr::read_csv(path,
                    col_type = 'dddcccciiiiiiiiiiiii', skip = 1,
                    col_names = c('SUMLEV', 'REGION', 'DIVISION','STATE','COUNTY','STNAME','CITY','Y2000','E2000','Y2001',
                                  'Y2002','Y2003','Y2004','Y2005','Y2006','Y2007','Y2008','Y2009','Y2010','E2010' ) ) %>%
    dplyr::mutate("STATE" = stringr::str_pad(.data$STATE, 2, side = 'left', pad = '0'),
                  "COUNTY" = stringr::str_pad(.data$COUNTY, 3, side = 'left', pad = '0') ) %>%
    dplyr::mutate("ID" = paste0(.data$STATE, .data$COUNTY) ) %>%
    dplyr::select("ID", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005", "Y2006", "Y2007", "Y2008", "Y2009" ) %>%

    dplyr::full_join(pop10, by = 'ID') %>%
    dplyr::filter(.data$SUMLEV == 50)  %>%
    tidyr::pivot_longer( c(.data$Y2000, .data$Y2001, .data$Y2002, .data$Y2003, .data$Y2004,
                           .data$Y2005, .data$Y2006, .data$Y2007, .data$Y2008, .data$Y2009,
                           .data$Y2010, .data$Y2011, .data$Y2012, .data$Y2013, .data$Y2014,
                           .data$Y2015, .data$Y2016, .data$Y2017, .data$Y2018, .data$Y2019),
                         names_to = 'YEAR', values_to = 'POP' ) %>%
    dplyr::mutate( "YEAR" = as.integer( stringr::str_replace(.data$YEAR, 'Y', '') ) ) %>%
    dplyr::mutate( "LONG_ID" = paste(.data$ID, .data$YEAR) ) %>%
    dplyr::select( "LONG_ID", "ID", "YEAR", "POP") %>%
    return()
}

#' GetCleanedData
#' @description Returns a pre-cleaned dataset.  Same output as Build_Dataset
#' @return the cleaned dataset
#' @export
GetCleanedData <- function()
{
  path <- system.file('extdata','df_cleaned.tsv', package = 'UnempDeaths')
  return(read_tsv(path, col_types = 'cncnnnnnnnnnnnnnnnnnnn' ) )
}

# GetPopAge <- function(){
#   # see https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2018/cc-est2018-alldata.pdf
#   path <- 'C:/Users/Christopher/Documents/R/UnempDeaths/inst/extdata/cc-est2018-alldata.csv'
# #  path <- system.file('extdata','cc-est2018-alldata.csv', package = 'UnempDeaths')
#   read_csv(path, col_types = 'cccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn') %>%
#     select(STATE, COUNTY, YEAR, AGEGRP, TOT_POP ) %>%
#     filter(AGEGRP != 0 ) %>%
#     mutate("AGE" = if_else(AGEGRP != 18, paste('Age', (AGEGRP - 1) * 5, 'to', AGEGRP * 5 - 1, sep = '_'), 'Age_85_and_older' ) ) %>%
#     select( - AGEGRP) %>%
#     filter(YEAR > 2 ) %>%
#     mutate(YEAR = YEAR + 2007) %>%
#     rename("POP" = TOT_POP) %>%
#     return()
# }

#' GetWorkingAgePop
#' @description retrieves sum total of population by county and year aged 20 to 64
#' @returns dataframe of county population data by age from 2010 to 2018
#' @export
#' @importFrom tidyr %>%
#' @importFrom readr read_csv
#' @importFrom rlang .data
GetWorkingAgePop <- function() {
  # trying to lighten package, so replaced cc-est2018-alldata.csv (153.9 MB) and Y2K_Census_Headers.csv (17.6 MB) with a pre-cleaned file
  # Left old code for reference in comments
  path <- system.file('extdata', 'CleanedWorkingAgePop.csv', package = 'UnempDeaths')
  readr::read_csv(file = path) %>%
    return()
#   path <- system.file('extdata', 'cc-est2018-alldata.csv', package = 'UnempDeaths')
# # path <- 'C:/Users/Christopher/Documents/R/UnempDeaths/inst/extdata/cc-est2018-alldata.csv'
#   rvalue <- readr::read_csv(path, col_types = 'cccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn') %>%
#     dplyr::select("STATE", "COUNTY", "YEAR", "AGEGRP", "TOT_POP" ) %>%
#     dplyr::rename("POP" = .data$TOT_POP) %>%
#     dplyr::filter(.data$YEAR > 2 & .data$AGEGRP != 0) %>%
#     dplyr::mutate("YEAR" = .data$YEAR + 2007, 'ID' = paste0(.data$STATE, .data$COUNTY) ) %>%
#     dplyr::select( - 'STATE', - 'COUNTY' ) %>%
#     dplyr::mutate('AGEGRP' = if_else(.data$AGEGRP < 5,
#                                      'YOUTH',
#                                      if_else(.data$AGEGRP > 13,
#                                              'ELDERLY',
#                                              'WORKING_AGE') ) ) %>%
#     dplyr::group_by(.data$ID, .data$YEAR, .data$AGEGRP) %>%
#     dplyr::summarize('POP' = sum(.data$POP) ) %>%
#     dplyr::ungroup() %>%
#     tidyr::pivot_wider(names_from = .data$AGEGRP, values_from = .data$POP)
#   path <- system.file('extdata', 'Y2k_Census_Headers.csv', package = 'UnempDeaths')
# #  path <- 'C:/Users/Christopher/Documents/R/UnempDeaths/inst/extdata/Y2k_Census_Headers.csv'
#   rvalue <-
#     readr::read_csv(path,
#              col_types =
#                'cccccccccccccccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn' ) %>%
#     dplyr::filter(.data$LEVEL_2 == 50) %>%
#     dplyr::mutate('ID' = paste0(stringr::str_pad(.data$STATE, 2, side = 'left', pad = '0'),
#                          stringr::str_pad(.data$COUNTY, 3, side = 'left', pad = '0') ) ) %>%
#     dplyr::transmute(.data$ID,
#               'YEAR' = 2000,
#               'ELDERLY' = .data$AGE_65_74 + .data$AGE_75_84 + .data$AGE_85P,
#               'WORKING_AGE' = .data$AGE_20_24 + .data$AGE_25_34 + .data$AGE_35_44 + .data$AGE_45_54 + .data$AGE_55_59 + .data$AGE_60_64,
#               'YOUTH' =  .data$UNDER_5 + .data$UNDER_5 + .data$AGE_5_9 + .data$AGE_10_14 + .data$AGE_15_19) %>%
#     dplyr::bind_rows(rvalue)
#
#   rvalue <- rbind(rvalue, UnempDeaths:::GrowthEstimates(rvalue, cols = c('YOUTH', 'WORKING_AGE', 'ELDERLY'), start = 2000, end = 2010) ) %>%
#     dplyr::transmute('LONG_ID' = paste(.data$ID, .data$YEAR),
#                      'ELDERLY' = as.numeric(.data$ELDERLY),
#                      'WORKING_AGE' = as.numeric(.data$WORKING_AGE),
#                      'YOUTH' = as.numeric(.data$YOUTH) )
#  return(rvalue)
}

