#' Build_Dataset
#' @description builds dataset containing data on counties, GPS, deaths, 2016 income, and population
#' @return returns dataset containing data on counties, GPS, deaths, 2016 income, and population
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom stats sd
#' @importFrom rlang .data
#' @export
Build_Dataset<- function()
{
  county_unemp <- UnempDeaths::GetCountyUnemployment() %>%
    dplyr::filter(!is.na(.data$COUNTY) & .data$STATE_FIPS != 72) %>%  # Puerto Rico is 72, not in all datasets
    dplyr::mutate("ESTIMATED" = dplyr::if_else(is.na(.data$ESTIMATED),0,1) ) %>%
    dplyr::mutate("COUNTY" = dplyr::if_else(.data$COUNTY == 'District of Columbia', 'District of Columbia, DC', .data$COUNTY) ) %>%
    dplyr::mutate("ID" = paste0(.data$STATE_FIPS, .data$COUNTY_FIPS) ) %>%
    dplyr::mutate("LONG_ID" = paste0(.data$STATE_FIPS, .data$COUNTY_FIPS, ' ', .data$YEAR) ) %>%
    dplyr::select("ID", "LONG_ID", "YEAR", "COUNTY", "ESTIMATED", "LABOR_FORCE", "EMPLOYED")

  county_pop <- UnempDeaths::GetCountyPop() %>%
    UnempDeaths::AddEstimates() %>%
    dplyr::select("LONG_ID", "POP")

  df <- county_unemp %>%
    dplyr::inner_join(UnempDeaths::GetCountyGPS(), by = "ID" ) %>% # tired of Alaskan counties that don't fit well in census
    dplyr::full_join(county_pop, by = 'LONG_ID') %>%
    dplyr::full_join(UnempDeaths::GetCountyDeaths() ) %>%
    dplyr::filter(!is.na(.data$EMPLOYED) ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(UnempDeaths::GetIncome() ) %>%
    dplyr::mutate("PCI_2016" = .data$AGG_INCOME / .data$POP) %>%
    dplyr::select( - "ID")

  df[is.na(df$HOMICIDE),'HOMICIDE'] <- 0
  df[is.na(df$SUICIDE),'SUICIDE'] <- 0
  df[is.na(df$UNDETERMINED),'UNDETERMINED'] <- 0
  df[is.na(df$UNINTENTIONAL),'UNINTENTIONAL'] <- 0
  df[is.na(df$ESTIMATED),'ESTIMATED'] <- 0

  rm(county_pop)
  rm(county_unemp)

  df <- df %>%
    dplyr::mutate("UNEMPLOYED" = .data$LABOR_FORCE - .data$EMPLOYED,
                  "RATE_UNEMPLOYED" = .data$UNEMPLOYED / .data$LABOR_FORCE)

  mean_unemp <- mean(df$RATE_UNEMPLOYED)
  sd_unemp <- stats::sd(df$RATE_UNEMPLOYED)

  df %>%
    dplyr::group_by(.data$COUNTY) %>%
    dplyr::mutate("ZRATE" = (.data$RATE_UNEMPLOYED - mean_unemp) / sd_unemp ) %>%
    dplyr::mutate("BAD_YEARS" = UnempDeaths::CountBadYears(.data$YEAR, .data$ZRATE ) ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$YEAR > 1998 & .data$YEAR < 2019 & !is.na(.data$POP) ) %>%
    dplyr::mutate("DENSITY" = .data$POP / .data$LAND) %>%
    dplyr::left_join( UnempDeaths::GetWorkingAgePop(), by = 'LONG_ID' ) %>%
    dplyr::filter(!is.na(.data$WORKING_AGE) &
                    !is.na(.data$ELDERLY) &
                    !is.na(.data$YOUTH) ) %>%
    dplyr::mutate( 'POP2' = .data$ELDERLY + .data$WORKING_AGE + .data$YOUTH) %>%
    dplyr::mutate( 'ELDERLY' = Reconcile(.data$ELDERLY, .data$POP, .data$POP2),
                   'WORKING_AGE' = Reconcile(.data$WORKING_AGE, .data$POP, .data$POP2 ),
                   'YOUTH' = Reconcile(.data$YOUTH, .data$POP, .data$POP2) ) %>%
    dplyr::select( - 'POP2' ) %>%
    dplyr::mutate( "OUTSIDE" = .data$POP - .data$WORKING_AGE,
                   "DISCOURAGED" = .data$WORKING_AGE - .data$LABOR_FORCE) %>%
    return()
}
