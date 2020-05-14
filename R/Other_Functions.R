#' AddEstimates
#' @description given population dataset made by GetCountyPop, uses lm to predict populations for 1998 and 1999
#' @param county_pop a dataset of population by county from 2000 to 2020
#' @return a population dataset with estimates for 1999 and 1998 added
#' @export
#' @example data <- GetCountyPop %>% AddEstimates()
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats lm
AddEstimates <- function(county_pop){
  bindValue <- data.frame(ID = rep(unique(county_pop$ID), 2 ), YEAR = rep(1998:1999, each = length(unique(county_pop$ID ) ) ) )
  bindValue <- bindValue %>%
    dplyr::mutate("LONG_ID" = paste(.data$ID, .data$YEAR),
                  "POP" = 0 ) %>%
    dplyr::mutate("YEAR" = as.numeric(.data$YEAR),
                  "POP" = as.numeric(.data$POP) )
  for (ID in unique(bindValue$ID ) ){
    POP <- county_pop$POP[county_pop$ID == ID]
    YEAR <- county_pop$YEAR[county_pop$ID == ID]
    if (!(2000 %in% YEAR ) ) next()  # if we don't have 2000 - 2010, not poitn predicting 1998 and 1999
    model <- stats::lm(POP ~ YEAR )

    LONG_ID <- paste(ID, 1999)
    bindValue[bindValue$LONG_ID == LONG_ID, 'POP'] <- model$coefficients[1] + model$coefficients[2] * 1999
    LONG_ID <- paste(ID, 1998)
    bindValue[bindValue$LONG_ID == LONG_ID, 'POP'] <- model$coefficients[1] + model$coefficients[2] * 1998
  }
  return(rbind(county_pop,bindValue) )
}

#' NaByCol
#' @description given a dataset, prints out the number of NAs in each column
#' @param data a dataset to print out NAs for
#' @export
NaByCol <- function(data){
  for (n in names(data) ){
    print(n)
    print(sum(is.na(data[,n])))
  }
}

#' ExcelToTSV
#' @description given a folder of excel files, writes a series of tsv files to another given folder
#' @param from_path address of folder from which to take data--must ONLY include excel files
#' @param to_path address of folder in which to write TSV files
#' @param ... additional params for the read_excel call
#' @export
#' @importFrom readr write_tsv
#' @importFrom readxl read_excel
ExcelToTSV <- function(from_path, to_path, ...){
  files <- list.files(path = from_path)
  for (p in files){
    temp <- as.data.frame(read_excel(paste0(from_path,'/',p), ...))
    #    return(temp)
    len <- str_length(p)
    p2 <- str_replace(p, '.xlsx','.tsv')
    print(p2)
    write_tsv(temp, paste0(to_path, '/', p2 ) )
  }
}

#' PredictDeaths
#' @description gives predictions of number of deaths from model
#' @param model the model being used to make prediction
#' @param data the dataset being used--cannot differ much from package dataset
#' @param change what share of the employed to move to unemployed
#' @return returns predicted number of deaths
#' @export
#' @importFrom readr read_tsv
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom stats predict
PredictDeaths <- function(model, data, change = 0.01){
  change <- 1 - change
  control <- data %>%
    dplyr::filter(.data$YEAR == max(.data$YEAR) ) %>%
    dplyr::mutate ("YEAR" = .data$YEAR + 3 ) %>%
    stats::predict(model, newdata = .data )
  test <- data %>%
    dplyr::filter (.data$YEAR == max(.data$YEAR) ) %>%
    dplyr::mutate ("YEAR" = .data$YEAR + 3 ) %>%
    dplyr::mutate ("EMPLOYED" = .data$EMPLOYED * change) %>%
    dplyr::mutate ("UNEMPLOYED" = .data$LABOR_FORCE - .data$EMPLOYED ) %>%
    dplyr::mutate ("RATE_UNEMPLOYED" = .data$UNEMPLOYED / .data$LABOR_FORCE ) %>%
    stats::predict(model, newdata = .data )

  return(sum(test) - sum(control ) )
}

#' CountBadYears
#' @description Counts number of bad yeras based on a given threshold
#' @param YEAR the YEAR column from the data
#' @param Z_U_RATE zScore of employment data--would work for other columns too
#' @param Threshold The number of standard deviations necessary for a year to count as bad.  Defaults to 1
#' @export
CountBadYears <- function(YEAR, Z_U_RATE, Threshold = 1){
  num_bad_years <- 0
  rValue <- NULL
  for (i in 1:length(YEAR) ){
    if (Z_U_RATE[i] >= Threshold) num_bad_years <- num_bad_years + 1
    rValue <- c(rValue, num_bad_years)
  }
  return(rValue)
}

#' ClusterData
#' @description Clusters the dataframe by LAT and LONG features
#' @param df the dataframe to be sent--build spcifically for dataset with this package
#' @param n_clusters the number of kmeans clusters to use
#' @param ... additional variables for kmeans
#' @export
#' @return the clustered dataset
#' @importFrom stats kmeans
#' @importFrom dplyr select
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
ClusterData <- function(df, n_clusters, ...)
{
  CLUSTER <- df %>%
    dplyr::select('LAT', 'LONG') %>%
    stats::kmeans(n_clusters, ...)
  CLUSTER <- CLUSTER$cluster

  df <- cbind(df, CLUSTER ) %>%
    dplyr::group_by(.data$YEAR, .data$CLUSTER) %>%
#    summarize('LABOR_FORCE' = sum(LABOR_FORCE), 'EMPLOYED' = sum(EMPLOYED), 'LAND' = sum(LAND),
#              'WATER' = sum(WATER), 'POP' = sum(POP), 'UNINTENTIONAL' = sum(UNINTENTIONAL),
#              'SUICIDE' = sum(SUICIDE), 'HOMICIDE' = sum(HOMICIDE), 'UNDETERMINED' = sum(UNDETERMINED),
#              'UNEMPLOYED' = sum(UNEMPLOYED), 'OUTSIDE_LABOR_FORCE' = sum(OUTSIDE_LABOR_FORCE), 'WORKING_AGE' = sum(WORKING_AGE),
#              'ELDERLY' = sum(ELDERLY), 'YOUTH' = sum(YOUTH) ) %>%
    dplyr::summarize('LABOR_FORCE' = sum(.data$LABOR_FORCE), 'EMPLOYED' = sum(.data$EMPLOYED),
                     'LAND' = sum(.data$LAND), 'POP' = sum(.data$POP),
                     'UNINTENTIONAL' = sum(.data$UNINTENTIONAL),'SUICIDE' = sum(.data$SUICIDE),
                     'HOMICIDE' = sum(.data$HOMICIDE), 'UNDETERMINED' = sum(.data$UNDETERMINED),
                     'UNEMPLOYED' = sum(.data$UNEMPLOYED), 'OUTSIDE' = sum(.data$OUTSIDE),
                     'WORKING_AGE' = sum(.data$WORKING_AGE), 'ELDERLY' = sum(.data$ELDERLY),
                     'YOUTH' = sum(.data$YOUTH) ) %>%
    dplyr::ungroup()
  return(df)
}

#' GrowthEstimates
#' @description Given a dataset and two years, creates a new dataset containing population estimates for years between those two days
#' @return A dataset containing population estimates--expotential growth--between the given years for the given population columns
#' @param data a dataset with YEAR, ID columbs, as well as specified population columns
#' @param cols a string or list of strings naming columns to make estimates on
#' @param start a year for which you have data from which to start making estimates
#' @param end a year for which you have data until which you make estimates
GrowthEstimates <- function(data, cols, start = 2000, end = 2010){
  rValue <- NA
  for (col in cols){
    bindValue <- NA
    for (ID in unique(data$ID) ){
      startPop <- data[data$YEAR == start & data$ID == ID, col][[1]]
      endPop <- data[data$YEAR == end & data$ID == ID, col][[1]]
      if(length(startPop) ==0 | length(endPop) == 0 ) next()
      if (all(is.na(bindValue)) ) bindValue <-
        GrowthEstimatesInner(startPop, endPop, ID, col = col, start = start, end = end)
      else bindValue <-
        rbind(bindValue, GrowthEstimatesInner(endPop, endPop, ID, col = col, start = start, end = end))
    }
    if (all(is.na(rValue)) ) rValue <- bindValue
    else rValue <- cbind(rValue, bindValue[,3])
  }
  names(rValue) <- c('ID', 'YEAR', cols)
  return(rValue)
}

#' GrowthEstimatesInner
#' @description Helper function for GrowthEstimates; returns estimates for a single columb given population and years
#' @return a dataframe of population estimates with features ID, YEAR and column name
#' @param startPop population for starting year
#' @param endPop population for ending year
#' @param ID the for the county being estimated
#' @param col the name of the column being estimated
#' @param start starting year
#' @param end ending year
#' @importFrom dplyr mutate
GrowthEstimatesInner <- function(startPop, endPop, ID, col, start = 2000, end = 2010 ){

  k <- log(endPop / startPop) / (end - start )
  start <- start + 1; end <- end - 1
  target <- as.data.frame(cbind(rep(ID, end - start + 1), start:end, startPop * exp((start:end - start) * k) ) )
  names(target) <- c('ID', 'YEAR', col)
  target %>%
    dplyr::mutate('ID' = ID ) %>%
    return()
}

#' Reconcile
#' @description estimates at different levels are not fully commensurate.  This is for helping to reconcile population counts
#' @return the reconciled population figure
#' @param target the feature being reconciled
#' @param goal the population that target and other features should sum to
#' @param circumstances the original sum of the target and other featuers
Reconcile <- function(target, goal, circumstances){
  return(target * goal / circumstances)
}

#' predict_logratio
#' @description given the dataset, shifts some of the population, and then performs a logratio transformation
#' @param shift how much of the population to shift from one category to another
#' @param type a number of value -3, -2, -1, 1, 2, or 3 that tells the function how to shift the population.  Use value 0 to check
#' @param model the model using logratios to make predictions on
#' @param df the data used to create the model before logratio was applied
#' @return the change in the target variable--homicides, suicides, or unintentoinal deaths--from applying the shift
#' @export
#' @importFrom stringr str_length
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stats predict
#' @importFrom rlang .data
predict_logratio <- function(df, model, shift = 0.5, type = -3 ){
  # dreadful, dismal, bad, good, great, excellent
  if (is.character(type) ){
    type <- switch(stringr::str_length(type), 0,0,-1,1,2,-2,0,-3,3)
  }
  # EMPLOYED -> DISCOURAGED
  if (type == -3 ){
    newdata <- df %>%
      dplyr::mutate( 'DISCOURAGED' = .data$DISCOURAGED + shift )%>%
      dplyr::mutate( 'EMPLOYED'    = .data$EMPLOYED    - shift )
  }
  # UNEMPLOYED & (EMPLOYED) -> DISCOURAGED
  if (type == -2 ){
    newdata <- df %>%
      dplyr::mutate( 'DISCOURAGED' = .data$DISCOURAGED + shift )%>%
      dplyr::mutate( 'EMPLOYED'    = dplyr::if_else(.data$UNEMPLOYED >= shift,
                                                    .data$EMPLOYED,
                                                    .data$EMPLOYED - (shift - .data$UNEMPLOYED ) ) ) %>%
      dplyr::mutate( 'UNEMPLOYED'  = if_else(.data$UNEMPLOYED >= shift,
                                             .data$UNEMPLOYED - shift,
                                             0 ) )
  }
  if (type == 0){
    newdata <- df
  }
  # EMPLOYED -> UNEMPLOYED
  if (type == -1){
    newdata <- df %>%
      dplyr::mutate( 'UNEMPLOYED' = .data$UNEMPLOYED + shift ) %>%
      dplyr::mutate( 'EMPLOYED'   = .data$EMPLOYED   - shift )
  }
  # DISCOURAGED -> UNEMPLOYED
  if (type == 1){
    newdata <- df %>%
      dplyr::mutate( 'TEMP' = .data$DISCOURAGED) %>%
      dplyr::mutate( 'DISCOURAGED' = if_else( .data$DISCOURAGED >= shift,
                                              .data$DISCOURAGED - shift,
                                              0) ) %>%
      dplyr::mutate( UNEMPLOYED  = if_else( .data$TEMP >= shift,
                                            .data$UNEMPLOYED + shift,
                                            .data$UNEMPLOYED + .data$TEMP ) ) %>%
      dplyr::select( - 'TEMP' )
  }
  # UNEMPLOYED -> EMPLOYED
  if (type == 2){
    newdata <- df %>%
      dplyr::mutate( 'TEMP' = .data$UNEMPLOYED) %>%
      dplyr::mutate( 'UNEMPLOYED' = dplyr::if_else( .data$UNEMPLOYED >= shift,
                                                    .data$UNEMPLOYED - shift,
                                                    0) ) %>%
      dplyr::mutate( EMPLOYED   = dplyr::if_else( .data$TEMP >= shift,
                                                  .data$EMPLOYED + shift,
                                                  .data$EMPLOYED + .data$TEMP ) ) %>%
      dplyr::select( - 'TEMP' )
  }
  # DISCOURAGED & (UNEMPLOYED)  -> EMPLOYED
  if (type == 3){
    newdata <- df %>%
      dplyr::mutate( 'TEMP_D' = .data$DISCOURAGED) %>%
      dplyr::mutate( 'TEMP_U' = .data$UNEMPLOYED) %>%
      dplyr::mutate( 'DISCOURAGED' = dplyr::if_else(.data$DISCOURAGED >= shift, .data$DISCOURAGED - shift, 0) ) %>%
      dplyr::mutate( 'UNEMPLOYED'  = dplyr::if_else( .data$TEMP_D >= shift,
                                                     .data$UNEMPLOYED,
                                                     dplyr::if_else( .data$UNEMPLOYED > (shift - .data$TEMP_D),
                                                                     .data$UNEMPLOYED - (shift - .data$TEMP_D),
                                                                     0) ) ) %>%
      dplyr::mutate( 'EMPLOYED' = dplyr::if_else( .data$TEMP_U + .data$TEMP_D >= shift,
                                                  .data$EMPLOYED +shift,
                                                  .data$EMPLOYED + .data$TEMP_U + .data$TEMP_D ) ) %>%
      dplyr::select( - 'TEMP_U', - 'TEMP_D' )
  }
  newdata   <- newdata[ ,c('DISCOURAGED', 'UNEMPLOYED','EMPLOYED','YOUTH','ELDERLY', 'DENSITY' ) ]
  newdata   <- ilr_partial(newdata, c('DENSITY') )
  p_base    <- stats::predict(model)
  p_project <- stats::predict(model, newdata = newdata)
  return( sum(p_project - p_base) / sum(p_base) * 100)
}

#' ilr_partial
#' @description performs an ilr transformation on the dataset, but excluding several columns
#' @param exceptions the columns not to include in the transformation
#' @param df the data to transform
#' @return the df, with several of the variables transformed, and the exceptions unchanged
#' @export
#' @importFrom compositions ilr
#' @importFrom dplyr select
ilr_partial <- function(df, exceptions ){
  y <- df %>%
    dplyr::select(exceptions)
  x <- df %>%
    dplyr::select( - exceptions) %>%
    compositions::ilr()
  return(cbind(y,x) )
}

