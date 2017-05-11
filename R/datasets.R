
#' Table: FBI crime data
#'
#' A dataset containing crime data
#' \itemize{
#'   \item Year.
#'   \item Population.
#'   ...
#' }
#'
#' @format A data frame with 20 years of crime from 1994 through 2013.
#' Columns are: Year, Population, Violent_crime, Violent_crime_rate, Murder_and_nonnegligent_manslaughter, Murder_and_nonnegligent_manslaughter_rate, Rape_legacy_definition, Rape_legacy_definition_rate, Robbery, Robbery_rate, Aggravated_assault, Aggravated_assault_rate, Property_crime, Property_crime_rate, Burglary, Burglary_rate, Larceny_theft, Larceny_theft_rate, Motor_vehicle_theft, Motor_vehicle_theft_rate
#'
#' @source FBI
"crime"

#' Table: Income Dataset
#'
#' A dataset containing income slabs
#' \itemize{
#'   \item Year.
#'   \item Households.
#'   ...
#' }
#'
#' @format A data frame with income data from 1967 through 2015.
#'
#' @source Census Bureau
"income"

#' Table: Employment Population Ratio
#'
#' A dataset containing ratio between population and employment
#' \itemize{
#'   \item Year.
#'   \item Months of the Year.
#' }
#'
#' @format A data frame with employment ratio from 2007 through 2016.
#'
#' @source Department of Labor
"employment_population_ratio"

#' Table: Unemployment Rate
#'
#' A dataset containing unemployment rate
#' \itemize{
#'   \item Year.
#'   \item Months of the Year.
#' }
#'
#' @format A data frame with unemployment rate from 2007 through 2016.
#'
#' @source Department of Labor
"unemployment_rate"

#' Table: Life Expectancy
#'
#' A dataset containing Life Expectancy by Country or regions (like world, Arab world, etc) between 1960 and 2015
#' \itemize{
#'   \item Country.
#'   \item Year.
#'   \item Life Expectancy (Age).
#' }
#'
#' @format A data frame with life expectacny from 1960 through 2015.
#'
#' @source World Bank @ http://data.worldbank.org/indicator/SP.DYN.LE00.IN
"LifeExpectancy"
