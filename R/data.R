#' Hourly arrivals into the University of Iowa Hospital Emergency Department
#'
#' A data set containing the 17 columns described below. There are 41640
#' observations running from 2013 to 2018. Data set are already sorted by time.
#'
#' @format data frames with 17 columns:
#' \describe{
#'   \item{Year}{Calendar year}
#'   \item{Quarter}{Fiscal year quarter}
#'   \item{Month}{Integer for month of year}
#'   \item{Day}{Integer for day of month}
#'   \item{Hour}{Integer for hour of day}
#'   \item{Arrivals}{Number of arrivals into the ED (outcome)}
#'   \item{Date}{Date}
#'   \item{Weekday}{Indicator for day of week}
#'   \item{temp}{hourly concurrent temperature}
#'   \item{xmas}{Christmas day indicator}
#'   \item{xmas2}{Day after Christmas}
#'   \item{nye}{New Years Eve indicator}
#'   \item{nyd}{New Years Day indicator}
#'   \item{thx}{Thanksgiving day indicator}
#'   \item{thx}{Thanksgiving day (after) indicator}
#'   \item{ind}{Independence day indicator}
#'   \item{game_Day}{Hawkeye football game day indicator}
#' }
#' @source UIHC Emergency Department.
"uihc_ed_arrivals"

