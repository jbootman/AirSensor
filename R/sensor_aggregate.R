#' @keywords airsensor
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median na.omit quantile sd t.test time
#'
#' @title Aggregate airsensor Object
#' 
#' @param sensor Sensor/ws_monitor \emph{airsensor} object.
#' @param FUN The function to be applied to each vector of numeric \code{sensor} data.
#' @param unit Character string specifying temporal units for binning.
#' @param count Number of units per bin.
#'
#' @description Aggregate (\emph{airsensor}) object along its
#' datetime axis. Temporal aggregation involves splitting a \emph{airsensor} object into
#' separate bins along its datetime axis. \code{FUN} is mapped to the \emph{airsensor}
#' numeric variables in each bin, which are then recombined into an aggregated
#' \emph{sensor} object containing the same metadata as the incoming \code{airsensor}.
#' The functionality extends that of \code{pat_aggregate()}.
#'
#' @details \code{FUN} must operate on univariate numeric vectors and return a
#' scalar value. Besides the data variable, no additional arguments will be
#' provided to this function. This means that functions like \code{mean} and
#' \code{max} will need to be wrapped in a function that specifies
#' \code{na.rm = TRUE}. See the examples below.
#'
#' @return Returns an aggregated \emph{airsensor} object.
#' 
#' @examples
#' library(AirSensor)
#' 
#' sensor_aggregate(example_sensor)
sensor_aggregate <- function(sensor, FUN =  function(x) { mean(x, na.rm = TRUE) }, unit = "hours", count = 24) {
  
  MazamaCoreUtils::stopIfNull(sensor)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(unit)
  MazamaCoreUtils::stopIfNull(count)
  
  data <- sensor$data
  meta <- sensor$meta
  
  if ( !"datetime" %in% names(data) ) {
    stop("Column 'datetime' is is missing from 'sensor'.")
  }
  
  if ( nrow(data) == 0 ) {
    stop("Parameter 'sensor' has no data.")
  }
  
  # Remove any duplicate data records
  data <- dplyr::distinct(data)
  
  # Create break units from count and unit params
  if ( stringr::str_detect(unit, 'minutes') ) {
    lubridateBreakUnit <- paste(count, unit, sep = ' ')
    seqBreakUnit <- paste(count, 'mins', sep = ' ')
  } else if (stringr::str_detect(unit, 'hour') ) {
    lubridateBreakUnit <- paste(count, unit, sep = ' ')
    seqBreakUnit <- paste(count, unit, sep = ' ')
  } else {
    stop('Only hours and minutes are currently supported units.')
  }
  
  # ----- Aggregate Data -------------------------------------------------------
  
  # Only use numeric columns for aggregation matrix
  numeric_cols <- which(unlist(lapply(data, is.numeric)))
  
  # Convert to eXtensible Time Series (xts) data.frame
  # Separate only useful data for calculation (i.e. only numeric)
  data <- xts::xts(
    x = data[numeric_cols],
    order.by = data$datetime,
    unique = TRUE,
    tzone = 'UTC'
  )
  
  # Split the xts into a list of binned xts matrices
  data_bins <- xts::split.xts(
    data,
    f = unit,
    drop = FALSE,
    k = count
  )
  
  # ----- Datetime Axis --------------------------------------------------------
  
  # Get the first index of aligned time for future use.
  datetime <- as.numeric(
    lapply(
      X = data_bins, 
      # Select first datetime index in bin to use as aggregated datetime axis
      FUN = function(x) lubridate::floor_date(zoo::index(x)[1], unit = lubridateBreakUnit) ## First # [nrow(x)] ## Last
    )
  )
  # Convert saved datetime vector back to POSIX* from int
  class(datetime) <- c("POSIXct", "POSIXt")
  attr(datetime, 'tzone') <- 'UTC'
  
  dateRange <- range(datetime)
  starttime <- MazamaCoreUtils::parseDatetime(dateRange[1], timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(dateRange[2], timezone = "UTC")
  
  # Create dataframe with continuous axis
  datetimeAxis <- dplyr::tibble('datetime' = seq(starttime, endtime, by = seqBreakUnit))
  
  # ----- Assemble 'data' ------------------------------------------------------
  
  # Map each binned hourly data.frame to the user defined lambda-like 
  # function f applied via apply to each vector in the mapped data.frame
  mapped <- base::Map(
    data_bins,
    f = function(d, f = FUN) { apply(d, 2, f) }
  )
  
  dataMatrix <-
    do.call(rbind, mapped)
  
  # Add mapped data to pa_timeseries object with aggregate datetime axis
  data <- 
    data.frame(
      'datetime' = datetime, 
      dataMatrix,check.names = F
    ) %>%
    # Cleanup any NaN or Inf that might have snuck in
    dplyr::mutate_all( function(x) replace(x, which(is.nan(x)), NA) ) %>%
    dplyr::mutate_all( function(x) replace(x, which(is.infinite(x)), NA) )
  
  data <- dplyr::left_join(datetimeAxis, data, by = 'datetime', copy = TRUE)
  
  # ----- Return ---------------------------------------------------------------
  
  sensor$data <- data
  return(sensor)
}