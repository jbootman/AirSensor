#' @export
#' @importFrom rlang .data
#' 
#' @title Join PurpleAir time series data for a single sensor
#' 
#' @param ... Any number of valid PurpleAir Time series \emph{pat} objects.
#' 
#' @return A PurpleAir Time series \emph{pat} object.
#' 
#' @description Create a merged timeseries using of any number of \emph{pat} 
#' objects for a single sensor. If \emph{pat} objects are non-contiguous, the 
#' resulting \emph{pat} will have gaps.
#' 
#' @note An error is generated if the incoming \emph{pat} objects have 
#' non-identical metadata.
#' 
#' @examples 
#' library(AirSensor)
#' 
#' aug01_08 <- 
#'   example_pat %>%
#'   pat_filterDate(20180801, 20180808)
#' 
#' aug15_22 <- 
#'   example_pat %>% 
#'   pat_filterDate(20180815, 20180822)
#'   
#' pat_join(aug01_08, aug15_22) %>%
#'   pat_multiPlot(plottype = "pm25")
#' 

pat_join <- function(
  ...
) {

  # Accept any number of pat objects
  patList <- list(...)  
  
  # ----- Validate parameters --------------------------------------------------
  
  # ----- Join (concatenate) timeseries ----------------------------------------
  
  # NOTE:  If the fist element is NOT a "pa_timeseries", assume we are being 
  # NOTE:  handed a list of pat objects rather than separate pat objects.
  suppressWarnings({
    if( !"pa_timeseries" %in% class(patList[[1]]) ) {
      patList <- patList[[1]]
    }
  })
  
  # Initialize empty lists 
  dataList <- list()
  metaList <- list()
  
  patCount <- length(patList)
  
  for( i in seq_along(patList) ) {
    
    # Guarantee proper 'datetime' ordering
    patList[[i]]$data <- 
      patList[[i]]$data %>%
      dplyr::arrange(.data$datetime)
    
    # Check parameters
    if( !pat_isPat(patList[[i]]) )
      stop("arguments contain a non-'pat' object")
    if( pat_isEmpty(patList[[i]]) )
      stop("arguments contain an empty 'pat' object")
    
    metaList[[i]] <- patList[[i]]$meta
    
    # NOTE:  Monthly pat objects have an extra UTC day at the beginning and end
    # NOTE:  to guarantee that we always have a complete month in the local
    # NOTE:  timezone. We trim things here so that we don't have overlapping
    # NOTE:  timesteps:
    
    if ( i == patCount ) {
      # use patList[[i]] end
      endtime <- range(patList[[i]]$data$datetime, na.rm = TRUE)[2]
    } else {
      # use patList[[i+1]] start and find the previous timestep
      endtime <- patList[[i+1]]$data$datetime[1]
      index <- which(patList[[i]]$data$datetime == endtime)
      if ( length(index) > 0 && index > 2) {
        endtime <- patList[[i]]$data$datetime[index - 1]
      }
    }
    
    dataList[[i]] <- 
      patList[[i]]$data %>%
      dplyr::filter(.data$datetime <= endtime)
  
  }
  
  # TODO:  We have a basic problem with the pwfsl_closest~ variables.
  # TODO:  These can change when a new, temporary monitor gets installed.
  # TODO:  We don't want to have two separate metadata records for a single 
  # TODO:  Sensor as the metadata is supposed to be location-specific and
  # TODO:  not time-dependent. Unfortunately, the location of the nearest
  # TODO:  PWFSL monitor is time-dependent and any choice we make will break
  # TODO:  things like pat_externalFit() for those periods when a temporary
  # TODO:  monitor is closer than a permanent monitor.
  # TODO:
  # TODO:  Ideally, enhanceSynopticData() would have some concept of
  # TODO:  "permanent" monitors but this is far beyond what is currently
  # TODO:  supported.
  
  # Deal with pwfsl_closest~ issue by always using the most recent value
  meta_last <- metaList[[length(metaList)]]
  for ( i in seq_along(metaList) ) {
    metaList[[i]]$pwfsl_closestDistance <- meta_last$pwfsl_closestDistance
    metaList[[i]]$pwfsl_closestMonitorID <- meta_last$pwfsl_closestMonitorID
  }
  
  # Check that meta matches
  if( length(unique(metaList)) != 1 )
    stop("`pat` objects must be of the same monitor")
  
  meta <- patList[[1]]$meta
  data <- do.call(rbind, dataList) # duplicates removed below in pat_distinct()
  
  # ----- Create the PurpleAir Timeseries (pat) object ------------------------
  
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(invisible(pat))
  
}
