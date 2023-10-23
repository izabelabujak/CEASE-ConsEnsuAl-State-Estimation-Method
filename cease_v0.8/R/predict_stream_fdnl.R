#' Calculates the flowing drainage network length (FDNL) of the entire stream based on the stream state predictions for the given time.
#' Input:
#' @param datetime - date and time when to calculate the FDNL based on predicted stream states, e.g., '2021-09-23 12:00'
#' @param stream_reaches - dataframe with all stream raches and their lengths. Columns:
#'      * location_id - id of the anchor point, e.g., "EX1"
#'      * length - length of the stream reach, e.g., 54
#' @param sensors_measurements - a dataframe containing water levels measured by all sensors.
#'        * first column 'observed_at' contains the date and time when the measurement was taken (e.g., '2021-07-01 12:00')
#'        * each sensor id in separated column, (e.g., "X1", "X2", "X3", ...), the cell contains the measured water level
#' @param thresholds - output of the function 'calculate_thresholds()'
#' Output:
#' @return active stream network length in the requested date and time.
predict_stream_fdnl <- function(datetime, stream_reaches, sensors_measurements, thresholds, debug=FALSE) {
  fdnl <- NA

  # Predict the stream state for each reach and use it for the total fdnl calculation
  for (row in seq_len(nrow(stream_reaches))) {
    anchor_point <- stream_reaches[row, "location_id"]

    # run model to predict the stream state in the given anchor point
    stream_state <- predict_reach_state(anchor_point, datetime, sensors_measurements, thresholds=thresholds)

    if (debug) {
      print(paste0("[Debug] Anchor point ", anchor_point, " stream state ", stream_state))
    }

    if (is.na(stream_state)) {
      if (debug) {
        print(paste0("Could not predict reach state for reach ", anchor_point, " on ", datetime))
      }
      return(NA)
    }

    if (stream_state == 1) {
      # the whole reach is flowing
      stream_reach_length <- stream_reaches[which(stream_reaches$location_id == anchor_point), "length"]
      if (is.na(stream_reach_length)) {
        stream_reach_length <- 0
      }
      # Sanity check:
      stopifnot("Stream reach length must be a number" = is.numeric(stream_reach_length))
    } else {
      # the reach is not flowing
      stream_reach_length <- 0
    }

    # add the flowing stream length to the fdnl
    if (is.na(fdnl)) {
      # this is the very first prediction
      fdnl <- stream_reach_length
    } else {
      fdnl <- fdnl + stream_reach_length
    }
  }

  return(fdnl)
}

#' This function calculates FDNL in the given time interval.
#' Input:
#' @param date_start - start date of the interval for which to calculate the FDNL, e.g., '2021-06-01 00:00'
#' @param date_end - end date of the interval for which to calculate the FDNL, e.g., '2021-10-26 00:00'
#' @param stream_reaches - dataframe with all stream reaches and their lengths. Columns:
#'      * location_id - id of the anchor point, e.g., "EX1"
#'      * length - length of the stream reach, e.g., 54
#' @param sensors_measurements - a dataframe containing water levels measured by all sensors.
#'        * first column 'observed_at' contains the date and time when the measurement was taken (e.g., '2021-07-01 12:00')
#'        * each sensor id in separated column, (e.g., "X1", "X2", "X3", ...), the cell contains the measured water level
#' @param thresholds - output of the function 'calculate_thresholds()'
#' @return dataframe with columns:
#'      * datetime - a date and time for which the FDNL has been calculated
#'      * fdnl - the active stream network length calculated for the given date and time
predict_stream_fdnl_for_interval <- function(date_start, date_end, interval, stream_reaches, sensors_measurements, thresholds) {
  results <- data.frame()
  row <- 1
  dates <- seq(as.POSIXct(date_start), as.POSIXct(date_end), by=interval)

  for (i in seq_along(dates)) {
      datetime <- format(dates[i], "%Y-%m-%d %H:%M:%S")
      fdnl <- tryCatch(
        {
          predict_stream_fdnl(datetime, stream_reaches, sensors_measurements, thresholds=thresholds)
        },
        error=function(cond) {
          message(cond)
          NA
        }
      )
      results[row, 'datetime'] <- as.POSIXct(datetime)
      results[row, 'fdnl'] <- fdnl
      row <- row + 1

      if (i %% 24 == 0) {
        print(paste(datetime, fdnl))
      }
  }
  return(results)
}