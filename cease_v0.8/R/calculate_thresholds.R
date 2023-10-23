#' This function calculates a threshold for each stream reach and sensor.
#' Input: 
#'   * stream_reaches - dataframe with column location_id storing stream reach id (e.g., EX1)
#'   * stream_observations - dataframe, manual observations of the stream state
#' Output: dataframe with the following columns:
#'   * location_id - string type, id of the stream reach (e.g., EX1, EX25, ...)
#'   * sensor - string type, id of the sensor (e.g., X5, XGW1, ...)
#'   * value - float type, threshold's value (e.g., 5.25)
#'   * score - integer type, the score representing how good is the sensor (e.g., 3)
calculate_thresholds <- function(stream_reaches, stream_observations, sensors_measurements, debug=FALSE) {
  thresholds <- data.frame()
  thresholds_row <- 1
  for (row in seq_len(nrow(stream_reaches))) {
    anchor_point <- stream_reaches[row, "location_id"]

    stream_location_observations <- subset(stream_observations, location_id == anchor_point)
    sensors <- names(sensors_measurements[-c(1)])
    for (sensor in sensors) {
      threshold <- calculate_threshold(sensor, stream_location_observations, sensors_measurements, debug)
      thresholds[thresholds_row, "location_id"] <- anchor_point
      thresholds[thresholds_row, "sensor"] <- sensor
      thresholds[thresholds_row, "value"] <- threshold$value
      thresholds[thresholds_row, "score"] <- threshold$score
      thresholds_row <- thresholds_row + 1
    }
  }
  return(thresholds)
}

#' This function calculates a threshold for a given stream reach
#' Input arguments:
#'    * sensor_location_id - string type, id of the stream reach (e.g., EX1)
#'    * stream_location_observations - dataframe, manual observations of the stream state. Columns:
#'        * sensor, string, id of the sensor, (e.g., X1)
#'        * state, string, possible values: "flow", "no_flow", "wt" (weakly trickling)
#'        * water_level, float, water level measured by the sensor (e.g., 12.4)
#'    * sensors_measurements - dataframe, water level measurements for all sensors. Columns
#'        * first column contains the date and time when the measurement was taken (e.g., '2021-07-01 12:00')
#'        * each sensor id in separated column, (e.g., "X1", "X2", "X3", ...)
#' Output:
#'    * list(value, score) where 
#'        value (float) is the threshold, and
#'        score (integer) is the number representing how reliable is the threshold
#' @return list("value", "score" = best_score))
calculate_threshold <- function(sensor_location_id, stream_location_observations, sensors_measurements, debug=FALSE) {
  if (debug) {
    print(paste0("[Debug] Find threshold: sensor_location_id=", sensor_location_id))
  }

  stream_observation <- subset(stream_location_observations, sensor == sensor_location_id)
  stopifnot("Stream's observation water level measurement must be a number" = (nrow(stream_observation) == 0 || is.numeric(stream_observation$water_level)))

  no_flow <- subset(stream_observation, state == "no_flow")
  flow <- subset(stream_observation, state == "wt" | state == "flow")

  no_flow_count <- nrow(no_flow)
  flow_count <- nrow(flow)

  if (debug) {
    print(paste0("[Debug] Flow: ", flow_count, " no flow: ", no_flow_count))
  }

  # Examine all possible cases:
  if (no_flow_count == 0 && flow_count > 0) {
    # Anchor point's stream state was always observed as FLOW for this sensor's measurements
    best_threshold <- min(sensors_measurements[, sensor_location_id], na.rm = TRUE)
    best_score <- 0
  } else if (no_flow_count > 0 && flow_count == 0) {
    # Anchor point's stream state was always observed as NO FLOW for this sensor's measurements
    best_threshold <- max(sensors_measurements[, sensor_location_id], na.rm = TRUE)
    best_score <- 0
  } else if (no_flow_count == 0 && flow_count == 0) {
    # Anchor point's stream state was never observed for this sensor's measurements
    best_threshold <- NA
    best_score <- NA
  } else {
    # Anchor point's stream state was was FLOW or NO FLOW for this sensor's measurements.
    # Let's predict the stream state based on the historical data, i.e., flow and no flow
    # thresholds.
    best_threshold <- NA
    best_score <- NA
    same_score <- 0

    thresholds <- data.frame()
    thresholds_row <- 1
    for (row in seq_len(nrow(stream_observation))) {
      state <- stream_observation[row, "state"]
      water_level <- stream_observation[row, "water_level"]
      threshold <- water_level
      if (state == "wt" || state == "flow") {
        # make it smaller, otherwise we will classify this observation incorrectly
        # because FLOW must be larger than threshold
        threshold <- threshold - SENSOR_VALUE_RESOLUTION()
      }

      incorrectly_classified_no_flow <- nrow(subset(no_flow, water_level > threshold))
      incorrectly_classified_flow <- nrow(subset(flow, water_level <= threshold))
      score <- incorrectly_classified_no_flow + incorrectly_classified_flow

      if (!is.na(threshold)) {
        thresholds[thresholds_row, "water_level"] <- water_level
        thresholds[thresholds_row, "threshold"] <- threshold
        thresholds[thresholds_row, "score"] <- score
        thresholds[thresholds_row, "state"] <- state
        thresholds_row <- thresholds_row + 1
      }
    }

    if (debug) {
      print(thresholds)
    }

    # the lower the score the better
    if (nrow(thresholds) > 0) {
      best_score <- min(thresholds$score)
      best <- subset(thresholds, score == best_score)
      same_score <- nrow(best)
      best_threshold <- mean(best$threshold)
    }

    if (debug) {
      print(paste("same_score=", same_score))
    }
  }

  if (debug) {
    print(paste("threshold=", best_threshold, " score=", best_score))
  }

  return(list("value" = best_threshold, "score" = best_score))
}

#' sentinel values for min integer
MIN_THRESHOLD_VALUE <- function() {
  return(-99999)
}

#' sentinel values for max integer
MAX_THRESHOLD_VALUE <- function() {
  return(99999)
}

SENSOR_VALUE_RESOLUTION <- function() {
  return(0.001)
}
