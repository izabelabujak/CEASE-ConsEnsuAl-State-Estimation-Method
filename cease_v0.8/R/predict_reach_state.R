#' This function predicts the stream state ('flow' or 'no_flow') in the specific anchor location
#' in the give date and time based on the stream observations and sensor measurements.
#' Input:
#' @param anchor_point - name of the anchor point, e.g., EX1
#' @param datetime - date and time of the stream state to predict, e.g., '2021-08-12 19:25'
#' @param sensors_measurements - a dataframe containing water levels measured by all sensors.
#'        * first column 'observed_at' contains the date and time when the measurement was taken (e.g., '2021-07-01 12:00')
#'        * each sensor id in separated column, (e.g., "X1", "X2", "X3", ...), the cell contains the measured water level
#' @param thresholds - output of the function 'calculate_thresholds()'
#' Output
#' @return(flow_state), integer, possible values:
#'    * NA - Could not predict the flow state. Example reason, lack of sensor measurement for the given time, or could not
#'        solve sensor voting consensus.
#'    * 1 - stream flow
#'    * 0 - no stream flow
predict_reach_state <- function(anchor_point, datetime, sensors_measurements, thresholds, debug=FALSE) {
  if (debug) {
    print(paste0("\n\nPredict reach state failed: anchor_point=", anchor_point, " datetime=", datetime))
  }

  # find the water level measured by the sensor in `sensor_location_id`` at time `datatime``
  datetime_start <- as.POSIXct(datetime) - 5 * 60 # 5 minutes earlier
  datetime_end <- as.POSIXct(datetime) + 5 * 60 # 5 minutes later
  sensors_measurements_row <- subset(sensors_measurements, datetime_start <= observed_at & observed_at <= datetime_end)
  if (nrow(sensors_measurements_row) == 0) {
    if (debug) {
      print(paste0("Predict reach state failed: could not find sensor measurements on ", datetime))
    }
    return(NA)
  }

  # We will write prediction results, done for each sensor individually, into a data frame.
  # The predicted stream state (output of this function) is taken from the majority of the
  # prediction results.
  flow_predictions <- data.frame()
  flow_predictions_row <- 1
  for (i in seq(2, ncol(sensors_measurements_row))) {
    sensor_location_id <- colnames(sensors_measurements)[i]
    sensor_water_level <- sensors_measurements_row[1, i]

    if (debug) {
      print(paste0("[Debug] Predict reach state: sensor_location_id=", sensor_location_id, " sensor_water_level=", sensor_water_level))
    }
    # Sanity check:
    stopifnot("Sensor's water level measurement must be a number" = is.numeric(sensor_water_level))
    if (is.na(sensor_water_level)) {
      # skip this sensor, it has nothing to say because is did not measure anything
      next
    }

    # stream_location_observations has all sensor values read for particular anchor_point
    threshold <- list(value = NA, score = NA)
    threshold_row <- subset(thresholds, location_id == anchor_point & sensor == sensor_location_id)
    if (nrow(threshold_row) == 1) {
      threshold <- list()
      threshold$value <- threshold_row[1, "value"]
      threshold$score <- threshold_row[1, "score"]
    }

    # Examine all possible cases:
    if (is.na(sensor_water_level)) {
      prediction <- -1
    } else if (is.na(threshold$value)) {
      prediction <- -1
    } else if (sensor_water_level > threshold$value) {
      prediction <- 1
    } else {
      prediction <- 0
    }
    if (debug) {
      print(paste0("[Debug] Predict reach state: sensor_location_id=", sensor_location_id, " prediction=", prediction))
    }

    # The predicted stream state can be only:
    # 1: FLOW
    # 0: NO_FLOW
    # -1: NA
    flow_predictions[flow_predictions_row, "sensor_location_id"] <- sensor_location_id
    flow_predictions[flow_predictions_row, "threshold"] <- threshold$value
    flow_predictions[flow_predictions_row, "reliability"] <- threshold$score
    flow_predictions[flow_predictions_row, "distance"] <- abs(sensor_water_level - threshold$value)
    flow_predictions[flow_predictions_row, "prediction"] <- prediction
    flow_predictions_row <- flow_predictions_row + 1
  }

  # The final predicted stream state is based on the majority of predicted stream state values.
  # I.e., if most sensors indicate that there is FLOW, then return FLOW.
  flow <- nrow(subset(flow_predictions, prediction == 1))
  no_flow <- nrow(subset(flow_predictions, prediction == 0))
  if (debug) {
    print(flow_predictions)
    print(paste0("[Debug] Predict reach state: flow=", flow, " no_flow=", no_flow))
  }

  # Let's handle the case when there is the same number of votes for FLOW and NO_FLOW. To solve this
  # we will remove the less reliable prediction.
  if (flow > 0 && no_flow > 0 && flow == no_flow) {
    less_reliable_prediction <- find_less_reliable_prediction(flow_predictions, debug)
    if (!is.na(less_reliable_prediction)) {
      if (less_reliable_prediction == 1) {
        flow = flow - 1
      } else {
        no_flow = no_flow - 1
      }
    }
  }

  predicted_reach_state = 1 # FLOW
  # Let's do the final prediction based on sensors votes.
  if (flow == no_flow) {
    # Model could not predict the stream state.
    predicted_reach_state <- NA
  } else if (flow < no_flow) {
    # Majority says that the stream state was not flowing.
    predicted_reach_state <- 0
  }

  if (debug) {
    print(paste0("[Debug] predicted_reach_state=", predicted_reach_state))
  }

  return(predicted_reach_state)
}

#' Internal function to find less reliable prediction. Used to solve ties.
find_less_reliable_prediction <- function(predictions, debug=FALSE) {
  if (debug) {
    print(paste0("[Debug] Find less reliable sensor"))
    print(predictions)
  }

  stopifnot("Sensors prediction must be in a data.frame" = is.data.frame(predictions))
  stopifnot("Sensors prediction data frame must contain sensor_location_id column" = ("sensor_location_id" %in% names(predictions)))
  stopifnot("Sensors prediction data frame must contain reliability column" = ("sensor_location_id" %in% names(predictions)))
  stopifnot("Sensors prediction data frame must contain prediction column" = ("sensor_location_id" %in% names(predictions)))

  predictions <- subset(predictions, prediction != -1)
  if (nrow(predictions) == 0) {
    return(NA)
  }

  worst_reliability <- max(predictions$reliability)
  worst_predictions <- subset(predictions, reliability == worst_reliability)
  if (debug) {
    print(paste0("[Debug] Worst reliability: ", worst_reliability))
    print(worst_predictions)
  }

  worst_distance <- min(worst_predictions$distance)
  worst_predictions <- subset(worst_predictions, distance == worst_distance)
  if (debug) {
    print(paste0("[Debug] Worst distance: ", worst_distance))
    print(worst_predictions)
  }

  flow_predictions <- nrow(subset(worst_predictions, prediction == 1))
  no_flow_predictions <- nrow(subset(worst_predictions, prediction == 0))

  if (flow_predictions == no_flow_predictions) {
    return(NA)
  } else if (no_flow_predictions == 0) {
    return(1)
  } else if (flow_predictions == 0) {
    return(0)
  } else if (flow_predictions < no_flow_predictions) {
    return(1)
  } else {
    return(0)
  }
}