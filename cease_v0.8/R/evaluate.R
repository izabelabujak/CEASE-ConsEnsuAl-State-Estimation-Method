#' This function evaluates the model by comparing predicted stream states with the stream states observed by humans.
#' Input:
#'    * stream_reaches, data frame, column 'location_id' containing ids of stream reaches (e.g., EX1, ...)
#'    * sensors_measurements - dataframe containing water level measurements for all sensors.
#'        * first column 'observed_at' contains the date and time when the measurement was taken (e.g., '2021-07-01 12:00')
#'        * each sensor id in separated column, (e.g., "X1", "X2", "X3", ...), the cell contains the measured water level
#'    * thresholds - output of the function 'calculate_thresholds()'
#'    * stream_observations - dataframe, manual observations of the stream state.
#'        * Column 'sensor', string, id of the sensor, (e.g., X1)
#'        * Column 'state', string, possible values: "flow", "no_flow", "wt" (weakly trickling)
#'        * Column 'water_level', float, water level measured by the sensor (e.g., 12.4)
#' Output: A list containing the following elements
#'    * sensitivity
#'    * specificity
#'    * accuracy
#'    * true_positive
#'    * false_positive
#'    * false_negative
#'    * true_negative
#'    * failure - number of cases when the model failed to predict state because of for example lack of data from sensors
#'    * no_decision - number of cases when the model could not predict state because tie in the consensual voting
#'    * total - total number of stream state predictions
evaluate <- function(stream_reaches, sensors_measurements, thresholds, stream_observations, same_group_size=TRUE, debug=FALSE) {
  set.seed(NULL)
  comparisons_results <- data.frame()
  comparisons_results_row <- 1

  surveys_observations <- convert_stream_observations_to_survey_observations(stream_observations)

  if (same_group_size) {
    surveys_observations <- subset(surveys_observations, location_id %in% stream_reaches[,'location_id'] )
    # make flow/no flow subsets equal size
    flow_subset <- subset(surveys_observations, state == "flow" | state == "wt")
    noflow_subset <- subset(surveys_observations, state == "no_flow")
    print(c(nrow(flow_subset), nrow(noflow_subset)))
    number_of_elements <- min(c(nrow(flow_subset), nrow(noflow_subset)))
    print(number_of_elements)

    flow_subset <- flow_subset[sample(nrow(flow_subset), number_of_elements), ]
    noflow_subset <- noflow_subset[sample(nrow(noflow_subset), number_of_elements), ]
    print(nrow(flow_subset))
    print(nrow(noflow_subset))

    surveys_observations <- rbind(flow_subset, noflow_subset)
  }

  # looping over df with anchor points
  for (row in seq_len(nrow(stream_reaches))) {
    anchor_point <- stream_reaches[row, 'location_id']

    survey_observations <- subset(surveys_observations, location_id == anchor_point)
    ## comparing manual measurement and predicted flow states
    for (item in seq_len(nrow(survey_observations))) {
      manual_observations_time <- survey_observations[item, 'observed_at']
      flow_state_manual <- survey_observations[item, 'state']

      ### run model and predict stream state for the location
      flow_state_predicted <- predict_reach_state(anchor_point, manual_observations_time, sensors_measurements, thresholds)

      if (debug) {
        print(paste0(row, "/", nrow(stream_reaches), ". Evaluating at anchor point: ", anchor_point, " on ", manual_observations_time, ". observed state=", flow_state_manual, ", predicted state=", flow_state_predicted))
      }

      ### compare the two and classify the model outcomes to four categories
      if (is.na(flow_state_predicted)) {
        if (debug) {
          print(paste0("[Debug] Prediction failure: anchor_point=", anchor_point, " manual_observations_time=", manual_observations_time, " flow_state_manual=", flow_state_manual))
        }
        comparisons_results[comparisons_results_row, 'comparison_result'] <- 'failure'
      } else if (flow_state_predicted == -1) {
        comparisons_results[comparisons_results_row, 'comparison_result'] <- 'no_decision'
      } else if ((flow_state_manual == "flow" || flow_state_manual == "wt") && flow_state_predicted == 1) {
        comparisons_results[comparisons_results_row, 'comparison_result'] <- 'true_positive'
      } else if (flow_state_manual == "no_flow" && flow_state_predicted == 1) {
        comparisons_results[comparisons_results_row, 'comparison_result'] <- 'false_positive'
      } else if ((flow_state_manual == "flow" || flow_state_manual == "wt") && flow_state_predicted == 0) {
        comparisons_results[comparisons_results_row, 'comparison_result'] <- 'false_negative'
      } else if (flow_state_manual == "no_flow" && flow_state_predicted == 0) {
        comparisons_results[comparisons_results_row, 'comparison_result'] <- 'true_negative'
      }
      comparisons_results_row <- comparisons_results_row + 1
    }
  }

  # return three values for this model: sensitivity, specificity and accuracy
  true_positive <- nrow(subset(comparisons_results, comparison_result == 'true_positive'))
  false_positive <- nrow(subset(comparisons_results, comparison_result == 'false_positive'))
  false_negative <- nrow(subset(comparisons_results, comparison_result == 'false_negative'))
  true_negative <- nrow(subset(comparisons_results, comparison_result == 'true_negative'))
  failure <- nrow(subset(comparisons_results, comparison_result == 'failure'))
  no_decision <- nrow(subset(comparisons_results, comparison_result == 'no_decision'))
  total <- nrow(comparisons_results)

  sensitivity <- round((true_positive / (false_negative + true_positive)), 3)
  specificity <- round(true_negative / (false_positive + true_negative), 3)
  accuracy <- round((true_positive + true_negative) / (false_positive + true_positive + false_negative + true_negative), 3)

  print(paste0("Sensitivity: ", sensitivity, " Specificity: ", specificity, " Accuracy: ", accuracy, " Failure: ", failure, " No_decision: ", no_decision, " Total: ", total))

  result <- list("sensitivity" = sensitivity,
                 "specificity" = specificity,
                 "accuracy" = accuracy,
                 "true_positive" = true_positive,
                 "false_positive" = false_positive,
                 "false_negative" = false_negative,
                 "true_negative" = true_negative,
                 "failure" = failure,
                 "no_decision" = no_decision,
                 "total" = total
            )

  return(result)
}

convert_stream_observations_to_survey_observations <- function(survey_observations) {
  df <- survey_observations[, c("location_id", "observed_at", "state")]
  df <- df[!duplicated(df[c("location_id", "observed_at")]), ]
  return(df)
}
