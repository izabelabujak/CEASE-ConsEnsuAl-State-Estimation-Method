# clear the environment (all global variables)
rm(list = ls())

# load model
devtools::load_all("cease_v0.8", TRUE)

# Read the example randomized input data
stream_reaches = read.csv(file="example_data/example_stream_reaches.csv", header = TRUE)

stream_observations_for_training = read.csv(file="example_data/example_stream_observations_training.csv", header = TRUE)
stream_observations_for_training$observed_at = as.POSIXct(stream_observations_for_training$observed_at)

stream_observations_for_evaluation = read.csv(file="example_data/example_stream_observations_evaluation.csv", header = TRUE)
stream_observations_for_evaluation$observed_at = as.POSIXct(stream_observations_for_evaluation$observed_at)

sensors_measurements = read.csv(file="example_data/example_sensors_measurements.csv", header = TRUE)
sensors_measurements$observed_at = as.POSIXct(sensors_measurements$observed_at)

# calcualate thresholds
thresholds <- calculate_thresholds(stream_reaches, stream_observations_for_training, sensors_measurements)

# evaluate the model on the example random data and store the results in a CSV file
evaluate_result <- evaluate(stream_reaches, sensors_measurements, thresholds, stream_observations_for_evaluation, same_group_size = FALSE, debug=FALSE)
write.csv(evaluate_result, "evaluation_results.csv")

# run model for the entire interval and calculate fdnl. 
date_start="2021-09-01"
date_end="2021-09-03"
interval="1 hour"
stream_fdnl_for_interval <- predict_stream_fdnl_for_interval(date_start, date_end, interval, stream_reaches, sensors_measurements, thresholds)

# visualise FDNL changes using ggplot. This results show FDNL calculated on example randomized data.
library("ggplot2")
ggplot(stream_fdnl_for_interval, aes(x=datetime, y=fdnl)) + geom_line()
