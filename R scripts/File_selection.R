source("../../R scripts/Algorithm_version_selection.R")

# File selection based on the algorithm used
if (algorithm == "BirdNET") {
  
  # File selection based on the algorithm version used
  if (BirdNET_version == "Lite") {
    files <- list.files(pattern = "\\BirdNET_results*", recursive = TRUE, include.dirs = TRUE)
  } else {
    files <- list.files(pattern = "\\BirdNET_Analyzer_results*", recursive = TRUE, include.dirs = TRUE)
  }
  
  # File selection based on the filtering procedure used
  if (filtered) {
    files <- files[grepl("*filtered*", files)]
  } else {
    files <- files[!grepl("*filtered*", files)]
  }
  
  # File selection based on the overlap allowed between consecutive prediction segments
  if (overlap == 1) {
    files <- files[grepl("*overlap_1s*", files)]
  } else if (overlap == 2) {
    files <- files[grepl("*overlap_2s*", files)]
  } else {
    files <- files[!grepl("*overlap*", files)]
  }
  
  # File selection based on the detection sensitivity used
  if (detection_sensitivity == 1) {
    files <- files[grepl("*ds10.*", files)]
  } else if (detection_sensitivity == 1.5) {
    files <- files[grepl("*ds15.*", files)]
  } else {
    files <- files[grepl("*ds05.*", files)]
  }
} else if (algorithm == "Google") {
  google_CSV_path <- "../../Google_Algorithm/CSVs/BirdNET format/"
  files <- paste0(google_CSV_path, list.files(path = google_CSV_path, pattern = "\\.csv", recursive = TRUE, include.dirs = TRUE))
}

species_list_files <- list.files(pattern = "\\BirdNET_Analyzer_species_list.csv$", recursive = TRUE, include.dirs = TRUE)



