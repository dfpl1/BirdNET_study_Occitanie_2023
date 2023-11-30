source("./generate_annotation_CSV.R")
source("../../R scripts/Algorithm_version_selection.R")
library(ggplot2)

recording_path <- paste0("../../Recordings/Selected/")

BirdNET_precision_results <- read.csv(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, "/", algorithm, "_precision_results.csv"))

optimal_confidence_thresholds <- read.csv(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, "/", algorithm, "_optimal_confidence_thresholds.csv"))

first_threshold <- optimal_confidence_thresholds$conf_thresholds[1]
second_threshold <- optimal_confidence_thresholds$conf_thresholds[2]
third_threshold <- optimal_confidence_thresholds$conf_thresholds[3]

selected_confidence_levels <- c(first_threshold, second_threshold, third_threshold)
all_confidence_levels <- seq(0.1, 0.99, 0.01)

global_detection_results <- data.frame()

recording_info_extended <- read.csv("../../CSVs/recordings_extended.csv")

n_recordings <- nrow(recordings)

get_global_recordings_by_subset_size <- function(subset_size, confidence_levels, n_samples_per_subset_size) {
  current_subset_global_detection_results <- data.frame()
  
  for (k in 1:n_samples_per_subset_size) { 
    recording_sample <- sample(1:n_recordings, subset_size)
    
    temp_BirdNET_precision_results <- BirdNET_precision_results %>%
      filter(recording_id %in% recording_sample)
    
    temp_annotations <- annotations %>%
      filter(recording_id %in% recording_sample)
    
    total_birds_present <- temp_annotations %>%
      filter(sound_category == "Bird",
             species != "Unidentified bird",
             confidence_level == 1) %>%
      select(species) %>%
      unique()
    
    n_species_present <- nrow(total_birds_present)
    
    # Calculating the number of species potentially present in the current recording subset
    current_subset_species_list <- c()
    
    for(i in 1:length(recording_sample)) {
      current_recording_species_list_file_name <- gsub("labels.txt", "BirdNET_Analyzer_species_list.csv", (recording_info_extended %>%
                                                                                                             filter(recording_id == recording_sample[i]))$annotation_file_path[1])
      current_recording_species_list <- read.csv(paste0(recording_path, current_recording_species_list_file_name), header = FALSE)
      colnames(current_recording_species_list) <- c("species")
      current_recording_species_list <- tidyr::separate(data = current_recording_species_list, col = species, into = c("Scientific.name", "English.name"), sep = "_")
      
      current_subset_species_list <- c(current_subset_species_list, current_recording_species_list$Scientific.name)
    }
    
    n_species_potentially_present <- length(unique(current_subset_species_list))
    
    # Finding the number of species correctly and incorrectly identified by BirdNET in the current recording subset
    for (i in 1:length(confidence_levels)) {
      temp_global_detection_results <- temp_BirdNET_precision_results %>%
        filter(confidence >= confidence_levels[i]) %>%
        group_by(recording_id, species) %>%
        summarize(correct_identifications = sum(as.numeric(correct_identification)),
                  incorrect_identifications = sum(as.numeric(!correct_identification))) %>%
        mutate(correct_identifications = case_when(correct_identifications > 0 ~ 1,
                                                   TRUE ~ 0),
               incorrect_identifications = case_when(incorrect_identifications > 0 ~ 1,
                                                     TRUE ~ 0)) %>%
        group_by(species) %>%
        summarize(recordings_where_correctly_identified = sum(correct_identifications),
                  recordings_where_incorrectly_identified = sum(incorrect_identifications))
      
      # True positives at the dataset level
      n_species_correctly_detected <- temp_global_detection_results %>%
        filter(recordings_where_correctly_identified > 0) %>%
        select(species) %>%
        nrow()
      
      # False positives at the dataset level
      n_species_incorrectly_detected <- temp_global_detection_results %>%
        filter(recordings_where_incorrectly_identified > 0,
               !(species %in% total_birds_present$species)) %>%
        select(species) %>%
        nrow()
      
      # TPR = TP / (TP + FN)
      # TP + FN = All species actually present in the recording subset
      TPR <- n_species_correctly_detected / n_species_present
      
      # FPR = FP / (FP + TN)
      # FP + TN = All species potentially present but actually absent from the recording subset
      FPR <- n_species_incorrectly_detected / (n_species_potentially_present - n_species_present)
      
      precision <- n_species_correctly_detected / (n_species_correctly_detected + n_species_incorrectly_detected)
      F1 <- 2 * 1 * precision * TPR / (precision + TPR)
      
      current_subset_global_detection_results <- rbind(current_subset_global_detection_results, c(subset_size, paste(recording_sample,collapse=","), confidence_levels[i], n_species_correctly_detected, n_species_incorrectly_detected, n_species_present, n_species_potentially_present, TPR, FPR, precision, F1))
    }
  }
  colnames(current_subset_global_detection_results) <- c("n_recordings_sampled", "recording_ids", "minimum_confidence_threshold", "n_species_correctly_detected", "n_species_incorrectly_detected", "n_species_present", "n_species_potentially_present", "TPR", "FPR", "precision", "F1")
  
  return(current_subset_global_detection_results)
}

for (j in seq(10, 230, by = 10)) {
  print(paste0("Subset size: ", j))
  global_detection_results <- rbind(global_detection_results, get_global_recordings_by_subset_size(j, selected_confidence_levels, 200))
}

global_detection_results_by_all_conf_thresholds <- get_global_recordings_by_subset_size(n_recordings, all_confidence_levels, 1)


global_detection_results_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), "global_detection_results_by_recording_subset.csv")
write.csv(x=global_detection_results, file=global_detection_results_file_name, row.names = FALSE)

global_detection_results_by_all_conf_thresholds_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), "global_detection_results_by_all_thresholds.csv")
write.csv(x=global_detection_results_by_all_conf_thresholds, file=global_detection_results_by_all_conf_thresholds_file_name, row.names = FALSE)
