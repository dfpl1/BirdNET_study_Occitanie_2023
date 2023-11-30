library(dplyr)
library(lubridate)
source("./Algorithm_version_selection.R")

recordings_extended_info <- read.csv("../CSVs/recordings_extended.csv")
annotations <- read.csv("../CSVs/annotations.csv")
global_species_list <- read.csv("../CSVs/global_species_list.csv")

BirdNET_recordings_file_path <- "/media/david/One Touch/PSI-BIOM/Travail/Enregistrements analyses BirdNET/"

species_annotated_per_recording <- annotations %>%
  filter(confidence_level == 1,
         sound_category == "Bird",
         label != "Oiseau sp.") %>%
  select(recording_id, label) %>%
  unique() %>%
  group_by(recording_id) %>%
  summarize(n_species = n(),
            species_list = paste(label, collapse = ", "))

# Adding 0s to recordings without any bird detection
for (i in 1:nrow(recordings_extended_info)) {
  current_recording_id <- (recordings_extended_info$recording_id)[i]
  if (nrow(species_annotated_per_recording %>%
           filter(recording_id == current_recording_id)) == 0) {
    new_row <- data.frame(current_recording_id, 0, "")
    names(new_row) <- c("recording_id", "n_species", "species_list")
    species_annotated_per_recording <- rbind(species_annotated_per_recording,
                                                  new_row)
  }
}

confidence_thresholds <- c(0.75, 0.8, 0.85, 0.9, 0.95)

n_recordings <- nrow(recordings_extended_info)
BirdNET_CSV_suffix <- gsub("_.csv", ".csv",
                           gsub("__", "_", 
                                gsub("O", "o",
                                     gsub("/", "",
                                          paste("_BirdNET", BirdNET_version, "_results", overlap_text, ds_text, filtered_text, ".csv", sep = "_")))))

synthesized_BirdNET_results <- data.frame()
BirdNET_2h_analysis_detailed_results <- data.frame()

# Writing annotation files with BirdNET identifications with confidence levels >= 0.75 and grouping results by recording and species
for (i in 1:n_recordings) {
  print(paste0("Processing recording ", i, " out of ", n_recordings))
  current_BirdNET_results_file_names <- c(gsub(".wav", BirdNET_CSV_suffix, recordings_extended_info$hard_drive_recording_file_path[i]),
                                          gsub(".wav", BirdNET_CSV_suffix, recordings_extended_info$hard_drive_second_recording_file_path[i]),
                                          gsub(".wav", BirdNET_CSV_suffix, recordings_extended_info$hard_drive_third_recording_file_path[i]),
                                          gsub(".wav", BirdNET_CSV_suffix, recordings_extended_info$hard_drive_fourth_recording_file_path[i])
                                          )
  
  current_BirdNET_results <- data.frame()
  
  # Considering BirdNET results from all four recordings linked together
  for (j in 1:length(current_BirdNET_results_file_names)) {
    current_recording_BirdNET_results <- read.csv2(current_BirdNET_results_file_names[j], sep = "\t") %>%
      rename("begin_time" = "Begin.Time..s.",
             "end_time" = "End.Time..s.") %>%
      mutate(recording_name = gsub(BirdNET_CSV_suffix, ".wav", gsub(BirdNET_recordings_file_path, "", current_BirdNET_results_file_names[j])))
      
    current_BirdNET_results <- rbind(current_BirdNET_results, current_recording_BirdNET_results)
                                   
    # --------------Writing an annotation file with the most confident BirdNET identifications------------
    current_recording_BirdNET_annotations <- current_recording_BirdNET_results %>%
      filter(Confidence >= confidence_thresholds[1]) %>%
      mutate(label = paste(Common.Name, Confidence)) %>%
      select(begin_time, end_time, label)
    
    current_BirdNET_2h_analysis_detailed_results <- current_recording_BirdNET_results %>%
      filter(Confidence >= confidence_thresholds[1]) %>%
      mutate(recording_id = recordings_extended_info$recording_id[i],
             recording_name = gsub(BirdNET_CSV_suffix, ".wav", basename(current_BirdNET_results_file_names[j])))
    
    BirdNET_2h_analysis_detailed_results <- rbind(BirdNET_2h_analysis_detailed_results, current_BirdNET_2h_analysis_detailed_results)
    
    # Removing unnecessary tabs and spaces
    annotation_string <- gsub("\\\n ", "\\\n", 
                              gsub("\\\t\\\n", "\\\n", 
                                   gsub("\\\t ", "\\\t", 
                                        paste(apply(t(current_recording_BirdNET_annotations), 2, paste, collapse = '\t'), collapse = '\n'))))
    
    annotation_string_array <- sapply(strsplit(annotation_string, " "), function(x){x[!x ==""]})
    annotation_string <- paste(annotation_string_array, collapse = " ")
    
    cat(annotation_string, file = gsub(BirdNET_CSV_suffix, "_highest_confidence_BirdNET_detections.txt", current_BirdNET_results_file_names[j]))
    
  }
  
  current_BirdNET_results_1st_threshold <- current_BirdNET_results %>%
    filter(Confidence >= confidence_thresholds[1]) %>%
    mutate(begin_time = as.double(begin_time),
           begin_time_in_min = seconds_to_period(begin_time)) %>%
    unique()
  
  current_synthesized_BirdNET_results <- current_BirdNET_results_1st_threshold %>%
    left_join(current_BirdNET_results_1st_threshold %>%
                group_by(Common.Name) %>%
                summarize(max_conf = max(Confidence)) %>%
                mutate(species_num_in_recording = row_number()),
              by = "Common.Name") %>%
    left_join(current_BirdNET_results_1st_threshold %>%
                group_by(Common.Name, recording_name) %>%
                summarize(start_times = paste(begin_time_in_min, collapse = ", "),
                          first_detection_with_1st_threshold_confidence = min(begin_time),
                          first_detection_with_2nd_threshold_confidence = min(case_when(Confidence >= confidence_thresholds[2] ~ begin_time,
                                                                                        TRUE ~ 100000)),
                          first_detection_with_3rd_threshold_confidence = min(case_when(Confidence >= confidence_thresholds[3] ~ begin_time,
                                                                                        TRUE ~ 100000)),
                          first_detection_with_4th_threshold_confidence = min(case_when(Confidence >= confidence_thresholds[4] ~ begin_time,
                                                                                        TRUE ~ 100000)),
                          first_detection_with_5th_threshold_confidence = min(case_when(Confidence >= confidence_thresholds[5] ~ begin_time,
                                                                                        TRUE ~ 100000))) %>%
                select(Common.Name, recording_name, start_times, first_detection_with_1st_threshold_confidence, first_detection_with_2nd_threshold_confidence, first_detection_with_3rd_threshold_confidence, first_detection_with_4th_threshold_confidence, first_detection_with_5th_threshold_confidence),
              by = c("Common.Name", "recording_name")) %>%
    arrange(Common.Name, start_times) %>%
    mutate(recording_id = recordings_extended_info$recording_id[i],
           confidence_higher_than = case_when(max_conf >= confidence_thresholds[5] ~ confidence_thresholds[5],
                                              max_conf >= confidence_thresholds[4] ~ confidence_thresholds[4],
                                              max_conf >= confidence_thresholds[3] ~ confidence_thresholds[3],
                                              max_conf >= confidence_thresholds[2] ~ confidence_thresholds[2],
                                              TRUE ~ confidence_thresholds[1]),
           correct_identification = "?") %>%
    left_join(species_annotated_per_recording %>%
                select(recording_id, n_species, species_list),
              by = "recording_id") %>%
    rename("common_name" = "Common.Name",
           "n_species_target" = "n_species",
           "annotated_species_list" = "species_list") %>%
    select(correct_identification, recording_id, recording_name, common_name, species_num_in_recording, confidence_higher_than, max_conf, n_species_target, start_times, annotated_species_list, first_detection_with_1st_threshold_confidence, first_detection_with_2nd_threshold_confidence, first_detection_with_3rd_threshold_confidence, first_detection_with_4th_threshold_confidence, first_detection_with_5th_threshold_confidence) %>%
    unique()
  
  current_synthesized_BirdNET_results["first_detection_with_2nd_threshold_confidence"][current_synthesized_BirdNET_results["first_detection_with_2nd_threshold_confidence"] == 100000] <- as.numeric(NA)
  current_synthesized_BirdNET_results["first_detection_with_3rd_threshold_confidence"][current_synthesized_BirdNET_results["first_detection_with_3rd_threshold_confidence"] == 100000] <- as.numeric(NA)
  current_synthesized_BirdNET_results["first_detection_with_4th_threshold_confidence"][current_synthesized_BirdNET_results["first_detection_with_4th_threshold_confidence"] == 100000] <- as.numeric(NA)
  current_synthesized_BirdNET_results["first_detection_with_5th_threshold_confidence"][current_synthesized_BirdNET_results["first_detection_with_5th_threshold_confidence"] == 100000] <- as.numeric(NA)
  
  synthesized_BirdNET_results <- rbind(synthesized_BirdNET_results, current_synthesized_BirdNET_results)
}

synthesized_BirdNET_results["n_species_target"][is.na(synthesized_BirdNET_results["n_species_target"])] <- 0
synthesized_BirdNET_results["annotated_species_list"][is.na(synthesized_BirdNET_results["annotated_species_list"])] <- ""

synthesized_BirdNET_results <- synthesized_BirdNET_results %>%
  arrange(recording_id, recording_name, common_name)

# Keeping detailed results in order to study the precision in BirdNET identifications later on
BirdNET_2h_analysis_detailed_results <- BirdNET_2h_analysis_detailed_results %>%
  rename("confidence" = "Confidence",
         "English_name" = "Common.Name") %>%
  left_join(global_species_list,
            by = "English_name") %>%
  rename("species" = "Scientific_name") %>%
  select(recording_id, recording_name, begin_time, end_time, species, confidence)

# Writing CSV files
synthesized_BirdNET_results_file_name <- file.path(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/synthesized_BirdNET_results.csv"))
write.csv(x=synthesized_BirdNET_results, file=synthesized_BirdNET_results_file_name, row.names = FALSE)

BirdNET_2h_analysis_detailed_results_file_name <- file.path(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/BirdNET_2h_analysis_detailed_results.csv"))
write.csv(x=BirdNET_2h_analysis_detailed_results, file=BirdNET_2h_analysis_detailed_results_file_name, row.names = FALSE)
