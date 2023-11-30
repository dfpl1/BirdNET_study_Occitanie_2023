library(dplyr)
source("./generate_recording_CSV.R")

BirdNET_versions <- c("BirdNET_Analyzer", "BirdNET_Lite")
filter_options <- c("", "filtered")
overlap_options <- c("", "overlap_1s", "overlap_2s")
sensitivity_options <- c("ds05", "ds10", "ds15")

hard_drive_path <- "/media/david/One Touch/PSI-BIOM/Travail/Enregistrements analyses BirdNET/"

csv_files <- list.files(pattern = ".csv$", recursive = TRUE, include.dirs = TRUE)
hard_drive_csv_files <- paste0(hard_drive_path, list.files(path = hard_drive_path, pattern = ".csv$", recursive = TRUE, include.dirs = TRUE))

for (BirdNET_version in BirdNET_versions) {
  for (filter_option in filter_options) {
    for (overlap_option in overlap_options) {
      for (sensitivity_option in sensitivity_options) {
        local_recordings_yet_to_analyze <- recording_extended_info
        hard_drive_recordings_yet_to_analyze <- recording_extended_info
        
        for (i in 1:nrow(recording_extended_info)) {
          temp_file_name <- gsub("_\\.", "\\.", gsub("__", "_", paste(gsub(".wav", "", recording_extended_info$recording_file_path[i]), BirdNET_version, "results", overlap_option, sensitivity_option, ".csv", sep = "_")))
          filtered_csv_files <- csv_files[grepl(temp_file_name, csv_files)]
          local_recordings_yet_to_analyze$already_analyzed[i] <- length(filtered_csv_files) > 0 
          
          temp_first_file_name <- gsub("_\\.", "\\.", gsub("__", "_", paste(gsub(".wav", "", recording_extended_info$hard_drive_recording_file_path[i]), BirdNET_version, "results", overlap_option, sensitivity_option, ".csv", sep = "_")))
          temp_second_file_name <- gsub("_\\.", "\\.", gsub("__", "_", paste(gsub(".wav", "", recording_extended_info$hard_drive_second_recording_file_path[i]), BirdNET_version, "results", overlap_option, sensitivity_option, ".csv", sep = "_")))
          temp_third_file_name <- gsub("_\\.", "\\.", gsub("__", "_", paste(gsub(".wav", "", recording_extended_info$hard_drive_third_recording_file_path[i]), BirdNET_version, "results", overlap_option, sensitivity_option, ".csv", sep = "_")))
          temp_fourth_file_name <- gsub("_\\.", "\\.", gsub("__", "_", paste(gsub(".wav", "", recording_extended_info$hard_drive_fourth_recording_file_path[i]), BirdNET_version, "results", overlap_option, sensitivity_option, ".csv", sep = "_")))
          
          hard_drive_recordings_yet_to_analyze$already_analyzed[i] <- (temp_first_file_name %in% hard_drive_csv_files &
                                                                         temp_second_file_name %in% hard_drive_csv_files &
                                                                         temp_third_file_name %in% hard_drive_csv_files &
                                                                         temp_fourth_file_name %in% hard_drive_csv_files)
        }
        
        local_recordings_yet_to_analyze <- local_recordings_yet_to_analyze %>%
          filter(!already_analyzed) %>%
          select(-already_analyzed) 
        
        hard_drive_recordings_yet_to_analyze <- hard_drive_recordings_yet_to_analyze %>%
          filter(!already_analyzed) %>%
          select(-already_analyzed) 
        
        if (filter_option == "filtered" && nrow(local_recordings_yet_to_analyze) > 0) {
          recording_file_path_array <- strsplit(local_recordings_yet_to_analyze$recording_file_path, "/")
          annotation_file_path_array <- strsplit(local_recordings_yet_to_analyze$annotation_file_path, "/")
          
          for (i in 1:nrow(local_recordings_yet_to_analyze)) {
            local_recordings_yet_to_analyze$recording_file_path[i] <- paste(recording_file_path_array[[i]][1], recording_file_path_array[[i]][2], "Filtered", gsub(".wav", "_filtered.wav", recording_file_path_array[[i]][3]), sep = "/")
            local_recordings_yet_to_analyze$annotation_file_path[i] <- paste(annotation_file_path_array[[i]][1], annotation_file_path_array[[i]][2], "Filtered", gsub(".wav", "_filtered.wav", annotation_file_path_array[[i]][3]), sep = "/")
            local_recordings_yet_to_analyze$file_name[i] <- gsub(".wav", "_filtered.wav", local_recordings_yet_to_analyze$file_name[i])
            local_recordings_yet_to_analyze$local_directory[i] <- paste0(local_recordings_yet_to_analyze$local_directory[i], "/Filtered")
          }
        }
        
        # Writing CSV files
        local_recordings_yet_to_analyze_file_name <- file.path(paste0("../../CSVs/", BirdNET_version, "/", sensitivity_option, "/", gsub("filtered", "Filtered/", filter_option), gsub("overlap_1s", "Overlap_1s/", gsub("overlap_2s", "Overlap_2s/", overlap_option)), "recordings_yet_to_analyze.csv"))
        write.csv(x=local_recordings_yet_to_analyze, file=local_recordings_yet_to_analyze_file_name, row.names = FALSE)
        
        # Not processing filtered recordings on the hard drive
        if (filter_option != "filtered") {
          hard_drive_recordings_yet_to_analyze_file_name <- file.path(paste0("../../CSVs/", BirdNET_version, "/", sensitivity_option, "/", gsub("filtered", "Filtered/", filter_option), gsub("overlap_1s", "Overlap_1s/", gsub("overlap_2s", "Overlap_2s/", overlap_option)), "hard_drive_recordings_yet_to_analyze.csv"))
          write.csv(x=hard_drive_recordings_yet_to_analyze, file=hard_drive_recordings_yet_to_analyze_file_name, row.names = FALSE)
        }
      }
    }
  }
}
