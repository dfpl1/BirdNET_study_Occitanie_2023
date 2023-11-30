library(tuneR)
library(soundecology)
library(dplyr)
library(tidyr)
source("./Noise measurement (NDSI)/Soundscape-analysis-with-R-master/AcouIndexAlpha.r")

setwd("../Recordings/Selected")

current_analysis <- "1 minute"
# current_analysis <- "2 hours"

if (current_analysis == "1 minute") {
  files <- list.files(pattern = "\\.wav$", recursive = TRUE, include.dirs = TRUE)
  files <- files[!grepl("filtered", files)]
} else {
  hard_drive_path <- "/media/david/One Touch/PSI-BIOM/Travail/Enregistrements analyses BirdNET/"
  files <- list.files(path = hard_drive_path, pattern = "\\.wav$", recursive = TRUE, include.dirs = TRUE)
  files <- files[!grepl("filtered", files)]
  files <- files[!grepl("Recordings_to_rename", files)]
  files <- files[!grepl("Sites démonstrateurs", files)]
  files <- paste0(hard_drive_path, files)
}

NDSI_indices <- NULL

for (i in 1:length(files)) {
  print(files[i])
  
  if (current_analysis == "1 minute") {
    # Only the first minute is analysed, corresponding to the part of the recording where annotations have been made.
    wave <- readWave(files[i], from = 1, to = 60, 
                     units = "seconds")
  } else {
    wave <- readWave(files[i], from = 1, to = 1799, 
                     units = "seconds")
  }
  
  # We changed anthro_min for 0 instead of 1000 so as to include all non-biological sounds (not only anthropogenic ones but also geophonic ones)
  Result <- AcouIndexAlpha(wave, stereo=FALSE, min_freq = 2000, max_freq = 10000, anthro_min = 0, anthro_max = 2000, bio_min=2000,
                         bio_max=12000, wl=512, j=5, AcouOccupancy=TRUE, Bioac=TRUE, Hf=TRUE, Ht=TRUE, H=TRUE, ACI=TRUE, AEI_villa=TRUE, M=TRUE, NDSI=TRUE,
                         ADI=TRUE, NP=TRUE)
  
  NDSI_indices <- rbind(NDSI_indices, Result$Mono_left)
}

rownames(NDSI_indices) <- files

if (current_analysis == "1 minute") {
  recordings <- read.csv("../../CSVs/recordings.csv")
} else{
  recordings_extended_info <- read.csv("../../CSVs/recordings_extended.csv")
  recordings <- recordings_extended_info %>%
    rowwise() %>%
    mutate(recording_folder = gsub(basename(recording_file_path), "", recording_file_path),
           recording_list = paste(recording_file_path, second_recording, third_recording, fourth_recording, sep = paste0(", ", recording_folder)),
           recording_file_path = strsplit(as.character(recording_list), ", ")) %>% 
    unnest(recording_file_path) %>%
    select(recording_id, recording_file_path)
}

acoustic_data <- cbind(NDSI_indices, files)
colnames(acoustic_data) <- c(colnames(NDSI_indices), "recording_file_path")
acoustic_data <- acoustic_data %>%
  right_join(recordings %>%
              select(recording_id, recording_file_path),
            by = "recording_file_path")


#Writing CSV file
if (current_analysis == "1 minute") {
  acoustic_data_file_name <- file.path("../../CSVs", "recordings_acoustic_data.csv")
} else {
  acoustic_data_file_name <- file.path("../../CSVs", "2h-recordings_acoustic_data.csv")
}
write.csv(x=acoustic_data, file=acoustic_data_file_name, row.names = FALSE)
