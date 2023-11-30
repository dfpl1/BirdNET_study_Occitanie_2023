source("./generate_recording_CSV.R")
source("../../R scripts/File_selection.R")

CSV_file_path <- paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text)
  
annotations_by_audio_fragment <- read.csv("../../CSVs/annotations_by_audio_fragment.csv")
BirdNET_precision_results <- read.csv(paste0(CSV_file_path, "/", algorithm, "_precision_results.csv"))

annotations_by_audio_fragment$correctly_identified <- NA

n_annotations <- nrow(annotations_by_audio_fragment)

for (i in 1:n_annotations) {
  print(paste("Processing annotation", i, "out of", n_annotations))
  
  BirdNET_identification <- BirdNET_precision_results %>%
    filter(recording_id == annotations_by_audio_fragment$recording_id[i],
           initial_time == annotations_by_audio_fragment$initial_time[i],
           final_time == annotations_by_audio_fragment$final_time[i],
           species == annotations_by_audio_fragment$species[i],
           correct_identification == TRUE)
  
  annotations_by_audio_fragment$correctly_identified[i] <- (nrow(BirdNET_identification) > 0)
}

#Writing CSV file
BirdNET_recall_by_audio_fragment_file_name <- file.path(CSV_file_path, paste0(algorithm, "_recall_by_audio_fragment.csv"))
write.csv(x=annotations_by_audio_fragment, file=BirdNET_recall_by_audio_fragment_file_name, row.names = FALSE)
