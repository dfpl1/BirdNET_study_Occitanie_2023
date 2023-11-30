source("./generate_recording_CSV.R")

annotations <- data.frame()

annotation_files <- list.files(pattern = "_labels.txt$", recursive = TRUE, include.dirs = TRUE)

n_files <- length(annotation_files)

for (j in 1:n_files) {
  # print(paste0("Processing annotations from file ", j, " out of ", n_files))
  file_name <- annotation_files[j]
  conn <- file(file_name, open="r")
  linn <-readLines(conn)
  
  for (i in seq(1, length(linn), 2)){
    line1 <- linn[i]
    line2 <- linn[i+1]
    
    recording_id <- j
    initial_time <- sub("\t.*", "", line1)
    final_time <- gsub(".*\t(.+)\t.*", "\\1", line1)
    
    label_parts <- strsplit(sub(".*\t", "", line1), split = " ")[[1]]
    if (length(label_parts) == 5) { #Bird
      label <- paste(label_parts[1], label_parts[2])
      sound_category <- "Bird"
      
      confidence_level <- label_parts[3]
      sound_type_code <- label_parts[4]
      code_n_ind <- label_parts[5]
      
    } else if (length(label_parts) == 4) { #Wild species
      label <- paste(label_parts[1], label_parts[2])
      sound_category <- case_when(label == "Ort sp." ~ "Orthoptera",
                              label %in% c("Hyla meridionalis", "Pelophylax sp.", "Anoure sp.") ~ "Frog",
                              label == "Marmotta marmotta" ~ "Mammal",
                              label == "Tibicina haematodes" ~ "Cicada",
                              label == "Oiseau sp." ~ "Bird",
                              TRUE ~ "?")
      
      confidence_level <- label_parts[3]
      sound_type_code <- case_when(sound_category %in% c("Orthoptera", "Frog", "Cicada") ~ 1,
                                   sound_category == "Mammal" ~ 2,
                                   TRUE ~ 0)
      code_n_ind <- label_parts[4]
      
    } else if (length(label_parts) == 2) { #Domestic species
      label <- paste(label_parts[1], label_parts[2])
      sound_category <- case_when(label != "Non identifie" ~ "Domestic animal",
                              TRUE ~ "Noise")
      confidence_level <- 1
      sound_type_code <- 0
      code_n_ind <- 0
      
    } else { #Noise
      label <- label_parts[1]
      sound_category <- "Noise"
      
      confidence_level <- 1
      sound_type_code <- 0
      code_n_ind <- 0
    }
    
    min_frequency <- gsub(".*\t(.+)\t.*", "\\1", line2)
    max_frequency <- sub(".*\t", "", line2)
    
    sound_type <- case_when(sound_type_code == 0 ~ "Unknown",
                            sound_type_code == 1 ~ "Song",
                            sound_type_code == 2 ~ "Call",
                            sound_type_code == 3 ~ "Flight call",
                            sound_type_code == 4 ~ "Drumming",
                            sound_type_code == 5 ~ "Flapping",
                            TRUE ~ "")
    
    annotations <- rbind(annotations, c(recording_id, label, sound_category, sound_type, initial_time, final_time, min_frequency, max_frequency, confidence_level, code_n_ind))
  }
  
  close(conn)
}

colnames(annotations) <- c("recording_id", "label", "sound_category", "sound_type", "initial_time", "final_time", "min_frequency", "max_frequency", "confidence_level", "code_n_ind")

annotations <- annotations %>%
  mutate(recording_id = as.numeric(recording_id),
         initial_time = as.double(initial_time),
         final_time = as.double(final_time),
         min_frequency = as.double(min_frequency),
         max_frequency = as.double(max_frequency),
         confidence_level = as.integer(confidence_level),
         code_n_ind = as.integer(code_n_ind))

species_labels <- read.csv2("../../CSVs/species_labels.csv", sep = ",")

annotations <- annotations %>%
  left_join(species_labels, 
            by = "label") %>% 
  mutate(species = case_when(sound_category %in% c("Domestic animal", "Frog", "Mammal", "Cicada") ~ label,
                             sound_category == "Orthoptera" & label == "Ort sp." ~ "Orthoptera sp.",
                             sound_category == "Noise" ~ "-",
                             TRUE ~ species),
         final_time = case_when(final_time <= 60 ~ final_time,
                                TRUE ~ 60))

# Summarizing annotations in each 3-second audio fragment
annotations_by_audio_fragment <- data.frame(matrix(ncol = 6, nrow = 0))
recording_ids <- unique(annotations$recording_id)

start_times <- 0:57
end_times <- 3:60

for (i in 1:length(recording_ids)) {
  current_recording_annotations <- annotations %>%
    filter(recording_id == recording_ids[i],
           confidence_level == 1,
           label != "Oiseau sp.",
           sound_category == "Bird")
  
  for (j in 1:length(start_times)) {
    annotations_in_current_chunk <- current_recording_annotations %>%
                                             filter(initial_time <= end_times[j],
                                                    final_time >= start_times[j]) %>%
                                             group_by(recording_id, species, final_time, initial_time) %>%
                                             summarize(total_time = round(min(final_time, end_times[j]) - max(initial_time, start_times[j]), 2),
                                                       min_code_n_ind = min(as.numeric(code_n_ind)))
    
    if (nrow(annotations_in_current_chunk) > 0) {
      for (k in 1:nrow(annotations_in_current_chunk)) {
        annotations_by_audio_fragment <- rbind(annotations_by_audio_fragment, c(recording_ids[i], start_times[j], end_times[j], annotations_in_current_chunk$species[k], annotations_in_current_chunk$total_time[k], annotations_in_current_chunk$min_code_n_ind[k]))
      }
    }
  }
}

colnames(annotations_by_audio_fragment) <- c("recording_id", "initial_time", "final_time", "species", "total_time", "min_code_n_ind")


#Writing CSV files
annotations_file_name <- file.path("../../CSVs", "annotations.csv")
write.csv(x=annotations, file=annotations_file_name, row.names = FALSE)

annotations_by_audio_fragment_file_name <- file.path("../../CSVs", "annotations_by_audio_fragment.csv")
write.csv(x=annotations_by_audio_fragment, file=annotations_by_audio_fragment_file_name, row.names = FALSE)

#Removing unneeded variables
rm(conn, file_name, final_frequency, final_time, i, initial_frequency, initial_time, j, label, label_parts, line1, line2, linn, max_frequency, min_frequency, confidence_level, code_n_ind, recording_id, sound_category, sound_type, sound_type_code)
