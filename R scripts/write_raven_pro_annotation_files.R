library(dplyr)
library(tuneR)

recording_path <- "../Recordings/Selected/"

annotations <- read.csv("../CSVs/annotations.csv")
bird_annotations <- annotations %>%
  mutate(species = case_when(confidence_level == 1 ~ species,
                             TRUE ~ "Unidentified bird")) %>%
  filter(sound_category == "Bird")

recording_ids <- unique(annotations$recording_id)

recordings_extended <- read.csv("../CSVs/recordings_extended.csv")

current_annotations <- data.frame()

# Considering BirdNET results from all four recordings linked together
for (i in 1:length(recording_ids)) {
  file_path <- recordings_extended[i,]$recording_file_path
  wav_file <- paste0(recording_path, file_path)
  sound <- readWave(wav_file)
  max_recording_frequency <- sound@samp.rate / 2
  
  current_max_freq_annotations <- bird_annotations %>%
    filter(recording_id == i,
           max_frequency == -1)
  
  # if (nrow(current_max_freq_annotations) > 0) {
  #   print(current_max_freq_annotations)
  # }
  
  current_annotations <- bird_annotations %>%
    filter(recording_id == i) %>%
    mutate(selection = row_number(),
           view = "Spectrogram 1",
           channel = 1,
           min_frequency = round(min_frequency, 1),
           max_frequency = case_when(max_frequency == -1 ~ max_recording_frequency,
                                     TRUE ~ round(max_frequency, 1))) %>%
    select(selection, view, channel, initial_time, final_time, min_frequency, max_frequency, species)
  
  # --------------Writing a Raven Pro annotation file from an Audacity annotation file------------
  # Removing unnecessary tabs and spaces
  annotation_string <- gsub("\\\n ", "\\\n", 
                            gsub("\\\t\\\n", "\\\n", 
                                     gsub("\\\t ", "\\\t", 
                                          paste0("Selection	View	Channel	Begin Time (s)	End Time (s)	Low Freq (Hz)	High Freq (Hz)	Species\n",
                                                 paste(apply(t(current_annotations), 2, paste, collapse = '\t'), collapse = '\n')))))
  
  annotation_string_array <- sapply(strsplit(annotation_string, " "), function(x){x[!x ==""]})
  annotation_string <- paste(annotation_string_array, collapse = " ")
  
  annotation_file_name <- paste0(recording_path, gsub(".txt", "_Raven_Pro.txt", (recordings_extended %>%
    filter(recording_id == i))$annotation_file_path))
  
  cat(annotation_string, file = annotation_file_name)
}