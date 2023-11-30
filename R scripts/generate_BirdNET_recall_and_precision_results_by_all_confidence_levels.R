source("./Annotation_analysis.R")
setwd("../Recordings/Selected/")
source("../../R scripts/File_selection.R")

# The default confidence threshold is already 0.1
conf_thresholds <- c(10:99)/100

species_list <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(species_list) <- c("Scientific_name", "English_name")

species_list <- species_list %>%
  mutate(Scientific_name = as.character(Scientific_name),
         English_name = as.character(English_name))

bird_annotations <- annotations %>%
  filter(sound_category == "Bird",
         confidence_level == 1,
         label != "Oiseau sp.") %>%
  select(-sound_category, -confidence_level, -label)

bird_annotations_by_recording <- bird_annotations %>%
  group_by(recording_id, species) %>%
  mutate(initial_times = paste0(initial_time, collapse = ", "),
         final_times  = paste0(final_time, collapse = ", "),
         min_frequencies  = paste0(min_frequency, collapse = ", "),
         max_frequencies  = paste0(max_frequency, collapse = ", "),
         code_n_inds  = paste0(code_n_ind, collapse = ", ")) %>%
  select(-initial_time, -final_time, -min_frequency, -max_frequency, -code_n_ind) %>%
  unique()


for (m in 1:length(conf_thresholds)) {
  print(paste0("Confidence threshold: ", conf_thresholds[m]))
  BirdNET_recall_results <- data.frame(matrix(ncol = 10, nrow = 0))
  BirdNET_precision_results <- data.frame(matrix(ncol = 6, nrow = 0))
  PR_results_by_species_recording_and_confidence <- data.frame(matrix(ncol = 7, nrow = 0))
  
  for (j in 1:length(files)) {
    
    temp_species_list <- read.csv2(species_list_files[j], sep="_", header = FALSE)
    colnames(temp_species_list) <- c("Scientific_name", "English_name")
    species_list <- unique(union_all(species_list, temp_species_list))
    
    if(BirdNET_version == "Lite") {
      # It eliminates the detections of species not present in France.
      BirdNET_results <- read.csv2(files[j]) %>%
        mutate(start_time = as.numeric(Start..s.),
               end_time = as.numeric(End..s.)) %>%
        select(-Start..s., -End..s.) %>%
        filter(end_time <= 60,
               as.numeric(Confidence) >= conf_thresholds[m],
               Scientific.name %in% temp_species_list$Scientific_name)
    
      temp_species_list <- temp_species_list %>%
        select(-English_name)
      
    } else {
      BirdNET_results <- read.csv2(files[j], sep="\t") %>%
        mutate(start_time = as.numeric(Begin.Time..s.),
               end_time = as.numeric(End.Time..s.))
      
      if (dim(BirdNET_results)[1] > 0) {
        BirdNET_results <- BirdNET_results %>%
        left_join(temp_species_list %>%
                    select(English_name, Scientific_name),
                  by = c("Common.Name" = "English_name")) %>%
        select(-Begin.Time..s., -End.Time..s., -Common.Name) %>%
        rename("Scientific.name" = "Scientific_name") %>%
        filter(end_time <= 60,
               as.numeric(Confidence) >= conf_thresholds[m])
      }
    }
    
    recording_id <- j
    
    if (nrow(BirdNET_results) > 0) {
      for (i in 1:nrow(BirdNET_results)){
        line <- BirdNET_results[i,]
        
        annotations_in_time_frame <- bird_annotations %>%
          filter(recording_id == j,
                 initial_time < as.numeric(line["end_time"][[1]]), 
                 final_time > as.numeric(line["start_time"][[1]]),
                 species == line["Scientific.name"][[1]])
        
        initial_time <- as.numeric(line["start_time"][[1]])
        final_time <- as.numeric(line["end_time"][[1]])
        species <- line["Scientific.name"][[1]]
        confidence <- as.numeric(line["Confidence"][[1]])
        correct_identification <- case_when(nrow(annotations_in_time_frame) > 0 ~ TRUE,
                                            TRUE ~ FALSE)
        
        BirdNET_precision_results <- rbind(BirdNET_precision_results, c(recording_id, initial_time, final_time, species, confidence, correct_identification))
      }
    }
    
    annotation_results <- bird_annotations_by_recording %>%
      filter(recording_id == j)
  
    if (nrow(annotation_results) > 0) {
      for (i in 1:nrow(annotation_results)) {
        line <- annotation_results[i,]
        
        initial_times <- as.numeric(strsplit(line["initial_times"][[1]], split = ", ")[[1]])
        final_times <- as.numeric(strsplit(line["final_times"][[1]], split = ", ")[[1]])
        min_frequencies <- as.numeric(strsplit(line["min_frequencies"][[1]], split = ", ")[[1]])
        max_frequencies <- as.numeric(strsplit(line["max_frequencies"][[1]], split = ", ")[[1]])
        code_n_inds <- as.numeric(strsplit(line["code_n_inds"][[1]], split = ", ")[[1]])
        species <- line["species"][[1]]
        
        BirdNET_confidence <- NA
  
        total_time <- 0
        min_frequency <-100000 
        max_frequency <- 0
        min_code_n_ind <- 4
        confidences <- c()
        total_correct_detections <- 0
        total_time_detected <- 0
        
        for (k in 1:length(initial_times)) {
          if (nrow(BirdNET_results) > 0) {
            BirdNET_results_in_time_frame <- BirdNET_results %>%
              filter(start_time < final_times[k],
                     end_time > initial_times[k],
                     Scientific.name == species)
            
            if (nrow(BirdNET_results_in_time_frame) > 0) {
              for (l in 1:nrow(BirdNET_results_in_time_frame)) {
                current_confidence <- as.numeric(BirdNET_results_in_time_frame["Confidence"][l,])
                
                if (current_confidence >= conf_thresholds[m]) {
                  confidences <- append(confidences, current_confidence)
                  total_correct_detections <- total_correct_detections + 1
                  total_time_detected <- total_time_detected + min(as.numeric(BirdNET_results_in_time_frame["end_time"][l,]), final_times[k]) - max(as.numeric(BirdNET_results_in_time_frame["start_time"][l,]), initial_times[k])
                }
              }
            }
            
            total_time <- total_time + final_times[k] - initial_times[k]
            min_frequency <- min(min_frequency, min_frequencies[k])
            max_frequency <- max(max_frequency, max_frequencies[k])
            min_code_n_ind <- min(min_code_n_ind, code_n_inds[k])
          }
        }
        
        detected_by_BirdNET <- total_correct_detections > 0
        mean_BirdNET_confidence <- mean(confidences)
        
        proportion_of_time_detected <- total_time_detected / total_time
    
        BirdNET_recall_results <- rbind(BirdNET_recall_results, c(recording_id, total_time, species, min_frequency, max_frequency, min_code_n_ind, detected_by_BirdNET, total_correct_detections, proportion_of_time_detected, mean_BirdNET_confidence))
      }
    }
    
    colnames(BirdNET_precision_results) <- c("recording_id", "initial_time", "final_time", "species", "confidence", "correct_identification")
    colnames(BirdNET_recall_results) <- c("recording_id", "total_time", "species", "min_frequency", "max_frequency", "min_code_n_ind", paste0("detected_by_", algorithm), "total_detections", "proportion_of_time_detected", paste0("mean_", algorithm, "_confidence"))
    
    temp_recording_id <- recording_id
    
    precision_results <- BirdNET_precision_results %>%
      filter(recording_id == temp_recording_id) %>%
      group_by(species, correct_identification) %>%
      summarize(mean_confidence = mean(as.double(confidence)))
    
    recall_results <- BirdNET_recall_results %>%
      filter(recording_id == temp_recording_id)
    
    annotated_species <- data.frame(BirdNET_recall_results$species)
    colnames(annotated_species) <- colnames(temp_species_list)[1]
    
    temp_species_list <- unique(union_all(temp_species_list %>%
                                            select(Scientific_name), annotated_species))
    

    for(i in 1:nrow(temp_species_list)) {

      current_species <- temp_species_list$Scientific_name[i]

      # True positive: the species was correctly detected at least once in the recording
      tp <- min(precision_results %>%
        filter(species == current_species,
               correct_identification == TRUE) %>%
        nrow(), 1)

      # False positive: the species was wrongly detected at least once in the recording and was not correctly detected at any moment in the recording.
      # Sometimes a species can be both correctly detected and incorrectly detected in the same recording. These cases will be considered true positives.
      fp <- case_when(tp == 1 ~ 0,
                      TRUE ~ min(precision_results %>%
                        filter(species == current_species,
                               correct_identification == FALSE) %>%
                        nrow(), 1))

      # True negative: the species was not present and was not detected
      tn <- as.numeric(!(current_species %in% precision_results$species) && !(current_species %in% recall_results$species))

      # False negative: the species was present but was not detected by BirdNET
      fn <- as.numeric(!(current_species %in% precision_results$species) && (current_species %in% recall_results$species))

      PR_results_by_species_recording_and_confidence <- rbind(PR_results_by_species_recording_and_confidence, c(conf_thresholds[m], recording_id, current_species, tp, fp, tn, fn))
    }

    colnames(PR_results_by_species_recording_and_confidence) <- c("confidence_threshold", "recording_id", "species", "true_positive", "false_positive", "true_negative", "false_negative")
  }
  
  PR_results_by_species_recording_and_confidence_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0("PR_results_by_confidence_threshold/", algorithm, "_PR_results_by_species_and_recording_with_conf_", as.character(conf_thresholds[m]), ".csv"))
  write.csv(x=PR_results_by_species_recording_and_confidence, file=PR_results_by_species_recording_and_confidence_file_name, row.names = FALSE)
  
  if (m == 1) {
    # Filtering out all species that can't be present in the study area (it only applies to the Google algorithm)
    BirdNET_precision_results <- BirdNET_precision_results %>%
      filter(!is.na(species))
    
    BirdNET_precision_results_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_precision_results.csv"))
    write.csv(x=BirdNET_precision_results, file=BirdNET_precision_results_file_name, row.names = FALSE)
  }
}

global_species_list_file_name <- file.path("../../CSVs/global_species_list.csv")
write.csv(x=species_list, file=global_species_list_file_name, row.names = FALSE)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
#Removing unneeded variables
rm(annotated_species, annotation_results, annotations_in_time_frame, BirdNET_precision_results_file_name, BirdNET_recall_results_file_name, BirdNET_results, BirdNET_results_in_time_frame, line, annotations_file_name, BirdNET_confidence, BirdNET_species_list, code_n_inds, confidence, confidences, correct_identification, current_confidence, current_species, detected_by_BirdNET, files, final_time, final_times, fn, fp, fpr, temp_species_list, i, initial_time, initial_times, j, k, l, m, max_frequencies, max_frequency, mean_BirdNET_confidence, min_code_n_ind, min_frequencies, min_frequency, PR_results_by_species_recording_and_confidence_file_name, precision, precision_results_by_recording, proportion_of_time_detected, recall, recording_id, recordings_extended_info_file_name, recordings_where_detected_or_present, species, species_list, temp_species_list, total_correct_detections, total_time, total_time_detected, tn, tp, tpr)


