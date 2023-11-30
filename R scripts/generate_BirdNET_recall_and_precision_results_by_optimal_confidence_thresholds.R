source("./Annotation_analysis.R")
setwd("../Recordings/Selected/")
source("../../R scripts/File_selection.R")

# Analyzing all annotations together or separating them by the number of birds vocalizing at the same time
differenciate_by_code_n_inds <- FALSE

if (differenciate_by_code_n_inds) {
  BirdNET_recall_results_by_n_inds_1st_threshold <- data.frame()
  BirdNET_recall_results_by_n_inds_2nd_threshold <- data.frame()
  BirdNET_recall_results_by_n_inds_3rd_threshold <- data.frame()
} else {
  BirdNET_recall_results_1st_threshold <- data.frame()
  BirdNET_recall_results_2nd_threshold <- data.frame()
  BirdNET_recall_results_3rd_threshold <- data.frame()
}

optimal_confidence_thresholds <- read.csv(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, "/", algorithm, "_optimal_confidence_thresholds.csv"))

first_threshold <- optimal_confidence_thresholds$conf_thresholds[1]
second_threshold <- optimal_confidence_thresholds$conf_thresholds[2]
third_threshold <- optimal_confidence_thresholds$conf_thresholds[3]

BirdNET_precision_results <- data.frame()

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

# Bird species potentially present in all recordings
global_species_list <- read.csv("../../CSVs/global_species_list.csv") %>%
  select(Scientific_name, English_name) %>%
  rename("species" = "Scientific_name")

for (j in 1:length(files)) {
  
  if(BirdNET_version == "Lite") {
    # It eliminates the detections of species not present in France.
    BirdNET_results <- read.csv2(files[j]) %>%
      mutate(start_time = as.numeric(Start..s.),
             end_time = as.numeric(End..s.)) %>%
      select(-Start..s., -End..s.) %>%
      filter(end_time <= 60,
             Scientific.name %in% global_species_list$species)
  } else {
    BirdNET_results <- read.csv2(files[j], sep="\t") %>%
      mutate(start_time = as.numeric(Begin.Time..s.),
             end_time = as.numeric(End.Time..s.))
    
    if (dim(BirdNET_results)[1] > 0) {
      BirdNET_results <- BirdNET_results %>%
        left_join(global_species_list %>%
                    select(English_name, species),
                  by = c("Common.Name" = "English_name")) %>%
        select(-Begin.Time..s., -End.Time..s., -Common.Name) %>%
        rename("Scientific.name" = "species") %>%
        filter(end_time <= 60)
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

      if (differenciate_by_code_n_inds) {
        total_times <- c()
        codes_n_ind <- c()
        
        total_times_detected_1st_threshold <- c()
        total_times_detected_2nd_threshold <- c()
        total_times_detected_3rd_threshold <- c()
        
        mean_confidences_1st_threshold <- c()
        mean_confidences_2nd_threshold <- c()
        mean_confidences_3rd_threshold <- c()
      } else {
        total_time <- 0
        min_frequency <-100000 
        max_frequency <- 0
        min_code_n_ind <- 4
      }
      
      confidences_1st_threshold <- c()
      total_correct_detections_1st_threshold <- 0
      total_time_detected_1st_threshold <- 0
      
      confidences_2nd_threshold <- c()
      total_correct_detections_2nd_threshold <- 0
      total_time_detected_2nd_threshold <- 0
      
      confidences_3rd_threshold <- c()
      total_correct_detections_3rd_threshold <- 0
      total_time_detected_3rd_threshold <- 0
      
      for (k in 1:length(initial_times)) {
        if (nrow(BirdNET_results) > 0) {
          BirdNET_results_in_time_frame <- BirdNET_results %>%
            filter(start_time < final_times[k],
                   end_time > initial_times[k],
                   Scientific.name == species)
          
          if (nrow(BirdNET_results_in_time_frame) > 0) {
            for (l in 1:nrow(BirdNET_results_in_time_frame)) {
              current_confidence <- as.numeric(BirdNET_results_in_time_frame["Confidence"][l,])
              
              if (current_confidence >= first_threshold) {
                confidences_1st_threshold <- append(confidences_1st_threshold, current_confidence)
                total_correct_detections_1st_threshold <- total_correct_detections_1st_threshold + 1
                total_time_detected_1st_threshold <- total_time_detected_1st_threshold + min(as.numeric(BirdNET_results_in_time_frame["end_time"][l,]), final_times[k]) - max(as.numeric(BirdNET_results_in_time_frame["start_time"][l,]), initial_times[k])
                
                if (current_confidence >= second_threshold) {
                  confidences_2nd_threshold <- append(confidences_2nd_threshold, current_confidence)
                  total_correct_detections_2nd_threshold <- total_correct_detections_2nd_threshold + 1
                  total_time_detected_2nd_threshold <- total_time_detected_2nd_threshold + min(as.numeric(BirdNET_results_in_time_frame["end_time"][l,]), final_times[k]) - max(as.numeric(BirdNET_results_in_time_frame["start_time"][l,]), initial_times[k])
                  
                  if (current_confidence >= third_threshold) {
                    confidences_3rd_threshold <- append(confidences_3rd_threshold, current_confidence)
                    total_correct_detections_3rd_threshold <- total_correct_detections_3rd_threshold + 1
                    total_time_detected_3rd_threshold <- total_time_detected_3rd_threshold + min(as.numeric(BirdNET_results_in_time_frame["end_time"][l,]), final_times[k]) - max(as.numeric(BirdNET_results_in_time_frame["start_time"][l,]), initial_times[k])
                  }
                }
              }
            }
          }
        }
        
        if (differenciate_by_code_n_inds) {
          total_times <- c(total_times, final_times[k] - initial_times[k])
          codes_n_ind <- c(codes_n_ind, code_n_inds[k])
          
          total_times_detected_1st_threshold <- c(total_times_detected_1st_threshold, total_time_detected_1st_threshold)
          total_time_detected_1st_threshold <- 0
          mean_confidences_1st_threshold <- c(mean_confidences_1st_threshold, mean(confidences_1st_threshold))
          confidences_1st_threshold <- c()
          
          total_times_detected_2nd_threshold <- c(total_times_detected_2nd_threshold, total_time_detected_2nd_threshold)
          total_time_detected_2nd_threshold <- 0
          mean_confidences_2nd_threshold <- c(mean_confidences_2nd_threshold, mean(confidences_2nd_threshold))
          confidences_2nd_threshold <- c()
          
          total_times_detected_3rd_threshold <- c(total_times_detected_3rd_threshold, total_time_detected_3rd_threshold)
          total_time_detected_3rd_threshold <- 0
          mean_confidences_3rd_threshold <- c(mean_confidences_3rd_threshold, mean(confidences_3rd_threshold))
          confidences_3rd_threshold <- c()
        } else {
          total_time <- total_time + final_times[k] - initial_times[k]
          min_frequency <- min(min_frequency, min_frequencies[k])
          max_frequency <- max(max_frequency, max_frequencies[k])
          min_code_n_ind <- min(min_code_n_ind, code_n_inds[k])
        }
      }
      
      if (differenciate_by_code_n_inds) {
        for(n in 1:length(total_times)) {
          BirdNET_recall_results_by_n_inds_1st_threshold <- rbind(BirdNET_recall_results_by_n_inds_1st_threshold, c(recording_id, total_times[n], species, codes_n_ind[n], total_times_detected_1st_threshold[n], mean_confidences_1st_threshold[n]))
        }
      } else {
        detected_by_BirdNET_1st_threshold <- total_correct_detections_1st_threshold > 0
        
        if (detected_by_BirdNET_1st_threshold) {
          mean_BirdNET_confidence_1st_threshold <- mean(confidences_1st_threshold)
        } else {
          mean_BirdNET_confidence_1st_threshold <- 0
        }
        
        if (total_time_detected_1st_threshold > total_time) {
          total_time_detected_1st_threshold <- total_time
        }
        
        proportion_of_time_detected_1st_threshold <- total_time_detected_1st_threshold / total_time
        
        BirdNET_recall_results_1st_threshold <- rbind(BirdNET_recall_results_1st_threshold, c(recording_id, total_time, species, min_frequency, max_frequency, min_code_n_ind, detected_by_BirdNET_1st_threshold, total_correct_detections_1st_threshold, proportion_of_time_detected_1st_threshold, mean_BirdNET_confidence_1st_threshold))
      }
      
      if (differenciate_by_code_n_inds) {
        for(n in 1:length(total_times)) {
          BirdNET_recall_results_by_n_inds_2nd_threshold <- rbind(BirdNET_recall_results_by_n_inds_2nd_threshold, c(recording_id, total_times[n], species, codes_n_ind[n], total_times_detected_2nd_threshold[n], mean_confidences_2nd_threshold[n]))
        }
      } else {
        detected_by_BirdNET_2nd_threshold <- total_correct_detections_2nd_threshold > 0
        
        if (detected_by_BirdNET_2nd_threshold) {
          mean_BirdNET_confidence_2nd_threshold <- mean(confidences_2nd_threshold)
        } else {
          mean_BirdNET_confidence_2nd_threshold <- 0
        }
        
        if (total_time_detected_2nd_threshold > total_time) {
          total_time_detected_2nd_threshold <- total_time
        }
        
        proportion_of_time_detected_2nd_threshold <- total_time_detected_2nd_threshold / total_time
        
        BirdNET_recall_results_2nd_threshold <- rbind(BirdNET_recall_results_2nd_threshold, c(recording_id, total_time, species, min_frequency, max_frequency, min_code_n_ind, detected_by_BirdNET_2nd_threshold, total_correct_detections_2nd_threshold, proportion_of_time_detected_2nd_threshold, mean_BirdNET_confidence_2nd_threshold))
      }
      
      
      if (differenciate_by_code_n_inds) {
        for(n in 1:length(total_times)) {
          BirdNET_recall_results_by_n_inds_3rd_threshold <- rbind(BirdNET_recall_results_by_n_inds_3rd_threshold, c(recording_id, total_times[n], species, codes_n_ind[n], total_times_detected_3rd_threshold[n], mean_confidences_3rd_threshold[n]))
        }
      } else {
        detected_by_BirdNET_3rd_threshold <- total_correct_detections_3rd_threshold > 0
        
        if (detected_by_BirdNET_3rd_threshold) {
          mean_BirdNET_confidence_3rd_threshold <- mean(confidences_3rd_threshold)
        } else {
          mean_BirdNET_confidence_3rd_threshold <- 0
        }
        
        if (total_time_detected_3rd_threshold > total_time) {
          total_time_detected_3rd_threshold <- total_time
        }
        
        proportion_of_time_detected_3rd_threshold <- total_time_detected_3rd_threshold / total_time
        
        BirdNET_recall_results_3rd_threshold <- rbind(BirdNET_recall_results_3rd_threshold, c(recording_id, total_time, species, min_frequency, max_frequency, min_code_n_ind, detected_by_BirdNET_3rd_threshold, total_correct_detections_3rd_threshold, proportion_of_time_detected_3rd_threshold, mean_BirdNET_confidence_3rd_threshold))
      }
    }
  }
}

colnames(BirdNET_precision_results) <- c("recording_id", "initial_time", "final_time", "species", "confidence", "correct_identification")

BirdNET_precision_results_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_precision_results.csv"))
write.csv(x=BirdNET_precision_results, file=BirdNET_precision_results_file_name, row.names = FALSE)

if (differenciate_by_code_n_inds) {
  colnames(BirdNET_recall_results_by_n_inds_1st_threshold) <- c("recording_id", "total_time", "species", "code_n_ind", "total_time_detected", paste0(algorithm, "_confidence"))
  colnames(BirdNET_recall_results_by_n_inds_2nd_threshold) <- c("recording_id", "total_time", "species", "code_n_ind", "total_time_detected", paste0(algorithm, "_confidence"))
  colnames(BirdNET_recall_results_by_n_inds_3rd_threshold) <- c("recording_id", "total_time", "species", "code_n_ind", "total_time_detected", paste0(algorithm, "_confidence"))
  
  BirdNET_recall_results_by_n_inds_1st_threshold[is.na(BirdNET_recall_results_by_n_inds_1st_threshold)] <- 0
  BirdNET_recall_results_by_n_inds_2nd_threshold[is.na(BirdNET_recall_results_by_n_inds_2nd_threshold)] <- 0
  BirdNET_recall_results_by_n_inds_3rd_threshold[is.na(BirdNET_recall_results_by_n_inds_3rd_threshold)] <- 0
  
  BirdNET_recall_results_by_n_inds_1st_threshold <- BirdNET_recall_results_by_n_inds_1st_threshold %>%
    group_by(recording_id, species, code_n_ind) %>%
    summarize(total_time = sum(as.numeric(total_time)),
              total_time_detected = min(sum(as.numeric(total_time_detected), sum(as.numeric(total_time)))),
              BirdNET_confidence = mean(as.numeric(BirdNET_confidence))) %>%
    mutate(proportion_of_time_detected = total_time_detected / total_time)
  
  BirdNET_recall_results_by_n_inds_2nd_threshold <- BirdNET_recall_results_by_n_inds_2nd_threshold %>%
    group_by(recording_id, species, code_n_ind) %>%
    summarize(total_time = sum(as.numeric(total_time)),
              total_time_detected = min(sum(as.numeric(total_time_detected), sum(as.numeric(total_time)))),
              BirdNET_confidence = mean(as.numeric(BirdNET_confidence))) %>%
    mutate(proportion_of_time_detected = total_time_detected / total_time)
  
  BirdNET_recall_results_by_n_inds_3rd_threshold <- BirdNET_recall_results_by_n_inds_3rd_threshold %>%
    group_by(recording_id, species, code_n_ind) %>%
    summarize(total_time = sum(as.numeric(total_time)),
              total_time_detected = min(sum(as.numeric(total_time_detected), sum(as.numeric(total_time)))),
              BirdNET_confidence = mean(as.numeric(BirdNET_confidence))) %>%
    mutate(proportion_of_time_detected = total_time_detected / total_time)
  
  
  #Writing CSV files
  BirdNET_recall_results_by_n_inds_1st_threshold_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_recall_results_by_n_inds_", toString(first_threshold*100), ".csv"))
  write.csv(x=BirdNET_recall_results_by_n_inds_1st_threshold, file=BirdNET_recall_results_by_n_inds_1st_threshold_file_name, row.names = FALSE)
  
  BirdNET_recall_results_by_n_inds_2nd_threshold_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_recall_results_by_n_inds_", toString(second_threshold*100), ".csv"))
  write.csv(x=BirdNET_recall_results_by_n_inds_2nd_threshold, file=BirdNET_recall_results_by_n_inds_2nd_threshold_file_name, row.names = FALSE)
  
  BirdNET_recall_results_by_n_inds_3rd_threshold_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_recall_results_by_n_inds_", toString(third_threshold*100), ".csv"))
  write.csv(x=BirdNET_recall_results_by_n_inds_3rd_threshold, file=BirdNET_recall_results_by_n_inds_3rd_threshold_file_name, row.names = FALSE)
  
} else {
  colnames(BirdNET_recall_results_1st_threshold) <- c("recording_id", "total_time", "species", "min_frequency", "max_frequency", "min_code_n_ind", paste0("detected_by_", algorithm), "total_detections", "proportion_of_time_detected", paste0("mean_", algorithm, "_confidence"))
  colnames(BirdNET_recall_results_2nd_threshold) <- c("recording_id", "total_time", "species", "min_frequency", "max_frequency", "min_code_n_ind", paste0("detected_by_", algorithm), "total_detections", "proportion_of_time_detected", paste0("mean_", algorithm, "_confidence"))
  colnames(BirdNET_recall_results_3rd_threshold) <- c("recording_id", "total_time", "species", "min_frequency", "max_frequency", "min_code_n_ind", paste0("detected_by_", algorithm), "total_detections", "proportion_of_time_detected", paste0("mean_", algorithm, "_confidence"))

  #Writing CSV files
  BirdNET_recall_results_1st_threshold_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_recall_results_", toString(first_threshold*100), ".csv"))
  write.csv(x=BirdNET_recall_results_1st_threshold, file=BirdNET_recall_results_1st_threshold_file_name, row.names = FALSE)
  
  BirdNET_recall_results_2nd_threshold_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_recall_results_", toString(second_threshold*100), ".csv"))
  write.csv(x=BirdNET_recall_results_2nd_threshold, file=BirdNET_recall_results_2nd_threshold_file_name, row.names = FALSE)
  
  BirdNET_recall_results_3rd_threshold_file_name <- file.path(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_recall_results_", toString(third_threshold*100), ".csv"))
  write.csv(x=BirdNET_recall_results_3rd_threshold, file=BirdNET_recall_results_3rd_threshold_file_name, row.names = FALSE)
  
  #Removing unneeded variables
  rm(annotation_results, annotations_in_time_frame, BirdNET_precision_results_file_name, BirdNET_recall_results_1st_threshold_file_name, BirdNET_recall_results_2nd_threshold_file_name, BirdNET_recall_results_3rd_threshold_file_name, BirdNET_results, BirdNET_results_in_time_frame, line, annotations_file_name, BirdNET_confidence, code_n_inds, confidence, confidences_1st_threshold, confidences_2nd_threshold, confidences_3rd_threshold, correct_identification, current_confidence, detected_by_BirdNET_1st_threshold, detected_by_BirdNET_2nd_threshold, detected_by_BirdNET_3rd_threshold, files, final_time, final_times, i, initial_time, initial_times, j, k, max_frequencies, max_frequency, mean_BirdNET_confidence_1st_threshold, mean_BirdNET_confidence_2nd_threshold, mean_BirdNET_confidence_3rd_threshold, min_code_n_ind, min_frequencies, min_frequency, proportion_of_time_detected_1st_threshold, proportion_of_time_detected_2nd_threshold, proportion_of_time_detected_3rd_threshold, recording_id, recordings_extended_info_file_name, species, total_correct_detections_1st_threshold, total_correct_detections_2nd_threshold, total_correct_detections_3rd_threshold, total_time, total_time_detected_1st_threshold, total_time_detected_2nd_threshold, total_time_detected_3rd_threshold)
}



