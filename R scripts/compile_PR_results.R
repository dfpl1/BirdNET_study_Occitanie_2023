source("./Annotation_analysis.R")
setwd("../Recordings/Selected/")
source("../../R scripts/Algorithm_version_selection.R")

# Beta coefficients for the F-scores

# It weighs both precision and recall equally
Beta1 = 1

# It weighs precision higher than recall
Beta2 = 0.25

# It weighs precision much higher than recall
Beta3 = 0.1

PR_results_by_species_recording_and_confidence <- data.frame(matrix(ncol = 7, nrow = 0))

setwd(paste0("../../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, "/PR_results_by_confidence_threshold/"))

PR_results_files <- list.files(pattern = ".csv$")

for (i in 1:length(PR_results_files)) {
  temp_PR_results <- read.csv(PR_results_files[i])
  
  PR_results_by_species_recording_and_confidence <- rbind(PR_results_by_species_recording_and_confidence, temp_PR_results)
}

PR_results_by_species_recording_and_confidence_file_name <- file.path("../", paste0(algorithm, "_PR_results_by_species_recording_and_confidence.csv"))
write.csv(x=PR_results_by_species_recording_and_confidence, file=PR_results_by_species_recording_and_confidence_file_name, row.names = FALSE)




PR_results_by_recording_and_confidence <- PR_results_by_species_recording_and_confidence %>%
  group_by(recording_id, confidence_threshold) %>%
  summarize(TP = sum(as.numeric(true_positive)),
            FP = sum(as.numeric(false_positive)),
            TN = sum(as.numeric(true_negative)),
            FN = sum(as.numeric(false_negative))) %>%
  mutate(TPR = TP / (TP + FN),
         FPR = FP / (FP + TN),
         recall = TPR,
         precision = TP / (TP + FP),
         F_score1 = (1 + Beta1 * Beta1) * precision * recall / (Beta1 * Beta1 * precision + recall),
         F_score2 = (1 + Beta2 * Beta2) * precision * recall / (Beta2 * Beta2 * precision + recall),
         F_score3 = (1 + Beta3 * Beta3) * precision * recall / (Beta3 * Beta3 * precision + recall)) %>%
  filter(!is.na(precision))

PR_results_by_species_and_confidence <- PR_results_by_species_recording_and_confidence %>%
  group_by(species, confidence_threshold) %>%
  summarize(TP = sum(as.numeric(true_positive)),
            FP = sum(as.numeric(false_positive)),
            TN = sum(as.numeric(true_negative)),
            FN = sum(as.numeric(false_negative))) %>%
  mutate(TPR = TP / (TP + FN),
         FPR = FP / (FP + TN),
         recall = TPR,
         precision = TP / (TP + FP),
         F_score1 = (1 + Beta1 * Beta1) * precision * recall / (Beta1 * Beta1 * precision + recall),
         F_score2 = (1 + Beta2 * Beta2) * precision * recall / (Beta2 * Beta2 * precision + recall),
         F_score3 = (1 + Beta3 * Beta3) * precision * recall / (Beta3 * Beta3 * precision + recall)) %>%
  filter(!is.na(precision))

PR_results_by_confidence <- PR_results_by_species_recording_and_confidence %>%
  group_by(confidence_threshold) %>%
  summarize(TP = sum(as.numeric(true_positive)),
            FP = sum(as.numeric(false_positive)),
            TN = sum(as.numeric(true_negative)),
            FN = sum(as.numeric(false_negative))) %>%
  mutate(TPR = TP / (TP + FN),
         FPR = FP / (FP + TN),
         recall = TPR,
         precision = TP / (TP + FP),
         F_score1 = (1 + Beta1 * Beta1) * precision * recall / (Beta1 * Beta1 * precision + recall),
         F_score2 = (1 + Beta2 * Beta2) * precision * recall / (Beta2 * Beta2 * precision + recall),
         F_score3 = (1 + Beta3 * Beta3) * precision * recall / (Beta3 * Beta3 * precision + recall)) %>%
  filter(!is.na(precision))


selected_species <- total_birds_detected %>%
  filter(total_time >= 30,
         species != "Unidentified bird") %>%
  select(species)

PR_results_by_selected_species <- PR_results_by_species_and_confidence %>%
  filter(species %in% selected_species$species) %>%
  mutate(TPR = TP / (TP + FN),
         FPR = FP / (FP + TN),
         recall = TPR,
         precision = TP / (TP + FP),
         F_score1 = (1 + Beta1 * Beta1) * precision * recall / (Beta1 * Beta1 * precision + recall),
         F_score2 = (1 + Beta2 * Beta2) * precision * recall / (Beta2 * Beta2 * precision + recall),
         F_score3 = (1 + Beta3 * Beta3) * precision * recall / (Beta3 * Beta3 * precision + recall)) %>%
  filter(!is.na(precision))

PR_results_by_habitat_and_confidence <- PR_results_by_species_recording_and_confidence %>%
  left_join(recording_extended_info %>%
              select(recording_id, habitat),
            by = "recording_id") %>%
  group_by(habitat, confidence_threshold) %>%
  summarize(TP = sum(as.numeric(true_positive)),
            FP = sum(as.numeric(false_positive)),
            TN = sum(as.numeric(true_negative)),
            FN = sum(as.numeric(false_negative))) %>%
  mutate(TPR = TP / (TP + FN),
         FPR = FP / (FP + TN),
         recall = TPR,
         precision = TP / (TP + FP),
         F_score1 = (1 + Beta1 * Beta1) * precision * recall / (Beta1 * Beta1 * precision + recall),
         F_score2 = (1 + Beta2 * Beta2) * precision * recall / (Beta2 * Beta2 * precision + recall),
         F_score3 = (1 + Beta3 * Beta3) * precision * recall / (Beta3 * Beta3 * precision + recall)) %>%
  filter(!is.na(precision))


PR_results_by_recording_and_confidence_file_name <- file.path("../", paste0(algorithm, "_PR_results_by_recording.csv"))
write.csv(x=PR_results_by_recording_and_confidence, file=PR_results_by_recording_and_confidence_file_name, row.names = FALSE)

PR_results_by_species_and_confidence_file_name <- file.path("../", paste0(algorithm, "_PR_results_by_species.csv"))
write.csv(x=PR_results_by_species_and_confidence, file=PR_results_by_species_and_confidence_file_name, row.names = FALSE)

PR_results_by_habitat_and_confidence_file_name <- file.path("../", paste0(algorithm, "_PR_results_by_habitat.csv"))
write.csv(x=PR_results_by_habitat_and_confidence, file=PR_results_by_habitat_and_confidence_file_name, row.names = FALSE)

PR_results_by_selected_species_and_confidence_file_name <- file.path("../", paste0(algorithm, "_PR_results_by_selected_species.csv"))
write.csv(x=PR_results_by_selected_species, file=PR_results_by_selected_species_and_confidence_file_name, row.names = FALSE)

PR_results_by_confidence_file_name <- file.path("../", paste0(algorithm, "_PR_results.csv"))
write.csv(x=PR_results_by_confidence, file=PR_results_by_confidence_file_name, row.names = FALSE)


rm(PR_results_by_recording_and_confidence_file_name, PR_results_by_selected_species_and_confidence_file_name, PR_results_by_species_and_confidence_file_name)