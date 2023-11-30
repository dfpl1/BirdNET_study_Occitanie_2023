library(dplyr)
library(ggplot2)
library(grid)
library(pracma)
library(qdapRegex)
# library(PRROC)
source("./Algorithm_version_selection.R")

filtered_label <- case_when(filtered ~ " with frequency filter",
                            TRUE ~ "")

CSV_path <- paste0("../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, "/")

#--------------Global PR results-----------------

BirdNET_PR_results <- read.csv(paste0(CSV_path, algorithm, "_PR_results.csv"))

BirdNET_PR_results <- BirdNET_PR_results %>%
  mutate(confidence_threshold = as.numeric(confidence_threshold))

# Getting precision and results at the identification level
BirdNET_precision_results <- read.csv(paste0(CSV_path, algorithm, "_precision_results.csv")) %>%
  mutate(id_precision = as.numeric(correct_identification))
BirdNET_recall_results <- read.csv(paste0(CSV_path, algorithm, "_recall_by_audio_fragment.csv")) %>%
  mutate(id_recall = as.numeric(correctly_identified)) %>%
  left_join(BirdNET_precision_results %>%
              select(-correct_identification, -id_precision),
            by = c("recording_id", "initial_time", "final_time", "species"))

BirdNET_recall_results[is.na(BirdNET_recall_results)] = 1


# Getting the number of species potentially present by recording
recording_info_extended <- read.csv("../CSVs/recordings_extended.csv")
recording_path <- paste0("../Recordings/Selected/")
potentially_present_species_by_recording <- data.frame()

for(i in 1:nrow(recording_info_extended)) {
  current_recording_species_list_file_name <- gsub("labels.txt", "BirdNET_Analyzer_species_list.csv", (recording_info_extended %>%
                                                                                                         filter(recording_id == i))$annotation_file_path[1])
  current_recording_species_list <- read.csv(paste0(recording_path, current_recording_species_list_file_name), header = FALSE)
  
  potentially_present_species_by_recording <- rbind(potentially_present_species_by_recording, c(i, nrow(current_recording_species_list)))
}

colnames(potentially_present_species_by_recording) <- c("recording_id", "n_species_potentially_present")

id_precision_by_species_and_confidence_threshold <- data.frame()
id_recall_by_species_and_confidence_threshold <- data.frame()
id_FPR_by_recording_and_confidence_threshold <- data.frame()

all_confidence_thresholds <- seq(0.1, 0.99, 0.01)

for (i in all_confidence_thresholds) {
  current_threshold_BirdNET_precision_results <- BirdNET_precision_results %>%
    filter(confidence >= i) %>%
    group_by(species) %>%
    summarize(mean_id_precision = mean(id_precision),
              id_precision_sd = sd(id_precision),
              n = n(),
              id_precision_se = id_precision_sd / sqrt(n)) %>%
    mutate(confidence_threshold = i)
  
  current_threshold_BirdNET_recall_results <- BirdNET_recall_results %>%
    filter(confidence >= i) %>%
    group_by(species) %>%
    summarize(mean_id_recall = mean(id_recall),
              id_recall_sd = sd(id_recall),
              n = n(),
              id_recall_se = id_recall_sd / sqrt(n)) %>%
    mutate(confidence_threshold = i)
  
  current_threshold_BirdNET_FPR_results <- BirdNET_recall_results %>%
    filter(confidence >= i) %>%
    group_by(recording_id, initial_time, final_time) %>%
    summarize(n_species_present = n()) %>%
    full_join(BirdNET_precision_results %>%
                filter(confidence >= i) %>%
                group_by(recording_id, initial_time, final_time) %>%
                summarize(n_species_correctly_detected = sum(correct_identification == TRUE),
                          n_species_incorrectly_detected = sum(correct_identification == FALSE)),
              by = c("recording_id", "initial_time", "final_time")) %>%
    left_join(potentially_present_species_by_recording,
              by = "recording_id") %>%
    filter(!is.na(n_species_present),
           !is.na(n_species_correctly_detected)) %>%
    mutate(id_FPR = n_species_incorrectly_detected / (n_species_potentially_present - n_species_present)) %>%
    group_by(recording_id) %>%
    summarize(mean_id_FPR = mean(id_FPR),
              id_FPR_sd = sd(id_FPR),
              n = n(),
              id_FPR_se = id_FPR_sd / sqrt(n)) %>%
    mutate(confidence_threshold = i)
    
  
  id_precision_by_species_and_confidence_threshold <- rbind(id_precision_by_species_and_confidence_threshold, current_threshold_BirdNET_precision_results)
  id_recall_by_species_and_confidence_threshold <- rbind(id_recall_by_species_and_confidence_threshold, current_threshold_BirdNET_recall_results)
  id_FPR_by_recording_and_confidence_threshold <- rbind(id_FPR_by_recording_and_confidence_threshold, current_threshold_BirdNET_FPR_results)
}

id_precision_by_species_and_confidence_threshold[is.na(id_precision_by_species_and_confidence_threshold)] = 0
id_recall_by_species_and_confidence_threshold[is.na(id_recall_by_species_and_confidence_threshold)] = 0
id_FPR_by_recording_and_confidence_threshold[is.na(id_FPR_by_recording_and_confidence_threshold)] = 0

id_precision_and_recall_by_confidence_threshold <- id_precision_by_species_and_confidence_threshold %>%
  group_by(confidence_threshold) %>%
  summarize(id_precision = mean(mean_id_precision)) %>%
  left_join(id_recall_by_species_and_confidence_threshold %>%
              group_by(confidence_threshold) %>%
              summarize(id_recall = mean(mean_id_recall)),
            by = "confidence_threshold") %>%
  left_join(id_FPR_by_recording_and_confidence_threshold %>%
              group_by(confidence_threshold) %>%
              summarize(id_FPR = mean(mean_id_FPR)),
            by = "confidence_threshold")

BirdNET_PR_results <- BirdNET_PR_results %>%
  mutate(confidence_threshold = as.character(confidence_threshold)) %>%
  rename("rec_precision" = "precision",
         "rec_recall" = "recall") %>%
  left_join(id_precision_and_recall_by_confidence_threshold %>%
              mutate(confidence_threshold = as.character(confidence_threshold)),
            by = "confidence_threshold") %>%
  mutate(confidence_threshold = as.numeric(confidence_threshold))


BirdNET_PR_results_at_id_and_rec_levels_file_name <- file.path(CSV_path, paste0(algorithm, "_PR_results_at_id_and_rec_levels.csv"))
write.csv(x=BirdNET_PR_results, file=BirdNET_PR_results_at_id_and_rec_levels_file_name, row.names = FALSE)

# Alternative way of calculating and plotting the ROC and PR curves

# positive_points_classification <- c()
# negative_points_classification <- c()
# 
# for (i in 1:nrow(BirdNET_PR_results)) {
#   positive_points_classification <- c(positive_points_classification, BirdNET_PR_results[i,]$TP / (BirdNET_PR_results[i,]$TP + BirdNET_PR_results[i,]$FN))
#   negative_points_classification <- c(negative_points_classification, BirdNET_PR_results[i,]$TN / (BirdNET_PR_results[i,]$TN + BirdNET_PR_results[i,]$FP))
# }
# 
# roc<-roc.curve(scores.class0 = positive_points_classification, scores.class1 = negative_points_classification, curve = TRUE, sorted = TRUE)
# plot(roc)
# 
# pr <- pr.curve(scores.class0 = positive_points_classification, scores.class1 = negative_points_classification, curve = TRUE, sorted = TRUE)
# plot(pr)


rec_roc_auc <- -trapz(BirdNET_PR_results$TPR, BirdNET_PR_results$FPR)/max(BirdNET_PR_results$FPR)
rec_pr_auc <- trapz(BirdNET_PR_results$rec_precision, BirdNET_PR_results$rec_recall)/max(BirdNET_PR_results$rec_recall)

id_roc_auc <- -trapz(BirdNET_PR_results$id_recall, BirdNET_PR_results$id_FPR)/max(BirdNET_PR_results$id_FPR)
id_pr_auc <- trapz(BirdNET_PR_results$id_precision, BirdNET_PR_results$id_recall)/max(BirdNET_PR_results$id_recall)

overlap_label_regex <- qdapRegex::ex_between(overlap_label, "(", ")")[[1]]

rec_ROC_title <- paste0(case_when(algorithm == "Google" ~ "Google",
                                  TRUE ~ overlap_label_regex), filtered_label, " - rec_ROC curve, AUC = ", round(rec_roc_auc, digits = 3))
rec_PR_title <- paste0(case_when(algorithm == "Google" ~ "Google",
                                 TRUE ~ overlap_label_regex), filtered_label, " - rec_PR curve, AUC = ", round(rec_pr_auc, digits = 3))

id_ROC_title <- paste0(case_when(algorithm == "Google" ~ "Google",
                                  TRUE ~ overlap_label_regex), filtered_label, " - id_ROC curve, AUC = ", round(id_roc_auc, digits = 3))
id_PR_title <- paste0(case_when(algorithm == "Google" ~ "Google",
                                 TRUE ~ overlap_label_regex), filtered_label, " - id_PR curve, AUC = ", round(id_pr_auc, digits = 3))



# ROC curve

jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/rec_ROC_curve.jpeg")), width = 3300, height = 2400, res = 500)
ggplot(BirdNET_PR_results, aes(x = FPR, y = TPR, color = confidence_threshold)) +
  geom_point() + 
  guides(color = guide_colorbar(reverse = FALSE, barheight = 10)) +
  xlab("rec_FPR") +
  ylab("rec_TPR") +
  ylim(c(0, 0.33)) +
  xlim(c(0, 0.016)) +
  labs(color = "Confidence \n  threshold") +
  ggtitle(rec_ROC_title) +   
  theme_bw() +
  # Eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(0,0,10,0))
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
dev.off()

jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/id_ROC_curve.jpeg")), width = 3300, height = 2400, res = 500)
ggplot(BirdNET_PR_results, aes(x = id_FPR, y = id_recall, color = confidence_threshold)) +
  geom_point() + 
  guides(color = guide_colorbar(reverse = FALSE, barheight = 10)) +
  xlab("id_FPR") +
  ylab("id_TPR") +
  ylim(c(0, 0.33)) +
  xlim(c(0, 0.016)) +
  labs(color = "Confidence \n  threshold") +
  ggtitle(id_ROC_title) +   
  theme_bw() +
  # Eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(0,0,10,0))
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
dev.off()


# Precision-Recall curve

jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/rec_PR_curve.jpeg")), width = 3300, height = 2400, res = 500)
ggplot(BirdNET_PR_results, aes(x = rec_recall, y = rec_precision, color = confidence_threshold)) +
  geom_point() + 
  guides(color = guide_colorbar(reverse = FALSE, barheight = 10)) +
  xlab("rec_recall") +
  ylab("rec_precision") +
  ylim(c(0.35, 0.95)) +
  xlim(c(0, 0.35)) +
  labs(color = "Confidence \n  threshold") +
  ggtitle(rec_PR_title) +   
  theme_bw() +
  # Eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(0,0,10,0))
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
dev.off()

jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/id_PR_curve.jpeg")), width = 3300, height = 2400, res = 500)
ggplot(BirdNET_PR_results, aes(x = id_recall, y = id_precision, color = confidence_threshold)) +
  geom_point() + 
  guides(color = guide_colorbar(reverse = FALSE, barheight = 10)) +
  xlab("id_recall") +
  ylab("id_precision") +
  ylim(c(0.35, 0.95)) +
  xlim(c(0, 0.35)) +
  labs(color = "Confidence \n  threshold") +
  ggtitle(id_PR_title) +   
  theme_bw() +
  # Eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(0,0,10,0))
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
dev.off()


# F-score curve

jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/F-score_curve.jpeg")), width = 3000, height = 2100, res = 500)
ggplot(BirdNET_PR_results, aes(x = confidence_threshold)) +
  geom_point(aes(y = F_score1, color = "1")) + 
  geom_point(aes(y = F_score2, color = "0.25")) + 
  geom_point(aes(y = F_score3, color = "0.1")) + 
  scale_colour_manual(values = c("#0D47A1","#2196F3","#81D4FA")) +
  scale_x_continuous(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
  xlab("Confidence threshold") +
  ylab("F-score") +
  labs(color = "β value") +
  ggtitle("F-score by confidence threshold") +   
  theme_bw() +
  # Eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(hjust = 0.5, 
                              margin = margin(0,0,10,0))
  ) +
  # Draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
dev.off()


#----------PR results by species-------------------

total_birds_detected <- read.csv("../CSVs/total_birds_detected_(annotations).csv")
BirdNET_PR_results_by_selected_species <- read.csv(paste0("../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, "_PR_results_by_selected_species.csv")))

# Selected species
selected_species <- BirdNET_PR_results_by_selected_species %>%
  select(species) %>%
  unique()

for (i in 1:nrow(selected_species)) {
  jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/Selected species/", selected_species$species[i], "_F-score_curve.jpeg")), width = 4300, height = 3100, res = 500)
  ggplot(BirdNET_PR_results_by_selected_species %>%
           filter(species == selected_species$species[i]), aes(x = confidence_threshold)) +
    geom_point(aes(y = F_score1, color = "1")) + 
    geom_point(aes(y = F_score2, color = "0.2")) + 
    geom_point(aes(y = F_score3, color = "0.1")) + 
    scale_colour_manual(values = c("#0D47A1","#2196F3","#81D4FA")) +
    scale_x_continuous(breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
    scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
    xlab("Confidence threshold") +
    ylab("F-score") +
    labs(color = "β value") +
    ggtitle("F-score by confidence threshold") +   
    theme_bw() +
    # Eliminates background, gridlines, and chart border
    theme(
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.title = element_text(hjust = 0.5, 
                                margin = margin(0,0,10,0))
    ) +
    # Draws x and y axis line
    theme(axis.line = element_line(color = 'black'))
  dev.off()
}


#-----------------Selecting two confidence thresholds from the F-scores---------

# It only keeps the rows with confidence thresholds divisible by 5 (10, 15, 20, 25...)
BirdNET_PR_results_simplified <- BirdNET_PR_results %>%
  filter(round(100*confidence_threshold, 0) %% 5 == 0)

# The optimal confidence thresholds according to F-scores with Beta-values of 1, 0.25 and 0.1 are 0.35, 0.65 and 0.75, respectively.
conf_thresholds <- c(BirdNET_PR_results_simplified[which.max(BirdNET_PR_results_simplified$F_score1),]$confidence_threshold, 
                     BirdNET_PR_results_simplified[which.max(BirdNET_PR_results_simplified$F_score2),]$confidence_threshold,
                     BirdNET_PR_results_simplified[which.max(BirdNET_PR_results_simplified$F_score3),]$confidence_threshold)

conf_thresholds_df <- data.frame(conf_thresholds)

optimal_confidence_thresholds_file_name <- file.path(paste0("../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text), paste0(algorithm, "_optimal_confidence_thresholds.csv"))
write.csv(x=conf_thresholds_df, file=optimal_confidence_thresholds_file_name, row.names = FALSE)
