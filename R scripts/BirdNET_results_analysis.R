library(ggrepel)
library(dplyr)
library(ggplot2)
library(viridis)
library('data.table')
library(pracma)
library(ggtext)
library(ggbreak) 
library(wesanderson)
library(qdapRegex)
source("./Algorithm_version_selection.R")

#---------------------------Confidence analysis---------------------------------

CSV_path <- paste0("../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text)
R_plots_path <- paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text)

BirdNET_precision_results <- read.csv(paste0(CSV_path, "/", algorithm, "_precision_results.csv"))

optimal_confidence_thresholds <- read.csv(paste0(CSV_path, "/", algorithm, "_optimal_confidence_thresholds.csv"))

first_threshold <- optimal_confidence_thresholds$conf_thresholds[1]
second_threshold <- optimal_confidence_thresholds$conf_thresholds[2]
third_threshold <- optimal_confidence_thresholds$conf_thresholds[3]

precision_results_by_species <- BirdNET_precision_results %>%
  group_by(species, correct_identification) %>%
  summarize(mean_confidence = mean(confidence),
            n_identifications = n())

precision_by_confidence_level <- precision_results_by_species %>%
  group_by(correct_identification) %>%
  summarize(mean_confidence = mean(mean_confidence))

correctly_identified_species <- precision_results_by_species %>%
  filter(correct_identification == TRUE) %>%
  select(species) %>%
  unique()

precision_by_confidence_level_in_detected_species <- precision_results_by_species %>%
  filter(species %in% correctly_identified_species$species) %>%
  group_by(correct_identification) %>%
  summarize(mean_confidence = mean(mean_confidence))


# Checking the normality of the distribution of the confidence levels to determine whether we can perform an ANOVA analysis or not
# if (length(BirdNET_precision_results$confidence) < 5000) {
#   shapiro.test(BirdNET_precision_results$confidence)
#   hist(BirdNET_precision_results$confidence)
# }

# The distribution of the confidence levels is not normal. Therefore, we have to transform the data before we can perform the ANOVA analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the distribution into a normal one.
# Thus, a Kruskal-Wallis test will have to be performed insted of an ANOVA.

kruskal.test(confidence ~ correct_identification, data = BirdNET_precision_results)


confidences_correct_identifications <- (BirdNET_precision_results %>%
                                          filter(correct_identification == TRUE))$confidence
confidences_incorrect_identifications <- (BirdNET_precision_results %>%
                                            filter(correct_identification == FALSE))$confidence



# BirdNET got 1044 correct identifications and 890 incorrect identifications

# T-test to determine whether the mean confidence level is independent of the correctness of the identification
t.test(confidences_correct_identifications, confidences_incorrect_identifications)

# p-value < 2.2e-16
# The mean of the confidence level is not independent of the correctness of the identification: correct identifications have a 
# significantly higher confidence level in comparison to incorrect identifications


confidences_incorrect_identifications_df <- data.frame(confidences_incorrect_identifications)
colnames(confidences_incorrect_identifications_df) <- c("confidence")
confidences_incorrect_identifications_df <- confidences_incorrect_identifications_df %>%
  mutate(identification_type = "Incorrect")

confidences_correct_identifications_df <- data.frame(confidences_correct_identifications)
colnames(confidences_correct_identifications_df) <- c("confidence")
confidences_correct_identifications_df <- confidences_correct_identifications_df %>%
  mutate(identification_type = "Correct")

all_confidences_identifications_df <- union_all(confidences_incorrect_identifications_df, confidences_correct_identifications_df)

confidences_identifications_df <- all_confidences_identifications_df %>%
  mutate(confidence_category = case_when(confidence < 0.1 ~ "0",
                                         confidence < 0.2 ~ "0.1",
                                         confidence < 0.3 ~ "0.2",
                                         confidence < 0.4 ~ "0.3",
                                         confidence < 0.5 ~ "0.4",
                                         confidence < 0.6 ~ "0.5",
                                         confidence < 0.7 ~ "0.6",
                                         confidence < 0.8 ~ "0.7",
                                         confidence < 0.9 ~ "0.8",
                                         confidence <= 1 ~ "0.9",
                                         TRUE ~ "1"),
         confidence_category = as.factor(confidence_category)) %>%
  group_by(identification_type, confidence_category) %>%
  count()

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/distribution_of_confidence_levels.jpeg"), width = 4500, height = 3000, res = 500)
ggplot(confidences_identifications_df %>%
         filter(identification_type == "Correct"), aes(confidence_category)) +
  geom_bar(aes(y = n, group = 1, fill = "Correct"),
           width = 0.45, position = position_nudge(0.725), stat = "identity", color = "#293C65") +
  geom_col(aes(y = n, fill = "Incorrect"), data = confidences_identifications_df %>%
             filter(identification_type == "Incorrect"),
           width = 0.45, position = position_nudge(0.275), color = "#293C65") +
  scale_x_discrete(limits = c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1")) +
  xlab("Confidence score") +
  ylab("Number of identifications") +
  labs(fill = "Identification type") +
  ggtitle(paste("Distribution of confidence scores in", algorithm, "identifications")) +
  scale_y_break(c(5000, 21000), scales = 0.5) +
  scale_fill_manual(values = c("#4E92DA", "#A0DDFF")) +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/boxplot_confidence_level_by_correctness_of_the_identification.jpeg"), width = 3000, height = 3000, res = 500)
ggplot(all_confidences_identifications_df, aes(x = identification_type, y = confidence, fill = identification_type)) +
  geom_boxplot() +
  xlab("Identification type") +
  ylab("Confidence score") +
  labs(fill = "Identification type") +
  ggtitle(paste0("Confidence score of ", algorithm, " identifications")) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#4E92DA", "#A0DDFF"))
dev.off()


# BirdNET precision by confidence level

precision_results_by_confidence_category <- BirdNET_precision_results %>%
  mutate(confidence_category = case_when(confidence < 0.1 ~ "0",
                                         confidence < 0.2 ~ "0.1",
                                         confidence < 0.3 ~ "0.2",
                                         confidence < 0.4 ~ "0.3",
                                         confidence < 0.5 ~ "0.4",
                                         confidence < 0.6 ~ "0.5",
                                         confidence < 0.7 ~ "0.6",
                                         confidence < 0.8 ~ "0.7",
                                         confidence < 0.9 ~ "0.8",
                                         confidence <= 1 ~ "0.9",
                                         TRUE ~ "1"),
         confidence_category = as.factor(confidence_category),
         id_precision = as.numeric(correct_identification)) %>%
  group_by(confidence_category) %>%
  summarise(mean_id_precision = mean(id_precision),
            id_precision_sd = sd(id_precision),
            n = n(),
            id_precision_se = id_precision_sd / sqrt(n))

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_confidence_level.jpeg"), width = 4000, height = 2700, res = 500)
ggplot(precision_results_by_confidence_category, aes(x=confidence_category, y=mean_id_precision)) +
  geom_bar(position = position_nudge(x = 0.5), 
           stat = "identity",
           colour = 'black',
           fill = "#6B9BD5") +
  geom_errorbar(aes(x=confidence_category, ymin=mean_id_precision-id_precision_se, ymax=mean_id_precision+id_precision_se), 
                position = position_nudge(x = 0.5), 
                width=0.5, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  scale_x_discrete(limits = c("0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1")) +
  xlab("Confidence score") +
  ylab("Mean id_precision") +
  ggtitle(paste0("Mean precision by confidence score in ", algorithm, " identifications")) +
  theme_bw()
dev.off()


# BirdNET recall by confidence level

BirdNET_recall_results_1st_threshold <- read.csv(paste0(CSV_path, "/", algorithm, "_recall_results_", toString(first_threshold*100), ".csv"))
BirdNET_recall_results_1st_threshold[is.na(BirdNET_recall_results_1st_threshold)] = 0
BirdNET_recall_results_2nd_threshold <- read.csv(paste0(CSV_path, "/", algorithm, "_recall_results_", toString(second_threshold*100), ".csv"))
BirdNET_recall_results_2nd_threshold[is.na(BirdNET_recall_results_2nd_threshold)] = 0
BirdNET_recall_results_3rd_threshold <- read.csv(paste0(CSV_path, "/", algorithm, "_recall_results_", toString(third_threshold*100), ".csv"))
BirdNET_recall_results_3rd_threshold[is.na(BirdNET_recall_results_3rd_threshold)] = 0

species_labels <- read.csv2("../CSVs/species_labels.csv", sep = ",") %>%
  mutate(species = case_when(label == "Den med" ~ "Dendrocoptes medius",
                             TRUE ~ species))

if (algorithm == "Google") {
  recall_results_by_species <- BirdNET_recall_results_1st_threshold %>%
    group_by(species) %>%
    summarize(mean_confidence_in_correct_identifications = mean(mean_Google_confidence[mean_Google_confidence > 0]),
              mean_id_recall = min(mean(proportion_of_time_detected), 1),
              mean_rec_recall = mean(as.numeric(detected_by_Google)),
              total_manual_detections = n()) %>%
    left_join(species_labels,
              by = "species")
} else {
  recall_results_by_species <- BirdNET_recall_results_1st_threshold %>%
    group_by(species) %>%
    summarize(mean_confidence_in_correct_identifications = mean(mean_BirdNET_confidence[mean_BirdNET_confidence > 0]),
              mean_id_recall = min(mean(proportion_of_time_detected), 1),
              mean_rec_recall = mean(as.numeric(detected_by_BirdNET)),
              total_manual_detections = n()) %>%
    left_join(species_labels,
              by = "species")
}

recall_results_by_species[is.na(recall_results_by_species)] = 0

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_(proportion_of_time_detected)_by_confidence_in_correct_identifications.jpeg"), width = 3300, height = 2400, res = 500)
ggplot(recall_results_by_species %>%
         filter(mean_confidence_in_correct_identifications > 0), 
       aes(x=mean_confidence_in_correct_identifications, 
           y=mean_id_recall, label=label)) +
  geom_point() + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method=lm, se=TRUE) +
  xlab("Mean confidence in correct identifications") +
  ylab("Mean id_recall") +
  ggtitle(paste0(algorithm, " id_recall by confidence score"))
dev.off()

total_birds_detected <- read.csv("../CSVs/total_birds_detected_(annotations).csv")

recall_and_precision_results_by_species <- recall_results_by_species %>%
  left_join(precision_results_by_species,
            by = "species") %>%
  left_join(total_birds_detected %>%
              select(species, total_time, label),
            by = c("species", "label")) %>%
  mutate(total_time = case_when(is.na(total_time) ~ 0,
                                TRUE ~ total_time),
         original_time = total_time,
         total_time = log2(total_time))

recall_and_precision_results_by_species[is.na(recall_and_precision_results_by_species)] = 0

recall_results_by_species_with_at_least_30s <- recall_results_by_species %>%
  mutate(species_number = paste0(species, " (n = ", total_manual_detections, ")")) %>%
  left_join(total_birds_detected %>%
              select(species, total_time),
            by = "species") %>%
  filter(total_time >= 30) %>%
  left_join(recall_and_precision_results_by_species %>%
              select(species, original_time),
            by = "species") %>%
  mutate(minutes = round(round(original_time, 0)/60, 0),
         seconds = round(round(original_time, 0) %% 60, 0),
         species_time = paste0(species, " (t = ", case_when(minutes > 0 ~ paste0(minutes, "m "),
                                                            TRUE ~ ""), round(seconds, 0), "s)"))

recall_results_by_species_with_less_than_30s <- recall_results_by_species %>%
  mutate(species_number = paste0(species, " (n = ", total_manual_detections, ")")) %>%
  left_join(total_birds_detected %>%
              select(species, total_time),
            by = "species") %>%
  filter(total_time < 30)

average_id_recall_by_species_with_less_than_30s <- (recall_results_by_species_with_less_than_30s %>%
                                summarize(avg_id_recall = mean(mean_id_recall)))$avg_recall

average_id_recall_by_species_with_at_least_30s <- (recall_results_by_species_with_at_least_30s %>%
                                summarize(avg_id_recall = mean(mean_id_recall)))$avg_recall

# Average recall is higher for species with vocalization times higher than 30s

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_(proportion_of_time_detected)_by_confidence_in_correct_identifications_(only_species_with_at_least_30_seconds).jpeg"), width = 3300, height = 2400, res = 500)
ggplot(recall_results_by_species_with_at_least_30s %>%
         filter(mean_confidence_in_correct_identifications > 0), 
       aes(x=mean_confidence_in_correct_identifications, 
           y=mean_id_recall, label=label)) +
  geom_point() + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method=lm, se=TRUE) +
  xlab("Mean confidence in correct identifications") +
  ylab("Mean id_recall") +
  ggtitle(paste0(algorithm, " id_recall by confidence score \n(only species with ≥30s total time)"))
dev.off()


jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_(proportion_of_recordings_where_detected)_by_confidence_in_correct_identifications.jpeg"), width = 3300, height = 2400, res = 500)
ggplot(recall_results_by_species %>%
         filter(mean_confidence_in_correct_identifications > 0), 
       aes(x=mean_confidence_in_correct_identifications, 
           y=mean_rec_recall, 
           label=label)) +
  geom_point() + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method=lm, se=TRUE) +
  xlab("Mean confidence in correct identifications") +
  ylab("Mean rec_recall") +
  ggtitle(paste0(algorithm, " rec_recall by confidence score"))
dev.off()


jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_(proportion_of_recordings_where_detected)_by_confidence_in_correct_identifications_(only_species_with_at_least_30s).jpeg"), width = 3300, height = 2400, res = 500)
ggplot(recall_results_by_species_with_at_least_30s %>%
         filter(mean_confidence_in_correct_identifications > 0), 
       aes(x=mean_confidence_in_correct_identifications, 
           y=mean_rec_recall, 
           label=label)) +
  geom_point() + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method=lm, se=TRUE) +
  xlab("Mean confidence in correct identifications") +
  ylab("Mean rec_recall") +
  ggtitle(paste0(algorithm, " rec_recall by confidence score \n(only species with ≥30s total time)"))
dev.off()


# BirdNET precision by anthropic noise

recording_acoustic_data <- read.csv("../CSVs/recordings_acoustic_data.csv")

# Analyses with different confidence thresholds

confidence_levels <- c(first_threshold, second_threshold, third_threshold)
confidence_labels <- c(toString(first_threshold*100), toString(second_threshold*100), toString(third_threshold*100))
confidence_plot_titles <- c(paste0(" with confidence score ≥ ", toString(first_threshold)), paste0(" with confidence score ≥ ", toString(second_threshold)), paste0(" with confidence score ≥ ", toString(third_threshold)))

BirdNET_PR_results_by_species <- read.csv(paste0(CSV_path, "/", algorithm, "_PR_results_by_species.csv"))


# Checking whether the number of birds singing or calling simultaneously has an influence on BirdNET performance
BirdNET_recall_results_by_audio_fragment <- read.csv(paste0(CSV_path, "/", algorithm, "_recall_by_audio_fragment.csv"))

recall_results_by_audio_fragment <- BirdNET_recall_results_by_audio_fragment %>%
  group_by(recording_id, initial_time, final_time) %>%
  summarize(n_species_annotated = n_distinct(species)) %>%
  ungroup() %>%
  left_join(recording_acoustic_data %>%
              select(recording_id, NDSI_left),
            by = "recording_id")

recall_results_by_audio_fragment <- recall_results_by_audio_fragment %>%
  left_join(BirdNET_recall_results_by_audio_fragment,
            by = c("recording_id", "initial_time", "final_time"))

BirdNET_precision_by_n_species_singing_simultaneously <- recall_results_by_audio_fragment %>%
  filter(n_species_annotated > 0) %>%
  left_join(BirdNET_precision_results,
            by = c("recording_id", "initial_time", "final_time")) %>%
  mutate(correct_identification = case_when(is.na(correct_identification) ~ FALSE,
                                            TRUE ~ correct_identification)) %>%
  group_by(n_species_annotated, species.y) %>%
  summarize(mean_id_precision = mean(as.numeric(correct_identification)),
            mean_NDSI = mean(NDSI_left)) %>%
  rename("species" = "species.y")

BirdNET_recall_by_n_species_singing_simultaneously <- recall_results_by_audio_fragment %>%
  group_by(n_species_annotated, species) %>%
  summarize(mean_id_recall = mean(as.numeric(correctly_identified)),
            mean_singing_time_per_fragment = mean(total_time),
            mean_NDSI = mean(NDSI_left))

BirdNET_recall_by_n_individuals_singing_simultaneously <- recall_results_by_audio_fragment %>%
  group_by(min_code_n_ind, species) %>%
  summarize(mean_id_recall = mean(as.numeric(correctly_identified)),
            mean_singing_time_per_fragment = mean(total_time),
            mean_NDSI = mean(NDSI_left))

BirdNET_recall_by_singing_time_per_fragment <- recall_results_by_audio_fragment %>%
  mutate(total_time = round(total_time / 0.25) * 0.25) %>%
  group_by(total_time, species) %>%
  summarize(mean_id_recall = mean(as.numeric(correctly_identified)),
            mean_NDSI = mean(NDSI_left))

# Bar plots analyzing precision
BirdNET_mean_precision_by_n_species_singing_simultaneously <- BirdNET_precision_by_n_species_singing_simultaneously %>%
  group_by(n_species_annotated) %>%
  summarize(id_precision_sd = sd(mean_id_precision),
            mean_id_precision = mean(mean_id_precision),
            n = n(),
            id_precision_se = id_precision_sd / sqrt(n))

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_number_of_species_vocalizing_simultaneously.jpeg"), width = 4000, height = 3000, res = 500)
ggplot(BirdNET_mean_precision_by_n_species_singing_simultaneously, aes(x=as.factor(n_species_annotated), y=mean_id_precision)) + 
  geom_bar(stat = "identity",
           colour = 'black',
           fill = "#6B9BD5") +
  geom_errorbar(aes(x=as.factor(n_species_annotated), ymin=mean_id_precision-id_precision_se, ymax=mean_id_precision+id_precision_se), 
                width=0.5, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  xlab("Number of species vocalizing simultaneously") +
  ylab("Mean id_precision") +
  ggtitle(paste0(algorithm, " precision by number of species vocalizing simultaneously (confidence score >= 0.1)")) +
  theme_bw()
dev.off()

# Bar plots analyzing recall
BirdNET_mean_recall_by_n_species_singing_simultaneously <- BirdNET_recall_by_n_species_singing_simultaneously %>%
  group_by(n_species_annotated) %>%
  summarize(id_recall_sd = sd(mean_id_recall),
            mean_id_recall = mean(mean_id_recall),
            n = n(),
            id_recall_se = id_recall_sd / sqrt(n))

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_number_of_species_vocalizing_simultaneously.jpeg"), width = 4000, height = 3000, res = 500)
ggplot(BirdNET_mean_recall_by_n_species_singing_simultaneously, aes(x=as.factor(n_species_annotated), y=mean_id_recall)) + 
  geom_bar(stat = "identity",
           colour = 'black',
           fill = "#6B9BD5") +
  geom_errorbar(aes(x=as.factor(n_species_annotated), ymin=mean_id_recall-id_recall_se, ymax=mean_id_recall+id_recall_se), 
                width=0.5, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  xlab("Number of species vocalizing simultaneously") +
  ylab("Mean id_recall") +
  ggtitle(paste0(algorithm, " recall by number of species vocalizing simultaneously (confidence score >= 0.1)")) +
  theme_bw()
dev.off()

# Bar plots analyzing F1-scores
BirdNET_F1_score_by_n_species_singing_simultaneously <- BirdNET_recall_by_n_species_singing_simultaneously %>%
  left_join(BirdNET_precision_by_n_species_singing_simultaneously,
            by = c("n_species_annotated", "species")) %>%
  mutate(mean_id_F1_score = 2 * 1 * mean_id_precision * mean_id_recall / (mean_id_precision + mean_id_recall))

BirdNET_F1_score_by_n_species_singing_simultaneously[is.na(BirdNET_F1_score_by_n_species_singing_simultaneously)] = 0


BirdNET_mean_F1_score_by_n_species_singing_simultaneously <- BirdNET_F1_score_by_n_species_singing_simultaneously %>%
  group_by(n_species_annotated) %>%
  summarize(id_F1_score_sd = sd(mean_id_F1_score),
            mean_id_F1_score = mean(mean_id_F1_score),
            n = n(),
            id_F1_score_se = id_F1_score_sd / sqrt(n))

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/F1_score_by_number_of_species_vocalizing_simultaneously.jpeg"), width = 4000, height = 3000, res = 500)
ggplot(BirdNET_mean_F1_score_by_n_species_singing_simultaneously, aes(x=as.factor(n_species_annotated), y=mean_id_F1_score)) + 
  geom_bar(stat = "identity",
           colour = 'black',
           fill = "#6B9BD5") +
  geom_errorbar(aes(x=as.factor(n_species_annotated), ymin=mean_id_F1_score-id_F1_score_se, ymax=mean_id_F1_score+id_F1_score_se), 
                width=0.5, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  xlab("Number of species vocalizing simultaneously") +
  ylab("Mean id_F1-score") +
  ggtitle(paste0(algorithm, " F1-score at the identification level\nby number of species vocalizing simultaneously (confidence score >= 0.1)")) +
  theme_bw()
dev.off()


BirdNET_recall_by_n_individuals_singing_simultaneously <- BirdNET_recall_by_n_individuals_singing_simultaneously %>%
  mutate(n_ind_label = case_when(min_code_n_ind == 1 ~ "1 bird",
                                 min_code_n_ind == 2 ~ "2 birds of the same species",
                                 min_code_n_ind == 3 ~ ">2 birds of the same species",
                                 TRUE ~ "≥2 birds of different species"))
  
BirdNET_recall_by_n_individuals_singing_simultaneously$n_ind_label <- factor(BirdNET_recall_by_n_individuals_singing_simultaneously$n_ind_label, c("1 bird", "2 birds of the same species", ">2 birds of the same species", "≥2 birds of different species"))

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_number_of_birds_vocalizing_simultaneously.jpeg"), width = 4000, height = 3000, res = 500)
ggplot(BirdNET_recall_by_n_individuals_singing_simultaneously, aes(x = as.factor(n_ind_label), y = mean_id_recall, fill = n_ind_label)) + 
  geom_boxplot() +
  xlab("Number of birds vocalizing simultaneously") +
  ylab("Mean id_recall") +
  ggtitle(paste0(algorithm, " recall by number of birds vocalizing simultaneously (confidence score >= 0.1)")) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#84B8E1", "#84B8E1", "#84B8E1", "#84B8E1"))
dev.off()

BirdNET_mean_recall_by_singing_time_per_fragment <- BirdNET_recall_by_singing_time_per_fragment %>%
  group_by(total_time) %>%
  summarize(id_recall_sd = sd(mean_id_recall),
            mean_id_recall = mean(mean_id_recall),
            n = n(),
            id_recall_se = id_recall_sd / sqrt(n))

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_actual_vocalization_time_in_the_audio_chunk_analyzed.jpeg"), width = 4900, height = 3000, res = 500)
ggplot(BirdNET_mean_recall_by_singing_time_per_fragment, aes(x=as.factor(total_time), y=mean_id_recall)) + 
  geom_bar(stat = "identity",
           colour = 'black',
           fill = "#6B9BD5") +
  geom_errorbar(aes(x=as.factor(total_time), ymin=mean_id_recall-id_recall_se, ymax=mean_id_recall+id_recall_se), 
                width=0.5, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  xlab("Vocalization time (s)") +
  ylab("Mean id_recall") +
  ggtitle(paste0(algorithm, " recall by actual bird vocalization time in each 3-second audio segment analyzed (confidence score >= 0.1)")) +
  theme_bw()
dev.off()



# Checking the normality of the distribution of the recall variable to determine whether we can perform an ANOVA analysis or not
# if (length(BirdNET_recall_by_n_species_singing_simultaneously$mean_recall) < 5000) {
#   shapiro.test(BirdNET_recall_by_n_species_singing_simultaneously$mean_recall)
#   hist(BirdNET_recall_by_n_species_singing_simultaneously$mean_recall)
# }

# The distribution of recall values is not normal. Therefore, we have to transform it before we can perform the ANOVA analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the recall distribution into normal ones.
# Thus, a Spearman's correlation test will have to be performed insted of a Pearson's one.

cor.test(BirdNET_recall_by_n_species_singing_simultaneously$mean_id_recall, BirdNET_recall_by_n_species_singing_simultaneously$n_species_annotated, method = "spearman")

# The potential influence of the number of species vocalizing at the same time on recall results is high enough to be considered significant. 

# And a Spearman's correlation test will have to be performed insted of a Pearson's one for the vocalization time analysis.
cor.test(BirdNET_mean_recall_by_singing_time_per_fragment$total_time, BirdNET_mean_recall_by_singing_time_per_fragment$mean_id_recall, method = "spearman")
# Spearman's correlation coefficient is 0.965, with a p-value < 2.2e-16, which indicates that the total vocalization time captured within a given prediction segment significantly influences recall results.


# Checking the normality of the distribution of the precision variable to determine whether we can perform an ANOVA analysis or not
# if (length(BirdNET_precision_by_n_species_singing_simultaneously$mean_precision) < 5000) {
#   shapiro.test(BirdNET_precision_by_n_species_singing_simultaneously$mean_precision)
#   hist(BirdNET_precision_by_n_species_singing_simultaneously$mean_precision)
# }

# The distribution of precision values is not normal. Therefore, we have to transform it before we can perform the ANOVA analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the precision distribution into normal ones.
# Thus, a Spearman's correlation test will have to be performed insted of a Pearson's one.

cor.test(BirdNET_precision_by_n_species_singing_simultaneously$mean_id_precision, BirdNET_precision_by_n_species_singing_simultaneously$n_species_annotated, method = "spearman")

# The potential influence of the number of species vocalizing at the same time on precision results is high enough to be considered significant. 

# Now testing the influence of the number of species vocalizing at the same time on F1-scores
cor.test(BirdNET_F1_score_by_n_species_singing_simultaneously$mean_id_F1_score, BirdNET_F1_score_by_n_species_singing_simultaneously$n_species_annotated, method = "spearman")


#---------------------------------------------------------------------------

# Generating results for each confidence level selected

for (i in 1:length(confidence_levels)) {
  
  #--------------------------Precision and recall analysis-------------------
  
  precision_results_by_species <- BirdNET_precision_results %>%
    filter(confidence >= confidence_levels[i]) %>%
    mutate(species = as.factor(species),
           precision = as.numeric(correct_identification)) %>%
    group_by(species) %>%
    summarize(mean_id_precision = mean(precision),
              total_detections = n()) %>%
    arrange(mean_id_precision)
  
  if (i == 1) {
    BirdNET_recall_results_to_use <- BirdNET_recall_results_1st_threshold
  } else if (i == 2) {
    BirdNET_recall_results_to_use <- BirdNET_recall_results_2nd_threshold
  } else {
    BirdNET_recall_results_to_use <- BirdNET_recall_results_3rd_threshold
  }
  
  if (algorithm == "Google") {
    recall_results_by_species <- BirdNET_recall_results_to_use %>%
      group_by(species) %>%
      summarize(mean_confidence_in_correct_identifications = mean(mean_Google_confidence[mean_Google_confidence > 0]),
                mean_id_recall = mean(proportion_of_time_detected),
                mean_rec_recall = mean(as.numeric(detected_by_Google)),
                total_manual_detections = n()) %>%
      left_join(species_labels,
                by = "species")
  } else {
    recall_results_by_species <- BirdNET_recall_results_to_use %>%
      group_by(species) %>%
      summarize(mean_confidence_in_correct_identifications = mean(mean_BirdNET_confidence[mean_BirdNET_confidence > 0]),
                mean_id_recall = mean(proportion_of_time_detected),
                mean_rec_recall = mean(as.numeric(detected_by_BirdNET)),
                total_manual_detections = n()) %>%
      left_join(species_labels,
                by = "species")
  }
  
  recall_results_by_species[is.na(recall_results_by_species)] = 0
    
  
  recall_and_precision_results_by_species <- recall_results_by_species %>%
    left_join(precision_results_by_species,
              by = "species") %>%
    left_join(total_birds_detected %>%
                select(species, total_time, label),
              by = c("species", "label")) %>%
    mutate(total_time = case_when(is.na(total_time) ~ 0,
                                  TRUE ~ total_time),
           original_time = total_time,
           total_time = log2(total_time))
  
  recall_and_precision_results_by_species[is.na(recall_and_precision_results_by_species)] = 0
  
  
  if (i == 1) {
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/recall_(proportion_of_recordings_where_detected)_by_precision.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species, aes(x=mean_id_precision, y=mean_rec_recall, label=label)) +
      geom_point() + 
      geom_text_repel(size = 2.5,
                nudge_y = 0.02) +
      geom_smooth(method=lm, se=TRUE) +
      xlab("Mean id_precision") +
      ylab("Mean rec_recall") +
      ggtitle(paste0(algorithm, " rec_recall by precision"))
    dev.off()
    
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/recall_(proportion_of_time_detected)_by_precision.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species, aes(x=mean_id_precision, y=mean_id_recall, label=label)) +
      geom_point() +
      geom_text_repel(size = 2.5,
                nudge_y = 0.02) +
      geom_smooth(method=lm, se=TRUE) +
      xlab("Mean id_precision") +
      ylab("Mean id_recall") +
      ggtitle(paste0(algorithm, " id_recall by precision"))
    dev.off()
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/recall_(proportion_of_recordings_where_detected)_by_precision_only_species_with_at_least_10_detections.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species %>%
             filter(total_detections >= 10), aes(x=mean_id_precision, y=mean_rec_recall, label=label)) +
      geom_point() + 
      geom_text_repel(size = 2.5,
                nudge_y = 0.02) +
      geom_smooth(method=lm, se=TRUE) +
      xlab("Mean id_precision") +
      ylab("Mean rec_recall") +
      ggtitle(paste0(algorithm, " rec_recall by precision \n(only species with ≥10 detections)"))
    dev.off()
    
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/recall_(proportion_of_time_detected)_by_precision_only_species_with_at_least_10_detections.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species %>%
             filter(total_detections >= 10), aes(x=mean_id_precision, y=mean_id_recall, label=label)) +
      geom_point() +
      geom_text_repel(size = 2.5,
                nudge_y = 0.02) +
      geom_smooth(method=lm, se=TRUE) +
      xlab("Mean id_precision") +
      ylab("Mean id_recall") +
      ggtitle(paste0(algorithm, " id_recall by precision \n(only species with ≥10 detections)"))
    dev.off()
  }
  
  
  #-------------------------- Identification type analysis ----------------------
  
  BirdNET_PR_results_by_species_and_selected_conf_levels <- BirdNET_PR_results_by_species %>%
    filter(confidence_threshold == confidence_levels[i],
           TP + FP + FN > 0) %>%
    select(species, confidence_threshold, TP, FP, FN) %>%
    mutate(total_identifications = TP + FP + FN) %>%
    arrange((TP / (TP + FN + FP)), -FP / (TP + FN + FP), -FP, -FN) %>%
    mutate(species_order = row_number())
  
  
  tps <- melt(setDT(BirdNET_PR_results_by_species_and_selected_conf_levels %>%
                      select(species, TP)), id.vars = "species", variable.name = "identification_type", value.name = "n_identifications")
  fps <- melt(setDT(BirdNET_PR_results_by_species_and_selected_conf_levels %>%
                      select(species, FP)), id.vars = "species", variable.name = "identification_type", value.name = "n_identifications")
  fns <- melt(setDT(BirdNET_PR_results_by_species_and_selected_conf_levels %>%
                      select(species, FN)), id.vars = "species", variable.name = "identification_type", value.name = "n_identifications")
  
  identification_types_by_species <- union_all(union_all(tps, fps), fns) %>%
    arrange(species, desc(identification_type)) %>%
    left_join(BirdNET_PR_results_by_species_and_selected_conf_levels %>%
                select(species, total_identifications, species_order),
              by = "species") %>%
    mutate(share = n_identifications / total_identifications,
           identification_type = as.factor(case_when(identification_type == "FN" ~ "False negative",
                                                     identification_type == "FP" ~ "False positive",
                                                     TRUE ~ "True positive")))
  
  factor_order <- c("False negative", "False positive", "True positive")
  identification_types_by_species$identification_type <- factor(identification_types_by_species$identification_type, levels=factor_order)
  
  plot_width <- case_when(i == 1 ~ 12000,
                          i == 2 ~ 12000,
                          i == 3 ~ 12000)
  
  # Bar plot showing the proportion of each identification type for each species either present in the recordings or detected by BirdNET
  jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/", algorithm, "_identifications_by_type_and_species_", confidence_labels[i], ".jpeg"), width = plot_width, height = 2800, res = 500)
  ggplot(identification_types_by_species, aes(fill = identification_type, y = share, x = reorder(species, species_order))) + 
    geom_bar(position = "fill", stat = "identity") +
    geom_text(data = subset(identification_types_by_species, n_identifications != 0),
              aes(x = species, y = share, label = n_identifications, group = identification_type),
              position = position_stack(vjust = .5),
              stat = "identity") +
    ylab("Proportion of detections") +
    xlab("Species") +
    labs(fill = "Identification type") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"),
          panel.background = element_blank()) +
    scale_fill_manual(values = c("#FFCC33", "#ED195C", "#0099D7")) +
    ggtitle(paste0(algorithm, " results by species — only identifications", confidence_plot_titles[i]))
  dev.off()
  
  if (i == 1) {
    identification_types_by_selected_species <- identification_types_by_species %>%
      filter(total_identifications >= 10)
    
    identification_types_by_species_sum <- identification_types_by_species %>%
      group_by(identification_type) %>%
      summarize(total = sum(n_identifications))
    
    identification_types_by_selected_species_sum <- identification_types_by_selected_species %>%
      group_by(identification_type) %>%
      summarize(total = sum(n_identifications))
    
    all_species_tp <- (identification_types_by_species_sum %>%
                         filter(identification_type == "True positive") %>% 
                         select(total))$total
    
    all_species_fp <- (identification_types_by_species_sum %>%
                         filter(identification_type == "False positive") %>% 
                         select(total))$total
    
    all_species_fn <- (identification_types_by_species_sum %>%
                         filter(identification_type == "False negative") %>% 
                         select(total))$total
    
    selected_species_tp <- (identification_types_by_selected_species_sum %>%
                              filter(identification_type == "True positive") %>% 
                              select(total))$total
    
    selected_species_fp <- (identification_types_by_selected_species_sum %>%
                              filter(identification_type == "False positive") %>% 
                              select(total))$total
    
    selected_species_fn <- (identification_types_by_selected_species_sum %>%
                         filter(identification_type == "False negative") %>% 
                         select(total))$total
    
    all_species_rec_precision <- all_species_tp / (all_species_tp + all_species_fp)
    all_species_rec_recall <- all_species_tp / (all_species_tp + all_species_fn)
    
    selected_species_rec_precision <- selected_species_tp / (selected_species_tp + selected_species_fp)
    selected_species_rec_recall <- selected_species_tp / (selected_species_tp + selected_species_fn)
    
    # The overall recall score increases from 0.37 to 0.46 while the overall precision score remains mostly unaffected (0.37 vs 0.35, respectively)
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/", algorithm, "_identifications_by_type_and_species_at_least_in_10_recordings.jpeg"), width = plot_width, height = 2600, res = 500)
    ggplot(identification_types_by_selected_species, aes(fill = identification_type, y = share, x = reorder(species, species_order))) + 
      geom_bar(position = "fill", stat = "identity") +
      geom_text(data = subset(identification_types_by_selected_species, n_identifications != 0),
                aes(x = species, y = share, label = n_identifications, group = identification_type),
                position = position_stack(vjust = .5),
                stat = "identity") +
      ylab("Proportion of detections") +
      xlab("Species") +
      labs(fill = "Identification type") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"),
            panel.background = element_blank()) +
      scale_fill_manual(values = c("#FFCC33", "#ED195C", "#0099D7")) +
      ggtitle(paste0(algorithm, " results by species", filtered_label, overlap_label, ds_label, " — only identifications", confidence_plot_titles[i], " and species with ≥10 detections"))
    dev.off()
  }
  
  
  
  #--------------------------Precision analysis-----------------------------------
  
  precision_results_by_species <- precision_results_by_species %>%
    mutate(species_number = paste0(species, " (n = ", total_detections, ")")) %>%
    left_join(BirdNET_PR_results_by_species_and_selected_conf_levels %>%
                mutate(mean_rec_precision = TP / (TP + FP)) %>%
                select(species, mean_rec_precision),
              by = "species")
  
  # Plotting BirdNET precision by species
  
  jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_species_", confidence_labels[i], ".jpeg"), width = 7500, height = 2400, res = 500)
  ggplot(precision_results_by_species, aes(x=reorder(reorder(species_number, -total_detections), mean_rec_precision))) +
    geom_bar(position = position_dodge(), 
             stat = "identity",
             color = 'black', 
             aes(y = mean_rec_precision, fill = "rec_precision")) +
    geom_line(stat = "identity", 
             size = 1.5, 
             group = 1,
             color = '#1d416e', 
             aes(y = mean_id_precision, fill = 'id_precision')) +
    scale_fill_manual(name = "Precision type",
                      breaks = c("id_precision", "rec_precision"),
                      values = c("id_precision" = "#1d416e", "rec_precision" = "#abe1ff")) +
    xlab("Species") +
    ylab("Mean precision") +
    ggtitle(paste0("Mean precision by bird species in ", algorithm, " identifications", confidence_plot_titles[i])) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"))
  dev.off()
  
  if (i == 1) {
    
    # Plotting BirdNET precision by total singing/calling time
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_total_time.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species, aes(x=total_time, y=mean_id_precision, label=label)) +
      geom_point() + 
      geom_text_repel(size = 2.5,
                      nudge_y = 0.02) +
      geom_smooth(method=lm, se=TRUE) +
      xlab("Time (s), log2 scale") +
      ylab("Mean id_precision") +
      ggtitle(paste0(algorithm, " id_precision by species total singing/calling time"))
    dev.off()
    
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_total_time_(only_species_with_at_least_10_detections).jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species %>%
             filter(total_detections >= 10), aes(x=total_time, y=mean_id_precision, label=label)) +
      geom_point() + 
      geom_text_repel(size = 2.5,
                      nudge_y = 0.02) +
      geom_smooth(method=lm, se=TRUE) +
      xlab("Time (s), log2 scale") +
      ylab("Mean id_precision") +
      ggtitle(paste0(algorithm, " id_precision by species total singing/calling time \n(only species with ≥10 detections)"))
    dev.off()
    
    online_foreground_recordings_by_species <- read.csv("../CSVs/online_foreground_recordings_for_selected_species.csv")
    
    # Adding all species having been detected 10 or more times by BirdNET
    species_labels <- rbind(species_labels, 
                            c("Ath noc", "Athene noctua"), 
                            c("Cor nix", "Corvus cornix"), 
                            c("Cur ibe", "Curruca iberiae"), 
                            c("Cur ala", "Curruca melanocephala"), 
                            c("Dry mar", "Dryocopus martius"), 
                            c("Gal the", "Galerida theklae"), 
                            c("Ixo min", "Ixobrychus minutus"), 
                            c("Lan col", "Lanius collurio"),
                            c("Lan sen", "Lanius senator"),
                            c("Mus str", "Muscicapa striata"),
                            c("Pho pho", "Phoenicurus phoenicurus"),
                            c("Phy ibe", "Phylloscopus ibericus"),
                            c("Tri neb", "Tringa nebularia"))
    
    precision_results_by_available_recordings_for_selected_species <- precision_results_by_species %>%
      # mutate(species = gsub("Dendrocoptes medius", "Leiopicus medius", species),
      #        species_number = gsub("Dendrocoptes medius", "Leiopicus medius", species_number)) %>%
      filter(total_detections >= 10) %>%
      left_join(online_foreground_recordings_by_species,
                by = "species") %>%
      left_join(species_labels %>%
                  select(species, label),
                by = "species")
    
    # Checking the normality of the precision distribution to determine whether we can perform a Pearson correlation analysis or not
    shapiro.test(precision_results_by_available_recordings_for_selected_species$mean_rec_precision)
    hist(precision_results_by_available_recordings_for_selected_species$mean_rec_precision)
    
    shapiro.test(log2(precision_results_by_available_recordings_for_selected_species$total_online_recordings))
    hist(log2(precision_results_by_available_recordings_for_selected_species$total_online_recordings))
    
    # The distribution of the logarithm of the total number of foreground recordings available on Xeno-canto and the Macaulay Library is normal, but the distribution of precision values is not. 
    # Therefore, we have to transform it before we can perform the Pearson's correlation analysis.
    # Nonetheless, neither sqrt nor logarithmic nor 1/x transformations transform the precision distribution into a normal.
    # Thus, a Spearman's correlation test will have to be performed instead of a Pearson's one.
    
    cor.test(log2(precision_results_by_available_recordings_for_selected_species$total_online_recordings), precision_results_by_available_recordings_for_selected_species$mean_rec_precision, method = "spearman")
    # Spearman's correlation coefficient is 0.593, with a p-value of 1.18e-6, which indicates that total number of foreground recordings on Xeno-canto and the Macaulay Library significantly influences precision results.
    
    
    
    selected_recall_and_precision_results_by_species <- recall_and_precision_results_by_species %>%
      filter(total_detections >= 10)
    
    # Checking the normality of the precision distribution to determine whether we can perform a Pearson correlation analysis or not
    shapiro.test(selected_recall_and_precision_results_by_species$mean_id_precision)
    hist(selected_recall_and_precision_results_by_species$mean_id_precision)
    
    shapiro.test(selected_recall_and_precision_results_by_species$total_time)
    hist(selected_recall_and_precision_results_by_species$total_time)
    
    # The distribution of total_time is normal, but the distribution of precision values is not. 
    # Therefore, we have to transform it before we can perform the Pearson's correlation analysis.
    # Nonetheless, neither sqrt nor logarithmic nor 1/x transformations transform the precision distribution into a normal.
    # Thus, a Spearman's correlation test will have to be performed insted of a Pearson's one.
    
    cor.test(selected_recall_and_precision_results_by_species$total_time, selected_recall_and_precision_results_by_species$mean_id_precision, method = "spearman")
    # Spearman's correlation coefficient is 0.778, with a p-value of 4.24e-07, which indicates that total singing time significantly improves precision results.

    
    # Plotting BirdNET precision by species
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_species_with_at_least_10_detections.jpeg"), width = 7500, height = 2400, res = 500)
    ggplot(precision_results_by_species %>%
             filter(total_detections >= 10), aes(x=reorder(reorder(species_number, -total_detections), mean_rec_precision))) +
      geom_bar(position = position_dodge(), 
               stat = "identity",
               color = 'black', 
               aes(y = mean_rec_precision, fill = "rec_precision")) +
      geom_line(stat = "identity", 
                size = 1.5, 
                group = 1,
                color = '#1d416e', 
                aes(y = mean_id_precision, fill = 'id_precision')) +
      scale_fill_manual(name = "Precision type",
                        breaks = c("id_precision", "rec_precision"),
                        values = c("id_precision" = "#1d416e", "rec_precision" = "#abe1ff")) +
      xlab("Species") +
      ylab("Mean precision") +
      ggtitle(paste0("Mean precision by bird species in ", algorithm, " identifications — only species with ≥10 detections")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"))
    dev.off()
  }
  
  
  #--------------------------Recall analysis-----------------------------------
  
  recall_results_by_species <- recall_results_by_species %>%
    left_join(recall_and_precision_results_by_species %>%
                select(species, original_time),
              by = "species") %>%
    mutate(minutes = round(round(original_time, 0)/60, 0),
           seconds = round(round(original_time, 0) %% 60, 0),
           species_number = paste0(species, " (n = ", total_manual_detections, ")"),
           species_time = paste0(species, " (t = ", case_when(minutes > 0 ~ paste0(minutes, "m "),
                                                              TRUE ~ ""), round(seconds, 0), "s)"))
    
  
  # Plotting BirdNET recall by species
  
  jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_species_", confidence_labels[i], ".jpeg"), width = 7500, height = 2400, res = 500)
  ggplot(recall_results_by_species, aes(x=reorder(reorder(species_time, -total_manual_detections), mean_rec_recall))) +
    geom_bar(position = position_dodge(), 
             stat = "identity",
             color = 'black', 
             aes(y = mean_rec_recall, fill = "rec_recall")) +
    geom_line(stat = "identity", 
              size = 1.5, 
              group = 1,
              color = '#1d416e', 
              aes(y = mean_id_recall, fill = 'id_recall')) +
    scale_fill_manual(name = "Recall type",
                      breaks = c("id_recall", "rec_recall"),
                      values = c("id_recall" = "#1d416e", "rec_recall" = "#abe1ff")) +
    ylim(0, 1) +
    xlab("Species") +
    ylab("Mean recall") +
    ggtitle(paste0("Recall by ", algorithm, " by bird species", confidence_plot_titles[i])) +
    theme_bw()  +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"))
  dev.off()
  
  # Only species having sung or called at least 30 seconds
  
  if (i == 1) {
    
    # Plotting BirdNET recall by total singing/calling time
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_total_time.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_and_precision_results_by_species, aes(x=total_time, y=mean_rec_recall, label=label)) +
      geom_point() + 
      geom_text_repel(size = 2.5,
                      nudge_y = 0.02) +
      xlab("Time (s), log2 scale") +
      ylab("Mean rec_recall") +
      ggtitle(paste0(algorithm, " rec_recall by species total singing/calling time"))
    dev.off()
    
    recall_results_with_total_time_as_factor <- recall_and_precision_results_by_species %>%
      mutate(time_factor = case_when(original_time < 10 ~ "0-10",
                                               original_time >= 10 & original_time < 60 ~ "10-60",
                                               original_time >= 60 & original_time < 500 ~ "60-500",
                                               TRUE ~ "≥500"))
    
    recall_results_with_total_time_as_factor$time_factor <- factor(recall_results_with_total_time_as_factor$time_factor, 
                                                             levels=c("0-10", "10-60", "60-500", "≥500"))
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_total_time_boxplot.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_results_with_total_time_as_factor, aes(x=time_factor, y=mean_rec_recall)) + 
      geom_boxplot() +
      xlab("Time (s)") +
      ylab("Mean rec_recall") +
      ggtitle(paste0(algorithm, " rec_recall by species total singing/calling time"))
    dev.off()
    
    recall_results_with_time_per_recording_as_factor <- recall_and_precision_results_by_species %>%
      left_join(total_birds_detected %>%
                  select(species, n_recordings_clearly_detected),
                by = "species") %>%
      mutate(time_per_recording = original_time / n_recordings_clearly_detected,
             time_per_recording_factor = case_when(time_per_recording < 5 ~ "<5",
                                                   time_per_recording >= 5 & time_per_recording < 10 ~ "5-10",
                                                   time_per_recording >= 10 & time_per_recording < 20 ~ "10-20",
                                                   TRUE ~ "≥20"))
    
    recall_results_with_time_per_recording_as_factor$time_per_recording_factor <- factor(recall_results_with_time_per_recording_as_factor$time_per_recording_factor, 
                                                                   levels=c("<5", "5-10", "10-20", "≥20"))
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_time_per_recording_boxplot.jpeg"), width = 3300, height = 2400, res = 500)
    ggplot(recall_results_with_time_per_recording_as_factor, aes(x=time_per_recording_factor, y=mean_rec_recall)) + 
      geom_boxplot() +
      xlab("Time (s)") +
      ylab("Mean rec_recall") +
      ggtitle(paste0(algorithm, " rec_recall by species average singing/calling time per recording"))
    dev.off()
    
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_species_with_at_least_30s.jpeg"), width = 7500, height = 2400, res = 500)
    ggplot(recall_results_by_species_with_at_least_30s, aes(x=reorder(reorder(species_time, -total_manual_detections), mean_rec_recall))) +
      geom_bar(position = position_dodge(), 
               stat = "identity",
               color = 'black', 
               aes(y = mean_rec_recall, fill = "rec_recall")) +
      geom_line(stat = "identity", 
                size = 1.5, 
                group = 1,
                color = '#1d416e', 
                aes(y = mean_id_recall, fill = 'id_recall')) +
      scale_fill_manual(name = "Recall type",
                        breaks = c("id_recall", "rec_recall"),
                        values = c("id_recall" = "#1d416e", "rec_recall" = "#abe1ff")) +
      ylim(0, 1) +
      xlab("Species") +
      ylab("Mean recall") +
      ggtitle(paste0("Mean recall by ", algorithm, " by bird species with ≥30 seconds of total time")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"))
    dev.off()
    
    recall_results_by_available_recordings_for_selected_species <- recall_results_by_species_with_at_least_30s %>%
      left_join(online_foreground_recordings_by_species,
                by = "species") %>%
      unique()
    
    jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_(proportion_of_time_detected)_by_online_recordings_(only_species_with_at_least_30_seconds).jpeg"), width = 4000, height = 3000, res = 500)
    ggplot(recall_results_by_available_recordings_for_selected_species, aes(x=log2(total_online_recordings), y = mean_id_recall, label=label)) +
      geom_point(color = "black") + 
      geom_text_repel(size = 2.5,
                      nudge_y = 0.02) +
      geom_smooth(method = lm, se = TRUE) +
      xlab("Number of recordings, log2 scale") +
      ylab("Mean id_recall") +
      ggtitle(paste0("Mean id_recall by ", algorithm, "\nby number of foreground recordings on XC and ML for each species\n(only species with ≥30 seconds of total vocalization time)")) +
      theme_bw()
    dev.off()
    
    # Checking the normality of the recall distribution to determine whether we can perform a Pearson correlation analysis or not
    shapiro.test(recall_results_by_available_recordings_for_selected_species$mean_rec_recall)
    hist(recall_results_by_available_recordings_for_selected_species$mean_rec_recall)
    
    shapiro.test(log2(recall_results_by_available_recordings_for_selected_species$total_online_recordings))
    hist(log2(recall_results_by_available_recordings_for_selected_species$total_online_recordings))
    
    # The recall distribution is not normal. Therefore, we can't perform Pearson's correlation analysis.
    
    cor.test(log2(recall_results_by_available_recordings_for_selected_species$total_online_recordings), recall_results_by_available_recordings_for_selected_species$mean_rec_recall, method = "spearman")
    # Spearman's correlation coefficient is -0.477, with a p-value of 0.002, which indicates that the total number of foreground recordings on Xeno-canto and the Macaulay Library does have a significant negative influence on recall results.
  }
}


#-------------------------- F1 score analysis ----------------------

BirdNET_PR_results_by_species[is.na(BirdNET_PR_results_by_species)] <- 0

f_score_results_by_available_recordings_for_selected_species <- BirdNET_PR_results_by_species %>%
  filter(confidence_threshold == 0.1) %>%
  # mutate(species = gsub("Dendrocoptes medius", "Leiopicus medius", species)) %>%
  right_join(recall_results_by_available_recordings_for_selected_species %>%
               select(label, species, total_online_recordings)) %>%
  filter(species %in% precision_results_by_available_recordings_for_selected_species$species) %>%
  # mutate(label = gsub("Den med", "Lei med", label)) %>%
  group_by(label, total_online_recordings) %>%
  summarize(f1_score = mean(F_score1),
            f025_score = mean(F_score2))
  
jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/f1_score_by_online_recordings_(only_species_with_at_least_30_seconds_and_at_least_10_detections).jpeg"), width = 4000, height = 3000, res = 500)
ggplot(f_score_results_by_available_recordings_for_selected_species, aes(x=log2(total_online_recordings), y=f1_score, label=label)) +
  geom_point(color = "black") + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method = lm, se = TRUE) +
  xlab("Number of recordings, log2 scale") +
  ylab("Mean F1 score") +
  ggtitle("F1 score by the number of foreground recordings on XC and ML for each species\n(only species with ≥30 seconds of total vocalization time and ≥10 detections)") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/f025_score_by_online_recordings_(only_species_with_at_least_30_seconds_and_at_least_10_detections).jpeg"), width = 4000, height = 3000, res = 500)
ggplot(f_score_results_by_available_recordings_for_selected_species, aes(x=log2(total_online_recordings), y=f025_score, label=label)) +
  geom_point(color = "black") + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method = lm, se = TRUE) +
  xlab("Number of recordings, log2 scale") +
  ylab("Mean F0.25 score") +
  ggtitle("F0.25 score by the number of foreground recordings on XC and ML for each species\n(only species with ≥30 seconds of total vocalization time and ≥10 detections)") +
  theme_bw()
dev.off()

selected_recall_results_by_available_recordings_for_selected_species <- BirdNET_PR_results_by_species %>%
  filter(confidence_threshold == 0.1) %>%
  # mutate(species = gsub("Dendrocoptes medius", "Leiopicus medius", species)) %>%
  right_join(recall_results_by_available_recordings_for_selected_species %>%
               select(label, species, total_online_recordings)) %>%
  # mutate(label = gsub("Den med", "Lei med", label)) %>%
  group_by(label, total_online_recordings) %>%
  summarize(rec_recall = mean(recall))

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_online_recordings_(only_species_with_at_least_30_seconds).jpeg"), width = 4000, height = 3000, res = 500)
ggplot(selected_recall_results_by_available_recordings_for_selected_species, aes(x=log2(total_online_recordings), y=rec_recall, label=label)) +
  geom_point(color = "black") + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method = lm, se = TRUE) +
  xlab("Number of recordings, log2 scale") +
  ylab("Mean rec_recall") +
  ggtitle("Recall score by the number of foreground recordings on XC and ML for each species\n(only species with ≥30 seconds of total vocalization time)") +
  theme_bw()
dev.off()

selected_precision_results_by_available_recordings_for_selected_species <- BirdNET_PR_results_by_species %>%
  filter(confidence_threshold == 0.1) %>%
  # mutate(species = gsub("Dendrocoptes medius", "Leiopicus medius", species)) %>%
  left_join(online_foreground_recordings_by_species %>%
               select(species, total_online_recordings)) %>%
  left_join(species_labels,
            by = "species") %>%
  filter(species %in% precision_results_by_available_recordings_for_selected_species$species,
         !is.na(species)) %>%
  group_by(label, total_online_recordings) %>%
  # mutate(label = gsub("Den med", "Lei med", label)) %>%
  summarize(rec_precision = mean(precision))

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_online_recordings_(only_species_with_at_least_10_detections).jpeg"), width = 4000, height = 3000, res = 500)
ggplot(selected_precision_results_by_available_recordings_for_selected_species, aes(x=log2(total_online_recordings), y=rec_precision, label=label)) +
  geom_point(color = "black") + 
  geom_text_repel(size = 2.5,
                  nudge_y = 0.02) +
  geom_smooth(method = lm, se = TRUE) +
  xlab("Number of recordings, log2 scale") +
  ylab("Mean rec_precision") +
  ggtitle("Precision score by the number of foreground recordings on XC and ML for each species\n(only species with ≥10 detections)") +
  theme_bw()
dev.off()

# Checking the normality of the F1-score distribution to determine whether we can perform a Pearson correlation analysis or not
shapiro.test(f_score_results_by_available_recordings_for_selected_species$f1_score)
hist(f_score_results_by_available_recordings_for_selected_species$f1_score)

shapiro.test(log2(f_score_results_by_available_recordings_for_selected_species$total_online_recordings))
hist(log2(f_score_results_by_available_recordings_for_selected_species$total_online_recordings))

# The f1-score distribution is not normal. Therefore, we can't perform Pearson's correlation analysis.

cor.test(log2(f_score_results_by_available_recordings_for_selected_species$total_online_recordings), f_score_results_by_available_recordings_for_selected_species$f1_score, method = "spearman")
# Spearman's correlation coefficient is -0.58, with a p-value of 3.4e-4, which indicates that the total number of foreground recordings on Xeno-canto and the Macaulay Library does have a significant negative influence on F1 score results.

cor.test(log2(f_score_results_by_available_recordings_for_selected_species$total_online_recordings), f_score_results_by_available_recordings_for_selected_species$f025_score, method = "spearman")
# Spearman's correlation coefficient is -0.28, with a p-value of 0.108, which indicates that the total number of foreground recordings on Xeno-canto and the Macaulay Library doesn't have a significant influence on F0.25 score results.

cor.test(log2(selected_precision_results_by_available_recordings_for_selected_species$total_online_recordings), selected_precision_results_by_available_recordings_for_selected_species$rec_precision, method = "spearman")
# Spearman's correlation coefficient is 0.438, with a p-value of 4.66e-4, which indicates that the total number of foreground recordings on Xeno-canto and the Macaulay Library has a significant influence on precision results.

cor.test(log2(selected_recall_results_by_available_recordings_for_selected_species$total_online_recordings), selected_recall_results_by_available_recordings_for_selected_species$rec_recall, method = "spearman")
# Spearman's correlation coefficient is -0.489, with a p-value of 1.17e-3, which indicates that the total number of foreground recordings on Xeno-canto and the Macaulay Library has a significant influence on recall results.

  

#--------------Global detection results by confidence threshold-----------------

annotations <- read.csv("../CSVs/annotations.csv")
recordings <- read.csv("../CSVs/recordings_extended.csv")

for (i in 1:length(confidence_levels)) {
  temp_global_detection_results <- BirdNET_precision_results %>%
    filter(confidence >= confidence_levels[i]) %>%
    group_by(recording_id, species) %>%
    summarize(correct_identifications = sum(as.numeric(correct_identification)),
              incorrect_identifications = sum(as.numeric(!correct_identification))) %>%
    mutate(correct_identifications = case_when(correct_identifications > 0 ~ 1,
                                               TRUE ~ 0),
           incorrect_identifications = case_when(incorrect_identifications > 0 ~ 1,
                                                 TRUE ~ 0)) %>%
    group_by(species) %>%
    summarize(recordings_where_correctly_identified = sum(correct_identifications),
              recordings_where_incorrectly_identified = sum(incorrect_identifications))
  
  temp_detection_results_by_habitat <- BirdNET_precision_results %>%
    left_join(recordings %>%
                select(recording_id, habitat),
              by = "recording_id") %>%
    filter(confidence >= confidence_levels[i]) %>%
    group_by(habitat, species) %>%
    summarize(correct_identifications = sum(as.numeric(correct_identification)),
              incorrect_identifications = sum(as.numeric(!correct_identification))) %>%
    mutate(correct_identifications = case_when(correct_identifications > 0 ~ 1,
                                               TRUE ~ 0),
           incorrect_identifications = case_when(incorrect_identifications > 0 ~ 1,
                                                 TRUE ~ 0)) %>%
    group_by(habitat) %>%
    summarize(correctly_identified_species = sum(correct_identifications),
              incorrectly_identified_species = sum(incorrect_identifications))
  
  if (i == 1) {
    global_BirdNET_detection_results_1st_threshold <- temp_global_detection_results
    BirdNET_detection_results_by_habitat_1st_threshold <- temp_detection_results_by_habitat
  } else if (i == 2) {
    global_BirdNET_detection_results_2nd_threshold <- temp_global_detection_results
    BirdNET_detection_results_by_habitat_2nd_threshold <- temp_detection_results_by_habitat
  } else {
    global_BirdNET_detection_results_3rd_threshold <- temp_global_detection_results
    BirdNET_detection_results_by_habitat_3rd_threshold <- temp_detection_results_by_habitat
  }
}

global_detection_results <- total_birds_detected %>%
  filter(species != "Unidentified bird") %>%
  select(species, n_recordings_clearly_detected) %>%
  full_join(global_BirdNET_detection_results_1st_threshold,
            by = "species") %>%
  rename("correctly_identified_1st_threshold" = "recordings_where_correctly_identified",
         "incorrectly_identified_1st_threshold" = "recordings_where_incorrectly_identified") %>%
  full_join(global_BirdNET_detection_results_2nd_threshold,
            by = "species") %>%
  rename("correctly_identified_2nd_threshold" = "recordings_where_correctly_identified",
         "incorrectly_identified_2nd_threshold" = "recordings_where_incorrectly_identified") %>%
  full_join(global_BirdNET_detection_results_3rd_threshold,
            by = "species") %>%
  rename("correctly_identified_3rd_threshold" = "recordings_where_correctly_identified",
         "incorrectly_identified_3rd_threshold" = "recordings_where_incorrectly_identified") %>%
  arrange(species) %>%
  ungroup()

global_detection_results[is.na(global_detection_results)] <- 0

global_detection_results <- global_detection_results %>%
  mutate(non_present_species_1st_threshold = as.numeric(n_recordings_clearly_detected == 0 & incorrectly_identified_1st_threshold > 0) * incorrectly_identified_1st_threshold,
         non_present_species_2nd_threshold = as.numeric(n_recordings_clearly_detected == 0 & incorrectly_identified_2nd_threshold > 0) * incorrectly_identified_2nd_threshold,
         non_present_species_3rd_threshold = as.numeric(n_recordings_clearly_detected == 0 & incorrectly_identified_3rd_threshold > 0) * incorrectly_identified_3rd_threshold)

global_detection_results <- rbind(global_detection_results, c("Total detections", 
                                                              sum(global_detection_results$n_recordings_clearly_detected), 
                                                              sum(global_detection_results$correctly_identified_1st_threshold), 
                                                              sum(global_detection_results$incorrectly_identified_1st_threshold), 
                                                              sum(global_detection_results$correctly_identified_2nd_threshold), 
                                                              sum(global_detection_results$incorrectly_identified_2nd_threshold), 
                                                              sum(global_detection_results$correctly_identified_3rd_threshold), 
                                                              sum(global_detection_results$incorrectly_identified_3rd_threshold), 
                                                              sum(global_detection_results$non_present_species_1st_threshold), 
                                                              sum(global_detection_results$non_present_species_2nd_threshold), 
                                                              sum(global_detection_results$non_present_species_3rd_threshold)))

global_detection_results <- rbind(global_detection_results, c("Total species", 
                                                              sum(as.numeric(global_detection_results$n_recordings_clearly_detected > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$correctly_identified_1st_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$incorrectly_identified_1st_threshold > 0)) - 1,  
                                                              sum(as.numeric(global_detection_results$correctly_identified_2nd_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$incorrectly_identified_2nd_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$correctly_identified_3rd_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$incorrectly_identified_3rd_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$non_present_species_1st_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$non_present_species_2nd_threshold > 0)) - 1, 
                                                              sum(as.numeric(global_detection_results$non_present_species_3rd_threshold > 0)) - 1))


global_detection_results_file_name <- file.path(paste0(CSV_path), "global_detection_results.csv")
write.csv(x=global_detection_results, file=global_detection_results_file_name, row.names = FALSE)


#-------------Global detection results by time of day and habitat---------------

bird_species_richness_by_recording_and_time <- annotations %>%
  left_join(recordings, by = "recording_id") %>%
  filter(confidence_level == 1, sound_category == "Bird") %>%
  select(recording_id, recording_date, recording_time, label) %>%
  unique() %>%
  group_by(recording_id, recording_date, recording_time) %>%
  summarize(n_bird_species = n())

bird_species_richness_by_time <- bird_species_richness_by_recording_and_time %>%
  group_by(recording_time) %>%
  summarize(mean_richness = mean(n_bird_species))


jpeg(file = "../R plots/Annotation analysis/number_of_species_detected_by_time_of_the_day.jpeg", width = 3000, height = 2400, res = 500)
ggplot(bird_species_richness_by_recording_and_time, aes(x = as.factor(recording_time), y = n_bird_species, fill = as.factor(recording_time))) + 
  geom_boxplot() +
  xlab("Time of the day") +
  ylab("Bird species detected") +
  ggtitle("Bird species detected per minute by time of the day") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#84B8E1", "#84B8E1", "#84B8E1"))
dev.off()

# Checking the normality of the distribution of the number of species detected per recording to determine whether we can perform an ANOVA analysis or not
shapiro.test(bird_species_richness_by_recording_and_time$n_bird_species)
hist(bird_species_richness_by_recording_and_time$n_bird_species)

# The distribution of the number of species per recording is not normal. Therefore, we have to transform the data before we can perform the ANOVA analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the distribution into a normal one.
# Thus, a Kruskal-Wallis test will have to be performed insted of an ANOVA.

kruskal.test(n_bird_species ~ recording_time, data = bird_species_richness_by_recording_and_time)

# Performing t-tests to check whether the differences between the number of species detected at 7am and at 1pm are statistically significant.
t.test((bird_species_richness_by_recording_and_time %>%
          filter(recording_time == "07:00:00"))$n_bird_species, 
       (bird_species_richness_by_recording_and_time %>%
          filter(recording_time == "13:00:00"))$n_bird_species)

# Same test, this time comparing the number of species detected at 7am and at 0am.
t.test((bird_species_richness_by_recording_and_time %>%
          filter(recording_time == "07:00:00"))$n_bird_species, 
       (bird_species_richness_by_recording_and_time %>%
          filter(recording_time == "00:00:00"))$n_bird_species)

bird_species_richness_by_recording_and_habitat <- annotations %>%
  left_join(recordings, by = "recording_id") %>%
  filter(confidence_level == 1, sound_category == "Bird") %>%
  select(station_id, habitat, label) %>%
  unique() %>%
  group_by(station_id, habitat) %>%
  summarize(n_bird_species = n())


jpeg(file = "../R plots/Annotation analysis/number_of_species_detected_by_habitat.jpeg", width = 3000, height = 2400, res = 500)
ggplot(bird_species_richness_by_recording_and_habitat, aes(x = as.factor(habitat), y = n_bird_species, fill = as.factor(habitat))) + 
  geom_boxplot() +
  xlab("Habitat") +
  ylab("Bird species detected") +
  ggtitle("Bird species detected per day by habitat") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#84B8E1", "#84B8E1", "#84B8E1", "#84B8E1", "#84B8E1"))
dev.off()

# Checking the normality of the distribution of the number of species detected per recording to determine whether we can perform an ANOVA analysis or not
shapiro.test(bird_species_richness_by_recording_and_habitat$n_bird_species)
hist(bird_species_richness_by_recording_and_habitat$n_bird_species)

# The distribution of the number of species per recording can't be proven not to be normal. Therefore, we don't have to transform the data before we can perform the ANOVA analysis.
anova_species_richness_by_habitat <- aov(n_bird_species ~ as.factor(habitat), data = bird_species_richness_by_recording_and_habitat)
summary(anova_species_richness_by_habitat)

n_recordings_by_habitat <- recordings %>%
  count(habitat) %>%
  rename("Recordings analyzed" = "n")

bird_species_richness_by_habitat <- annotations %>%
  left_join(recordings, by = "recording_id") %>%
  filter(confidence_level == 1, sound_category == "Bird") %>%
  select(recording_id, recording_date, habitat, label) %>%
  unique() %>%
  group_by(habitat, label) %>%
  summarize(n_detections = n()) %>%
  count(habitat) %>%
  rename("Species annotated" = "n")


detection_results_by_habitat <- n_recordings_by_habitat %>%
  left_join(bird_species_richness_by_habitat,
            by = "habitat") %>%
  left_join(BirdNET_detection_results_by_habitat_1st_threshold,
            by = "habitat") %>%
  rename("Correctly_identified_1st_threshold" = "correctly_identified_species",
         "Incorrectly_identified_1st_threshold" = "incorrectly_identified_species") %>%
  left_join(BirdNET_detection_results_by_habitat_2nd_threshold,
            by = "habitat") %>%
  rename("Correctly_identified_2nd_threshold" = "correctly_identified_species",
         "Incorrectly_identified_2nd_threshold" = "incorrectly_identified_species") %>%
  left_join(BirdNET_detection_results_by_habitat_3rd_threshold,
            by = "habitat") %>%
  rename("Correctly_identified_3rd_threshold" = "correctly_identified_species",
         "Incorrectly_identified_3rd_threshold" = "incorrectly_identified_species",
         "Habitat" = "habitat")


detection_results_by_habitat_file_name <- file.path(paste0("../CSVs", algorithm_text, BirdNET_version, ds_text, filtered_text), "detection_results_by_habitat.csv")
write.csv(x=detection_results_by_habitat, file=detection_results_by_habitat_file_name, row.names = FALSE)



# It calculates the Precision-Recall AUC for each habitat

BirdNET_PR_results_by_recording <- read.csv(paste0(CSV_path, "/", algorithm, "_PR_results_by_recording.csv"))
recording_info_extended <- read.csv("../CSVs/recordings_extended.csv")

BirdNET_PR_results_by_recording[is.na(BirdNET_PR_results_by_recording)] = 0

BirdNET_PR_results_by_recording_and_habitat <- BirdNET_PR_results_by_recording %>%
  mutate(confidence_threshold = as.numeric(confidence_threshold)) %>%
  left_join(recording_info_extended %>%
              select(recording_id, habitat),
            by = "recording_id") %>%
  left_join(recording_acoustic_data %>%
              select(recording_id, NDSI_left),
            by = "recording_id")

pr_auc_by_recording <- data.frame()

for (i in 1:nrow(recordings)) {
  BirdNET_PR_results_by_selected_recording <- BirdNET_PR_results_by_recording_and_habitat %>%
    filter(recording_id == recordings[i,]$recording_id)
  pr_auc <- abs(trapz(BirdNET_PR_results_by_selected_recording$precision, BirdNET_PR_results_by_selected_recording$recall))
  pr_auc_by_recording <- rbind(pr_auc_by_recording, c(recordings[i,]$recording_id, pr_auc))
}

colnames(pr_auc_by_recording) <- c("recording_id", "AUC")


BirdNET_PR_results_by_recording_and_habitat <- BirdNET_PR_results_by_recording_and_habitat %>%
  left_join(pr_auc_by_recording,
            by = "recording_id") %>%
  mutate(habitat = as.factor(habitat)) %>%
  select(recording_id, habitat, NDSI_left, recall, precision, confidence_threshold) %>%
  rename("rec_recall" = "recall",
         "rec_precision" = "precision")

BirdNET_PR_results_by_recording_and_habitat_conf10 <- BirdNET_PR_results_by_recording_and_habitat %>%
  filter(confidence_threshold == 0.1) %>%
  select(-confidence_threshold)

BirdNET_PR_results_by_recording_and_habitat_conf35 <- BirdNET_PR_results_by_recording_and_habitat %>%
  filter(confidence_threshold == 0.35) %>%
  select(-confidence_threshold)

# Checking the normality of the distribution of the recall and precision variables to determine whether we can perform an ANOVA analysis or not
shapiro.test(BirdNET_PR_results_by_recording_and_habitat_conf10$rec_recall)
hist(BirdNET_PR_results_by_recording_and_habitat_conf10$rec_recall)

shapiro.test(BirdNET_PR_results_by_recording_and_habitat_conf10$rec_precision)
hist(BirdNET_PR_results_by_recording_and_habitat_conf10$rec_precision)

# The distribution of recall and precision values is not normal. Therefore, we have to transform them before we can perform the ANOVA analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the recall and precision distributions into normal ones.
# Thus, a Kruskal-Wallis test will have to be performed insted of an ANOVA.

kruskal.test(rec_recall ~ habitat, data = BirdNET_PR_results_by_recording_and_habitat_conf10)
kruskal.test(rec_precision ~ habitat, data = BirdNET_PR_results_by_recording_and_habitat_conf10)

BirdNET_PR_results_by_recording_and_habitat_conf10 <- BirdNET_PR_results_by_recording_and_habitat_conf10 %>%
  mutate(rec_F1_score = 2 * 1 * rec_precision * rec_recall / (rec_precision + rec_recall))

BirdNET_PR_results_by_recording_and_habitat_conf10[is.na(BirdNET_PR_results_by_recording_and_habitat_conf10)] = 0

kruskal.test(rec_F1_score ~ habitat, data = BirdNET_PR_results_by_recording_and_habitat_conf10)

# Habitat does not seem to have a significant impact on neither recall nor precision. 

BirdNET_recall_results_by_recording_conf10 <- melt(setDT(BirdNET_PR_results_by_recording_and_habitat_conf10 %>%
                                                  select(recording_id, rec_recall)), id.vars = "recording_id", variable.name = "metric", value.name = "value")
BirdNET_precision_results_by_recording_conf10 <- melt(setDT(BirdNET_PR_results_by_recording_and_habitat_conf10 %>%
                                                  select(recording_id, rec_precision)), id.vars = "recording_id", variable.name = "metric", value.name = "value")

BirdNET_recall_results_by_recording_conf35 <- melt(setDT(BirdNET_PR_results_by_recording_and_habitat_conf35 %>%
                                                           select(recording_id, rec_recall)), id.vars = "recording_id", variable.name = "metric", value.name = "value")
BirdNET_precision_results_by_recording_conf35 <- melt(setDT(BirdNET_PR_results_by_recording_and_habitat_conf35 %>%
                                                              select(recording_id, rec_precision)), id.vars = "recording_id", variable.name = "metric", value.name = "value")

BirdNET_recall_and_precision_results_by_habitat_conf35 <- union_all(BirdNET_recall_results_by_recording_conf35, BirdNET_precision_results_by_recording_conf35) %>%
  left_join(BirdNET_PR_results_by_recording_and_habitat_conf35 %>%
    select(recording_id, habitat),
    by = "recording_id")

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/precision_and_recall_by_habitat.jpeg"), width = 3000, height = 2400, res = 500)
ggplot(BirdNET_recall_and_precision_results_by_habitat_conf35, aes(x = habitat, y = value, fill = metric)) +
  geom_boxplot() +
  xlab("Habitat") +
  ylab("Value") +
  labs(fill = "Metric") +
  ggtitle("rec_precision and rec_recall distributions by dominant habitat") +
  theme_bw() +
  scale_fill_manual(values = c("#E4959C", "#95B5DA"))
dev.off()

# Checking the normality of the NDSI index distribution to determine whether we can perform a Pearson correlation analysis or not
shapiro.test(BirdNET_PR_results_by_recording_and_habitat_conf10$NDSI_left)
hist(BirdNET_PR_results_by_recording_and_habitat_conf10$NDSI_left)

# The distribution of NDSI, recall and precision values is not normal. 
# Therefore, we have to transform them before we can perform the Pearson's correlation analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the NDSI, recall or precision distributions into normal ones.
# Thus, a Spearman's correlation test will have to be performed insted of a Pearson's one.


# The NDSI index has a significant influence on the precision of the algorithm (r = 0.128, p-value = 0.053)
cor.test(BirdNET_PR_results_by_recording_and_habitat_conf10$NDSI_left, BirdNET_PR_results_by_recording_and_habitat_conf10$rec_precision, method = "spearman")

# The NDSI index has a significant influence on the recall of the algorithm (r = 0.283, p-value = 1.463e-5)
cor.test(BirdNET_PR_results_by_recording_and_habitat_conf10$NDSI_left, BirdNET_PR_results_by_recording_and_habitat_conf10$rec_recall, method = "spearman")

BirdNET_F1_results_by_recording_and_habitat_conf10 <- BirdNET_PR_results_by_recording_and_habitat_conf10 %>%
  mutate(rec_F1_score = 2 * 1 * rec_precision * rec_recall / (rec_precision + rec_recall))

BirdNET_F1_results_by_recording_and_habitat_conf10[is.na(BirdNET_F1_results_by_recording_and_habitat_conf10)] = 0

# The NDSI index has a significant influence on the F1-score of the algorithm (r = 0.144, p-value = 0.03)
cor.test(BirdNET_F1_results_by_recording_and_habitat_conf10$NDSI_left, BirdNET_F1_results_by_recording_and_habitat_conf10$rec_F1, method = "spearman")


jpeg(file = paste0(R_plots_path, "/", algorithm, " precision analysis/precision_by_anthropic_noise.jpeg"), width = 3200, height = 2500, res = 500)
ggplot(BirdNET_PR_results_by_recording_and_habitat_conf10, aes(x=NDSI_left, y=rec_precision)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, se = TRUE) +
  xlab("NDSI score") +
  ylab("rec_precision") +
  ggtitle(paste0(algorithm, " precision by NDSI score")) +
  theme(panel.background = element_blank(),
        axis.line = element_line())
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/recall_by_anthropic_noise.jpeg"), width = 3200, height = 2500, res = 500)
ggplot(BirdNET_PR_results_by_recording_and_habitat_conf10, aes(x=NDSI_left, y=rec_recall)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, se = TRUE) +
  xlab("NDSI score") +
  ylab("rec_recall") +
  ggtitle(paste0(algorithm, " recall by NDSI score")) +
  theme(panel.background = element_blank(),
        axis.line = element_line())
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/F1-score_by_anthropic_noise.jpeg"), width = 3200, height = 2500, res = 500)
ggplot(BirdNET_F1_results_by_recording_and_habitat_conf10, aes(x=NDSI_left, y=rec_F1_score)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, se = TRUE) +
  xlab("NDSI score") +
  ylab("rec_F1-score") +
  ggtitle(paste0(algorithm, " F1-score by NDSI score")) +
  theme(panel.background = element_blank(),
        axis.line = element_line())
dev.off()

# ------------------------Recall by sample size analysis------------------------

global_detection_results <- read.csv(paste0(CSV_path, "/global_detection_results_by_recording_subset.csv"))

grouped_global_detection_results <- global_detection_results %>%
  group_by(n_recordings_sampled, minimum_confidence_threshold) %>%
  summarize(ds_TPR = mean(TPR),
            ds_FPR = mean(FPR),
            ds_F1 = mean(F1),
            ds_TPR_to_FPR_ratio = ds_TPR/ds_FPR,
            ds_TPR_minus_FPR = mean(TPR-FPR),
            proportion_of_correct_species = mean(n_species_correctly_detected / (n_species_correctly_detected + n_species_incorrectly_detected)))

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/ds_TPR_by_n_recordings_sampled.jpeg"), width = 4000, height = 2500, res = 500)
ggplot(grouped_global_detection_results, aes(x = as.numeric(n_recordings_sampled), y = ds_TPR, fill = as.factor(minimum_confidence_threshold))) +
  geom_bar(position = position_nudge(x = 0.5), stat = "identity",
           colour = 'black') +
  xlab("Number of recordings sampled") +
  ylab("ds_TPR") +
  ylim(0, 0.75) +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("True Positive Rate at the dataset level by number of recordings sampled") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/ds_FPR_by_n_recordings_sampled.jpeg"), width = 4000, height = 2500, res = 500)
ggplot(grouped_global_detection_results, aes(x = as.numeric(n_recordings_sampled), y = ds_FPR, fill = as.factor(minimum_confidence_threshold))) +
  geom_bar(position = position_nudge(x = 0.5), stat = "identity",
           colour = 'black') +
  xlab("Number of recordings sampled") +
  ylab("ds_FPR") +
  ylim(0, 0.75) +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("False Positive Rate at the dataset level by number of recordings sampled") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/ds_F1-score_by_n_recordings_sampled_stacked_bar_plot.jpeg"), width = 4000, height = 2500, res = 500)
ggplot(grouped_global_detection_results, aes(x = as.numeric(n_recordings_sampled), y = ds_F1, fill = as.factor(minimum_confidence_threshold))) +
  geom_bar(position = position_nudge(x = 0.5), stat = "identity",
           colour = 'black') +
  xlab("Number of recordings sampled") +
  ylab("ds_F1-score") +
  ylim(0, 0.75) +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("F1-score at the dataset level by number of recordings sampled") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/ds_F1-score_by_n_recordings_sampled_unstacked_bar_plot.jpeg"), width = 5000, height = 3200, res = 500)
ggplot(grouped_global_detection_results, aes(x = as.numeric(n_recordings_sampled), y = ds_F1, fill = as.factor(minimum_confidence_threshold))) +
  geom_bar(position = "dodge", stat = "identity",
           colour = 'black') +
  xlab("Number of recordings sampled") +
  ylab("ds_F1-score") +
  ylim(0, 0.75) +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("F1-score at the dataset level by number of recordings sampled") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/ds_TPR_to_FPR_ratio_by_n_recordings_sampled.jpeg"), width = 4000, height = 2500, res = 500)
ggplot(grouped_global_detection_results %>%
         arrange(desc(minimum_confidence_threshold)), aes(x = as.numeric(n_recordings_sampled), y = ds_TPR_to_FPR_ratio, fill = as.factor(minimum_confidence_threshold))) +
  geom_bar(position = position_nudge(x = 0.5), 
           stat = "identity",
           colour = 'black') +
  xlab("Number of recordings sampled") +
  ylab("ds_TPR to ds_FPR ratio") +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("Ratio between TPR and FPR at the dataset level by number of recordings sampled") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/ds_TPR_minus_FPR_by_n_recordings_sampled.jpeg"), width = 4000, height = 2500, res = 500)
ggplot(grouped_global_detection_results, aes(x = as.numeric(n_recordings_sampled), y = ds_TPR_minus_FPR)) +
  geom_point(position = position_nudge(x = 0.5), 
           stat = "identity",
           color = 'black',
           shape=21, 
           size=2, 
           aes(fill = as.factor(minimum_confidence_threshold))) +
  xlab("Number of recordings sampled") +
  ylab("ds_TPR - ds_FPR") +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("TPR minus FPR at the dataset level by number of recordings sampled") +
  theme_bw()
dev.off()

jpeg(file = paste0(R_plots_path, "/", algorithm, " recall analysis/proportion_of_correct_species_by_n_recordings_sampled.jpeg"), width = 4000, height = 2500, res = 500)
ggplot(grouped_global_detection_results, aes(x = as.numeric(n_recordings_sampled), y = proportion_of_correct_species)) +
  geom_point(position = position_nudge(x = 0.5), 
             stat = "identity",
             color = 'black',
             shape=21, 
             size=2, 
             aes(fill = as.factor(minimum_confidence_threshold))) +
  xlab("Number of recordings sampled") +
  ylab("Proportion of species detected \nthat are actually present") +
  labs(fill = "Minimum confidence\nthreshold used") +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7")) +
  ggtitle("Proportion of species detected by BirdNET that are actually present \nin the acoustic dataset, by number of recordings sampled") +
  theme_bw()
dev.off()

ds_TPR_lm <- lm(TPR ~ log(n_recordings_sampled) + as.numeric(minimum_confidence_threshold), data = global_detection_results)
summary(ds_TPR_lm)
# It appears that there is a linear correlation between the TPR at the dataset level and the logarithm of the number of recordings sampled
# after controlling for the minimum confidence threshold used (p < 2e-16)

global_detection_results_adapted_for_lm <- global_detection_results %>%
  mutate(log_n_recordings_sampled = log(n_recordings_sampled),
         num_minimum_confidence_threshold = as.numeric(minimum_confidence_threshold))

# Getting the R² corresponding to each individual predictor variable instead of getting the R² corresponding to the whole model
sapply(c("log_n_recordings_sampled", "num_minimum_confidence_threshold"), 
       function(nm) summary(lm(global_detection_results_adapted_for_lm[c("TPR", nm)]))$adj.r.squared)
# Adjusted R² of 0.376 corresponding to the logarithm of the number of recordings sampled

ds_FPR_lm <- lm(FPR ~ log(n_recordings_sampled) + as.numeric(minimum_confidence_threshold), data = global_detection_results)
summary(ds_FPR_lm)
# The same does seem to be true as well of the FPR at the dataset level (p < 2e-16)

sapply(c("log_n_recordings_sampled", "num_minimum_confidence_threshold"), 
       function(nm) summary(lm(global_detection_results_adapted_for_lm[c("FPR", nm)]))$adj.r.squared)
# Adjusted R² of 0.095 corresponding to the logarithm of the number of recordings sampled

ds_F1_lm <- lm(F1 ~ log(n_recordings_sampled) + as.numeric(minimum_confidence_threshold), data = global_detection_results)
summary(ds_F1_lm)
# It appears that there is a linear correlation between the F1-score at the dataset level and the logarithm of the number of recordings sampled
# after controlling for the minimum confidence threshold used (p < 2e-16)

sapply(c("log_n_recordings_sampled", "num_minimum_confidence_threshold"), 
       function(nm) summary(lm(global_detection_results_adapted_for_lm[c("F1", nm)]))$adj.r.squared)
# Adjusted R² of 0.539 corresponding to the logarithm of the number of recordings sampled

# Precision and recall results by recorder type
BirdNET_PR_results_by_recorder <- BirdNET_PR_results_by_recording_and_habitat_conf10 %>%
  left_join(recording_info_extended %>%
              select(recording_id, ARU),
            by = "recording_id") %>%
  mutate(f1_score = 2 * rec_precision * rec_recall / (rec_precision + rec_recall))

BirdNET_PR_results_by_recorder[is.na(BirdNET_PR_results_by_recorder)] <- 0

aov_by_recorder_habitat_and_noise <- aov(f1_score ~ ARU + NDSI_left + habitat, BirdNET_PR_results_by_recorder)
summary(aov_by_recorder_habitat_and_noise)

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/F1_scores_by_recorder.jpeg"), width = 3000, height = 2400, res = 500)
ggplot(BirdNET_PR_results_by_recorder, aes(x = ARU, y = f1_score)) +
  geom_boxplot(fill = "#84B8E1") +
  xlab("Recorder") +
  ylab("rec_F1-score") +
  ggtitle("F1-scores at the recording level by recorder used") +
  theme_bw()
dev.off()

BirdNET_recall_and_precision_results_by_recorder <- union_all(BirdNET_recall_results_by_recording_conf35, BirdNET_precision_results_by_recording_conf35) %>%
  left_join(BirdNET_PR_results_by_recorder %>%
              select(recording_id, ARU),
            by = "recording_id") %>%
  rename("recorder" = "ARU")

jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/precision_and_recall_by_recorder.jpeg"), width = 3000, height = 2400, res = 500)
ggplot(BirdNET_recall_and_precision_results_by_recorder, aes(x = recorder, y = value, fill = metric)) +
  geom_boxplot() +
  xlab("Recorder") +
  ylab("Value") +
  labs(fill = "Metric") +
  ggtitle("rec_precision and rec_recall distributions by recorder used") +
  theme_bw() +
  scale_fill_manual(values = c("#E4959C", "#95B5DA"))
dev.off()


n_recordings_by_habitat_and_recorder <- BirdNET_PR_results_by_recorder %>%
  group_by(habitat, ARU) %>%
  summarize(n_recordings = n()) %>%
  arrange(ARU, habitat)


# Checking the normality of the distribution of the precision and recall to determine whether we can perform an ANOVA analysis or not
# if (length(BirdNET_PR_results_by_recorder$rec_precision) < 5000) {
#   shapiro.test(BirdNET_PR_results_by_recorder$rec_precision)
#   hist(BirdNET_PR_results_by_recorder$rec_precision)
# 
#   shapiro.test(BirdNET_PR_results_by_recorder$rec_recall)
#   hist(BirdNET_PR_results_by_recorder$rec_recall)
# }

# The distributions of precision and recall levels are not normal. Therefore, we have to transform the data before we can perform the ANOVA analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the distribution into a normal one.
# Thus, a Kruskal-Wallis test will have to be performed insted of an ANOVA.

# Since it isn't possible to control for confounding factors in the Kruskal-Wallis test,
# we remove data from alpine meadow recordings due to their higher levels of geophonic noise

kruskal.test(rec_precision ~ ARU, data = BirdNET_PR_results_by_recorder %>%
               filter(habitat != "alpine meadow"))
kruskal.test(rec_recall ~ ARU, data = BirdNET_PR_results_by_recorder %>%
               filter(habitat != "alpine meadow"))
kruskal.test(f1_score ~ ARU, data = BirdNET_PR_results_by_recorder %>%
               filter(habitat != "alpine meadow"))

kruskal.test(rec_precision ~ habitat, data = BirdNET_PR_results_by_recorder %>%
               filter(habitat != "alpine meadow"))
kruskal.test(rec_recall ~ habitat, data = BirdNET_PR_results_by_recorder %>%
               filter(habitat != "alpine meadow"))
kruskal.test(f1_score ~ habitat, data = BirdNET_PR_results_by_recorder %>%
               filter(habitat != "alpine meadow"))

# The recorder type does not seem to have a significant impact on recall but not on precision. 
# This effect would probably go away when controlling for habitat, but this would require using a Friedman's test.
# It's not possible to use a Friedman's test because our dataset is incomplete (e.g. there are no data for SMMicros in alpine meadows), so we cannot control for habitat.


jpeg(file = paste0(R_plots_path, "/", algorithm, " precision and recall analysis/f1_score_by_recorder_and_habitat.jpeg"), width = 3000, height = 2400, res = 500)
ggplot(BirdNET_PR_results_by_recorder %>%
         rename("Recorder" = "ARU"), aes(x = habitat, y = f1_score)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Habitat") +
  ylab("rec_F1-score") +
  geom_point(aes(color = Recorder), position=position_jitterdodge(dodge.width = 0.6, jitter.width = 0.25), alpha=0.95, size = 1.5) +
  ggtitle("F1-scores at the recording level by habitat and recorder used") +
  scale_color_manual(values = wes_palette("GrandBudapest2", n = 4)) +
  # scale_color_manual(values = c("#E4959C", "#95B5DA", "#795c00", "#96be25")) +
  theme_bw()
dev.off()


# Plotting PR and ROC curves based on results at the dataset level
global_detection_results_by_all_thresholds <- read.csv(paste0(CSV_path, "/global_detection_results_by_all_thresholds.csv")) %>%
  mutate(minimum_confidence_threshold = as.numeric(minimum_confidence_threshold))

global_detection_results_by_all_thresholds[is.na(global_detection_results_by_all_thresholds)] <- 0

ds_roc_auc <- -trapz(global_detection_results_by_all_thresholds$TPR, global_detection_results_by_all_thresholds$FPR)/max(global_detection_results_by_all_thresholds$FPR)
ds_pr_auc <- trapz(global_detection_results_by_all_thresholds$precision, global_detection_results_by_all_thresholds$TPR)/max(global_detection_results_by_all_thresholds$TPR)


overlap_label_regex <- qdapRegex::ex_between(overlap_label, "(", ")")[[1]]

ds_ROC_title <- paste0(case_when(algorithm == "Google" ~ "Google",
                                  TRUE ~ overlap_label_regex), filtered_label, " - ds_ROC curve, AUC = ", round(ds_roc_auc, digits = 3))
ds_PR_title <- paste0(case_when(algorithm == "Google" ~ "Google",
                                 TRUE ~ overlap_label_regex), filtered_label, " - ds_PR curve, AUC = ", round(ds_pr_auc, digits = 3))

# ROC curve
jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/ds_ROC_curve.jpeg")), width = 3300, height = 2400, res = 500)
ggplot(global_detection_results_by_all_thresholds, aes(x = FPR, y = TPR, color = minimum_confidence_threshold)) +
  geom_point() + 
  guides(color = guide_colorbar(reverse = FALSE, barheight = 10)) +
  xlab("ds_FPR") +
  ylab("ds_TPR") +
  ylim(c(0, 1)) +
  xlim(c(0, 1)) +
  labs(color = "Confidence \n  threshold") +
  ggtitle(ds_ROC_title) +   
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
jpeg(file = paste0("../R plots", algorithm_text, BirdNET_version, ds_text, filtered_text, overlap_text, paste0("/", algorithm, " precision and recall analysis/ds_PR_curve.jpeg")), width = 3300, height = 2400, res = 500)
ggplot(global_detection_results_by_all_thresholds, aes(x = TPR, y = precision, color = minimum_confidence_threshold)) +
  geom_point() + 
  guides(color = guide_colorbar(reverse = FALSE, barheight = 10)) +
  xlab("ds_recall") +
  ylab("ds_precision") +
  ylim(c(0.35, 1)) +
  xlim(c(0, 1)) +
  labs(color = "Confidence \n  threshold") +
  ggtitle(ds_PR_title) +   
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

# Determining the influence of input parameter values on AUC scores
AUC_results <- read.csv(paste0(CSV_path, "/../../", "AUC_scores_by_input_parameter_configuration.csv")) %>%
  filter(freq_filter == "Unfiltered")

ds05_results <- AUC_results %>%
  filter(detection_sensitivity == 0.5)

ds05_results_list <- c(ds05_results$id_PR_AUC, ds05_results$id_ROC_AUC, ds05_results$rec_PR_AUC, ds05_results$rec_ROC_AUC, ds05_results$ds_PR_AUC, ds05_results$ds_ROC_AUC)

ds10_results <- AUC_results %>%
  filter(detection_sensitivity == 1)

ds10_results_list <- c(ds10_results$id_PR_AUC, ds10_results$id_ROC_AUC, ds10_results$rec_PR_AUC, ds10_results$rec_ROC_AUC, ds10_results$ds_PR_AUC, ds10_results$ds_ROC_AUC)

ds15_results <- AUC_results %>%
  filter(detection_sensitivity == 1.5)

ds15_results_list <- c(ds15_results$id_PR_AUC, ds15_results$id_ROC_AUC, ds15_results$rec_PR_AUC, ds15_results$rec_ROC_AUC, ds15_results$ds_PR_AUC, ds15_results$ds_ROC_AUC)

overlap0s_results <- AUC_results %>%
  filter(overlap == "0s")

overlap0s_results_list <- c(overlap0s_results$id_PR_AUC, overlap0s_results$id_ROC_AUC, overlap0s_results$rec_PR_AUC, overlap0s_results$rec_ROC_AUC, overlap0s_results$ds_PR_AUC, overlap0s_results$ds_ROC_AUC)

overlap1s_results <- AUC_results %>%
  filter(overlap == "1s")

overlap1s_results_list <- c(overlap1s_results$id_PR_AUC, overlap1s_results$id_ROC_AUC, overlap1s_results$rec_PR_AUC, overlap1s_results$rec_ROC_AUC, overlap1s_results$ds_PR_AUC, overlap1s_results$ds_ROC_AUC)

overlap2s_results <- AUC_results %>%
  filter(overlap == "2s")

overlap2s_results_list <- c(overlap2s_results$id_PR_AUC, overlap2s_results$id_ROC_AUC, overlap2s_results$rec_PR_AUC, overlap2s_results$rec_ROC_AUC, overlap2s_results$ds_PR_AUC, overlap2s_results$ds_ROC_AUC)

t.test(x = ds05_results_list, y = ds10_results_list)
t.test(x = ds05_results_list, y = ds15_results_list)
t.test(x = overlap0s_results_list, y = overlap1s_results_list)
t.test(x = overlap0s_results_list, y = overlap2s_results_list)


