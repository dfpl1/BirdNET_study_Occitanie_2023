library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(tidyr)
library(viridis)
source("./Algorithm_version_selection.R")

R_plots_path <- paste0("../R plots/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text)

recordings_extended_info <- read.csv("../CSVs/recordings_extended.csv")
annotations <- read.csv("../CSVs/annotations.csv")
global_species_list <- read.csv("../CSVs/global_species_list.csv")
BirdNET_2h_analysis_detailed_results <- read.csv(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/BirdNET_2h_analysis_detailed_results.csv"))
BirdNET_precision_results <- read.csv(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/BirdNET_precision_results.csv"))

# Not including Sites démonstrateurs in the analysis (there aren't enough minutes recorded)
manually_checked_results <- read.csv(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/manually_checked_synthesized_BirdNET_results.csv")) %>%
  filter(!grepl("Sites démonstrateurs", recording_name))

# Checking which recordings don't reach the number of species annotated when analyzed with BirdNET
recordings_without_enough_species_detected <- manually_checked_results %>%
  filter(correct_identification == "TRUE") %>%
  select(recording_id, common_name, n_species_target) %>%
  unique() %>%
  group_by(recording_id, n_species_target) %>%
  summarize(n_species_correctly_detected_by_BirdNET = n()) %>%
  filter(n_species_target > n_species_correctly_detected_by_BirdNET) %>%
  left_join(recordings_extended_info,
            by = "recording_id")


recordings_extended_info <- recordings_extended_info %>%
  rowwise() %>%
  mutate(recording_directory = gsub(file_name, "", recording_file_path))

first_recordings <- recordings_extended_info$recording_file_path
second_recordings <- paste0(recordings_extended_info$recording_directory, recordings_extended_info$second_recording)
third_recordings <- paste0(recordings_extended_info$recording_directory, recordings_extended_info$third_recording)
fourth_recordings <- paste0(recordings_extended_info$recording_directory, recordings_extended_info$fourth_recording)


manually_checked_results <- manually_checked_results %>%
  rowwise() %>%
  mutate(correct_start_times = paste(setdiff(strsplit(start_times, ", ")[[1]], strsplit(start_times_of_incorrect_identifications, ", ")[[1]]), collapse = ", "),
         recording_number = case_when(recording_name %in% first_recordings ~ 1,
                                      recording_name %in% second_recordings ~ 2,
                                      recording_name %in% third_recordings ~ 3,
                                      TRUE ~ 4))

confidence_thresholds <- c(0.75, 0.8, 0.85, 0.9, 0.95)
confidence_labels <- c("75", "80", "85", "90", "95")
confidence_plot_titles <- c()
column_names <- c("first_detection_with_1st_threshold_confidence", "first_detection_with_2nd_threshold_confidence", "first_detection_with_3rd_threshold_confidence", "first_detection_with_4th_threshold_confidence", "first_detection_with_5th_threshold_confidence")

for (i in 1:length(confidence_thresholds)) {
  confidence_plot_titles <- c(confidence_plot_titles, paste0(" with confidence level ≥ ", toString(confidence_thresholds[i])))
}

# Summarizing results by recording
manually_checked_results_summary <- data.frame()

for (i in 1:length(confidence_thresholds)) {
  manually_checked_results["first_detection"] <- manually_checked_results[column_names[i]]
  
  current_manually_checked_results_summary <- manually_checked_results %>%
    filter(!is.na(first_detection),
           n_species_target > 0) %>%
    group_by(recording_id, n_species_target, common_name) %>%
    summarize(first_correct_detection = sort(as.numeric(setdiff(strsplit(paste(case_when(correct_identification == "TRUE" ~ as.character(first_detection + 1800 * (recording_number - 1)),
                                                                                                TRUE ~ ""), collapse = ', '), ", ")[[1]],
                                                                       c(""))))[1],
              first_incorrect_detection = sort(as.numeric(setdiff(strsplit(paste(case_when(correct_identification == "FALSE" ~ as.character(first_detection + 1800 * (recording_number - 1)),
                                                                                                  TRUE ~ ""), collapse = ', '), ", ")[[1]],
                                                                         c(""))))[1]) %>%
    group_by(recording_id, n_species_target) %>%
    summarize(n_species_correctly_detected = sum(ifelse(is.na(first_correct_detection), 0, 1)),
              n_species_incorrectly_detected = sum(ifelse(is.na(first_incorrect_detection), 0, 1)),
              first_correct_detections = paste(sort(as.numeric(setdiff(strsplit(paste(first_correct_detection, collapse = ', '), ", ")[[1]],
                                                                       c("")))), collapse = ", "),
              first_incorrect_detections = paste(sort(as.numeric(setdiff(strsplit(paste(first_incorrect_detection, collapse = ', '), ", ")[[1]],
                                                                         c("")))), collapse = ", ")) %>%
    rename("n_species_manually_detected" = "n_species_target") %>%
    rowwise() %>%
    mutate(time_until_meeting_the_target = ifelse(n_species_correctly_detected < n_species_manually_detected, 
                                                  as.numeric(NA),
                                                  as.numeric(strsplit(first_correct_detections, ", ")[[1]][n_species_manually_detected])),
           n_species_incorrectly_detected_until_meeting_the_target = ifelse(is.na(time_until_meeting_the_target),
                                                                            length(strsplit(first_incorrect_detections, ", ")[[1]]),
                                                                            nrow(data.frame(incorrect_detection_times = as.numeric(strsplit(first_incorrect_detections, ", ")[[1]])) %>%
                                                                              filter(incorrect_detection_times <= time_until_meeting_the_target))),
           confidence_threshold = confidence_thresholds[i])
  
  manually_checked_results_summary <- rbind(manually_checked_results_summary, current_manually_checked_results_summary)
}

manually_checked_results_summary$confidence_threshold <- as.factor(manually_checked_results_summary$confidence_threshold)

# Adding recordings where birds have been manually detected but where BirdNET has not provided any identification with confidence level >= 0.75
species_annotated_per_recording <- annotations %>%
  filter(confidence_level == 1,
         sound_category == "Bird",
         label != "Oiseau sp.") %>%
  select(recording_id, label) %>%
  unique() %>%
  group_by(recording_id) %>%
  summarize(n_species = n()) %>%
  left_join(recordings_extended_info %>%
              select(recording_id, recording_file_path),
            by = "recording_id") %>%
  filter(!grepl("Sites démonstrateurs", recording_file_path))

recordings_to_add <- setdiff(species_annotated_per_recording$recording_id,
                             manually_checked_results_summary$recording_id)

rows_to_add <- species_annotated_per_recording %>%
  filter(recording_id %in% recordings_to_add) %>%
  rename("n_species_manually_detected" = "n_species") %>%
  select(-recording_file_path) %>%
  mutate(n_species_correctly_detected = 0,
         n_species_incorrectly_detected = 0,
         first_correct_detections = NA,
         first_incorrect_detections = NA,
         time_until_meeting_the_target = NA,
         n_species_incorrectly_detected_until_meeting_the_target = NA)

for (i in 1:length(confidence_thresholds)) {
  manually_checked_results_summary <- rbind(manually_checked_results_summary, 
                                            rows_to_add %>%
                                                mutate(confidence_threshold = confidence_thresholds[i]))
}

manually_checked_results_summary["time_until_meeting_the_target"][is.na(manually_checked_results_summary["time_until_meeting_the_target"])] <- 7200

# Plotting the time needed by BirdNET to correctly identify the same number of species that were manually identified in 1 minute
cumulative_manually_checked_results_by_time_category <- manually_checked_results_summary %>%
  group_by(confidence_threshold) %>%
  summarize("15" = sum(ifelse(time_until_meeting_the_target / 60.0 < 15, 1, 0)),
            "30" = sum(ifelse(time_until_meeting_the_target / 60.0 < 30, 1, 0)),
            "45" = sum(ifelse(time_until_meeting_the_target / 60.0 < 45, 1, 0)),
            "60" = sum(ifelse(time_until_meeting_the_target / 60.0 < 60, 1, 0)),
            "75" = sum(ifelse(time_until_meeting_the_target / 60.0 < 75, 1, 0)),
            "90" = sum(ifelse(time_until_meeting_the_target / 60.0 < 90, 1, 0)),
            "105" = sum(ifelse(time_until_meeting_the_target / 60.0 < 105, 1, 0)),
            "120" = sum(ifelse(time_until_meeting_the_target / 60.0 < 120, 1, 0)))

t_cumulative_manually_checked_results_by_time_category <- t(cumulative_manually_checked_results_by_time_category)
colnames(t_cumulative_manually_checked_results_by_time_category) <- t_cumulative_manually_checked_results_by_time_category[1,]
t_cumulative_manually_checked_results_by_time_category <- t_cumulative_manually_checked_results_by_time_category[-1,]

t_cumulative_manually_checked_results_by_time_category <- melt(t_cumulative_manually_checked_results_by_time_category)
colnames(t_cumulative_manually_checked_results_by_time_category) <- c("time_needed", "confidence_threshold", "value")
t_cumulative_manually_checked_results_by_time_category <- t_cumulative_manually_checked_results_by_time_category %>%
  mutate(time_needed = ordered(factor(time_needed, levels = c("15", "30", "45", "60", "75", "90", "105", "120"))),
         confidence_threshold = ordered(factor(confidence_threshold, levels = c("0.75", "0.8", "0.85", "0.9", "0.95"))),
         value = as.numeric(value))

n_recordings_analyzed <- length(unique(manually_checked_results_summary$recording_id))

# Other plots
jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/time_needed_by_BirdNET_to_identify_the_number_of_species_detected_in_1_min.jpeg"), width = 6500, height = 2600, res = 500)
ggplot(t_cumulative_manually_checked_results_by_time_category, aes(fill = confidence_threshold, y = value / n_recordings_analyzed, x = time_needed)) + 
  geom_bar(position = "dodge", 
           colour = 'black',
           stat = "identity") +
  ylab("Proportion of recordings") +
  xlab("Recording time (min) analyzed with BirdNET") +
  labs(fill = "Confidence threshold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7", "#99CCFF", "#B0c3f9")) +
  ggtitle("Proportion of recordings where BirdNET correctly identified the same number of species that were manually detected in 1 minute")
dev.off()
 

manually_checked_results_summary <- manually_checked_results_summary %>%
  mutate(confidence_threshold = as.factor(confidence_threshold),
         proportion_of_species_correctly_detected = n_species_correctly_detected / (n_species_correctly_detected + n_species_incorrectly_detected),
         proportion_of_species_correctly_detected_relative_to_target = n_species_correctly_detected / n_species_manually_detected)

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/precision_by_confidence_threshold_used.jpeg"), width = 3500, height = 2600, res = 500)
ggplot(manually_checked_results_summary, aes(x = confidence_threshold, y = proportion_of_species_correctly_detected)) +
  geom_boxplot(fill = "#84B8E1") +
  xlab("Confidence threshold") +
  ylab("Precision") +
  ggtitle("Precision by confidence threshold used") +
  theme_bw() +
  theme(legend.position = "none")
dev.off() 

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/number_of_species_correctly_identified_by_BirdNET_relative_to_the_number_of_species_manually_identified_by_confidence_threshold_used.jpeg"), width = 4500, height = 3600, res = 500)
ggplot(manually_checked_results_summary, aes(x = confidence_threshold, y = proportion_of_species_correctly_detected_relative_to_target)) +
  geom_boxplot(fill = "#84B8E1") +
  xlab("Confidence threshold") +
  ylab("Number of species correctly detected by BirdNET in 2 h as a proportion \nof the number of species manually detected in 1 minute") +
  ggtitle("Number of species correctly detected by BirdNET in 2 h as a proportion \nof the number of species manually detected in 1 minute, by confidence threshold used") +
  scale_y_continuous(breaks=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                   labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  theme_bw() +
  theme(legend.position = "none")
dev.off() 


# Grouping recordings by the time needed by BirdNET to identify the same number of species that 
# were manually identified in 1 minute
grouped_manually_checked_results_summary <- manually_checked_results_summary %>%
  filter(n_species_manually_detected > 0) %>%
  mutate(recording_time_needed = case_when(time_until_meeting_the_target < 15 * 60 ~ "0-15",
                                           time_until_meeting_the_target < 30 * 60 ~ "15-30",
                                           time_until_meeting_the_target < 45 * 60 ~ "30-45",
                                           time_until_meeting_the_target < 60 * 60 ~ "45-60",
                                           time_until_meeting_the_target < 75 * 60 ~ "60-75",
                                           time_until_meeting_the_target < 90 * 60 ~ "75-90",
                                           time_until_meeting_the_target < 105 * 60 ~ "90-105",
                                           time_until_meeting_the_target < 120 * 60 ~ "105-120",
                                           time_until_meeting_the_target >= 120 * 60 ~ ">=120"),
         confidence_threshold = as.factor(confidence_threshold)) %>%
  group_by(confidence_threshold, recording_time_needed) %>%
  summarize(proportion_of_recordings = n() / 190)

grouped_manually_checked_results_summary$recording_time_needed <- factor(grouped_manually_checked_results_summary$recording_time_needed, 
                                                                                     levels=c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90", "90-105", "105-120", ">=120"))

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/time_interval_needed_by_BirdNET_to_identify_the_number_of_species_detected_in_1_min.jpeg"), width = 5200, height = 3000, res = 500)
ggplot(grouped_manually_checked_results_summary, aes(fill = recording_time_needed, y = proportion_of_recordings, x = confidence_threshold)) + 
  geom_bar(position = "dodge", 
           colour = 'black',
           stat = "identity") +
  ylab("Proportion of recordings") +
  xlab("Confidence threshold") +
  labs(fill = "Recording time (min) needed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = head(viridis_pal()(11), 9)) +
  ggtitle("Recording time needed by BirdNET to identify the same number of species that were manually identified in 1 minute")
dev.off()

# Generating BirdNET precision results from manually checked BirdNET identifications
detailed_manually_checked_results <- manually_checked_results %>%
  mutate(recording_name = basename(recording_name)) %>%
  rename("English_name" = "common_name") %>%
  left_join(global_species_list,
            by = "English_name") %>%
  mutate(start_times = strsplit(as.character(start_times), ", ")) %>% 
  unnest(start_times) %>%
  rename("species" = "Scientific_name",
         "incorrect_start_times" = "start_times_of_incorrect_identifications",
         "start_time" = "start_times") %>%
  rowwise() %>%
  mutate(correct_identification = ifelse((start_time %in% strsplit(incorrect_start_times, ", ")[[1]]), "FALSE", correct_identification),
         initial_time = period_to_seconds(as.period(start_time)),
         final_time = initial_time + 3) %>%
  select(recording_id, recording_name, species, initial_time, final_time, correct_identification)


# Generating the list of species found in SpatialTreeP sites
# spatialTreeP_species_list <- detailed_manually_checked_results %>% 
#   filter(recording_id >= 190, correct_identification == TRUE) %>% 
#   select(species, recording_id) %>%
#   unique() %>%
#   group_by(species) %>% 
#   summarize(n_recordings = n()) %>%
#   arrange(desc(n_recordings))

BirdNET_2h_analysis_precision_results <- detailed_manually_checked_results %>%
  left_join(BirdNET_2h_analysis_detailed_results %>%
              rename("initial_time" = "begin_time",
                     "final_time" = "end_time"),
            by = c("recording_id", "recording_name", "initial_time", "final_time", "species")) %>%
  mutate(correct_identification = as.logical(correct_identification)) %>%
  select(recording_id, recording_name, initial_time, final_time, species, confidence, correct_identification)

# Plotting precision results by species
for (i in 1:length(confidence_thresholds)) {
  precision_results_by_species <- BirdNET_2h_analysis_precision_results %>%
    filter(confidence >= confidence_thresholds[i]) %>%
    mutate(species = as.factor(species),
           precision = as.numeric(correct_identification)) %>%
    group_by(species) %>%
    summarize(mean_precision = mean(precision),
              total_detections = n()) %>%
    arrange(mean_precision) %>%
    mutate(species_number = paste0(species, " (n = ", total_detections, ")"))
  
  jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/precision_by_species_", confidence_labels[i], ".jpeg"), width = 8500, height = 2400, res = 500)
  ggplot(precision_results_by_species, aes(x=reorder(reorder(species_number, -total_detections), mean_precision), y=mean_precision)) +
    geom_bar(position = position_dodge(), 
             stat = "identity",
             colour = 'black',
             fill = "#759ED0") +
    xlab("Species") +
    ylab("Mean precision") +
    ggtitle(paste0("Mean precision by bird species in BirdNET identifications", confidence_plot_titles[i])) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  dev.off()
}

# Plotting precision results by the logarithm of the number of detections per recording
precision_results_by_n_detections_in_recording <- BirdNET_2h_analysis_precision_results %>%
  mutate(species = as.factor(species),
         precision = as.numeric(correct_identification)) %>%
  group_by(recording_id, recording_name, species) %>%
  summarize(precision = mean(precision),
            total_detections = n()) %>%
  mutate(n_detections_category = case_when(total_detections < 2 ~ "1",
                                                     total_detections < 3 ~ "2",
                                                     total_detections < 4 ~ "3",
                                                     total_detections < 5 ~ "4",
                                                     total_detections < 10 ~ "5-9",
                                                     total_detections < 20 ~ "10-19",
                                                     total_detections < 50 ~ "20-49",
                                                     total_detections < 100 ~ "50-99",
                                                     TRUE ~ ">=100")) %>%
  ungroup() %>%
  mutate(n_detections_category = ordered(factor(n_detections_category, levels = c("1", "2", "3", "4", "5-9", "10-19", "20-49", "50-99", ">=100"))))
  
  mean_precision_results_by_n_detections_in_recording <- precision_results_by_n_detections_in_recording %>%
  group_by(n_detections_category) %>%
  summarise(mean_precision = mean(precision),
            precision_sd = sd(precision),
            n = n(),
            precision_se = precision_sd / sqrt(n))

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/precision_by_number_of_detections_per_recording.jpeg"), width = 5000, height = 3600, res = 500)
ggplot(mean_precision_results_by_n_detections_in_recording, aes(x=n_detections_category, y=mean_precision)) +
  geom_bar(position = position_dodge(), 
           stat = "identity",
           colour = 'black',
           fill = "#759ED0") +
  geom_errorbar(aes(x=n_detections_category, ymin=mean_precision-precision_se, ymax=mean_precision+precision_se), 
                width=0.5, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  xlab("Number of detections") +
  ylab("Mean precision") +
  ggtitle("BirdNET precision by the number of detections of a species in a 30-minute recording (confidence level >= 0.75)") +
  theme_bw()
dev.off()

  

# Evaluating which restriction is more efficient at improving precision: 
# requiring a higher confidence level or requiring a higher number of detections per recording
min_n_detections_per_recording <- c(2, 3, 4, 5, 10)

comparative_results <- data.frame()

for (i in 1:length(confidence_thresholds)) {
  results_filtered_by_confidence <- BirdNET_2h_analysis_precision_results %>%
    filter(confidence >= confidence_thresholds[i])
  
  for (j in 1:length(min_n_detections_per_recording)) {
    results_filtered_by_min_n_detections <- results_filtered_by_confidence %>%
      mutate(species = as.factor(species),
             precision = as.numeric(correct_identification)) %>%
      group_by(recording_id, recording_name, species) %>%
      summarize(precision = mean(precision),
                total_detections = n()) %>%
      filter(total_detections >= min_n_detections_per_recording[j])
    
    current_comparative_results <- results_filtered_by_min_n_detections %>%
      group_by(recording_id, recording_name) %>%
      summarize(n_species_correctly_detected_per_recording = sum(ifelse(precision > 0, 1, 0)),
                n_species_incorrectly_detected_per_recording = sum(ifelse(precision == 0, 1, 0))) %>%
      mutate(confidence_threshold = confidence_thresholds[i],
             min_n_detections = min_n_detections_per_recording[j]) %>%
      group_by(confidence_threshold, min_n_detections) %>%
      summarize(mean_n_species_correctly_detected_per_recording = mean(n_species_correctly_detected_per_recording),
                mean_n_species_incorrectly_detected_per_recording = mean(n_species_incorrectly_detected_per_recording))
    
    comparative_results <- rbind(comparative_results, current_comparative_results)
  }
}

comparative_results <- comparative_results %>%
  ungroup() %>%
  mutate(confidence_threshold = ordered(factor(confidence_threshold, levels = c("0.75", "0.8", "0.85", "0.9", "0.95"))),
         min_n_detections = ordered(factor(min_n_detections, levels = c("2", "3", "4", "5", "10"))))

fill_correspondences <- c("0.75" = "#FDE725FF", 
                          "0.8" = "#55C667FF", 
                          "0.85" = "#238A8DFF", 
                          "0.9" = "#404788FF", 
                          "0.95" = "#440154FF")
shape_correspondences <- c("2" = 21, 
                           "3" = 22, 
                           "4" = 23, 
                           "5" = 24, 
                           "10" = 25)

# Plotting results comparing a restriction by confidence threshold and a restriction by 
# a minimum number of detections per recording
jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/comparison_between_confidence_threshold_and_min_n_detections.jpeg"), width = 4900, height = 2800, res = 500)
ggplot(comparative_results, aes(x = mean_n_species_incorrectly_detected_per_recording, 
                                y = mean_n_species_correctly_detected_per_recording, 
                                fill = confidence_threshold,
                                shape = min_n_detections)) +
  geom_point(size = 4) +
  xlab("False positives per recording") +
  ylab("True positives per recording") +
  scale_fill_manual("Confidence threshold", values = fill_correspondences) +
  scale_shape_manual("Minimum number of detections\nper recording", values = shape_correspondences) +
  guides(fill = guide_legend("Confidence threshold", override.aes = list(shape = 21))) +
  ggtitle("TPs and FPs per recording by confidence threshold and minimum number of detections used") +
  theme_bw()
dev.off()

# Merging precision results from the 2h-analysis (>=0.75) and the 1-minute analysis (>=0.1)
BirdNET_all_precision_results <- rbind(BirdNET_precision_results %>%
                                         left_join(recordings_extended_info %>%
                                                     select(recording_id, file_name)) %>%
                                         rename("recording_name" = "file_name"),
                                       BirdNET_2h_analysis_precision_results)

# Writing CSV files
BirdNET_2h_analysis_precision_results_file_name <- file.path(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/BirdNET_2h_analysis_precision_results.csv"))
write.csv(x=BirdNET_2h_analysis_precision_results, file=BirdNET_2h_analysis_precision_results_file_name, row.names = FALSE)

BirdNET_all_precision_results_file_name <- file.path(paste0("../CSVs/BirdNET_", BirdNET_version, ds_text, filtered_text, overlap_text, "/BirdNET_all_precision_results.csv"))
write.csv(x=BirdNET_all_precision_results, file=BirdNET_all_precision_results_file_name, row.names = FALSE)

  
# Checking whether the predominance of anthropic noise influences BirdNET precision
recordings_2h_acoustic_data <- read.csv("../CSVs/2h-recordings_acoustic_data.csv") %>%
  mutate(recording_name = basename(recording_file_path))

BirdNET_precision_results_by_recording <- BirdNET_all_precision_results %>%
  group_by(recording_id, recording_name) %>%
  summarize(precision = mean(as.numeric(as.logical(correct_identification)))) %>%
  left_join(recordings_2h_acoustic_data,
            by = c("recording_id", "recording_name"))

# Checking the normality of the NDSI index distribution to determine whether we can perform a Pearson correlation analysis or not
shapiro.test(BirdNET_precision_results_by_recording$NDSI_left)
hist(BirdNET_precision_results_by_recording$NDSI_left)

# The distribution of NDSI, recall and precision values is not normal. 
# Therefore, we have to transform them before we can perform the Pearson's correlation analysis.
# Nonetheless, neither sqrt nor logarithmic transformations transform the NDSI, recall or precision distributions into normal ones.
# Thus, a Spearman's correlation test will have to be performed insted of a Pearson's one.


# The NDSI index has a significant influence on the precision of the algorithm (r = ?, p-value = ?)
cor.test(BirdNET_precision_results_by_recording$NDSI_left, BirdNET_precision_results_by_recording$precision, method = "spearman")


jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/precision_by_anthropic_noise.jpeg"), width = 3200, height = 2500, res = 500)
ggplot(BirdNET_precision_results_by_recording, aes(x=NDSI_left, y=precision)) +
  geom_point(color = "black") +
  geom_smooth(method = lm, se = FALSE) +
  xlab("NDSI score") +
  ylab("Precision") +
  ggtitle("BirdNET precision by NDSI score") +
  theme(panel.background = element_blank(),
        axis.line = element_line())
dev.off()

# Comparing TPs vs FPs depending on the recording time analyzed and the confidence threshold used

tp_and_fp_by_time_analyzed <- manually_checked_results_summary %>%
  group_by(confidence_threshold, recording_id) %>%
  summarize("TP_15min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 15 * 60]),
            "FP_15min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 15 * 60]),
            "TP_30min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 30 * 60]),
            "FP_30min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 30 * 60]),
            "TP_45min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 45 * 60]),
            "FP_45min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 45 * 60]),
            "TP_60min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 60 * 60]),
            "FP_60min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 60 * 60]),
            "TP_75min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 75 * 60]),
            "FP_75min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 75 * 60]),
            "TP_90min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 90 * 60]),
            "FP_90min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 90 * 60]),
            "TP_105min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 105 * 60]),
            "FP_105min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 105 * 60]),
            "TP_120min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 120 * 60]),
            "FP_120min" = length(as.numeric(strsplit(first_incorrect_detections, ", ")[[1]]) %>% .[. < 120 * 60]))

tp_and_fp_by_time_analyzed_unnested <- rbind(tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_15min, FP_15min) %>%
                                      rename("TP" = "TP_15min",
                                             "FP"= "FP_15min") %>%
                                    mutate(recording_time = 15),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_30min, FP_30min) %>%
                                      rename("TP" = "TP_30min",
                                             "FP"= "FP_30min") %>%
                                      mutate(recording_time = 30),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_45min, FP_45min) %>%
                                      rename("TP" = "TP_45min",
                                             "FP"= "FP_45min") %>%
                                      mutate(recording_time = 45),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_60min, FP_60min) %>%
                                      rename("TP" = "TP_60min",
                                             "FP"= "FP_60min") %>%
                                      mutate(recording_time = 60),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_75min, FP_75min) %>%
                                      rename("TP" = "TP_75min",
                                             "FP"= "FP_75min") %>%
                                      mutate(recording_time = 75),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_90min, FP_90min) %>%
                                      rename("TP" = "TP_90min",
                                             "FP"= "FP_90min") %>%
                                      mutate(recording_time = 90),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_105min, FP_105min) %>%
                                      rename("TP" = "TP_105min",
                                             "FP"= "FP_105min") %>%
                                      mutate(recording_time = 105),
                                    tp_and_fp_by_time_analyzed %>%
                                      select(confidence_threshold, TP_120min, FP_120min) %>%
                                      rename("TP" = "TP_120min",
                                             "FP"= "FP_120min") %>%
                                      mutate(recording_time = 120))
  

tp_and_fp_by_time_analyzed_summary <- tp_and_fp_by_time_analyzed_unnested %>%
  group_by(confidence_threshold, recording_time) %>%
  summarise(mean_TP = mean(TP),
            mean_FP = mean(FP),
            sd_TP = sd(TP),
            sd_FP = sd(FP),
            n = n(),
            se_TP = sd_TP / sqrt(n),
            se_FP = sd_FP / sqrt(n)) %>%
  mutate(recording_time = as.factor(recording_time))

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/true_positives_by_time_analyzed.jpeg"), width = 5200, height = 3000, res = 500)
ggplot(tp_and_fp_by_time_analyzed_summary, aes(fill = confidence_threshold, y = mean_TP, x = recording_time)) + 
  geom_bar(position = "dodge", 
           colour = 'black',
           stat = "identity") +
  geom_errorbar(aes(x=recording_time, ymin=mean_TP-se_TP, ymax=mean_TP+se_TP), 
                position = "dodge", 
                width=0.9, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  ylab("Mean number of TPs") +
  xlab("Recording time (min) analyzed with BirdNET") +
  labs(fill = "Confidence threshold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7", "#99CCFF", "#B0c3f9")) +
  ggtitle("Mean number of TPs by recording time analyzed and confidence threshold used")
dev.off()

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/false_positives_by_time_analyzed.jpeg"), width = 5200, height = 3000, res = 500)
ggplot(tp_and_fp_by_time_analyzed_summary, aes(fill = confidence_threshold, y = mean_FP, x = recording_time)) + 
  geom_bar(position = "dodge", 
           colour = 'black',
           stat = "identity") +
  geom_errorbar(aes(x=recording_time, ymin=mean_FP-se_FP, ymax=mean_FP+se_FP), 
                position = "dodge", 
                width=0.9, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  ylab("Mean number of FPs") +
  xlab("Recording time (min) analyzed with BirdNET") +
  labs(fill = "Confidence threshold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7", "#99CCFF", "#B0c3f9")) +
  ggtitle("Mean number of FPs by recording time analyzed and confidence threshold used")
dev.off()

# Joining true and false positives into the same column so that 
# they can be represented as the same variable in the plot
tp_and_fp_by_time_analyzed_summary_for_plot <- data.frame()

for(i in 1:nrow(tp_and_fp_by_time_analyzed_summary)) {
  current_row_TP <- tp_and_fp_by_time_analyzed_summary[i,] %>%
    select(confidence_threshold, recording_time, mean_TP, sd_TP, n, se_TP) %>%
    rename("value" = "mean_TP",
           "sd" = "sd_TP",
           "se" = "se_TP")
  
  current_row_FP <- tp_and_fp_by_time_analyzed_summary[i,] %>%
    select(confidence_threshold, recording_time, mean_FP, sd_FP, n, se_FP) %>%
    rename("value" = "mean_FP",
           "sd" = "sd_FP",
           "se" = "se_FP") %>%
    mutate(value = value * -1)
  
  tp_and_fp_by_time_analyzed_summary_for_plot <- rbind(tp_and_fp_by_time_analyzed_summary_for_plot, current_row_TP, current_row_FP)
}

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/true_and_false_positives_by_time_analyzed.jpeg"), width = 5200, height = 3500, res = 500)
ggplot(tp_and_fp_by_time_analyzed_summary_for_plot, aes(fill = confidence_threshold, y = value, x = recording_time)) + 
  geom_bar(position = "dodge", 
           colour = 'black',
           stat = "identity") +
  geom_errorbar(aes(x=recording_time, ymin=value-se, ymax=value+se), 
                position = "dodge", 
                width=0.9, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  ylab("Mean number of TPs vs FPs") +
  xlab("Recording time (min) analyzed with BirdNET") +
  labs(fill = "Confidence threshold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7", "#99CCFF", "#B0c3f9")) +
  ggtitle("Mean number of TPs and FPs by recording time analyzed and confidence threshold used")
dev.off()

# Plot showing the proportion of species correctly detected by BirdNET relative to the number of species 
# manually detected in one minute, depending on the recording time analyzed and the confidence threshold used

proportion_of_species_correctly_detected_by_time_analyzed <- manually_checked_results_summary %>%
  group_by(confidence_threshold, recording_id) %>%
  summarize("proportion_correctly_detected_15min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 15 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_30min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 30 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_45min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 45 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_60min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 60 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_75min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 75 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_90min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 90 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_105min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 105 * 60]) / n_species_manually_detected,
            "proportion_correctly_detected_120min" = length(as.numeric(strsplit(first_correct_detections, ", ")[[1]]) %>% .[. < 120 * 60]) / n_species_manually_detected)

proportion_of_species_correctly_detected_by_time_analyzed_unnested <- rbind(proportion_of_species_correctly_detected_by_time_analyzed %>%
                                               select(confidence_threshold, proportion_correctly_detected_15min) %>%
                                               rename("proportion_correctly_detected" = "proportion_correctly_detected_15min") %>%
                                               mutate(recording_time = 15),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_30min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_30min") %>%
                                                 mutate(recording_time = 30),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_45min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_45min") %>%
                                                 mutate(recording_time = 45),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_60min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_60min") %>%
                                                 mutate(recording_time = 60),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_75min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_75min") %>%
                                                 mutate(recording_time = 75),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_90min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_90min") %>%
                                                 mutate(recording_time = 90),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_105min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_105min") %>%
                                                 mutate(recording_time = 105),
                                               proportion_of_species_correctly_detected_by_time_analyzed %>%
                                                 select(confidence_threshold, proportion_correctly_detected_120min) %>%
                                                 rename("proportion_correctly_detected" = "proportion_correctly_detected_120min") %>%
                                                 mutate(recording_time = 120))


proportion_of_species_correctly_detected_by_time_analyzed_unnested_summary <- proportion_of_species_correctly_detected_by_time_analyzed_unnested %>%
  group_by(confidence_threshold, recording_time) %>%
  summarise(mean_proportion_correctly_detected = mean(proportion_correctly_detected),
            sd_proportion_correctly_detected = sd(proportion_correctly_detected),
            n = n(),
            se_proportion_correctly_detected = sd_proportion_correctly_detected / sqrt(n)) %>%
  mutate(recording_time = as.factor(recording_time))

jpeg(file = paste0(R_plots_path, "/BirdNET 2h-analysis/proportion_of_species_correctly_detected_by_time_analyzed.jpeg"), width = 5200, height = 3000, res = 500)
ggplot(proportion_of_species_correctly_detected_by_time_analyzed_unnested_summary, aes(fill = confidence_threshold, y = mean_proportion_correctly_detected, x = recording_time)) + 
  geom_bar(position = "dodge", 
           colour = 'black',
           stat = "identity") +
  geom_errorbar(aes(x=recording_time, ymin=mean_proportion_correctly_detected-se_proportion_correctly_detected, ymax=mean_proportion_correctly_detected+se_proportion_correctly_detected), 
                position = "dodge", 
                width=0.9, 
                colour="black", 
                alpha=0.7, 
                size=0.5) +
  ylab("Mean proportion of species correctly detected") +
  xlab("Recording time (min) analyzed with BirdNET") +
  labs(fill = "Confidence threshold") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c("#2b4056", "#3874a5", "#56b1f7", "#99CCFF", "#B0c3f9")) +
  ggtitle("Mean proportion of species correctly detected relative to the number of species manually detected in 1 minute\nby recording time analyzed and confidence threshold used")
dev.off()
