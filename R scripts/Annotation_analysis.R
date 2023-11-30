library(ggplot2)
library(dplyr)
# source("./generate_annotation_CSV.R")

annotations <- read.csv("../CSVs/annotations.csv") %>%
  mutate(species = case_when(species == "Leiopicus medius" ~ "Dendrocoptes medius",
                             TRUE ~ species))
species_labels <- read.csv("../CSVs/species_labels.csv", sep = ",")
recording_extended_info <- read.csv("../CSVs/recordings_extended.csv")

species_detection_tallies_by_recording <- annotations %>%
  select(recording_id, label, sound_category, initial_time, final_time, confidence_level) %>%
  group_by(recording_id, label, sound_category) %>%
  summarize(total_time = sum((final_time-initial_time)[confidence_level == 1]),
            clear_detections = length(label[confidence_level == 1]),
            unclear_detections = length(label[confidence_level == 0]))

species_detection_tallies <- species_detection_tallies_by_recording %>%
  select(label, sound_category, total_time, clear_detections, unclear_detections) %>%
  group_by(label, sound_category) %>%
  summarize(total_time = sum(total_time),
            n_recordings_clearly_detected = length(label[clear_detections > 0]),
            n_recordings_possibly_detected = length(label[clear_detections > 0 || unclear_detections > 0]))

total_birds_detected <- species_detection_tallies %>% 
  filter(sound_category == "Bird", n_recordings_clearly_detected > 0) %>%
  left_join(species_labels,
            by = "label")

#Writing CSV file
total_birds_detected_name <- file.path("../CSVs", "total_birds_detected_(annotations).csv")
write.csv(x=total_birds_detected, file=total_birds_detected_name, row.names = FALSE)



jpeg(file = "../R plots/Annotation analysis/number_of_recordings_per_species.jpeg", width = 5000, height = 2600, res = 500)
ggplot(total_birds_detected %>%
         filter(label != "Oiseau sp."), aes(x=reorder(species, n_recordings_clearly_detected), y=n_recordings_clearly_detected)) +
  geom_bar(position=position_dodge(), 
           stat = "identity",
           fill = "#4D6AB0") +
  xlab("Species") +
  ylab("Number of recordings where detected") +
  ggtitle("Bird species manual detection count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"))
dev.off()


# Query to find all annotations for a particular species:
# annotations %>%
#   left_join(recording_extended_info, by = "recording_id") %>%
#   filter(label == "Eri rub",
#          confidence_level == 1) %>%
#   select(station_id, recording_time) %>%
#   unique() %>%
#   View()


# Query to retrieve the list of species detected in each SpatialTreeP site (for Luc)
# recording_metadata <- read.csv("../Recording selection/metadata.csv")
# 
# species_list_by_SpatialTreeP_site <- species_detection_tallies_by_recording %>%
#   filter(recording_id >= 190,
#          clear_detections > 0,
#          sound_category == "Bird",
#          label != "Oiseau sp.") %>%
#   left_join(species_labels,
#             by = "label") %>%
#   ungroup() %>%
#   left_join(recording_extended_info,
#             by = "recording_id") %>%
#   select(recording_id, station_id, species) %>%
#   left_join(recording_metadata %>%
#               select(station, commune, X, Y) %>%
#               rename("station_id" = "station",
#                      "latitude" = "X",
#                      "longitude" = "Y"),
#             by = "station_id") %>%
#   select(recording_id, commune, latitude, longitude, species)
# 
# # Writing CSV file
# species_list_by_SpatialTreeP_site_name <- file.path("../CSVs", "species_list_by_SpatialTreeP_site.csv")
# write.csv(x=species_list_by_SpatialTreeP_site, file=species_list_by_SpatialTreeP_site_name, row.names = FALSE)
