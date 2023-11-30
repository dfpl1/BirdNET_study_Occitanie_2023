library(dplyr)
library(rgdal)
setwd("../Recordings/Selected/")

#------------------------------ Basic recording data ---------------------------------

files <- list.files(pattern = ".wav$", recursive = TRUE, include.dirs = TRUE)
files <- files[!grepl("Filtered", files)]

recordings <- data.frame()

for (i in 1:length(files)) {
  recording_id <- i
  station_id <- as.numeric(strsplit(files[i], "/")[[1]][2])
  
  file_data <- strsplit(files[i], split = "_")
  
  #It homogenises the format for files either with or without a code before the date
  if(length(file_data[[1]]) < 3) {
    file_date <- sub(".*/", "", file_data[[1]][1])
    file_data[[1]] <- append(file_data[[1]], file_date, after = 1)
    file_data[[1]][1] <- paste0(sub("/.*", "", file_data[[1]][1]), "/", file_date)
    
    file_name <- paste0(sub(".*/", "", file_data[[1]][1]), "_", file_data[[1]][3])
  } else {
    file_name <- paste0(sub(".*/", "", file_data[[1]][1]), "_", file_data[[1]][2], "_", file_data[[1]][3])
  }
  
  recording_datetime <- as.POSIXct(paste(file_data[[1]][2], file_data[[1]][3]), "%Y%m%d %H%M%S", tz="UTC+2")
  recording_date <- file_data[[1]][2]
  recording_time <- format(recording_datetime, format = "%H:%M:%S")
  
  # Rounding recording time at the hour level
  if (recording_time > "06:45:00" && recording_time < "07:15:00") {
    recording_time <- "07:00:00"
  } else if (recording_time > "12:45:00" && recording_time < "13:15:00") {
    recording_time <- "13:00:00"
  } else if (recording_time > "23:45:00" || recording_time < "00:15:00") {
    recording_time <- "00:00:00"
  }
  
  recording_file_path <- paste0(strsplit(files[i], "/")[[1]][1], "/", as.character(station_id), "/", file_name)
  
  recordings <- rbind(recordings, c(recording_id, station_id, recording_date, recording_time, file_name, recording_file_path, gsub(".wav", "_labels.txt", files[i])))
}

colnames(recordings) <- c("recording_id", "station_id", "recording_date", "recording_time", "file_name", "recording_file_path", "annotation_file_path")
recordings <- recordings %>%
  mutate(recording_date = as.Date(recording_date, "%Y%m%d"),
         recording_id = as.integer(recording_id),
         station_id = as.integer(station_id))

#Writing CSV file
recordings_file_name <- file.path("../../CSVs", "recordings.csv")
write.csv(x=recordings, file=recordings_file_name, row.names = FALSE)

#Removing unneeded variables
rm(file_date, file_data, file_name, i, recording_id, recording_date, recording_datetime, recording_file_path, recording_time, station_id, recordings_file_name)


#------------------------------ Extended recording data ---------------------------------

recording_locations <- read.csv("../../Recording selection/recording_locations.csv") %>%
  select(-recording_date, -recording_time) %>%
  rename(station_id = station) %>%
  mutate(lower_case_file_name = gsub(".WAV", ".wav", file_name))

recording_extended_info <- recordings %>%
  rename(lower_case_file_name = file_name) %>%
  left_join(recording_locations,
            by = c("station_id", "lower_case_file_name")) %>%
  mutate(recording_file_path = case_when(file_name != lower_case_file_name ~ gsub(".wav", ".WAV", recording_file_path),
                                         TRUE ~ recording_file_path)) %>%
  select(-lower_case_file_name)

recording_metadata <- read.csv("../../Recording selection/metadata.csv") %>%
  select(-grid.no, -start_date, -end_date, -ARU) %>%
  rename(station_id = station,
         lon = X,
         lat = Y) %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))

recording_coordinates_to_transform <- recording_metadata %>%
  filter(coordinate_system == "Lambert93") %>%
  select(lon, lat)

already_transformed_recording_coordinates <- recording_metadata %>%
  filter(coordinate_system == "WGS84") %>%
  select(lon, lat)

#Coordinate transformation: from Lambert 93 to WGS 84
coordinates(recording_coordinates_to_transform) <- c("lon", "lat")
proj4string(recording_coordinates_to_transform) <- CRS("+init=epsg:2154")

CRS.new <- CRS("+init=epsg:4326")
new_station_coordinates <- rbind(data.frame(spTransform(recording_coordinates_to_transform, CRS.new)),
                                 already_transformed_recording_coordinates)

recording_metadata$lon <- new_station_coordinates$lon
recording_metadata$lat <- new_station_coordinates$lat

recording_extended_info <- recording_extended_info %>%
  left_join(recording_metadata,
            by = "station_id") %>%
  select(-coordinate_system, -precise_coordinates)

# Determining the total number of recordings per month
recordings_by_month <- data.frame(strftime(as.Date(recordings$recording_date), "%m"))
colnames(recordings_by_month) <- c("month")
recordings_by_month <- recordings_by_month %>%
  count(month)

#Adding the week of the year for the BirdNET algorithm
recording_extended_info <- recording_extended_info %>%
  rowwise() %>%
  mutate(week_number = as.integer(strftime(recording_date, format = "%V")),
         hard_drive_recording_file_path = gsub(gsub("../Recordings/Selected/", "", local_directory), hard_drive_directory, recording_file_path),
         hard_drive_second_recording_file_path = case_when(second_recording == "" ~ as.character(NA),
                                                           TRUE ~ gsub(file_name, second_recording, hard_drive_recording_file_path)),
         hard_drive_third_recording_file_path = case_when(third_recording == "" ~ as.character(NA),
                                                           TRUE ~ gsub(file_name, third_recording, hard_drive_recording_file_path)),
         hard_drive_fourth_recording_file_path = case_when(fourth_recording == "" ~ as.character(NA),
                                                           TRUE ~ gsub(file_name, fourth_recording, hard_drive_recording_file_path))
         )

#Writing CSV file
recordings_extended_info_file_name <- file.path("../../CSVs", "recordings_extended.csv")
write.csv(x=recording_extended_info, file=recordings_extended_info_file_name, row.names = FALSE)

#Removing unneeded variables
rm(CRS.new, new_station_coordinates, recording_coordinates_to_transform, already_transformed_recording_coordinates, recording_locations, recording_metadata)

