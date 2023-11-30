library(dplyr)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

metadata <- read.csv2("./metadata_acoustic_Aurignac.csv")

metadata <- metadata %>%
  mutate(date.start = as.Date(date.start,format="%d/%m/%Y"),
         date.end = as.Date(date.start,format="%d/%m/%Y"),
         day.start = format(date.start, format = "%d"),
         month.start = format(date.start, format = "%m"),
         day.end = format(date.end, format = "%d"),
         month.end = format(date.end, format = "%m"),
         month_half.start = case_when(day.start < 16 ~ '1st half',
                                      TRUE ~ '2nd half'),
         month_half.end = case_when(day.end < 16 ~ '1st half',
                                      TRUE ~ '2nd half'),
         period = paste(month_half.start, months(date.start)),
         site_id = row_number()) %>%
  select(-month_half.start, -month_half.end)

SM4_sites <- metadata %>%
  filter(ARU == 'SM4') %>%
  select(site_id) %>%
  unique()

audiomoth_sites <- metadata %>%
  filter(ARU == 'Audiomoth') %>%
  select(site_id) %>%
  unique()

# All 35 SM4 recording sites will be selected, and 25 Audiomoth recording sites will be randomly selected from the pool of 36 total sites recorded with Audiomoth recorders
selected_audiomoth_sites <- data.frame(c(SM4_sites$site_id, sample(audiomoth_sites$site_id, 25)))
colnames(selected_audiomoth_sites) <- c("site_id")
selected_audiomoth_sites <- selected_audiomoth_sites %>%
  left_join(metadata,
            by = "site_id") 

selected_audiomoth_sites <- arrange(selected_audiomoth_sites, site_id)

site_count_by_type_and_habitat <- selected_audiomoth_sites %>%
  filter(!is.na(station)) %>%    
  group_by(ARU, habitat) %>%          
  summarise(n_stations = n_distinct(station))

site_count_by_type_and_date <- selected_audiomoth_sites %>%
  group_by(ARU, period) %>%          
  summarise(n_sites = n_distinct(site_id))

write.csv(selected_audiomoth_sites, "./selected_audiomoth_sites.csv", row.names = FALSE)
