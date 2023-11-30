algorithm <- "BirdNET"
# algorithm <- "Google"

algorithm_text <- dplyr::case_when(algorithm == "Google" ~ "/Google",
                                   TRUE ~ "/BirdNET_")
algorithm_label <- ""

if (algorithm == "Google") {
  BirdNET_version <- ""
} else {
  # BirdNET_version <- "Lite"
  BirdNET_version <- "Analyzer"
}

filtered <- FALSE
filtered_text <- dplyr::case_when(filtered ~ "/Filtered",
                           TRUE ~ "")
filtered_label <- dplyr::case_when(filtered ~ " in filtered recordings",
                            TRUE ~ " in unfiltered recordings")

overlap <- 2
overlap_text <- dplyr::case_when(algorithm != "BirdNET" ~ "",
                                 overlap == 1 ~ "/Overlap_1s",
                                 overlap == 2 ~ "/Overlap_2s",
                                 TRUE ~ "")
overlap_label <- dplyr::case_when(algorithm != "BirdNET" ~ "",
                                  overlap == 1 ~ " (1 s overlap)",
                                  overlap == 2 ~ " (2 s overlap)",
                                  TRUE ~ " (No overlap)")

detection_sensitivity <- 1.5
ds_text <- dplyr::case_when(algorithm != "BirdNET" ~ "",
                            detection_sensitivity == 1 ~ "/ds10",
                            detection_sensitivity == 1.5 ~ "/ds15",
                                 TRUE ~ "/ds05")
ds_label <- dplyr::case_when(algorithm != "BirdNET" ~ "",
                             detection_sensitivity == 1 ~ " (sensitivity = 1)",
                             detection_sensitivity == 1.5 ~ " (sensitivity = 1.5)",
                                  TRUE ~ " (sensitivity = 0.5)")
