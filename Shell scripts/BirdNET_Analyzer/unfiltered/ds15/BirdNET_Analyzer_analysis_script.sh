#!/bin/bash
while IFS="," read -r recording_id station_id recording_date recording_time recording_file_path annotation_file_path ARU file_name local_directory server_directory_name server_directory_URL hard_drive_directory second_recording third_recording fourth_recording commune lon lat recorder habitat comment altitude week_number hard_drive_recording_file_path hard_drive_second_recording_file_path hard_drive_third_recording_file_path hard_drive_fourth_recording_file_path
do
  wav_file_path=`echo "../../../Recordings/Selected/$recording_file_path"`
  results_file_name=`echo "${wav_file_path/.wav/"_BirdNET_Analyzer_results_ds15.csv"}"`  
  temp_species_list_file_name=`echo "${wav_file_path/.wav/"_BirdNET_Analyzer_species_list.csv"}"`
  species_list_file_name=`echo "../../${temp_species_list_file_name/..\/..\/..\//""}"`
  analysis_command=`echo "python3 ../../BirdNET-Analyzer/analyze.py --i $wav_file_path --o $results_file_name --week $week_number --slist $species_list_file_name --sensitivity 1.5"`
  eval "$analysis_command"
  echo "Results obtained successfully for recording $wav_file_path"
done < <(tail -n +2 ../../../CSVs/BirdNET_Analyzer/ds15/recordings_yet_to_analyze.csv)


