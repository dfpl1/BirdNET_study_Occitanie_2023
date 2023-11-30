#!/bin/bash
while IFS="," read -r recording_id station_id recording_date recording_time recording_file_path annotation_file_path ARU file_name local_directory server_directory_name server_directory_URL hard_drive_directory second_recording third_recording fourth_recording commune lon lat recorder habitat comment altitude week_number hard_drive_recording_file_path hard_drive_second_recording_file_path hard_drive_third_recording_file_path hard_drive_fourth_recording_file_path
do
  wav_file_path=`echo "../../../Recordings/Selected/$recording_file_path"`
  results_file_name=`echo "${wav_file_path/.wav/"_BirdNET_Analyzer_species_list.csv"}"`
  generate_list_command=`echo "python3 ../../BirdNET-Analyzer/species.py --o $results_file_name --lat $lat --lon $lon --week $week_number --threshold 0.02 "`
  eval "$generate_list_command"
  echo "Species list obtained successfully for recording $wav_file_path"
done < <(tail -n +2 ../../../CSVs/recordings_extended.csv)



