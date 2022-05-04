#PBS -l walltime=15:00:00
#PBS -l select=1:ncpus=1:mem=80gb
#PBS -N Link2

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

ukb_new_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_extracted_new_2.rds
ukb_recoded_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_final_2.rds

CRC_final_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_final_V2_5_year.rds

Rscript 4_Link_variables2.R $ukb_new_path $ukb_recoded_path $CRC_final_path

