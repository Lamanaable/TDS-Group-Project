#PBS -l walltime=01:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N covars_women

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

CRC_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_QC.rds

women_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/covars_women.rds

ukb_uncode=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_extracted_new_2.rds

Rscript 6_covars_women.R $CRC_path $women_path $ukb_uncode

