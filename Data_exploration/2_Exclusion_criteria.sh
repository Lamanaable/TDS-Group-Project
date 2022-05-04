#PBS -l walltime=2:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N Exclusion

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

data_path=/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_extracted.rds
CRC_path=/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_new1.rds

Rscript 2_Exclusion_criteria.R $data_path $CRC_path

