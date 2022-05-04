#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N QC

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal

ukb_uncode=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_extracted_new_2.rds

CRC_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_linked.rds

Rscript 5_Quality_Check.R $ukb_uncode $CRC_path

