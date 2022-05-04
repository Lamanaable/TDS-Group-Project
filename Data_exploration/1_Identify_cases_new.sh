#PBS -l walltime=10:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N Explore_ICD9_ICD10

cd /rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration
module load anaconda3/personal


data_path=/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_extracted_new.rds

Rscript 1_Identify_cases_new.R $data_path

