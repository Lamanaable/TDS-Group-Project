#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=50gb
#PBS -N extraction
#PBS -q med-bio

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/scripts
module load anaconda3/personal

ukb_path=/rds/general/user/wq21/projects/hda_21-22/live/TDS/General/Data/ukb47946.csv

Rscript 2-extract_selected-copy.R $ukb_path 

