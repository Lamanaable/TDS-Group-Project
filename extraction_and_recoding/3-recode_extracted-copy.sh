#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N recoding
#PBS -q med-bio

cd /rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/scripts
module load anaconda3/personal

Rscript 3-recode_variables_copy.R

