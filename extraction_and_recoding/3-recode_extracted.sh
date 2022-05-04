<<<<<<< HEAD
#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=40gb
#PBS -N recoding
#PBS -q med-bio

cd /rds/general/user/hhe21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/scripts
module load anaconda3/personal

Rscript 3-recode_variables.R

||||||| bb562cd
=======
#PBS -l walltime=5:00:00
#PBS -l select=1:ncpus=1:mem=20gb
#PBS -N recoding
#PBS -q med-bio

cd /rds/general/user/bbodinie/projects/hda_21-22/live/TDS/General/extraction_and_recoding/scripts
module load anaconda3/personal

Rscript 3-recode_variables.R

>>>>>>> main
