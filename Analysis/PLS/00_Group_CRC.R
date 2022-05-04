CRC_data <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered.rds')

Outcome <- CRC_data[1:10]
Baseline <- CRC_data[11:33] 
Physical <- CRC_data[c(35,37,138:140)] 
SocioDemo <- CRC_data[c(34,36,38,39,53:58,67:71,77:87)] 
Lifestyle <- CRC_data[c(40:52,59,61:66,72:76,88:104)]
Family_history <- CRC_data[105:119]
Biomarkers <- CRC_data[120:137]
Meds_Ops <- CRC_data[c(60,141:167)]
Comorbidity <- CRC_data[168:225]

CRC_ordered <- cbind(Outcome,Baseline,Physical,SocioDemo,Lifestyle,
                     Family_history,Biomarkers,Meds_Ops,Comorbidity)
CRC_ordered <- CRC_ordered%>% rename(comorbidity_H_Eye_adnexa = comorbidity_G_Eye_adnexa,
                                     comorbidity_H_Ear = comorbidity_G_Ear,
                                     medication_diet_mineral_6719_Fish.oil=medication_diet_mineral_6719__Fish.oil..including.cod.liver.oil.,
                                     medication_vitamin_6155_Multivitamins=medication_vitamin_6155_Multivitamins.....minerals,
                                     comorbidity_20002_Gastro_Oesophageal_Reflux=comorbidity_20002_gastro.oesophageal.reflux..gord....gastric.reflux,
                                     operation_20004_cholecystectomy=operation_20004_cholecystectomy.gall.bladder.removal)

saveRDS(CRC_ordered,"/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V2.rds")
