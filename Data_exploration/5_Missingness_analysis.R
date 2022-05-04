missing <- readRDS('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/missing_table_5_year.rds')

missing <- missing[9:nrow(missing),]
mean(missing[,3])*100
missing <- missing[missing[,3]>0,]
mean(missing[,3])*100
missing <- as.data.frame(missing)

library(tidyverse)
missing$Vars  <- rownames(missing)

head(missing)

ggplot(data=missing,aes(x=Vars,y=100*missing_all)) +
  geom_bar(stat='identity',fill='royalblue') +
  theme_classic() +
  ylim(0,100) +
  ylab('missing rate (%)') +
  xlab('variables')

pdf('Outputs/missing_plot.pdf',width=6,height=3)
ggplot(data=missing,aes(x=100*missing_all)) +
  geom_bar(width = 0.1,fill='royalblue') +
  theme_classic() +
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent) +
  ylab('percentage of variables') +
  xlab('missing rate (%)')
dev.off()

  
  
  
