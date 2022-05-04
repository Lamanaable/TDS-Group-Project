#### STABILITY ANALYSIS PLOTS - LASSO/ENET , LAMBDA MIN/1SE

# Elastic net - lambda min ------------------------------------------------

stab<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/c_enet_5yr_stability_lambdamin.rds")

pdf("c_Stability_Analysis_enet_lambdamin.pdf", height = 8, width=8)
par(mar=c(12,5,2,4))
plot(stab[2:21]*100, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'percentage of selected (%)',
     ylim = c(0, 100), las = 1)
title(main='Elastic Net - Stability Analysis (lambda min)')
axis(1, at=seq(1,20), labels = names(stab)[2:21], las=2, cex.axis=0.7)
dev.off()

# Manually changing labels to include in presentation
dput(names(stab[2:21]))

var_labels<-c("Model - smoking pack", "General - TV time", "Diet - weekly beer", 
              "Biomaker - Neu", "Biomaker (incl bp) - pulse", "Medication - statins", "Operation - appendicectomy", 
              "Comorbidity - atherosclerotic heart disease", "Diet - smoking status previous", 
              "General - occupation home/family", "Comorbidity - ear", 
              "Comorbidity - gastritis duodenitis", "General - education high school", 
              "Medication - glucosamine", "Medication - beta blocker", 
              "Comorbidity - abnormalities of heart beat", "Diet - cereal", "Comorbidity - gastric ulcer", 
              "Comorbidity - liver", "Biomarker - T protein")

plot(stab[2:21]*100, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'Percentage selected (%)',
     ylim = c(0, 100), las = 1)
title(main='Elastic Net - Stability Analysis (lambda min)')
axis(1, at=seq(1,20), labels = var_labels, las=2, cex.axis=0.7)


# Elastic net - lambda 1SE ------------------------------------------------

stab<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/c_enet_5yr_stability_lambda1se.rds")

pdf("c_Stability_Analysis_enet_lambda1se.pdf", height = 8, width=8)
par(mar=c(12,5,2,4))
plot(stab[2:21]*100, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'percentage of selected (%)',
     ylim = c(0, 100), las = 1)
title(main='Elastic Net - Stability Analysis (lambda 1se)')
axis(1, at=seq(1,20), labels = names(stab)[2:21], las=2, cex.axis=0.7)
dev.off()

# Lasso - lambda min ------------------------------------------------

stab<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/b_5yr_lasso_stability_lambdamin.rds")

pdf("b_Stability_Analysis_lasso_lambdamin.pdf", height = 8, width=8)
par(mar=c(12,5,2,4))
plot(stab[1:20]*100, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'percentage of selected (%)',
     ylim = c(0, 100), las = 1)
title(main='Lasso- Stability Analysis (lambda min)')
axis(1, at=seq(1,20), labels = names(stab)[1:20], las=2, cex.axis=0.7)
dev.off()

# Lasso - lambda 1se ------------------------------------------------

stab<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/b_5yr_lasso_stability_lambda1se.rds")

pdf("b_Stability_Analysis_lasso_lambda1se.pdf", height = 8, width=8)
par(mar=c(12,5,2,4))
plot(stab[2:21]*100, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'percentage of selected (%)',
     ylim = c(0, 100), las = 1)
title(main='Lasso- Stability Analysis (lambda min)')
axis(1, at=seq(1,20), labels = names(stab)[2:21], las=2, cex.axis=0.7)
dev.off()