setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis")

AUC_df <- data.frame(model_name = c("Baseline",
                                    "Baseline (colon)",
                                    "Baseline (rectum)",
                                    "ss LASSO", 
                                    "ss sgPLS", 
                                    "Intersection",
                                    "SVM radial",
                                    "Random forest"),
                            AUC = c(0.518946,
                                    0.532,
                                    0.519,
                                    0.549,        
                                    0.554,     
                                    0.564,
                                    0.569,
                                    0.55),    
                             CI2.5 = c(0.486, 
                                       0.494,
                                       0.463,
                                       0.516,     
                                       0.521,    
                                       0.531,
                                       0.537,
                                       0.511),   
                           CI97.5 = c(0.5493,
                                      0.568,
                                      0.576,
                                      0.582,      
                                      0.586,     
                                      0.596,
                                      0.601,
                                      0.611))    
AUC_df


AUC_df$model_name <- as.factor(AUC_df$model_name)
AUC_df$model_name <- relevel(AUC_df$model_name, "Random forest")
AUC_df$model_name <- relevel(AUC_df$model_name, "SVM radial")
AUC_df$model_name <- relevel(AUC_df$model_name, "Intersection")
AUC_df$model_name <- relevel(AUC_df$model_name, "ss sgPLS")
AUC_df$model_name <- relevel(AUC_df$model_name, "ss LASSO")
AUC_df$model_name <- relevel(AUC_df$model_name, "Baseline (rectum)")
AUC_df$model_name <- relevel(AUC_df$model_name, "Baseline (colon)")
AUC_df$model_name <- relevel(AUC_df$model_name, "Baseline")
levels(AUC_df$model_name)


ggplot(data = AUC_df, aes(x = model_name, y = AUC, ymin = CI2.5, ymax = CI97.5)) + 
  geom_point() +
  geom_linerange() +
  geom_hline(yintercept = 0.5, color = 'red',alpha=0.5,linetype=2) +
  theme_bw() +
  ylab("AUC (95% CI)") +
  xlab("Model Name") +
  ggtitle("AUC Comparison with 95% CIs") +
  ggsave(filename="AUC_comparison.png")








