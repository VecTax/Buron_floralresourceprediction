### Code for Buron et al. (2026)
#

source("scripts/initialisation_prairies.R")
library(caret)
library(randomForest)

#Input data
data_grassland = read_xlsx("data_Burongrassland.xlsx")

#List of predictors
all_parameters_model = c("NNI_threemaxesmean", "EBI_threemaxesmean",
                         "OC",  "argiles", "pH_H2O",
                         "temp_climat",
                         "cum_dd",
                         "biotope3",
                         "cropland_area", "shannon_landscape",
                         "longest_streak", "sum_history",
                         "closest_mowingevent", "cumulevents_2023_mosaic")

#List of predicted variables
resources_list = c("prod_pollen_fl_total_log",
                   "pollen_shannon",
                   "sugar_nectar_fl_total_log",
                   "nectar_shannon",
                   "richness_log") 

### Random forest - cross-validation training ---------
base_seed = 123
models_list = list()
model_R2 = list()
for (i in 1:5) {
  choice = i
  var_ressource = resources_list[choice]
  cat("Modelling on:", var_ressource, "\n==========================================\n")
  
  #Create formula with all predictors
  formula_rf = as.formula(paste0(var_ressource, " ~ ", paste(all_parameters_model, collapse = " + ")))
  
  ctrl = trainControl(method = "cv", number = 10) #cross validation training
  data_clean = data_grassland %>%
    select(all_of(all_parameters_model), all_of(var_ressource)) %>% 
    na.omit()
  
  R2_temp = list()
  n_runs = 10 #number of runs per cross validation training
  pb = txtProgressBar(min = 0, max = n_runs, style = 3)
  
  for (k in 1:n_runs) {
    set.seed(base_seed + i * 1000 + k) #keep the same seed across the loop
    #Model training
    model = train(formula_rf, data = data_clean, 
                  method = "rf", trControl = ctrl)
    
    R2 = model$results %>%
      filter(mtry == model$bestTune$mtry) %>% #R² based on mtry best output
      pull(Rsquared)
    R2_temp = c(R2_temp, R2)
    setTxtProgressBar(pb, k) 
  }    
  close(pb)
  
  models_list[[var_ressource]] = model
  R2 = mean(unlist(R2_temp))
  model_R2[[var_ressource]] = R2
}

#Plot - predicted VS observed
plot_list = list()
for (var in names(models_list)) {
  model = models_list[[var]]
  R2 = model_R2[[var]]
  
  data_plot = data_grassland %>%
    select(all_of(var), all_of(all_parameters_model)) %>%
    na.omit()
  
  #Predict based on old data
  data_plot$predicted = predict(model, newdata = data_plot)
  if (var %in% c("prod_pollen_fl_total_log",
                 "sugar_nectar_fl_total_log",
                 "richness_log")) {
    data_plot$predicted = exp(data_plot$predicted)
    data_plot[[var]] = exp(data_plot[[var]])
    p = ggplot(data_plot, aes(x = .data[[var]], y = predicted)) +
      geom_point(alpha = .5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_x_log10()+
      scale_y_log10()
  } else {
    p = ggplot(data_plot, aes(x = .data[[var]], y = predicted)) +
      geom_point(alpha = .5) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed")
  }
  
  p=p+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.title = element_blank())+
    labs(x = "Observed", y = "Predicted", title = labelling_points(var)) +
    annotate("text",
             x = Inf, y = Inf,
             label = paste0("R² = ", round(R2, 3)),
             hjust = 3.3, vjust = 1.8)
  
  plot_list[[var]] = p
}

ggarrange(
  plotlist = plot_list,
  ncol = 2,
  nrow = 3,
  common.legend = FALSE,
  align = "hv") %>%
  annotate_figure(
    bottom = text_grob("Observed"),
    left = text_grob("Predicted", rot = 90))

ggsave("observedVSpredicted.svg", units = "px", width = 2000, height = 2000)

### Variable importance ---------
plot_list_importance = list()
importance_matrix = tibble()
for (var_ressource in names(models_list)) {
  
  #Create formula with all predictors
  formula_rf = as.formula(paste0(var_ressource, " ~ ", paste(all_parameters_model, collapse = " + ")))
  
  data_rf = data_grassland %>% 
    select(all_of(c(var_ressource, all_parameters_model))) %>% 
    na.omit()
  
  #Perform multiple runs for RF
  n_runs = 100
  cat(paste0("Random forest on ", var_ressource," - Numbers of runs: ", n_runs," \n"))
  
  importance_results = list()
  pb = txtProgressBar(min = 0, max = n_runs, style = 3)
  for(i in 1:n_runs){
    set.seed(i)
    data_rf_i = data_rf[sample(nrow(data_rf)), ]
    
    #Train model
    model_rf = randomForest(
      formula_rf,
      data = data_rf_i,
      importance = TRUE,
      ntree = 2000 #Number of trees per model
    )
    
    #Get model importance
    importance_results[[i]] = importance(model_rf)[, "%IncMSE"]
    setTxtProgressBar(pb, i) 
  }
  close(pb)
  
  importance_results = bind_rows(importance_results) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(var = var_ressource)
  
  importance_matrix = rbind(importance_matrix, importance_results)
}

#Plotting order
global_order = importance_matrix %>%
  group_by(name) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  arrange(mean_value) %>%
  pull(name)

#Plot
plot_list_importance = list()
i = 1
for (var_ressource in names(models_list)) {
  dataplot = importance_matrix %>% 
    filter(var == var_ressource) %>% 
    group_by(name) %>% 
    summarise(mean = mean(value),
              sd = sd(value),
              interval_min = mean-sd,
              interval_max = mean+sd)
  
  dataplot$name = factor(dataplot$name, levels = global_order)
  
  p=ggplot(dataplot)+
    aes(x=name, y=mean)+
    geom_col(fill = "grey70")+
    geom_errorbar(aes(
      ymin = mean - sd,
      ymax = mean + sd
    ), width = 0.2) +
    theme_bw()+
    theme(axis.text.x = element_text(angle=60, hjust=1), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          legend.position = "")+
    labs(title = paste0(letters[i], ") ", labelling_points(var_ressource)))+
    scale_x_discrete(labels = labelling)+
    coord_cartesian(
      ylim = c(min(dataplot$interval_min, na.rm = TRUE),
               max(dataplot$interval_max, na.rm = TRUE)))
  
  plot_list_importance[[var_ressource]] = p
  i = i + 1
}

ggarrange(
  plotlist = plot_list_importance,
  ncol = 2,
  nrow = 3,
  common.legend = FALSE,
  align = "hv") %>%
  annotate_figure(left = text_grob("Variable importance (%)", rot = 90))

ggsave("allRFimportance.svg", units = "px", width = 1800, height = 3000)

