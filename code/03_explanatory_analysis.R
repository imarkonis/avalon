library(data.table)
library(ggplot2)
library(caret)
library(doParallel)
library(foreach)
library(randomForest)
library(DALEX)

ncores <- detectCores() - 1
cs <- makeCluster(ncores)
registerDoParallel(cs)

load('./results/som_4x4_4_vars.rdata') 
my_palette <- colorRampPalette(c("#4C3F54", "#486824", "#F2C057", "#D13525"))
n_groups <- 16 # number of groups with similar properties | each node is a group

heatmap(cor(ros_events[, 1:14]))
iterations <- rep(10000, ncores)
data_size <- nrow(ros_subset)
train_size <- .8 * data_size

ros_subset$node <- as.factor(ros_som$unit.classif)
ros_subset[, n_ros_group := .N, by = node] #number of ros events per hclust group

colnames(ros_subset)
train_data_maxQ <- ros_subset[1:train_size, c(2:3, 5:7, 11, 13:15)] 
validation_data_maxQ <- ros_subset[(train_size + 1):data_size, c(2:3, 5:7, 11, 13:15)]

#decide which is best
maxQ_rf_model <- train(maxQ ~ ., data = train_data_maxQ, method = 'rf') 
maxQ_rf_model <- foreach(ntree = iterations, .combine = randomForest::combine, 
                         .multicombine = TRUE, .packages = 'randomForest') %dopar% {
                           randomForest(maxQ ~ ., data = train_data_maxQ, ntree = ntree)
                         }

varImpPlot(maxQ_rf_model)
var_imp <- data.frame(importance(maxQ_rf_model, type = 2))
var_imp$Variables <- row.names(var_imp)  
var_imp[order(var_imp$IncNodePurity, decreasing = T), ]

explain_maxQ_rf <- explain(model = maxQ_rf_model,  
                           data = train_data_maxQ[, -6],
                           y = train_data_maxQ$maxQ, 
                           label = "Random Forest")
maxQ_rf_perform <- model_performance(explain_maxQ_rf)

vip_maxQ_rf <- feature_importance(explain_maxQ_rf, n_sample = 8000,
                                  loss_function = loss_root_mean_square) 

plot(vip_maxQ_rf, max_vars = 10) +
  ggtitle("Permutation variable importance", 
          "Average variable importance based on 8,000 permutations") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

bd_maxQ_rf <- predict_parts(explainer = explain_maxQ_rf,
                            new_observation = validation_data_maxQ[1, ],
                            type = "break_down")
plot(bd_maxQ_rf) 

bd_maxQ_rf <- predict_parts(explainer = explain_maxQ_rf,
                            new_observation = validation_data_maxQ[1, ],
                            type = "shap")

bd_maxQ_rf <- predict_parts(explainer = explain_maxQ_rf,
                            new_observation = validation_data[1, ],
                            type = "break_down")
plot(bd_maxQ_rf) 

loss_root_mean_square(observed = train_data_maxQ$maxQ, 
                      predicted = predict(maxQ_rf_model, validation_data_maxQ))

maxQ_vimp_50 <- model_parts(explainer = explain_maxQ_rf, 
                            loss_function = loss_root_mean_square,
                            B = 50,
                            type = "difference")

plot(maxQ_vimp_50) +
  ggtitle("Mean variable-importance over 50 permutations", "") 

maxQ_ld_rf <- model_profile(explainer = explain_maxQ_rf,
                            type       = "conditional",
                            variables  = c("EventPrec", "AveTLo"))

plot(maxQ_ld_rf) +
  ggtitle("Local-dependence profiles for EventPrec and AveTLo", "")

maxQ_al_rf <- model_profile(explainer = explain_maxQ_rf,
                            type       = "accumulated",
                            variables  = c("EventPrec", "AveTLo"))

plot(maxQ_al_rf) +
  ggtitle("Accumulated local profiles for EventPrec and AveTLo", "")

ggsave('./results/plots/variable_importance_old_vars_boxplot_10_Qtypes_rev.png', g1, 'png', 
       width = 20, height = 13, units = 'cm')

data_for_rf <- ros_subset[, c(2:3, 5, 7:10, 14:16)]
ros_rf_EventMelt <- foreach(ntree = iterations, .combine = randomForest::combine, 
                            .multicombine = TRUE, .packages = 'randomForest') %dopar% {
                              randomForest(EventMelt ~ ., data = data_for_rf, ntree = ntree)
                            }
varImpPlot(ros_rf_EventMelt)
var_imp = data.frame(importance(ros_rf_EventMelt, type = 2))
var_imp$Variables = row.names(var_imp)  
print(var_imp[order(var_imp$IncNodePurity, decreasing = T), ])

data_for_rf <- ros_subset[, c(2:3, 5, 7:10, 14:16)]
ros_rf <- foreach(ntree = iterations, .combine = randomForest::combine, 
                  .multicombine = TRUE, .packages = 'randomForest') %dopar% {
                    randomForest(Qmax_time_norm ~ ., data = data_for_rf, ntree = ntree)
                  }
varImpPlot(ros_rf)
var_imp = data.frame(importance(ros_rf, type = 2))
var_imp$Variables = row.names(var_imp)  
print(var_imp[order(var_imp$IncNodePurity, decreasing = T), ])
