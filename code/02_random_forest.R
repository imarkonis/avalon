library(data.table)
library(ggplot2)
library(patchwork)
library(doParallel)
library(foreach)
library(randomForest)
library(DALEX)

###

ncores <- detectCores() - 4
cs <- makeCluster(ncores)
registerDoParallel(cs)

events <- readRDS('./data/lbou_events.rds')

###

events[, event := factor(event)]
events_for_rf <- events[, c(32, 36:47)]
not_na_count <- sapply(events_for_rf, function(y) sum(length(which(!is.na(y)))))
events_for_rf[, SVH := NULL]
events_for_rf[, T05prum := NULL]

events_for_rf <- events_for_rf[complete.cases(events_for_rf)]
summary(events_for_rf)

###

iterations <- rep(3000, ncores)
data_size <- nrow(events_for_rf)
train_size <- floor(.8 * data_size)

train_data <- events_for_rf[sample(train_size)]
validation_data <- events_for_rf[!events_for_rf$i.V1 %in% train_data$i.V1]
train_data <- train_data[, -2]
validation_data <- validation_data[, -2]
events_for_rf <- events_for_rf[, -2]

#heatmap(cor(events_for_rf))
#all_events_rf <- randomForest(event ~., data = events_for_rf)
#varImpPlot(all_events_rf)

avalan_rf_model <- foreach(ntree = iterations, .combine = randomForest::combine, 
                           .multicombine = TRUE, .packages = 'randomForest') %dopar% {
                             randomForest(event ~ ., data = train_data, ntree = ntree)
                           }

prediction_rf <- predict(avalan_rf_model, validation_data[, -1])
table(observed = validation_data$event, predicted = prediction_rf)

varImpPlot(avalan_rf_model)
#saveRDS(avalan_rf_model, './data/avalan_rf_model.rds')

explain_rf <- explain(model = avalan_rf_model,  
                      data = train_data[, -1],
                      y = train_data$event, 
                      label = "Random Forest")

eva_rf <- model_performance(explain_rf)
p1 <- plot(eva_rf, geom = "histogram") 
p2 <- plot(eva_rf, geom = "prc") 
p1 + p2

vip_50 <- model_parts(explainer = explain_rf, 
                      loss_function = loss_root_mean_square,
                      B = 1,
                      type = "difference")
plot(vip_50) +
  ggtitle("Mean variable importance over 50 permutations", "") 



