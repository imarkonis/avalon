library(data.table)
library(randomForest)

events <- readRDS('/data/lbou_events.rds')
events <- events[, 2:13]
events <- unique(events)

events_for_rf <- events[, c(28, 29, 31:41)]
not_na_count <- sapply(events_for_rf, function(y) sum(length(which(!is.na(y)))))
events_for_rf[, SVH := NULL]
events_for_rf[, T05prum := NULL]

events_complete <- events_for_rf[complete.cases(events_for_rf)]
events_for_rf <- events_complete[, -2]

test <- randomForest(event ~., data = events_for_rf)
varImpPlot(test)
