library(data.table)

lbou_aval <- fread('data/raw/Avalanche_event_1961_2021.csv')
lbou_meteo <- fread('data/raw/LBOU_daily_1961_2020.csv')

###

lbou_aval[, date := as.Date(date)]
lbou_aval_events <- lbou_aval[event == 1]

lbou_meteo[, date := as.Date(date)]
lbou_events_only <- lbou_meteo[lbou_aval_events, on = "date"]
lbou_events <- lbou_aval_events[lbou_meteo, on = "date"]

lbou_events[is.na(event), event := 0]
###

saveRDS(lbou_aval, './data/lbou_aval.rds')
saveRDS(lbou_meteo, './data/lbou_meteo.rds')
saveRDS(lbou_events, './data/lbou_events.rds')
saveRDS(lbou_events_only, './data/lbou_events_only.rds')

