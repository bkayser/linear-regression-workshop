
saveData <- function(i) {
    df <- tad[[i]] %>%
        mutate(day=factor(strftime(start_date, '%a'),
                          levels=c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')),
               minute_of_day=minute(start_date) + hour(start_date) * 60) %>%
        select(throughput=trans_throughput, 
               duration=trans_duration,
               day,
               minute_of_day,
               timestamp=start_date,
               start_time,
               external_duration,
               external_count,
               db_duration=trans_db_duration,
               db_count=trans_db_count)
    saveRDS(df, paste0("data/newrelic/appdata-", sprintf('%03i', i), '.RDS'))
    df
}

previewData <- function(i) {
    qplot(start_time, trans_duration, data=tad[[i]], alpha=0.5) + 
        aes(color=factor(strftime(start_date, '%a'))) + 
        scale_color_discrete()
}