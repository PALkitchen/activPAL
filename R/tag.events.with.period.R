tag.events.with.period <-
  function(events_file_data){
    transition_periods <- events_file_data %>%
      dplyr::mutate(id = 1:n()) %>%
      dplyr::group_by(period_date, period_name) %>%
      dplyr::mutate(group_id = 1:n()) %>%
      dplyr::filter(group_id == 1) %>%
      dplyr::filter(id != 1)
    events_file_data$event_count <- 1
    if(nrow(transition_periods) == 0){
      return(events_file_data)
    }
    for(i in (1:nrow(transition_periods))){
      if(events_file_data[(transition_periods[i,]$id-1),]$activity ==
         events_file_data[(transition_periods[i,]$id),]$activity){
        if(events_file_data[(transition_periods[i,]$id-1),]$interval >
           events_file_data[(transition_periods[i,]$id),]$interval){
          events_file_data[(transition_periods[i,]$id),]$event_count <- 0
        } else {
          events_file_data[(transition_periods[i,]$id-1),]$event_count <- 0
        }
      }
    }
    return(events_file_data)
  }

