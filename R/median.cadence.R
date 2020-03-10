median.stepping.cadence.bands.file <-
  function(events_file){
    library(dplyr)
    library(tidyr)
    lower_bound <- c(10,60)
    upper_bound <- c(60,86400)

    events_file <- events_file[which(events_file$activity == 2 & events_file$interval >= lower_bound[1]),]
    events_file$cadence <- events_file$steps / (events_file$interval / 60)
    events_file$group <- ""
    for (i in (1:length(lower_bound))){
      in_group <- which(events_file$interval >= lower_bound[i] & events_file$interval < upper_bound[i])
      if(length(in_group) > 0 ){
        # Convert the maximum and minimum duration into a label
        duration_label <- paste(lubridate::seconds_to_period(lower_bound[i]),"to",lubridate::seconds_to_period(upper_bound[i]))
        duration_label <- gsub("d"," day",duration_label)
        duration_label <- gsub("S"," seconds",duration_label)
        duration_label <- gsub("M"," minutes",duration_label)
        duration_label <- gsub("H"," hours",duration_label)
        duration_label <- gsub(" 0 seconds","",duration_label)
        duration_label <- gsub(" 0 minutes","",duration_label)
        duration_label <- gsub(" 0 hours","",duration_label)
        duration_label <- gsub(" 1 minutes"," 1 minute",duration_label)
        duration_label <- gsub(" 1 hours"," 1 hour",duration_label)
        events_file[in_group,]$group <- duration_label
      }
    }

    median_cadence_by_group <- events_file %>% dplyr::group_by(.data$uid, .data$group) %>%
      dplyr::summarise(median_cadence = weighted.median(.data$cadence,.data$interval))
    median_cadence_by_group[which(median_cadence_by_group$group == "1 minutes to 1 day"),]$group <- "1 minute +"
    median_cadence_by_group[which(median_cadence_by_group$group == "10 seconds to 1 minute"),]$group <- "< 1 minute"
    return(median_cadence_by_group)
  }
