build.non.wear.summary <-
  function(events_file_data){
    non_wear_data <- events_file_data[which(events_file_data$activity %in% c(4)),]

    non_wear_by_uid <- non_wear_data %>%
      dplyr::group_by(.data$uid, .data$activity) %>%
      dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)

    if(nrow(non_wear_by_uid) > 0){
      non_wear_by_uid$activity <- 4
      non_wear_by_uid$days <- length(unique(events_file_data$Date))
      non_wear_by_uid$duration_by_day <- non_wear_by_uid$bout_duration / non_wear_by_uid$days
    }
    return(non_wear_by_uid)
  }

build.sedentary.summary <-
  function(events_file_data){
    sedentary_data <- merge.adjacent.sedentary(events_file_data)
    sedentary_data[which(sedentary_data$activity == 5),]$activity <- rep(0,length(which(sedentary_data$activity == 5)))
    sedentary_data <- sedentary_data[which(sedentary_data$activity %in% c(0,3.1,3.2)),]

    # Set primary lying as time in bed
    sedentary_data[which(sedentary_data$activity %in% c(3.1)),]$activity <- rep(3,length(which(sedentary_data$activity %in% c(3.1))))
    # Set sedentary and seconday lying as sedentary
    sedentary_data[which(sedentary_data$activity %in% c(3.2)),]$activity <- rep(0,length(which(sedentary_data$activity %in% c(3.2))))
    sedentary_data <- categorise.sedentary.duration(sedentary_data)

    sedentary_data <- summarise.sedentary.by.group(sedentary_data)
    sedentary_data$bout_duration <- sedentary_data$bout_duration / sedentary_data$days

    sedentary_data[which(sedentary_data$activity == 0),]$activity <- "Sedentary"
    sedentary_data[which(sedentary_data$activity == 3),]$activity <- "Time in Bed"
    sedentary_data$bout_length <- paste(sedentary_data$activity , " (", sedentary_data$duration, ")", sep = "")

    sedentary_data <- sedentary_data[,c(1,9,5,4,6)]

    return(sedentary_data)
  }

merge.adjacent.sedentary <-
  function(data){
    adjacent_sedentary <- which((data$activity + c(tail(data$activity,-1),-10)) %in% c(0,6.2,6.4) & data$uid == c(tail(data$uid,-1),1))
    if(length(adjacent_sedentary) > 0){
      data[adjacent_sedentary,]$interval <- data[adjacent_sedentary,]$interval + data[(adjacent_sedentary + 1),]$interval
      data[adjacent_sedentary,]$MET_h <- data[adjacent_sedentary,]$MET_h + data[(adjacent_sedentary + 1),]$MET_h
      data <- data[-(adjacent_sedentary + 1),]
    }
    return(data)
  }

categorise.sedentary.duration <-
  function(data){
    duration_time <- c(14400,7200,3600,1800)
    duration_label <- c("2 - 4 hours","1 - 2 hours","30 min - 1 hour","< 30 min")
    duration_overflow <- "4 hours +"

    data$duration <- duration_overflow
    for(i in (1:length(duration_time))){
      data[which(data$interval <= duration_time[i]),]$duration <- duration_label[i]
    }
    data$duration <- factor(data$duration, levels = c(duration_overflow,duration_label))

    return(data)
  }

summarise.sedentary.by.group <-
  function(data){
    days_per_uid <- data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::select(.data$uid, .data$Date) %>%
      dplyr::distinct() %>%
      dplyr::summarise(days = n())

    sedentary_by_uid <- data %>%
      dplyr::group_by(.data$uid, .data$duration, .data$activity) %>%
      dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)

    sedentary_by_uid <- inner_join(sedentary_by_uid,days_per_uid, by = "uid")
    sedentary_by_uid$daily_bouts <- sedentary_by_uid$bouts / sedentary_by_uid$days
    sedentary_by_uid$daily_duration <- sedentary_by_uid$bout_duration / sedentary_by_uid$days

    return(sedentary_by_uid)
  }

build.sedentary.bout.summary <-
  function(events_file_data){
    library(dplyr)
    sedentary_bouts <- events_file_data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::filter(.data$activity == 0) %>%
      dplyr::summarise(bouts = n())
    valid_days <- events_file_data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::select(.data$uid, .data$Date) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(valid_days = n())
    sedentary_bouts <- inner_join(sedentary_bouts,valid_days, by = "uid")
    sedentary_bouts$sedentary_bout_per_day <- sedentary_bouts$bouts / sedentary_bouts$valid_days
    return(sedentary_bouts)
  }

#################################################

build.upright.summary <-
  function(events_file_data){
    library(dplyr)
    days_per_events <- events_file_data %>%
      dplyr::select(.data$uid, .data$Date) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(dates = n())
    events_file_data[which(events_file_data$activity == 2.1),]$activity <- rep(2,length(which(events_file_data$activity == 2.1)))
    events_file_data <- events_file_data %>%
      dplyr::filter(.data$activity %in% c(1,2))
    events_file_data <- categorise.stepping.bouts(events_file_data)

    upright_data <- generate.chart.data(events_file_data,days_per_events)

    return(upright_data)
  }

categorise.stepping.bouts <-
  function(events_data){
    events_data$bout_length <- "Stepping (1 minute +)"
    events_data[which(events_data$activity == 2 & events_data$interval < 60),]$bout_length <-"Stepping (< 1 minute)"
    events_data[which(events_data$activity == 1),]$bout_length <- "Quiet Standing"

    return(events_data)
  }

generate.chart.data <-
  function(events_data,days_per_file){
    events_data$date <- as.Date(events_data$time)
    events_data <- events_data %>%
      dplyr::group_by(.data$uid, .data$bout_length) %>%
      dplyr::summarise(bout_duration = sum(.data$interval), bouts = length(.data$interval))
    events_data <- inner_join(events_data,days_per_file, by = "uid")
    events_data$bout_duration <- events_data$bout_duration / events_data$dates
    # convert the duration from seconds to hours
    events_data$bout_duration <- events_data$bout_duration / 3600
    return(events_data)
  }

#################################################

build.walk.test.summary <-
  function(walk_test_data){
    walk_test_summary <- walk_test_data %>%
      dplyr::group_by(.data$uid, .data$date) %>%
      dplyr::select(.data$date, .data$uid, .data$steps, .data$duration) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::filter(.data$duration == min(.data$duration))

    return(walk_test_summary)
  }

build.daily.walk.test.summary <-
  function(walk_test_data){
    walk_test_summary <- walk_test_data %>%
      dplyr::group_by(.data$uid, .data$date) %>%
      dplyr::select(.data$uid, .data$steps, .data$duration) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::filter (.data$duration == min(.data$duration)) %>%
      dplyr::distinct()
    walk_test_summary$steps <- walk_test_summary$steps * 2
    return(walk_test_summary)
  }

build.stepping.summary <-
  function(events_file_data){
    stepping_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, date = .data$Date) %>%
      dplyr::summarise(daily_steps = sum(.data$steps)) %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(mean_steps = mean(.data$daily_steps), median_steps = stats::median(.data$daily_steps),
                sd_steps = tidyr::replace_na(stats::sd(.data$daily_steps),0), iqr_steps = stats::IQR(.data$daily_steps)/2)

    return(stepping_summary)
  }

#################################################

process.breaks.in.time.in.bed <-
  function(data){
    uid <- unique(data$uid)

    breaks_data <- data.frame(matrix(ncol = 4, nrow = length(uid)))
    colnames(breaks_data) <- c("uid","total_breaks","valid_days","breaks_per_day")

    for(i in (1:length(uid))){
      events_ext <- data[which(data$uid == uid[i]),]

      event_start <- c(0,events_ext$activity)
      event_end <- c(events_ext$activity,0)

      waking_day_start <- c(-1,events_ext$waking_day)
      waking_day_end <- c(events_ext$waking_day,-1)

      interruptions <- which(event_start == 3.1 & event_end == 1 & waking_day_start == 0 & waking_day_end == 0)
      interruptions <- events_ext[interruptions,]
      interruptions$Date <- as.Date(interruptions$time)
      breaks_data[i,]$uid <- uid[i]
      breaks_data[i,]$total_breaks <- nrow(interruptions)
      breaks_data[i,]$valid_days <- length(unique(as.Date(events_ext$time)))
      breaks_data[i,]$breaks_per_day <- breaks_data[i,]$total_breaks / breaks_data[i,]$valid_days
    }
    return(breaks_data)
  }
