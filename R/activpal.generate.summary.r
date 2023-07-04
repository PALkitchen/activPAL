build.non.wear.summary <-
  function(events_file_data){
    #' @import dplyr
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
  function(events_file_data, daily_report = FALSE){
    sedentary_data <- merge.adjacent.sedentary(events_file_data)
    sedentary_data[which(sedentary_data$activity == 5),]$activity <- rep(0,length(which(sedentary_data$activity == 5)))
    sedentary_data <- sedentary_data[which(sedentary_data$activity %in% c(0,3.1,3.2)),]

    # Set primary lying as time in bed
    sedentary_data[which(sedentary_data$activity %in% c(3.1)),]$activity <- rep(3,length(which(sedentary_data$activity %in% c(3.1))))
    # Set sedentary and secondary lying as sedentary
    sedentary_data[which(sedentary_data$activity %in% c(3.2)),]$activity <- rep(0,length(which(sedentary_data$activity %in% c(3.2))))
    sedentary_data <- categorise.sedentary.duration(sedentary_data)

    sedentary_data <- summarise.sedentary.by.group(sedentary_data, daily_report)

    sedentary_data$activity <- as.character(sedentary_data$activity)

    sedentary_data[which(sedentary_data$activity == 0),]$activity <- "Sedentary"
    sedentary_data[which(sedentary_data$activity == 3),]$activity <- "Time in Bed"
    sedentary_data$bout_length <- paste(sedentary_data$activity , " (", sedentary_data$duration, ")", sep = "")
    sedentary_data$bout_length <- factor(sedentary_data$bout_length, levels = c("Time in Bed (< 30 min)","Time in Bed (30 min - 1 hour)","Time in Bed (1 - 2 hours)",
                                                                                "Time in Bed (2 - 4 hours)","Time in Bed (4 hours +)",
                                                                                "Sedentary (< 30 min)","Sedentary (30 min - 1 hour)","Sedentary (1 - 2 hours)",
                                                                                "Sedentary (2 - 4 hours)","Sedentary (4 hours +)"))

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
      data[which(data$interval <= duration_time[i]),]$duration <- rep(duration_label[i],length(which(data$interval <= duration_time[i])))
    }
    data$duration <- factor(data$duration, levels = c(duration_overflow,duration_label))

    return(data)
  }

summarise.sedentary.by.group <-
  function(data, daily_report = FALSE){
    #' @import dplyr

    days_per_uid <- data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::select(.data$uid, .data$Date) %>%
      dplyr::distinct() %>%
      dplyr::summarise(days = n())

    if(daily_report){
      sedentary_by_uid <- data %>%
        dplyr::mutate(date = .data$Date) %>%
        dplyr::group_by(.data$uid, .data$date, .data$duration, .data$activity) %>%
        dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)
    }else{
      sedentary_by_uid <- data %>%
        dplyr::group_by(.data$uid, .data$duration, .data$activity) %>%
        dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)

      sedentary_by_uid <- dplyr::inner_join(sedentary_by_uid,days_per_uid, by = "uid")
      sedentary_by_uid$daily_bouts <- sedentary_by_uid$bouts / sedentary_by_uid$days
      sedentary_by_uid$daily_duration <- sedentary_by_uid$bout_duration / sedentary_by_uid$days

      sedentary_by_uid$bout_duration <- sedentary_by_uid$bout_duration / sedentary_by_uid$days
    }
    return(sedentary_by_uid)
  }

build.sedentary.bout.summary <-
  function(events_file_data){
    #' @import dplyr
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
    sedentary_bouts <- dplyr::inner_join(sedentary_bouts,valid_days, by = "uid")
    sedentary_bouts$sedentary_bout_per_day <- sedentary_bouts$bouts / sedentary_bouts$valid_days
    return(sedentary_bouts)
  }

build.daily.sedentary.bout.summary <-
  function(events_file_data){
    #' @import dplyr
    sedentary_bouts <- events_file_data %>%
      dplyr::mutate(date = as.Date(.data$time)) %>%
      dplyr::group_by(.data$uid, .data$date) %>%
      dplyr::filter(.data$activity == 0) %>%
      dplyr::summarise(bouts = n())
    valid_days <- events_file_data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::select(.data$uid, date = .data$Date) %>%
      dplyr::distinct()
    sedentary_bouts <- dplyr::inner_join(sedentary_bouts,valid_days, by = c("uid","date"))
    return(sedentary_bouts)
  }

#################################################

build.upright.summary <-
  function(events_file_data, upright_bouts = FALSE){
    #' @import dplyr
    days_per_events <- events_file_data %>%
      dplyr::select(.data$uid, .data$Date) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(dates = n())
    events_file_data <- events_file_data %>%
      dplyr::filter(.data$activity %in% c(1,2,2.1))
    if(nrow(events_file_data) > 0){
      events_file_data <- categorise.stepping.bouts(events_file_data, upright_bouts = FALSE)
      upright_data <- generate.chart.data(events_file_data,days_per_events)
    }else{
      upright_data <- data.frame(days_per_events$uid)
      colnames(upright_data) <- "uid"
      upright_data$bout_length <- "Quiet Standing"
      upright_data$bout_duration <- 0
      upright_data$bouts <- 0
      upright_data$dates <- days_per_events$dates
    }
    return(upright_data)
  }

categorise.stepping.bouts <-
  function(events_data, upright_bouts = FALSE){
    if(upright_bouts){
      longest_stepping <- events_data %>%
        filter(activity == 2) %>%
        group_by(upright_bout) %>%
        summarise(max_stepping = max(interval))
      events_data <- left_join(events_data, longest_stepping, by = c("upright_bout"))
      events_data[is.na(events_data)] <- 0

      events_data$bout_length <- "Stepping (10 minutes +)"
      events_data[which(events_data$activity == 2 & events_data$max_stepping < 600),]$bout_length <- "Stepping (1 - 10 minutes)"
      events_data[which(events_data$activity == 2 & events_data$max_stepping < 60),]$bout_length <- "Stepping (< 1 minute)"
      events_data[which(events_data$activity == 1),]$bout_length <- "Quiet Standing"
      if(length(which(events_data$activity == 2.1)) > 0){
        events_data[which(events_data$activity == 2.1),]$bout_length <- "Cycling"
      }
      events_data <- events_data[,c(which(colnames(events_data) == "max_stepping"))]

      return(events_data)
    }
    events_data$bout_length <- "Stepping (10 minutes +)"
    events_data[which(events_data$activity == 2 & events_data$interval < 600),]$bout_length <- "Stepping (1 - 10 minutes)"
    events_data[which(events_data$activity == 2 & events_data$interval < 60),]$bout_length <- "Stepping (< 1 minute)"
    events_data[which(events_data$activity == 1),]$bout_length <- "Quiet Standing"
    if(length(which(events_data$activity == 2.1)) > 0){
      events_data[which(events_data$activity == 2.1),]$bout_length <- "Cycling"
    }
    return(events_data)
  }

generate.chart.data <-
  function(events_data,days_per_file){
    #' @import dplyr
    events_data$date <- as.Date(events_data$time)
    events_data <- events_data %>%
      dplyr::group_by(.data$uid, .data$bout_length) %>%
      dplyr::summarise(bout_duration = sum(.data$interval), bouts = length(.data$interval))
    events_data <- dplyr::inner_join(events_data,days_per_file, by = "uid")
    events_data$bout_duration <- events_data$bout_duration / events_data$dates
    # convert the duration from seconds to hours
    events_data$bout_duration <- events_data$bout_duration / 3600
    return(events_data)
  }

#################################################

build.walk.test.summary <-
  function(walk_test_data){
    #' @import dplyr
    walk_test_summary <- walk_test_data %>%
      dplyr::arrange(desc(.data$steps), .data$duration) %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::slice_head(n = 1)

    return(walk_test_summary)
  }

build.daily.walk.test.summary <-
  function(walk_test_data){
    #' @import dplyr
    walk_test_summary <- walk_test_data %>%
      dplyr::group_by(.data$uid, .data$date) %>%
      dplyr::select(.data$uid, .data$steps, .data$duration) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::filter(.data$duration == min(.data$duration)) %>%
      dplyr::distinct()
    walk_test_summary$steps <- walk_test_summary$steps * 2
    return(walk_test_summary)
  }

build.stepping.summary <-
  function(events_file_data){
    #' @import dplyr
    #' @import tidyr
    stepping_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, date = .data$Date) %>%
      dplyr::summarise(daily_steps = sum(.data$steps)) %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(mean_steps = mean(.data$daily_steps), median_steps = stats::median(.data$daily_steps),
                sd_steps = tidyr::replace_na(stats::sd(.data$daily_steps),0), iqr_steps = stats::IQR(.data$daily_steps)/2)

    return(stepping_summary)
  }

build.stepping.intensity.summary <-
  function(events_file_data){
    #' @import dplyr
    events_file_data$interval <- round(events_file_data$interval, 1)
    events_file_data <- group.stepping.by.upright.bout(events_file_data)
    events_file_data <- events_file_data %>%
      dplyr::filter(.data$steps > 0) %>%
      dplyr::mutate(cadence = .data$steps / (.data$interval / 60)) %>%
      dplyr::group_by(.data$upright_bout) %>%
      dplyr::mutate(longest_bout = max(.data$interval))
    events_file_data$category <- "VPA (> 125 spm)"
    events_file_data[which(events_file_data$cadence < 125),]$category <- "MVPA (100 - 125 spm)"
    events_file_data[which(events_file_data$cadence < 100),]$category <- "MPA (75 - 100 spm)"
    events_file_data[which(events_file_data$cadence < 75),]$category <- "LPA (< 75 spm)"
    events_file_data$category <- factor(events_file_data$category,
                                        levels = c("VPA (> 125 spm)","MVPA (100 - 125 spm)",
                                                   "MPA (75 - 100 spm)","LPA (< 75 spm)"))

    events_file_data$duration <- "long (>= 60s)"
    events_file_data[which(events_file_data$longest_bout < 60),]$duration <- "short (< 60s)"
    events_file_data$duration <- factor(events_file_data$duration,
                                        levels = c("short (< 60s)","long (>= 60s)"))

    stepping_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$Date, .data$category, .data$duration) %>%
      dplyr::summarise(time = sum(.data$interval)/60) %>%
      dplyr::group_by(.data$uid, .data$category, .data$duration) %>%
      dplyr::summarise(time = sum(.data$time)/length(unique(events_file_data$Date)))

    stepping_summary <- stepping_summary %>%
      dplyr::select(.data$uid, .data$category, .data$duration, .data$time)

    stepping_summary$short_percent <- round((sum(stepping_summary[which(stepping_summary$duration == "short (< 60s)"),]$time) / sum(stepping_summary$time) * 100),1)
    return(stepping_summary)
  }

build.bouted.stepping.summary <-
  function(events_file_data){
    #' @import dplyr
    events_file_data$interval <- round(events_file_data$interval, 1)
    events_file_data <- group.stepping.by.upright.bout(events_file_data)
    events_file_data <- events_file_data %>%
      dplyr::filter(.data$steps > 0) %>%
      dplyr::mutate(cadence = .data$steps / (.data$interval / 60)) %>%
      dplyr::group_by(.data$upright_bout) %>%
      dplyr::mutate(longest_bout = max(.data$interval))

    events_file_data$duration <- "long"
    events_file_data[which(events_file_data$longest_bout < 600),]$duration <- "intermediate"
    events_file_data[which(events_file_data$longest_bout < 60),]$duration <- "short"
    events_file_data$duration <- factor(events_file_data$duration,
                                        levels = c("long","intermediate","short"))

    stepping_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$Date, .data$duration) %>%
      dplyr::summarise(steps = sum(.data$steps)) %>%
      dplyr::group_by(.data$uid, .data$duration) %>%
      dplyr::summarise(steps = sum(.data$steps)) %>%
      dplyr::mutate(steps = .data$steps / length(unique(events_file_data$Date))) %>%
      tidyr::complete(.data$duration) %>%
      tidyr::replace_na(list(steps = 0))

    stepping_summary <- stepping_summary %>%
      dplyr::select(.data$uid, .data$duration, .data$steps)

    return(stepping_summary)
  }

build.time.to.first.step.summary <-
  function(events_file_data){
    #' @import dplyr

    events_file_data$pre_activity <- c(0,head(events_file_data$activity,-1))
    events_file_data$next_activity <- c(tail(events_file_data$activity,-1),0)

    sit_stand_step_trans <- which(events_file_data$pre_activity == 0 & events_file_data$activity == 1 & events_file_data$next_activity == 2)
    events_file_data <- events_file_data[sit_stand_step_trans,]

    first_step_summary <- events_file_data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(time_first_step = stats::median(.data$interval))

    return(first_step_summary)
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

process.daily.breaks.in.time.in.bed <-
  function(data){
    uid <- unique(data$uid)
    uid_date <- unique(data$Date)

    breaks_data <- data.frame(matrix(ncol = 3, nrow = length(uid_date)))
    colnames(breaks_data) <- c("uid","date","breaks")

    for(i in (1:length(uid_date))){
      events_ext <- data[which(data$Date == uid_date[i]),]

      event_start <- c(0,events_ext$activity)
      event_end <- c(events_ext$activity,0)

      waking_day_start <- c(-1,events_ext$waking_day)
      waking_day_end <- c(events_ext$waking_day,-1)

      interruptions <- which(event_start == 3.1 & event_end == 1 & waking_day_start == 0 & waking_day_end == 0)
      interruptions <- events_ext[interruptions,]
      interruptions$Date <- as.Date(interruptions$time)
      breaks_data[i,]$uid <- uid
      breaks_data[i,]$date <- uid_date[i]
      breaks_data[i,]$breaks <- nrow(interruptions)
    }
    return(breaks_data)
  }

#################################################

build.travel.summary <-
  function(events_file_data){
    #' @import dplyr
    events_file_data <- group.stepping.by.upright.bout(events_file_data)
    events_file_data$bout_length <- ""

    upright_stepping <- events_file_data %>%
      dplyr::filter(.data$activity == 2) %>%
      dplyr::group_by(.data$upright_bout) %>%
      dplyr::summarise(max_interval = max(.data$interval))
    seated_travel_cycling_data <- dplyr::left_join(events_file_data, upright_stepping,
                                            by = "upright_bout")

    if(length(which(seated_travel_cycling_data$activity == 2)) > 0){
      seated_travel_cycling_data[which(seated_travel_cycling_data$activity == 2),]$bout_length <- "Active_Walking"
    }
    if(length(which(seated_travel_cycling_data$activity == 2.1)) > 0){
      seated_travel_cycling_data[which(seated_travel_cycling_data$activity == 2.1),]$bout_length <- "Cycling"
    }
    if(length(which(seated_travel_cycling_data$activity == 5)) > 0){
      seated_travel_cycling_data[which(seated_travel_cycling_data$activity == 5),]$bout_length <- "Seated_Transport"
    }
    seated_travel_cycling_data <- seated_travel_cycling_data[which(seated_travel_cycling_data$activity %in% c(2,2.1,5)),]
    short_upright_stepping <- which(seated_travel_cycling_data$activity == 2 & seated_travel_cycling_data$max_interval < 60)

    if(length(short_upright_stepping) > 0){
      seated_travel_cycling_data <- seated_travel_cycling_data[-short_upright_stepping,]
    }

    seated_travel_cycling_data$bout_length <- factor(seated_travel_cycling_data$bout_length,
                                                     levels = c("Active_Walking","Cycling","Seated_Transport"))

    if(nrow(seated_travel_cycling_data) == 0){
      travel_by_uid <- data.frame(rep(events_file_data[1,]$uid,3))
      colnames(travel_by_uid)[1] <- "uid"
      travel_by_uid$bout_length <- levels(seated_travel_cycling_data$bout_length)
      travel_by_uid$bouts <- 0
      travel_by_uid$bout_duration <- 0
    }else{
      travel_by_uid <- seated_travel_cycling_data %>%
        dplyr::group_by(.data$uid, .data$bout_length, .drop = FALSE) %>%
        dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)
    }

    travel_by_uid$days <- length(unique(events_file_data$Date))
    travel_by_uid$bout_duration <- travel_by_uid$bout_duration / travel_by_uid$days

    return(travel_by_uid)
  }

group.stepping.by.upright.bout <-
  function(data){
    data$pre_activity <- c(0,head(data$activity,-1))
    data$upright_bout <- 0
    data[which(data$activity %in% c(1,2.0,2.1,2.2) &
                 !(data$pre_activity %in% c(1,2.0,2.1,2.2))),]$upright_bout <-
      rep(1,length(which(data$activity %in% c(1,2.0,2.1,2.2) &
                           !(data$pre_activity %in% c(1,2.0,2.1,2.2)))))

    data$upright_bout <- cumsum(data$upright_bout)
    data[which(!data$activity %in% c(1,2.0,2.1,2.2)),]$upright_bout <-
      rep(0,length(which(!data$activity %in% c(1,2.0,2.1,2.2))))

    return(data)
  }

#################################################

build.activity.summary <-
  function(events_file_data){
    #' @import dplyr
    #' @import tidyr
    activity_periods <- events_file_data %>%
      tidyr::expand(.data$uid, .data$Date)
    all_events <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(total_events = sum(event_count),
                       total_duration = round(sum(interval)/60,1))
    sedentary_events <- events_file_data %>%
      dplyr::filter(activity %in% c(0,3.2,5)) %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(sedentary_events = sum(event_count),
                       sedentary_duration = round(sum(interval)/60,1))
    standing_events <- events_file_data %>%
      dplyr::filter(activity %in% c(2)) %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(standing_events = sum(event_count),
                       standing_duration = round(sum(interval)/60,1))
    stepping_events <- events_file_data %>%
      dplyr::filter(activity %in% c(2)) %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(stepping_events = sum(event_count),
                       stepping_duration = round(sum(interval)/60,1))
    cycling_events <- events_file_data %>%
      dplyr::filter(activity %in% c(2.1)) %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(cycling_events = sum(event_count),
                       cycling_duration = round(sum(interval)/60,1))
    lying_events <- events_file_data %>%
      dplyr::filter(activity %in% c(3.1)) %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(lying_events = sum(event_count),
                       lying_duration = round(sum(interval)/60,1))
    non_wear_events <- events_file_data %>%
      dplyr::filter(activity %in% c(4)) %>%
      dplyr::group_by(.data$uid, .data$Date) %>%
      dplyr::summarise(non_wear_events = sum(event_count),
                       non_wear_duration = round(sum(interval)/60,1))

    activity_summary <- dplyr::left_join(activity_periods, all_events,
                                         by = c("uid","Date"))
    activity_summary <- dplyr::left_join(activity_summary, sedentary_events,
                                         by = c("uid","Date"))
    activity_summary <- dplyr::left_join(activity_summary, standing_events,
                                         by = c("uid","Date"))
    activity_summary <- dplyr::left_join(activity_summary, stepping_events,
                                         by = c("uid","Date"))
    activity_summary <- dplyr::left_join(activity_summary, cycling_events,
                                         by = c("uid","Date"))
    activity_summary <- dplyr::left_join(activity_summary, lying_events,
                                         by = c("uid","Date"))
    activity_summary <- dplyr::left_join(activity_summary, non_wear_events,
                                         by = c("uid","Date"))
    activity_summary[is.na(activity_summary)] <- 0
    activity_summary$Date <- as.Date(activity_summary$Date, origin = "1970-01-01")
    return(activity_summary)
  }
