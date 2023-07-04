build.non.wear.summary.by.period <-
  function(events_file_data){
    #' @import dplyr
    non_wear_data <- events_file_data[which(events_file_data$activity %in% c(4)),]

    non_wear_by_uid <- non_wear_data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01")) %>%
      dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)

    if(nrow(non_wear_by_uid) > 0){
      non_wear_by_uid$activity <- 4
      non_wear_by_uid$days <- length(unique(events_file_data$Date))
      non_wear_by_uid$duration_by_day <- non_wear_by_uid$bout_duration / non_wear_by_uid$days
    }
    return(non_wear_by_uid)
  }

build.sedentary.summary.by.period <-
  function(events_file_data){
    sedentary_data <- merge.adjacent.sedentary.period(events_file_data)
    sedentary_data[which(sedentary_data$activity == 5),]$activity <- rep(0,length(which(sedentary_data$activity == 5)))
    sedentary_data <- sedentary_data[which(sedentary_data$activity %in% c(0,3.1,3.2)),]

    # Set primary lying as time in bed
    sedentary_data[which(sedentary_data$activity %in% c(3.1)),]$activity <- rep(3,length(which(sedentary_data$activity %in% c(3.1))))
    # Set sedentary and secondary lying as sedentary
    sedentary_data[which(sedentary_data$activity %in% c(3.2)),]$activity <- rep(0,length(which(sedentary_data$activity %in% c(3.2))))
    sedentary_data <- categorise.sedentary.duration.period(sedentary_data)

    sedentary_data <- summarise.sedentary.by.group.by.period(sedentary_data)

    sedentary_data$activity <- as.character(sedentary_data$activity)

    sedentary_data[which(sedentary_data$activity == 0),]$activity <- "Sedentary"
    sedentary_data[which(sedentary_data$activity == 3),]$activity <- "Time in Bed"
    sedentary_data$bout_length <- paste(sedentary_data$activity , " (", sedentary_data$duration, ")", sep = "")
    sedentary_data$bout_length <- factor(sedentary_data$bout_length, levels = c("Time in Bed (< 30 min)","Time in Bed (30 min - 1 hour)","Time in Bed (1 - 2 hours)",
                                                                                "Time in Bed (2 - 4 hours)","Time in Bed (4 hours +)",
                                                                                "Sedentary (< 30 min)","Sedentary (30 min - 1 hour)","Sedentary (1 - 2 hours)",
                                                                                "Sedentary (2 - 4 hours)","Sedentary (4 hours +)"))

    sedentary_data <- sedentary_data[,c(1,2,3,8,6,7)]

    if(nrow(sedentary_data) > 0){
      sedentary_data$period_date <- as.Date(sedentary_data$period_date, origin = "1970-01-01")
    }
    return(sedentary_data)
  }

merge.adjacent.sedentary.period <-
  function(data){
    data$whole_interval <- data$interval
    adjacent_sedentary <- which((data$activity + c(tail(data$activity,-1),-10)) %in% c(0,6.2,6.4) & data$uid == c(tail(data$uid,-1),1))
    if(length(adjacent_sedentary) > 0){
      data[adjacent_sedentary,]$whole_interval <- data[adjacent_sedentary,]$interval + data[(adjacent_sedentary + 1),]$interval
      data[adjacent_sedentary + 1,]$whole_interval <- data[adjacent_sedentary,]$interval
    }
    return(data)
  }

categorise.sedentary.duration.period <-
  function(data){
    duration_time <- c(14400,7200,3600,1800)
    duration_label <- c("2 - 4 hours","1 - 2 hours","30 min - 1 hour","< 30 min")
    duration_overflow <- "4 hours +"

    data$duration <- duration_overflow
    for(i in (1:length(duration_time))){
      data[which(data$whole_interval <= duration_time[i]),]$duration <- rep(duration_label[i],length(which(data$whole_interval <= duration_time[i])))
    }
    data$duration <- factor(data$duration, levels = c(duration_overflow,duration_label))

    return(data)
  }

summarise.sedentary.by.group.by.period <-
  function(data){
    #' @import dplyr
    sedentary_by_uid <- data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01"),
                      .data$duration, .data$activity) %>%
      dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)

    return(sedentary_by_uid)
  }

build.sedentary.bout.summary.by.period <-
  function(events_file_data){
    #' @import dplyr
    sedentary_bouts <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01")) %>%
      dplyr::filter(.data$activity == 0) %>%
      dplyr::summarise(bouts = n())
    if(nrow(sedentary_bouts) > 0){
      sedentary_bouts$period_date <- as.Date(sedentary_bouts$period_date, origin = "1970-01-01")
    }

    return(sedentary_bouts)
  }

#################################################

build.upright.summary.by.period <-
  function(events_file_data){
    #' @import dplyr
    if(length(which(events_file_data$activity %in% c(1,2,2.1))) == 0){
      upright_data <- events_file_data %>%
        group_by(uid, period_name, period_date) %>%
        transmute(bout_length = "Quiet Standing",
                  bout_duration = 0, bouts = 0)
      return(upright_data)
    }
    events_file_data <- events_file_data %>%
      dplyr::filter(.data$activity %in% c(1,2,2.1))
    if(nrow(events_file_data) > 0){
      events_file_data <- categorise.stepping.bouts(events_file_data)
    }

    upright_data <- generate.upright.summary.by.period(events_file_data)
    if(nrow(upright_data) > 0){
      upright_data$period_date <- as.Date(upright_data$period_date, origin = "1970-01-01")
    }

    return(upright_data)
  }

generate.upright.summary.by.period <-
  function(events_data){
    #' @import dplyr
    events_data <- events_data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01"),
                      .data$bout_length) %>%
      dplyr::summarise(bout_duration = sum(.data$interval), bouts = length(.data$interval))
    # convert the duration from seconds to hours
    events_data$bout_duration <- events_data$bout_duration / 3600
    return(events_data)
  }

#################################################

build.walk.test.summary.by.period <-
  function(walk_test_data){
    #' @import dplyr
    walk_test_summary <- walk_test_data %>%
      dplyr::arrange(desc(.data$steps), .data$duration) %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::slice_head(n = 1)

    return(walk_test_summary)
  }

build.stepping.summary.by.period <-
  function(events_file_data){
    #' @import dplyr
    #' @import tidyr
    stepping_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01")) %>%
      dplyr::summarise(daily_steps = sum(.data$steps))

    stepping_summary$period_date <- as.Date(stepping_summary$period_date, origin = "1970-01-01")
    return(stepping_summary)
  }

build.stepping.intensity.summary.by.period <-
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

    events_file_data$duration <- "long (>= 60s)"
    events_file_data[which(events_file_data$longest_bout < 60),]$duration <- "short (< 60s)"

    stepping_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01"),
                      .data$category, .data$duration) %>%
      dplyr::summarise(time = sum(.data$interval)/60)

    stepping_summary$period_date <- as.Date(stepping_summary$period_date, origin = "1970-01-01")
    return(stepping_summary)
  }

build.time.to.first.step.summary.by.period <-
  function(events_file_data){
    #' @import dplyr

    events_file_data$pre_activity <- c(0,head(events_file_data$activity,-1))
    events_file_data$next_activity <- c(tail(events_file_data$activity,-1),0)

    sit_stand_step_trans <- which(events_file_data$pre_activity == 0 & events_file_data$activity == 1 & events_file_data$next_activity == 2)
    events_file_data <- events_file_data[sit_stand_step_trans,]

    first_step_summary <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01")) %>%
      dplyr::summarise(time_first_step = stats::median(.data$interval))

    if(nrow(first_step_summary) > 0){
      first_step_summary$period_date <- as.Date(first_step_summary$period_date, origin = "1970-01-01")
    }
    return(first_step_summary)
  }

#################################################

process.breaks.in.time.in.bed.by.period <-
  function(data){
    event_start <- c(0,data$activity)
    event_end <- c(data$activity,0)

    waking_day_start <- c(-1,data$waking_day)
    waking_day_end <- c(data$waking_day,-1)

    interruptions <- which(event_start == 3.1 & event_end == 1 & waking_day_start == 0 & waking_day_end == 0)
    interruptions <- data[interruptions,]
    breaks_data <- interruptions %>%
      group_by(.data$uid, .data$period_name, period_date = as.Date(.data$period_date, origin = "1970-01-01")) %>%
      summarise(time_in_bed_breaks = n())
    if(nrow(breaks_data) > 0){
      breaks_data$period_date <- as.Date(breaks_data$period_date, origin = "1970-01-01")
    }
    return(breaks_data)
  }

#################################################

build.travel.summary.by.period <-
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
      seated_travel_cycling_data[short_upright_stepping,]$bout_length <- "Indoor_Walking"
    }

    seated_travel_cycling_data$bout_length <- factor(seated_travel_cycling_data$bout_length,
                                                     levels = c("Active_Walking","Indoor_Walking","Cycling","Seated_Transport"))

    travel_by_uid <- seated_travel_cycling_data %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date, .data$bout_length, .drop = FALSE) %>%
      dplyr::summarise(bouts = n(), bout_duration = sum(.data$interval) / 3600)
    if(is.na(travel_by_uid[1,]$uid)){
      travel_template <- events_file_data %>% expand(uid, period_name, period_date)
      active_walk <- travel_template
      active_walk$bout_length <- "Active_Walking"
      active_indoor_walk <- travel_template
      active_indoor_walk$bout_length <- "Indoor_Walking"
      active_cycling <- travel_template
      active_cycling$bout_length <- "Cycling"
      active_transport <- travel_template
      active_transport$bout_length <- "Seated_Transport"
      travel_by_uid <- dplyr::bind_rows(active_walk, indoor_walking, active_cycling, active_transport)
      travel_by_uid$bouts <- 0
      travel_by_uid$bout_duration <- 0
    }
    travel_by_uid$period_date <- as.Date(travel_by_uid$period_date, origin = "1970-01-01")
    return(travel_by_uid)
  }

#################################################

median.cadence.bands.by.period <-
  function(events_file, file_uid, upright_bout = TRUE){
    #' @import dplyr
    lower_bound <- c(10,60,600)
    upper_bound <- c(60,600,86400)

    if(length(which(events_file$activity == 2)) == 0){
      median_cadence_by_group <- events_file %>%
        group_by(uid, period_name, period_date) %>%
        transmute(group = "Median Cadence < 1 minute",
                  median_cadence = 0)
      return(median_cadence_by_group)
    }

    events_file <- group.stepping.by.upright.bout(events_file)
    events_file <- events_file[which(events_file$activity == 2 & events_file$interval >= lower_bound[1]),]
    events_file$cadence <- events_file$steps / (events_file$interval / 60)
    longest_stepping <- events_file %>%
      filter(activity == 2) %>%
      group_by(upright_bout) %>%
      summarise(max_stepping = max(interval))
    events_file <- left_join(events_file, longest_stepping, by = c("upright_bout"))
    events_file[is.na(events_file)] <- 0

    if(nrow(events_file) == 0){
      median_cadence_by_group <- as.data.frame(matrix(nrow = 3, ncol = 3))
      colnames(median_cadence_by_group) <- c("uid","group","median_cadence")
      median_cadence_by_group$uid <- file_uid
      median_cadence_by_group[1,]$group <- "< 1 minute"
      median_cadence_by_group[2,]$group <- "1 - 10 minutes"
      median_cadence_by_group[3,]$group <- "10 minutes +"
      median_cadence_by_group$median_cadence <- 0
      return(median_cadence_by_group)
    }
    events_file$group <- ""
    for (i in (1:length(lower_bound))){
      if(upright_bout){
        in_group <- which(events_file$max_stepping >= lower_bound[i] & events_file$max_stepping < upper_bound[i])
      }else{
        in_group <- which(events_file$interval >= lower_bound[i] & events_file$interval < upper_bound[i])
      }
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

    median_cadence_by_group <- events_file %>% dplyr::group_by(.data$uid, .data$period_name, .data$period_date, .data$group) %>%
      dplyr::summarise(median_cadence = weighted.median(.data$cadence,.data$steps))
    if(length(which(median_cadence_by_group$group == "10 minutes to 1 day")) > 0){
      median_cadence_by_group[which(median_cadence_by_group$group == "10 minutes to 1 day"),]$group <- "10 minutes +"
    }
    if(length(which(median_cadence_by_group$group == "1 minutes to 10 minutes")) > 0){
      median_cadence_by_group[which(median_cadence_by_group$group == "1 minutes to 10 minutes"),]$group <- "1 - 10 minutes"
    }
    if(length(which(median_cadence_by_group$group == "10 seconds to 1 minute")) > 0){
      median_cadence_by_group[which(median_cadence_by_group$group == "10 seconds to 1 minute"),]$group <- "< 1 minute"
    }
    median_cadence_by_group$period_date <- as.Date(median_cadence_by_group$period_date,origin = "1970-01-01")
    return(median_cadence_by_group)
  }

#################################################

build.activity.summary.by.period <-
  function(events_file_data, custom_periods){
    #' @import dplyr
    #' @import tidyr
    activity_periods <- events_file_data %>%
      tidyr::expand(.data$uid, .data$period_name, .data$period_date)
    period_times <- custom_periods %>%
      group_by(uid = .data$id, period_name = .data$category, .data$period_date) %>%
      summarise(period_length = (as.numeric(.data$end_date) - as.numeric(.data$start_date))/60)

    all_events <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(total_events = sum(event_count),
                       total_duration = round(sum(interval)/60,1))
    sedentary_events <- events_file_data %>%
      dplyr::filter(activity %in% c(0,3.2,5)) %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(sedentary_events = sum(event_count),
                sedentary_duration = round(sum(interval)/60,1))
    standing_events <- events_file_data %>%
      dplyr::filter(activity %in% c(1)) %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(standing_events = sum(event_count),
                standing_duration = round(sum(interval)/60,1))
    stepping_events <- events_file_data %>%
      dplyr::filter(activity %in% c(2)) %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(stepping_events = sum(event_count),
                stepping_duration = round(sum(interval)/60,1))
    cycling_events <- events_file_data %>%
      dplyr::filter(activity %in% c(2.1)) %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(cycling_events = sum(event_count),
                cycling_duration = round(sum(interval)/60,1))
    lying_events <- events_file_data %>%
      dplyr::filter(activity %in% c(3.1)) %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(lying_events = sum(event_count),
                lying_duration = round(sum(interval)/60,1))
    non_wear_events <- events_file_data %>%
      dplyr::filter(activity %in% c(4)) %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(non_wear_events = sum(event_count),
                non_wear_duration = round(sum(interval)/60,1))

    if(is.numeric(all_events$period_date)){
      period_times$period_date <- as.numeric(period_times$period_date)
    }

    no_data <- inner_join(period_times, all_events,
                          by = c("uid","period_name","period_date"))
    no_data$no_data_duration <- round(no_data$period_length - no_data$total_duration,1)
    no_data <- no_data %>% select(.data$uid, .data$period_name,
                                  .data$period_date, .data$no_data_duration)

    activity_summary <- dplyr::left_join(activity_periods, all_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, sedentary_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, standing_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, stepping_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, cycling_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, lying_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, non_wear_events,
                                         by = c("uid","period_name","period_date"))
    activity_summary <- dplyr::left_join(activity_summary, no_data,
                                         by = c("uid","period_name","period_date"))
    activity_summary[is.na(activity_summary)] <- 0
    activity_summary$period_date <- as.Date(activity_summary$period_date, origin = "1970-01-01")
    return(activity_summary)
  }
