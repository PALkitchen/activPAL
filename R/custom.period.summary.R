custom.period.summary <-
  function(input_folder,file_name,id,events_file_data,full_events_file,custom_periods){

    walk_test_30_s <- activpal.stepping.process.file.by.period(full_events_file,30,86400,custom_periods)
    walk_test_2_min <- activpal.stepping.process.file.by.period(full_events_file,120,86400,custom_periods)
    walk_test_6_min <- activpal.stepping.process.file.by.period(full_events_file,360,86400,custom_periods)
    walk_test_12_min <- activpal.stepping.process.file.by.period(full_events_file,720,86400,custom_periods)

    observation_summary <- custom_periods[,c(5,1,2,3,4)]
    colnames(observation_summary)[c(1,3:5)] <- c("uid","period_name","period_start","period_end")
    # observation_summary$period_duration <- round(as.numeric(difftime(observation_summary$period_end,
    #                                                                  observation_summary$period_start,
    #                                                                  units = "hours")),3)

    if(nrow(walk_test_30_s) == 0){
      walk_test_summary <- observation_summary
      walk_test_summary$peak_steps_30_seconds <- 0
      walk_test_summary$peak_steps_2_minute <- 0
      walk_test_summary$peak_steps_6_minute <- 0
      walk_test_summary$peak_steps_12_minute <- 0
    }else{
      walk_test_30_s <- format.walk.test.by.period(walk_test_30_s, "30_seconds")
      walk_test_2_min <- format.walk.test.by.period(walk_test_2_min, "2_minute")
      walk_test_6_min <- format.walk.test.by.period(walk_test_6_min, "6_minute")
      walk_test_12_min <- format.walk.test.by.period(walk_test_12_min, "12_minute")

      walk_test_summary <- dplyr::inner_join(observation_summary,
                                             dplyr::inner_join(dplyr::inner_join(dplyr::inner_join(
                                               walk_test_30_s,walk_test_2_min, by = c("period_name","period_date")),
                                               walk_test_6_min, by = c("period_name","period_date")),
                                               walk_test_12_min, by = c("period_name","period_date")),
                                             walk_test_12_min, by = c("period_name","period_date"))
    }

    lying_time_breaks <- process.breaks.in.time.in.bed.by.period(events_file_data)
    lying_time_breaks <- lying_time_breaks %>% dplyr::filter(!is.na(period_name))
    colnames(lying_time_breaks)[4] <- "Time in Bed Breaks"

    non_wear_data <- build.non.wear.summary.by.period(events_file_data)
    non_wear_data <- non_wear_data %>% dplyr::select(uid, period_name, period_date, bout_duration)
    non_wear_data$bout_duration <- round(non_wear_data$bout_duration,2)
    colnames(non_wear_data)[4] <- "Non Wear"

    sedentary_data <- build.sedentary.summary.by.period(events_file_data)
    sedentary_data <- sedentary_data[,-c(5)] %>%
      dplyr::filter(!is.na(period_name)) %>%
      tidyr::pivot_wider(names_from = "bout_length", names_expand = TRUE, values_from = "bout_duration")
    sedentary_data[is.na(sedentary_data)] <- 0
    sedentary_data[,c(4:ncol(sedentary_data)),] <- round(sedentary_data[,c(4:ncol(sedentary_data)),],3)

    upright_data <- build.upright.summary.by.period(events_file_data)
    upright_data <- format.upright.data.by.period(upright_data)
    upright_data[,c(4:ncol(upright_data)),] <- round(upright_data[,c(4:ncol(upright_data)),],3)

    stepping_data <- build.stepping.summary.by.period(events_file_data)
    stepping_data <- stepping_data %>% dplyr::filter(!is.na(period_name))

    travel_data <- build.travel.summary.by.period(events_file_data)
    travel_data <- format.travel.data.by.period(travel_data)
    travel_data[,c(4:ncol(travel_data)),] <- round(travel_data[,c(4:ncol(travel_data)),],3)

    median_cadence_data <- median.cadence.bands.by.period(events_file_data,id, upright_bout = FALSE)
    median_cadence_data <- format.median.cadence.by.period(median_cadence_data)
    median_cadence_data[,c(4:ncol(median_cadence_data)),] <- round(median_cadence_data[,c(4:ncol(median_cadence_data)),],1)

    mvpa_data <- build.stepping.intensity.summary.by.period(events_file_data)
    mvpa_data <- format.mvpa.data.by.period(mvpa_data)
    mvpa_data[,c(4:ncol(mvpa_data)),] <- round(mvpa_data[,c(4:ncol(mvpa_data)),],1)

    time_first_step_data <- build.time.to.first.step.summary.by.period(events_file_data)
    time_first_step_data <- time_first_step_data %>% dplyr::filter(!is.na(period_name))

    daily_stepping_data <- events_file_data %>%
      dplyr::group_by(.data$uid, .data$period_name, .data$period_date) %>%
      dplyr::summarise(Steps = sum(.data$steps))
    daily_stepping_data$period_date <- as.Date(daily_stepping_data$period_date, origin = "1970-01-01")

    activity_data <- build.activity.summary.by.period(events_file_data, custom_periods)

    observation_summary <- dplyr::left_join(observation_summary, sedentary_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, lying_time_breaks, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, daily_stepping_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, upright_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, non_wear_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, mvpa_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, median_cadence_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, travel_data, by = c("uid","period_name","period_date"))
    observation_summary <- dplyr::left_join(observation_summary, walk_test_summary, by = c("uid","period_name","period_date","period_start","period_end"))
    observation_summary <- dplyr::left_join(observation_summary, activity_data, by = c("uid","period_name","period_date"))

    observation_summary[is.na(observation_summary)] <- 0

    return(observation_summary)
  }

format.walk.test.by.period <-
  function(walk_test, period){
    walk_test_summary <- walk_test %>%
      dplyr::select(period_name, period_date, steps)
    colnames(walk_test_summary)[3] <- paste("peak_steps",period,sep="_")
    # colnames(walk_test_summary)[4] <- paste(period,"stepping_duration",sep="_")
    return(walk_test_summary)
  }

format.upright.data.by.period <-
  function(upright_data){
    upright_data$bout_length <- factor(upright_data$bout_length,
    levels = c("Quiet Standing","Stepping (< 1 minute)","Stepping (1 - 10 minutes)","Stepping (10 minutes +)"))
    upright_summary <- upright_data[,-c(6)] %>%
      dplyr::filter(!is.na(period_name)) %>%
      dplyr::filter(bout_length != "Cycling") %>%
      tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration", names_expand = TRUE)
    upright_summary[is.na(upright_summary)] <- 0
    return(upright_summary)
  }

format.travel.data.by.period <-
  function(travel_data){
    travel_summary <- travel_data[,-c(5)] %>%
      dplyr::filter(!is.na(period_name)) %>%
      tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration")
    travel_summary[is.na(travel_summary)] <- 0
    return(travel_summary)
  }

format.mvpa.data.by.period <-
  function(mvpa_data){
    mvpa_data$category <- factor(mvpa_data$category,
                                 levels = c("LPA (< 75 spm)", "MPA (75 - 100 spm)",
                                            "MVPA (100 - 125 spm)", "VPA (> 125 spm)"))
    mvpa_data$duration <- factor(mvpa_data$duration, levels = c("short (< 60s)","long (>= 60s)"))

    mvpa_summary <- mvpa_data %>%
      dplyr::filter(!is.na(period_name)) %>%
      tidyr::pivot_wider(names_from = c(4,5), values_from = 6, names_expand = TRUE)
    return(mvpa_summary)
  }

format.median.cadence.by.period <-
  function(median_cadence_data){
    median_cadence_data <- median_cadence_data %>%
      dplyr::filter(!is.na(group)) %>%
      mutate(group = paste("Median Cadence ",group,sep=""))
    median_cadence_data$group <- factor(median_cadence_data$group,
                                        levels = c("Median Cadence < 1 minute","Median Cadence 1 - 10 minutes","Median Cadence 10 minutes +"))
    median_cadence_data <- median_cadence_data %>%
      tidyr::pivot_wider(names_from = "group", values_from = "median_cadence", names_expand = TRUE)
    return(median_cadence_data)
  }
