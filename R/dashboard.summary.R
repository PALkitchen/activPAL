sort_order_groups <- c("SEDENTARY_TIME", "TIME_IN_BED", "UPRIGHT_TIME", "STEPPING_TIME",
                       "DAILY_SEDENTARY_BOUTS","DAILY_TIB_INTERRUPTIONS","STEPPING_BOUTS_UNDER_1_MIN",
                       "PEAK_2_MIN_STEPPING", "PEAK_6_MIN_STEPPING", "PEAK_10_MIN_STEPPING",
                       "DAILY_PEAK_30_SECOND_STEPPING", "MEDIAN_DAILY_STEP_COUNT","MEDIAN_RISE_TIME",
                       "MEDIAN_CADENCE_TO_1_MINUTE","MEDIAN_CADENCE_1_MINUTE_PLUS")
sort_order_names <- c("Sedentary","Time in Bed", "Standing", "Stepping",
                      "Sedentary Bouts", "TiB Breaks", "Stepping Under 1 Min",
                      "Walk 2 Min", "Walk 6 Min", "Walk 10 Min", "Walk 30 s", "Median Daily Steps", "Median Rise Time",
                      "Median Cadence Short", "Median Cadence Long")
sort_order_ascending <- c(1,1,1,1,
                          1,1,1,
                          1,1,1,
                          1,1,1,
                          1,1)
sort_order_title <- c(3,4,5,5,
                      3,4,5,
                      10,11,12,
                      9,6,7,
                      8,8)

items_per_page <- 50

process.activity.summary <-
  function(input_folder, chart_title, output_folder = "", anonymise = FALSE, standard_scales = FALSE, sort_order = "SEDENTARY_TIME"){
    #' @export
    if (!sort_order %in% sort_order_groups & sort_order != "ALL"){
      sort_order <- "SEDENTARY_TIME"
      warning("Invalid sorting criteria selected. Summary sorted by total sedentary time.")
    }

    file_names <- list.files(input_folder, pattern="*.csv",recursive = FALSE)
    validation_data <- file_names[grep("DailyValidation",file_names)]
    file_names <- file_names[grep("EventsEx",file_names)]

    validation_data <- load.validation.data(input_folder,validation_data)

    valid_days <- list()

    walk_test_2_min_data <- list()
    walk_test_6_min_data <- list()
    walk_test_10_min_data <- list()
    walk_test_30_s_data <- list()

    bouts_breaks_data <- list()
    sedentary_data <- list()
    upright_data <- list()
    stepping_data <- list()
    daily_stepping_data <- list()

    median_rise_time_data <- list()
    median_cadence_data <- list()
    non_wear_data <- list()

    no_valid_days <- c()

    median_rise_time <- process.rise.time(input_folder)

    for(i in (1:length(file_names))){
      events_import <- process.events.file(input_folder, file_names[i], validation_data)

      events_file_data <- events_import[[1]]

      if(!is.null(events_file_data)){
        valid_days[[i]] <- events_import[[2]]

        if(!is.null(median_rise_time)){
          median_rise_time_data[[i]] <- median_rise_time[which(median_rise_time$uid == parse.file.name(file_names[i])),]
        }

        median_cadence_data[[i]] <- median.stepping.cadence.bands.file(events_file_data)

        walk_test_2_min <- activpal.stepping.process.file(input_folder,file_names[i],valid_days[[i]],120,360,72000,TRUE)
        walk_test_6_min <- activpal.stepping.process.file(input_folder,file_names[i],valid_days[[i]],360,600,72000,TRUE)
        walk_test_10_min <- activpal.stepping.process.file(input_folder,file_names[i],valid_days[[i]],600,86400,72000,TRUE)
        walk_test_30_s <- activpal.stepping.process.file(input_folder,file_names[i],valid_days[[i]],30,86400,72000,TRUE)

        walk_test_2_min_data[[i]] <- build.walk.test.summary(walk_test_2_min)
        walk_test_6_min_data[[i]] <- build.walk.test.summary(walk_test_6_min)
        walk_test_10_min_data[[i]] <- build.walk.test.summary(walk_test_10_min)
        walk_test_30_s_data[[i]] <- build.walk.test.summary(walk_test_30_s)

        sedentary_bouts <- build.sedentary.bout.summary(events_file_data)
        lying_time_breaks <- process.breaks.in.time.in.bed(events_file_data)

        bouts_breaks_data[[i]] <- inner_join(sedentary_bouts,lying_time_breaks, by = c("uid", "valid_days"))
        non_wear_data[[i]] <- build.non.wear.summary(events_file_data)

        sedentary_data[[i]] <- build.sedentary.summary(events_file_data)
        upright_data[[i]] <- build.upright.summary(events_file_data)
        stepping_data[[i]] <- build.stepping.summary(events_file_data)
        daily_stepping_data[[i]] <- events_file_data %>%
          dplyr::group_by(.data$uid, date = .data$Date) %>%
          dplyr::summarise(steps = sum(.data$steps))
      } else{
        no_valid_days <- c(no_valid_days, file_names[i])
      }
      message(paste("Processed: ",file_names[i],sep=""))
    }

    valid_days <- dplyr::bind_rows(valid_days)

    walk_test_2_min_data <- dplyr::bind_rows(walk_test_2_min_data)
    walk_test_6_min_data <- dplyr::bind_rows(walk_test_6_min_data)
    walk_test_10_min_data <- dplyr::bind_rows(walk_test_10_min_data)
    walk_test_30_s_data <- dplyr::bind_rows(walk_test_30_s_data)

    bouts_breaks_data <- dplyr::bind_rows(bouts_breaks_data)
    sedentary_data <- dplyr::bind_rows(sedentary_data)
    upright_data <- dplyr::bind_rows(upright_data)
    stepping_data <- dplyr::bind_rows(stepping_data)
    daily_stepping_data <- dplyr::bind_rows(daily_stepping_data)

    if(is.null(median_rise_time)){
      message("No rise time data found. Rise Time will not be plotted.")
      median_rise_time_data <- NULL
    }else{
      median_rise_time_data <- dplyr::bind_rows(median_rise_time_data)
    }

    median_cadence_data <- dplyr::bind_rows(median_cadence_data)
    median_cadence_data <- median_cadence_data %>% dplyr::filter(!is.na(.data$median_cadence))
    non_wear_data <- dplyr::bind_rows(non_wear_data)
    non_wear_data$activity <- "non wear"

    if(anonymise){
      anonymise_table <- build.anonymise.mapping(input_folder)
      events_file_data$uid <- anonymise.id(events_file_data$uid,anonymise_table)
      valid_days$uid <- anonymise.id(valid_days$uid,anonymise_table)
      walk_test_2_min_data$uid <- anonymise.id(walk_test_2_min_data$uid,anonymise_table)
      walk_test_6_min_data$uid <- anonymise.id(walk_test_6_min_data$uid,anonymise_table)
      walk_test_10_min_data$uid <- anonymise.id(walk_test_10_min_data$uid,anonymise_table)
      walk_test_30_s_data$uid <- anonymise.id(walk_test_30_s_data$uid,anonymise_table)

      bouts_breaks_data$uid <- anonymise.id(bouts_breaks_data$uid,anonymise_table)
      sedentary_data$uid <- anonymise.id(sedentary_data$uid,anonymise_table)
      upright_data$uid <- anonymise.id(upright_data$uid,anonymise_table)
      stepping_data$uid <- anonymise.id(stepping_data$uid,anonymise_table)
      daily_stepping_data$uid <- anonymise.id(daily_stepping_data$uid,anonymise_table)
      if(!is.null(median_rise_time_data)){
        median_rise_time_data$uid <- anonymise.id(median_rise_time_data$uid,anonymise_table)
      }
      median_cadence_data$uid <- anonymise.id(median_cadence_data$uid,anonymise_table)
      non_wear_data$uid <- anonymise.id(non_wear_data$uid,anonymise_table)
    }

    graphics.off()

    #####
    valid_day_summary <- valid_days %>% dplyr::group_by(.data$uid, category = .data$valid) %>% dplyr::summarise(days = n()) %>%
      tidyr::pivot_wider(names_from = "category", values_from = "days", values_fill = list(days = 0))
    sedentary_summary <- sedentary_data %>% dplyr::select(.data$uid, .data$bout_length, .data$bout_duration) %>%
      tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration", values_fill = list(bout_duration = 0))
    upright_summary <- upright_data %>% dplyr::select(.data$uid, .data$bout_length, .data$bout_duration) %>%
      tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration", values_fill = list(bout_duration = 0))
    non_wear_summary <- non_wear_data[,c(1,2,6)] %>%
      tidyr::pivot_wider(names_from = "activity", values_from = "duration_by_day", values_fill = list(duration_by_day = 0))
    daily_stepping_summary <- daily_stepping_data %>% dplyr::group_by(.data$uid) %>%
      dplyr::summarise(lower_quartile_daily_steps = quantile(.data$steps, 0.25),
                median_daily_steps = stats::median(.data$steps),
                upper_quartile_daily_steps = quantile(.data$steps, 0.75))
    preferred_cadence_data <- median_cadence_data
    preferred_cadence_data$group <- paste("Preferred cadence (",preferred_cadence_data$group,")",sep="")
    preferred_cadence_summary <- preferred_cadence_data %>%
      tidyr::pivot_wider(names_from="group",values_from = "median_cadence", values_fill = list(median_cadence = 0))
    peak_30_s <- walk_test_30_s_data[,c(2:4)] %>% dplyr::mutate(category = "Max 30s step") %>% dplyr::group_by(.data$uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>% dplyr::filter(.data$duration == min(.data$duration)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, .data$steps, .data$duration) %>%
      tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
    peak_2_min <- walk_test_2_min_data[,c(2:4)] %>% dplyr::mutate(category = "Walk 2 Min") %>% dplyr::group_by(.data$uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>% dplyr::filter(.data$duration == min(.data$duration)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, .data$steps, .data$duration) %>%
      tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
    peak_6_min <- walk_test_6_min_data[,c(2:4)] %>% dplyr::mutate(category = "Walk 6 Min") %>% dplyr::group_by(.data$uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>% dplyr::filter(.data$duration == min(.data$duration)) %>%
      distinct() %>% dplyr::select(.data$uid, .data$category, .data$steps, .data$duration) %>%
      tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
    peak_10_min <- walk_test_10_min_data[,c(2:4)] %>% dplyr::mutate(category = "Walk 10 Min") %>% dplyr::group_by(.data$uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>% dplyr::filter(.data$duration == min(.data$duration)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, .data$steps, .data$duration) %>%
      tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))

    overall_summary <- dplyr::inner_join(valid_day_summary,sedentary_summary, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,upright_summary, by = "uid")
    overall_summary <- dplyr::left_join(overall_summary,non_wear_summary, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,daily_stepping_summary, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,preferred_cadence_summary, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,peak_30_s, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,peak_2_min, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,peak_6_min, by = "uid")
    overall_summary <- dplyr::inner_join(overall_summary,peak_10_min, by = "uid")

    for(i in (4:16)){
      overall_summary[,i] <- round(overall_summary[,i], 4)
    }
    #####

    grDevices::pdf(paste(chart_title,"_Full_Summary_",sort_order,".pdf",sep=""), width = 16.5, height = 11.7)
    if(sort_order == "ALL"){
      for(i in sort_order_groups){
        generate.devices.summary(sedentary_data, upright_data,
                                 walk_test_2_min_data, walk_test_6_min_data, walk_test_10_min_data, walk_test_30_s_data,
                                 stepping_data, valid_days, daily_stepping_data, bouts_breaks_data,
                                 no_valid_days, median_rise_time_data, median_cadence_data,
                                 chart_title, input_folder,
                                 standard_scales, i)
      }
    }else{
      generate.devices.summary(sedentary_data, upright_data,
                               walk_test_2_min_data, walk_test_6_min_data, walk_test_10_min_data, walk_test_30_s_data,
                               stepping_data, valid_days, daily_stepping_data, bouts_breaks_data,
                               no_valid_days, median_rise_time_data, median_cadence_data,
                               chart_title, input_folder,
                               standard_scales, sort_order)
    }
    if(length(no_valid_days) > 0){
      valid_day_text <- "The following files were excluded from the summary as they did not contain any days of valid activity data:"
      for (j in no_valid_days){
        valid_day_text <- paste(valid_day_text,"\n- ",j,sep="")
      }
      valid_day_text <- grid::textGrob(valid_day_text, gp = grid::gpar(fontface = "bold"), x = unit(0.7, "in"), y = unit(11, "in"), hjust = 0)
      print(gridExtra::grid.arrange(valid_day_text))
    }

    graphics.off()

  }

generate.sort.order <-
  function(list, sort_order){
    list <- list %>% dplyr::filter(.data$category == sort_order_names[which(sort_order == sort_order_groups)])
    if (sort_order_ascending[which(sort_order == sort_order_groups)] == 0){
      list <- list %>% dplyr::arrange(.data$duration)
    }else{
      list <- list %>% dplyr::arrange(desc(.data$duration))
    }
    return(list)
  }

generate.devices.summary <-
  function(sedentary_data, upright_data, walk_test_2_min_data, walk_test_6_min_data, walk_test_10_min_data, walk_test_30_s_data,
           stepping_data, valid_days, daily_stepping_data, bouts_breaks_data, no_valid_days, median_rise_time_data, median_cadence_data,
           chart_title, input_folder, standard_scales = FALSE, sort_order){

    if(nrow(sedentary_data) == 0){
      # No data was available
      return(NULL)
    }

    colnames(sedentary_data) <- colnames(upright_data)

    chart_data <- dplyr::bind_rows(sedentary_data,upright_data)
    chart_data$category <- "Sedentary"
    chart_data[grep("Time in Bed",chart_data$bout_length),]$category <- "Time in Bed"
    chart_data[grep("Quiet Standing",chart_data$bout_length),]$category <- "Standing"
    chart_data[grep("Stepping",chart_data$bout_length),]$category <- "Standing"

    activity_group <- c("Sedentary (4 hours +)", "Sedentary (2 - 4 hours)", "Sedentary (1 - 2 hours)",
                        "Sedentary (30 min - 1 hour)", "Sedentary (< 30 min)",
                        "Quiet Standing", "Stepping (1 minute +)", "Stepping (< 1 minute)",
                        "Time in Bed (4 hours +)", "Time in Bed (2 - 4 hours)", "Time in Bed (1 - 2 hours)",
                        "Time in Bed (30 min - 1 hour)", "Time in Bed (< 30 min)")

    chart_data$bout_length <- factor(chart_data$bout_length, levels = c(activity_group))
    chart_data$category <- factor(chart_data$category, levels = c("Time in Bed","Standing","Sedentary"))

    chart_summary <- chart_data %>%
      dplyr::group_by(.data$uid, .data$category) %>%
      dplyr::summarise(duration = sum(.data$bout_duration)) %>%
      dplyr::group_by(.data$category) %>%
      dplyr::filter(.data$duration == max(.data$duration)) %>%
      dplyr::select(.data$category, .data$duration) %>%
      dplyr::transmute(duration = ceiling(.data$duration))
    chart_summary$proportion <- chart_summary$duration / sum(chart_summary$duration)
    chart_summary$category <- as.character(chart_summary$category)
    chart_summary[4,]$category <- "2 min stepping"
    chart_summary[4,]$duration <- max(walk_test_2_min_data$steps) - max(walk_test_2_min_data$steps) %% 100 + 100
    chart_summary[5,]$category <- "6 min stepping"
    chart_summary[5,]$duration <- max(walk_test_6_min_data$steps) - max(walk_test_6_min_data$steps) %% 200 + 200
    chart_summary[6,]$category <- "10 min stepping"
    chart_summary[6,]$duration <- max(walk_test_10_min_data$steps) - max(walk_test_10_min_data$steps) %% 500 + 500
    chart_summary[7,]$category <- "daily steps"
    chart_summary[7,]$duration <- max(daily_stepping_data$steps) - max(daily_stepping_data$steps) %% 5000 + 5000
    chart_summary[8,]$category <- "valid days"
    chart_summary[8,]$duration <- max((valid_days %>% group_by(.data$uid) %>% summarise(days = n()))$days) + 7 -
      (max((valid_days %>% group_by(.data$uid) %>% summarise(days = n()))$days) %% 7)
    chart_summary[9,]$category <- "30 s stepping"
    chart_summary[9,]$duration <- max(walk_test_30_s_data$steps) - max(walk_test_30_s_data$steps) %% 20 + 20
    chart_summary[10,]$category <- "median rise time"
    if(is.null(median_rise_time_data)){
      chart_summary[10,]$duration <- 0
    }else{
      chart_summary[10,]$duration <- floor(max(median_rise_time_data$median_rise_time)) + 1
    }
    chart_summary[11,]$category <- "median cadence max"
    chart_summary[11,]$duration <- max(median_cadence_data$median_cadence) - max(median_cadence_data$median_cadence) %% 20 + 20
    chart_summary[12,]$category <- "median cadence min"
    chart_summary[12,]$duration <- min(median_cadence_data$median_cadence) - min(median_cadence_data$median_cadence) %% 20

    chart_element <- chart_data[grep("Stepping",chart_data$bout_length),]
    chart_element$bout_length <- factor(chart_element$bout_length, levels = activity_group)

    posture <- chart_data %>% dplyr::group_by(.data$uid, category = as.character(.data$category)) %>%
      dplyr::summarise(duration = sum(.data$bout_duration))
    stepping <- chart_data[grep("Stepping",chart_data$bout_length),] %>% dplyr::mutate(category = "Stepping") %>%
      dplyr::group_by(.data$uid, .data$category) %>% dplyr::summarise(duration = sum(.data$bout_duration))
    sedentary_bouts <- bouts_breaks_data[,c(1,4)] %>% dplyr::mutate(category = "Sedentary Bouts") %>%
      dplyr::select(.data$uid, .data$category, duration = .data$sedentary_bout_per_day)
    tib_breaks <- bouts_breaks_data[,c(1,6)] %>% dplyr::mutate(category = "TiB Breaks") %>%
      dplyr::select(.data$uid, .data$category, duration = .data$breaks_per_day)
    peak_2_min <- walk_test_2_min_data[,c(2,3)] %>% dplyr::mutate(category = "Walk 2 Min") %>%
      dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, duration = .data$steps)
    peak_6_min <- walk_test_6_min_data[,c(2,3)] %>% dplyr::mutate(category = "Walk 6 Min") %>%
      dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, duration = .data$steps)
    peak_10_min <- walk_test_10_min_data[,c(2,3)] %>% dplyr::mutate(category = "Walk 10 Min") %>%
      dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, duration = .data$steps)
    peak_30_s <- walk_test_30_s_data[,c(2,3)] %>% dplyr::mutate(category = "Walk 30 s") %>%
      dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>% dplyr::select(.data$uid, .data$category, duration = .data$steps)
    median_steps <- daily_stepping_data %>% dplyr::mutate(category = "Median Daily Steps") %>%
      dplyr::group_by(.data$uid, .data$category) %>% dplyr::summarise(duration = stats::median(.data$steps))
    below_1_minute <- chart_element %>% dplyr::filter(.data$bout_length == "Stepping (< 1 minute)") %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(short_duration = sum(.data$bout_duration))
    if(!is.null(median_rise_time_data)){
      median_rise_time <- median_rise_time_data %>% dplyr::mutate(category = "Median Rise Time") %>%
        dplyr::group_by(.data$uid) %>% dplyr::summarise(rise_time = min(.data$median_rise_time))
    }else{
      median_rise_time <- NULL
    }
    median_cadence_max <- median_cadence_data %>% dplyr::mutate(category = "Median Cadence Max") %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(median_cadence_max = max(.data$median_cadence))
    median_cadence_min <- median_cadence_data %>% dplyr::mutate(category = "Median Cadence Min") %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(median_cadence_min = min(.data$median_cadence))
    duration <- inner_join(below_1_minute,stepping, by = "uid")
    duration$percent <- round((duration$short_duration / duration$duration * 100),0)
    duration <- duration[,c(1,3,5)]
    colnames(duration)[3] <- "duration"
    duration$category <- "Stepping Under 1 Min"
    if(is.null(median_rise_time)){
      sort_data <- bind_rows(posture, stepping, sedentary_bouts, tib_breaks,
                             peak_2_min, peak_6_min, peak_10_min, peak_30_s,
                             median_steps, duration, median_cadence_max, median_cadence_min)
    }else{
      sort_data <- bind_rows(posture, stepping, sedentary_bouts, tib_breaks,
                             peak_2_min, peak_6_min, peak_10_min, peak_30_s,
                             median_steps, duration, median_rise_time,
                             median_cadence_max, median_cadence_min)
    }
    individual_summary <- generate.sort.order(sort_data, sort_order)

    pages <- nrow(individual_summary) %/% items_per_page + 1

    walk_test_10_min_data <- walk_test_10_min_data %>% dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps))
    walk_test_6_min_data <- walk_test_6_min_data %>% dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps))
    walk_test_2_min_data <- walk_test_2_min_data %>% dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps))
    walk_test_30_s_data <- walk_test_30_s_data %>% dplyr::group_by(.data$uid) %>% dplyr::filter(.data$steps == max(.data$steps))

    for(i in (1:pages)){
      # Plot and save the page on the chart
      page_individual_summary <- individual_summary

      page_individual_summary <- page_individual_summary[((i-1)*items_per_page+1):min((i*items_per_page),nrow(page_individual_summary)),]
      page_individual_summary <- page_individual_summary %>% dplyr::arrange(desc(as.numeric(row.names(page_individual_summary))))
      page_uid <- page_individual_summary$uid

      plot_data_validation_data <- generate.valid.day.sequence.chart(valid_days[which(valid_days$uid %in% page_uid),],
                                                                     chart_summary, page_individual_summary,standard_scales,TRUE)
      plot_data_sedentary_lying <- generate.sedentary.lying.chart(chart_data[which(chart_data$uid %in% page_uid),],
                                                                  chart_summary,bouts_breaks_data,page_individual_summary,standard_scales,FALSE,FALSE)
      plot_data_standing <- generate.standing.chart(chart_data[which(chart_data$uid %in% page_uid),],
                                                    chart_summary,page_individual_summary,standard_scales,FALSE,FALSE)
      plot_data_daily_stepping <- generate.daily.stepping.summary.chart(daily_stepping_data[which(daily_stepping_data$uid %in% page_uid),],
                                                                        chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_median_cadence <- generate.median.cadence.chart(median_cadence_data[which(median_cadence_data$uid %in% page_uid),],
                                                                chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_peak_30_s_stepping <- generate.daily.30.s.stepping.chart(walk_test_30_s_data[which(walk_test_30_s_data$uid %in% page_uid),],
                                                                         chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_peak_2_min_stepping <- generate.peak.2.min.stepping.chart(walk_test_2_min_data[which(walk_test_2_min_data$uid %in% page_uid),],
                                                                          chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_peak_6_min_stepping <- generate.peak.6.min.stepping.chart(walk_test_6_min_data[which(walk_test_6_min_data$uid %in% page_uid),],
                                                                          chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_peak_10_min_stepping <- generate.peak.10.min.stepping.chart(walk_test_10_min_data[which(walk_test_10_min_data$uid %in% page_uid),],
                                                                            chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_legend <- generate.legend()

      # Add right margins to the plots for correct formatting
      plot_data_validation_data <- plot_data_validation_data + ggplot2::theme(plot.margin = margin(4,14,4,4, "pt"))
      plot_data_standing <- plot_data_standing + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))
      plot_data_sedentary_lying <- plot_data_sedentary_lying + ggplot2::theme(plot.margin = margin(4,8,4,0, "pt"))
      plot_data_peak_2_min_stepping <- plot_data_peak_2_min_stepping + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))
      plot_data_peak_6_min_stepping <- plot_data_peak_6_min_stepping + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))
      plot_data_peak_10_min_stepping <- plot_data_peak_10_min_stepping + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))
      plot_data_peak_30_s_stepping <- plot_data_peak_30_s_stepping + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))
      plot_data_median_cadence <- plot_data_median_cadence + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))
      plot_data_daily_stepping <- plot_data_daily_stepping + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))

      if(!is.null(median_rise_time)){
        plot_data_rise_time <- generate.median.rise.time.chart(median_rise_time_data[which(median_rise_time_data$uid %in% page_uid),],
                                                               chart_summary, page_individual_summary,standard_scales,FALSE)
        plot_data_rise_time <- plot_data_rise_time + theme(plot.margin = margin(4,8,4,6, "pt"))
      }

      #####

      title_list <- list()
      font_size <- 10
      title_list[[1]] <- grid::textGrob("User ID      ", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[2]] <- grid::textGrob("Valid Days      ", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[3]] <- grid::textGrob("Sedentary", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[4]] <- grid::textGrob("Time in Bed", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[5]] <- grid::textGrob("Upright", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[6]] <- grid::textGrob("Daily Step Count", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[7]] <- grid::textGrob("Median Rise Time", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[8]] <- grid::textGrob("Preferred Cadence", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[9]] <- grid::textGrob("Max 0.5 min Steps", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[10]] <- grid::textGrob("Max 2 min Steps", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[11]] <- grid::textGrob("Max 6 min Steps", gp = grid::gpar(fontsize = font_size, fontface = "bold"))
      title_list[[12]] <- grid::textGrob("Max 10 min Steps", gp = grid::gpar(fontsize = font_size, fontface = "bold"))

      footer_list <- list()
      footer_list[[1]] <- grid::textGrob(format(Sys.time(),"%d-%b-%Y"), hjust = 0, vjust = 1)
      footer_list[[2]] <- grid::textGrob(paste("Page ",i," of ",pages,sep=""), vjust = 1)

      title_list[[sort_order_title[which(sort_order_groups %in% sort_order)]]] <-
        grid::grobTree(grid::rectGrob(gp = grid::gpar(fill="black")),
                       grid::textGrob(title_list[[sort_order_title[which(sort_order_groups %in% sort_order)]]]$label, gp = grid::gpar(fontface = "bold", col="white")))

      header_padding <- grid::rectGrob(gp = grid::gpar(fill="orange", col="orange"))
      header_www <- grid::grobTree( grid::rectGrob(gp = grid::gpar(fill="orange", col="orange")),
                                    grid::textGrob("www.palt.com", gp = grid::gpar(fontsize=15, col="white", fontface="bold"), vjust = 0.5, hjust = 0.1))

      footer_image <- grid::grobTree( load.logo(system.file("logos/BWPALIconStrip.png", package = "activPAL")
                                                ,width = 250, h_just = 0.2))

      plot_data_legend <- plot_data_legend + ggplot2::theme(plot.margin = margin(0,12,0,12, "pt"))
      if(is.null(median_rise_time)){
        header_image <- grid::grobTree( grid::rectGrob(gp = grid::gpar(fill="orange", col="orange")),
                                        load.logo(system.file("logos/PALbatch_icon_inverted.png", package = "activPAL"),
                                                  width = 50, h_just = 1))

        header_details <- grid::grobTree( grid::rectGrob(gp = grid::gpar(fill="orange", col="orange")),
                                          grid::textGrob("PALsummary - Participation and Ability outcomes",
                                                         gp = grid::gpar(fontsize=15, col="white", fontface="bold"),
                                                         vjust = 0.5, hjust = 0.875))
        layout_matrix <- rbind(c(rep(24,1),rep(25,5),rep(26,3),rep(27,2)),
                               c(11,12,13,14,15,16,17,18,19,20,21),
                               c(1,1,2,2,3,4,5,6,7,8,9),
                               c(rep(10,11)),
                               c(rep(NA,11)),
                               c(22,rep(28,9),23),
                               c(rep(NA,11)))

        full_charts <- gridExtra::grid.arrange(grobs = list(plot_data_validation_data, plot_data_sedentary_lying, plot_data_standing,
                                                 plot_data_daily_stepping, plot_data_median_cadence,
                                                 plot_data_peak_30_s_stepping, plot_data_peak_2_min_stepping,
                                                 plot_data_peak_6_min_stepping, plot_data_peak_10_min_stepping,
                                                 plot_data_legend,
                                                 title_list[[1]], title_list[[2]], title_list[[3]], title_list[[4]], title_list[[5]],
                                                 title_list[[6]], title_list[[8]], title_list[[9]], title_list[[10]],
                                                 title_list[[11]], title_list[[12]],
                                                 footer_list[[1]], footer_list[[2]],
                                                 header_image, header_details, header_padding, header_www, footer_image),
                                    ncol = 11,
                                    heights = c(3,2,(4 + nrow(page_individual_summary)),8,(items_per_page - nrow(page_individual_summary)),2,1),
                                    widths = c(4,2,4,4,6,4,4,4,4,4,4),
                                    layout_matrix = layout_matrix)
      }else{
        header_image <- grid::grobTree( grid::rectGrob(gp = grid::gpar(fill="orange", col="orange")),
                                        load.logo(system.file("logos/PALbatch_icon_inverted.png", package = "activPAL"),
                                                  width = 50, h_just = 1))

        header_details <- grid::grobTree( grid::rectGrob(gp = grid::gpar(fill="orange", col="orange")),
                                          grid::textGrob("PALsummary - Participation and Ability outcomes",
                                                         gp = grid::gpar(fontsize=15, col="white", fontface="bold"),
                                                         vjust = 0.5, hjust = 0.8))
        layout_matrix <- rbind(c(rep(26,1),rep(27,5),rep(28,4),rep(29,2)),
                               c(12,13,14,15,16,17,18,19,20,21,22,23),
                               c(1,1,2,2,3,4,5,6,7,8,9,10),
                               c(rep(11,12)),
                               c(rep(NA,12)),
                               c(24,rep(30,10),25),
                               c(rep(NA,12)))

        full_charts <- gridExtra::grid.arrange(plot_data_validation_data, plot_data_sedentary_lying, plot_data_standing,
                                    plot_data_daily_stepping, plot_data_rise_time, plot_data_median_cadence,
                                    plot_data_peak_30_s_stepping, plot_data_peak_2_min_stepping,
                                    plot_data_peak_6_min_stepping, plot_data_peak_10_min_stepping,
                                    plot_data_legend,
                                    title_list[[1]], title_list[[2]], title_list[[3]], title_list[[4]], title_list[[5]],
                                    title_list[[6]], title_list[[7]], title_list[[8]], title_list[[9]], title_list[[10]],
                                    title_list[[11]], title_list[[12]],
                                    footer_list[[1]], footer_list[[2]],
                                    header_image, header_details, header_padding, header_www, footer_image,
                                    ncol = 12,
                                    heights = c(3,2,(4 + nrow(page_individual_summary)),8,(items_per_page - nrow(page_individual_summary)),2,1),
                                    widths = c(4,2,4,4,6,4,4,4,4,4,4,4),
                                    layout_matrix = layout_matrix)
      }
      #####
      print(full_charts)

    }
  }
