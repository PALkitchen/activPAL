generate.physical.behaviour.summary <-
  function(input_folder, period_file = NULL, chart_title, output_folder = "",
           prefix_delimiter = NULL, prefix_length = NULL, fill_gaps = FALSE,
           minimum_wear_time = 20, anonymise = FALSE,
           generate_chart = TRUE, standard_scales = FALSE, sort_order = "MEDIAN_DAILY_STEP_COUNT"){
    #' Generates a PDF that visualises a range of summary physical behaviour outcomes for a group of individuals. \cr
    #' Outcomes are also exported in two csv files, with outcomes aggregated by calendar day and
    #' EventsExended classified waking day / time in bed
    #'
    #' @description activity.summary.window processes all the extended events files (format *EventsEx.csv)
    #'     in a folder and produces a chart showing a number of summary physical behaviour outcomes for
    #'     the files. The outcomes are grouped into three groups:
    #'     \itemize{
    #'     \item Mean daily time in different posture classes (time in bed, sedentary, upright and stepping)
    #'     \item Volume based measures of activity participation (step count, stepping duration and intensity,
    #'        time spent in different travel associated activities)
    #'     \item Measures of physical ability (time to first step, maximum step count, median cadence of
    #'        stepping bouts containing short (< 1 minute) and long (> 1 minute) periods of stepping)
    #'     }
    #'     If matching rise time files (format *RiseSitData.csv) are included in the folder an additional
    #'     column will be inserted to show the median rise time. \c
    #'
    #'     If the function is unable to process one or more of the extended events files, an additional
    #'     file (yyMMMdd_HHMM_FileErrorList.csv) will be generated with a list of the extended events
    #'     files that were not processed and details of the error that prevented the file from being
    #'     processed. Extended events files modified in Microsoft Excel may fail to process.
    #'
    #'     The error message \strong{The Events Extended file had an unexpected format and could not be processed}
    #'     indicates the file may have been altered. For these files, Re-exporting the Events Extended
    #'     file from PAL Analysis / PAL Batch may rectify the issue preventing the file from being
    #'     processed.

    #'
    #' @param input_folder The folder where the events files (format *EventsEx.csv)
    #'     and optional rise time files (format *RiseSitData.csv) to be processed are saved.
    #' @param period_file The location of a csv files containing the details of custom periods to
    #'     to be used when analysing the supplied events files.
    #'
    #'     If the location of a valid file is supplied the csv file with the suffix "_Custom_Summary"
    #'     will be generated containing the outcomes using the supplied periods.
    #'
    #'     Each row defines a single observation for a single events file. The following columns
    #'     must be in the spreadsheet.
    #'     filename = an identifier for the events file that the period is for.  This should be
    #'     the same as the identifier generated using the prefix_delimiter and prefix_length parameters
    #'     startime = the starting time of the period
    #'     endtime = the ending time of the period
    #'     The function will attempt to parse the supplied times in the following order
    #'     "YYYY-mm-dd HH:MM", "YYYY-mm-dd HH:MM:SS", "YYYY-dd-mm HH:MM", "YYYY-dd-mm HH:MM:SS",
    #'     "dd/mm/YYYY HH:MM", "dd/mm/YYYY HH:MM:SS", "mm/dd/YYYY HH:MM", "mm/dd/YYYY HH:MM:SS"
    #' @param chart_title Character string; the file identifier for the generated PDF file.
    #' @param output_folder The folder where the physical behaviour summary report is to be saved.
    #'     By default the report is saved in the current working directory
    #' @param prefix_delimiter Character string to be matched against the events file name when
    #'     generating the file identifier.  The generated identifier is the portion of the file name
    #'     preceding the supplied string.
    #' @param prefix_length An integer that specifies the number of characters to take from the
    #'     start of the events file name to generate the file identifier.
    #'     If prefix_delimiter and prefix_length are both provided, the shortest file identifier
    #'     that can be generated using the parameters is used.
    #'     If neither prefix_delimiter or prefix_length are provided, the first six characters
    #'     of the file name is used as the file identifier.
    #' @param fill_gaps Specifies if outcomes should be calculated for any time periods not
    #'     explicitly specified by a period file.
    #'     This parameter will only be used if a valid period file is supplied
    #' @param minimum_wear_time the minimum number of hours of valid wear
    #' @param anonymise logical; if true, set the file identifier as a non-identifiable string.
    #' @param generate_chart logical: if false the PDF chart is not generated and only the
    #'     csv files with the outcomes reported per calendar day / waking day are generated
    #' @param standard_scales logical; if true, time-based axis limits are set at 24 hours.
    #'     Using standard scales is likely to result in significant amounts of empty space.
    #' @param sort_order character string; selects the outcome measure that is used to
    #'     sort the observations.
    #'     By default, files are sorted by decreasing mean daily sedentary time.
    #' @details The following outcomes can be used to sort observations in the chart
    #'     SEDENTARY_TIME, TIME_IN_BED, UPRIGHT_TIME, STEPPING_TIME,
    #'     DAILY_SEDENTARY_BOUTS, DAILY_TIB_INTERRUPTIONS, STEPPING_BOUTS_UNDER_1_MIN,
    #'     PEAK_2_MIN_STEPPING, PEAK_6_MIN_STEPPING, PEAK_12_MIN_STEPPING,
    #'     DAILY_PEAK_30_SECOND_STEPPING, MEDIAN_DAILY_STEP_COUNT, MEDIAN_RISE_TIME,
    #'     MEDIAN_CADENCE_TO_1_MINUTE , MEDIAN_CADENCE_1_MINUTE_PLUS, STEPPING_INTENSITY

    #' @export
    #' @import dplyr
    #' @importFrom gridExtra grid.arrange

    sort_order_groups <- c("SEDENTARY_TIME", "TIME_IN_BED", "UPRIGHT_TIME", "STEPPING_TIME",
                           "DAILY_SEDENTARY_BOUTS","DAILY_TIB_INTERRUPTIONS","STEPPING_BOUTS_UNDER_1_MIN",
                           "PEAK_2_MIN_STEPPING", "PEAK_6_MIN_STEPPING", "PEAK_12_MIN_STEPPING",
                           "DAILY_PEAK_30_SECOND_STEPPING", "MEDIAN_DAILY_STEP_COUNT","MEDIAN_RISE_TIME",
                           "MEDIAN_CADENCE_TO_1_MINUTE","MEDIAN_CADENCE_1_MINUTE_PLUS", "STEPPING_INTENSITY","UID")
    sort_order_names <- c("Sedentary","Time in Bed", "Standing", "Stepping",
                          "Sedentary Bouts", "TiB Breaks", "Stepping Under 1 Min",
                          "Walk 2 Min", "Walk 6 Min", "Walk 12 Min", "Walk 30 s", "Median Daily Steps", "Median Rise Time",
                          "Median Cadence Short", "Median Cadence Long","Stepping Intensity")
    sort_order_ascending <- c(1,1,1,1,
                              1,1,1,
                              1,1,1,
                              1,1,1,
                              1,1,1)
    sort_order_title <- c(8,7,9,9,
                          8,7,12,
                          17,18,19,16,10,14,
                          5,5,4)

    if (!sort_order %in% sort_order_groups & sort_order != "ALL"){
      sort_order <- "SEDENTARY_TIME"
      warning("Invalid sorting criteria selected. Summary sorted by total sedentary time.")
    }

    file_names <- list.files(input_folder, pattern="*.csv",recursive = FALSE)
    validation_data <- file_names[grep("DailyValidation",file_names)]
    file_names <- file_names[grep("EventsEx",file_names)]

    if(length(file_names) == 0){
      stop(paste("The summary outcome report was not generated as the selected folder contains no events extended files.",
      "If there are events extended files in sub-folders you should move these files into the parent folder.", sep = " "))
    }

    validation_data <- load.validation.data(input_folder,validation_data)
    lookup_data <- load.lookup.period.file(period_file, fill_gaps)

    valid_days <- list()

    walk_test_2_min_data <- list()
    walk_test_6_min_data <- list()
    walk_test_12_min_data <- list()
    walk_test_30_s_data <- list()

    bouts_breaks_data <- list()
    sedentary_data <- list()
    upright_data <- list()
    stepping_data <- list()
    travel_data <- list()
    mvpa_data <- list()
    daily_stepping_data <- list()
    time_first_step_data <- list()

    median_rise_time_data <- list()
    median_cadence_data <- list()
    non_wear_data <- list()

    no_valid_days <- c()

    calendar_day_summary <- list()
    waking_day_summary <- list()
    custom_period_summary <- list()

    median_rise_time <- process.rise.time(input_folder)
    if(!is.null(median_rise_time)){
      median_rise_time$uid <- parse.file.name(median_rise_time$uid, prefix_delimiter, prefix_length)
    }

    skipped_files <- c()
    minimum_wear_minutes <- 3600 * minimum_wear_time
    for(i in (1:length(file_names))){
      tryCatch({
        events_import <- process.events.file(input_folder, file_names[i], validation_data, minimum_wear_time, prefix_delimiter, prefix_length)
        file_uid <- parse.file.name(file_names[i], prefix_delimiter, prefix_length)

        events_file_data <- events_import[[1]]

        if(!is.null(events_file_data)){
          events_file_data_calendar <- set.calendar.day.periods(events_file_data)
          events_file_data_waking_day <- set.waking.day.periods(events_file_data)
          calendar_day_periods <- get.calendar.day.periods(events_file_data_calendar, file_uid)
          calendar_day_periods$period_date <- as.Date(calendar_day_periods$period_date, origin = "1970-01-01")
          waking_day_periods <- get.waking.day.periods(events_file_data_waking_day, file_uid)
          waking_day_periods$period_date <- as.Date(waking_day_periods$period_date, origin = "1970-01-01")
          if(!is.null(lookup_data)){
            custom_periods <- lookup_data %>% dplyr::filter(id == file_uid)
            if(nrow(custom_periods) > 0){
              events_file_data_custom <- set.custom.periods(events_file_data,custom_periods)
              custom_period_summary[[i]] <- custom.period.summary(input_folder,file_names[i],file_uid,events_file_data_custom, custom_periods)
            }
          }
          waking_day_summary[[i]] <- custom.period.summary(input_folder,file_names[i],file_uid,events_file_data_waking_day, waking_day_periods)
          calendar_day_summary[[i]] <- custom.period.summary(input_folder,file_names[i],file_uid,events_file_data_calendar, calendar_day_periods)

          valid_day_list <- events_import[[2]]
          valid_day_list <- valid_day_list %>%
            dplyr::filter(valid == "valid") %>%
            dplyr::tally()
          if(valid_day_list > 0){
            valid_days[[i]] <- events_import[[2]]

            if(!is.null(median_rise_time)){
              median_rise_time_data[[i]] <- median_rise_time[which(median_rise_time$uid == file_uid),]
            }

            median_cadence_data[[i]] <- median.cadence.bands.file(events_file_data, file_uid)

            walk_test_2_min <- activpal.stepping.process.file(input_folder,file_names[i],file_uid,valid_days[[i]],120,86400,minimum_wear_minutes,FALSE)
            walk_test_6_min <- activpal.stepping.process.file(input_folder,file_names[i],file_uid,valid_days[[i]],360,86400,minimum_wear_minutes,FALSE)
            walk_test_12_min <- activpal.stepping.process.file(input_folder,file_names[i],file_uid,valid_days[[i]],720,86400,minimum_wear_minutes,FALSE)
            walk_test_30_s <- activpal.stepping.process.file(input_folder,file_names[i],file_uid,valid_days[[i]],30,86400,minimum_wear_minutes,FALSE)

            walk_test_2_min$uid <- file_uid
            walk_test_6_min$uid <- file_uid
            walk_test_12_min$uid <- file_uid
            walk_test_30_s$uid <- file_uid

            walk_test_2_min_data[[i]] <- build.walk.test.summary(walk_test_2_min)
            walk_test_6_min_data[[i]] <- build.walk.test.summary(walk_test_6_min)
            walk_test_12_min_data[[i]] <- build.walk.test.summary(walk_test_12_min)
            walk_test_30_s_data[[i]] <- build.walk.test.summary(walk_test_30_s)

            sedentary_bouts <- build.sedentary.bout.summary(events_file_data)
            daily_sedentary_bouts <- build.daily.sedentary.bout.summary(events_file_data)
            lying_time_breaks <- process.breaks.in.time.in.bed(events_file_data)
            daily_lying_time_breaks <- process.daily.breaks.in.time.in.bed(events_file_data)

            bouts_breaks_data[[i]] <- dplyr::inner_join(sedentary_bouts,lying_time_breaks, by = c("uid", "valid_days"))
            non_wear_data[[i]] <- build.non.wear.summary(events_file_data)

            sedentary_data[[i]] <- build.sedentary.summary(events_file_data)
            upright_data[[i]] <- build.upright.summary(events_file_data)
            stepping_data[[i]] <- build.stepping.summary(events_file_data)
            travel_data[[i]] <- build.travel.summary(events_file_data)
            mvpa_data[[i]] <- build.stepping.intensity.summary(events_file_data)
            time_first_step_data[[i]] <- build.time.to.first.step.summary(events_file_data)

            daily_stepping_data[[i]] <- events_file_data %>%
              dplyr::group_by(uid, date = Date) %>%
              dplyr::summarise(steps = sum(.data$steps))
          }else{
            no_valid_days <- c(no_valid_days, file_names[i])
          }
        } else{
          no_valid_days <- c(no_valid_days, file_names[i])
        }
        message(paste("Processed File ",i," of ",length(file_names),sep=""))
      },
      error = function(c){
        if(length(custom_period_summary) >= (i-1) & length(custom_period_summary) > 0){
          custom_period_summary[[i]] <- NULL
        }
        calendar_day_summary[[i]] <<- NULL
        waking_day_summary[[i]] <<- NULL
        walk_test_2_min_data[[i]] <<- NULL
        walk_test_6_min_data[[i]] <<- NULL
        walk_test_12_min_data[[i]] <<- NULL
        walk_test_30_s_data[[i]] <<- NULL
        bouts_breaks_data[[i]] <<- NULL
        non_wear_data[[i]] <<- NULL
        sedentary_data[[i]] <<- NULL
        upright_data[[i]] <<- NULL
        stepping_data[[i]] <<- NULL
        travel_data[[i]] <<- NULL
        mvpa_data[[i]] <<- NULL
        time_first_step_data[[i]] <<- NULL
        daily_stepping_data[[i]] <<- NULL
        skipped_files <<- c(skipped_files, paste(file_names[i],";",
                                                 substr(paste(c,sep=""), 1, regexpr(":",paste(c,sep=""))[1]-1),";",
                                                 substr(paste(c,sep=""), regexpr(":",paste(c,sep=""))[1]+2, nchar(paste(c,sep=""))-1),sep=""))
        message(paste("An error was encountered processing ", file_names[i],
                      ". Outcomes have not been generated for this file.", sep=""))
      })
    }
    if(length(valid_days) == 0){
      if(length(skipped_files) > 0){
        message(paste("No EventsEx files were processed!", sep=""))
        skipped_file_list <- tidyr::separate(data.frame(skipped_files), sep = ";", col = 1, into = c("The following files were not processed","Error Trace","Error Message"))
        error_file_name <- paste(format(Sys.time(), "%y%b%d_%H%M"),"_FileErrorList.csv",sep="")
        write.csv(skipped_file_list,error_file_name, row.names=FALSE)
        message(paste("Details of the files that were not processed has been saved to the file ",error_file_name,sep=""))
      }
      message("No outcome data will be generated.")
      return()
    }
    if(length(custom_period_summary) > 0){
      message("Outputting Custom Period Output")
      custom_period_summary <- dplyr::bind_rows(custom_period_summary)
      if(output_folder == ""){
        write.csv(custom_period_summary, paste(chart_title, "_Custom_Summary.csv", sep=""), row.names = FALSE)
      }else{
        write.csv(custom_period_summary, paste(output_folder, "/", chart_title, "_Custom_Summary.csv", sep=""), row.names = FALSE)
      }
    }
    message("Outputting Waking Day Output")
    waking_day_summary <- dplyr::bind_rows(waking_day_summary)
    if(output_folder == ""){
      write.csv(waking_day_summary, paste(chart_title, "_Waking_Day_Summary.csv", sep=""), row.names = FALSE)
    }else{
      write.csv(waking_day_summary, paste(output_folder, "/", chart_title, "_Waking_Day_Summary.csv", sep=""), row.names = FALSE)
    }
    message("Outputting Calendar Day Output")
    calendar_day_summary <- dplyr::bind_rows(calendar_day_summary)
    if(output_folder == ""){
      write.csv(calendar_day_summary, paste(chart_title, "_Calendar_Day_Summary.csv", sep=""), row.names = FALSE)
    }else{
      write.csv(calendar_day_summary, paste(output_folder, "/", chart_title, "_Calendar_Day_Summary.csv", sep=""), row.names = FALSE)
    }
    if(length(skipped_files) > 0){
      message(paste("Successfully generated outcomes for ",i - length(skipped_files), " EventsEx files\n",
                    length(skipped_files), " files were not processed", sep=""))
      skipped_file_list <- tidyr::separate(data.frame(skipped_files), sep = ";", col = 1, into = c("The following files were not processed","Error Trace","Error Message"))
      error_file_name <- paste(format(Sys.time(), "%y%b%d_%H%M"),"_FileErrorList.csv",sep="")
      write.csv(skipped_file_list,error_file_name, row.names=FALSE)
      message(paste("Details of the files that were not processed has been saved to the file ",error_file_name,sep=""))
    }else{
      message(paste("Successfully Processed ",i , " EventsEx files" ,sep=""))
    }
    if(generate_chart){
      valid_days <- dplyr::bind_rows(valid_days)

      walk_test_2_min_data <- dplyr::bind_rows(walk_test_2_min_data)
      walk_test_6_min_data <- dplyr::bind_rows(walk_test_6_min_data)
      walk_test_12_min_data <- dplyr::bind_rows(walk_test_12_min_data)
      walk_test_30_s_data <- dplyr::bind_rows(walk_test_30_s_data)

      bouts_breaks_data <- dplyr::bind_rows(bouts_breaks_data)
      sedentary_data <- dplyr::bind_rows(sedentary_data)
      upright_data <- dplyr::bind_rows(upright_data)
      stepping_data <- dplyr::bind_rows(stepping_data)
      travel_data <- dplyr::bind_rows(travel_data)
      daily_stepping_data <- dplyr::bind_rows(daily_stepping_data)
      mvpa_data <- dplyr::bind_rows(mvpa_data)
      time_first_step_data <- dplyr::bind_rows(time_first_step_data)

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
        anonymise_table <- build.anonymous.mapping(parse.file.name(stepping_data$uid, prefix_delimiter, prefix_length))
        events_file_data$uid <- anonymise.id(events_file_data$uid,anonymise_table)
        valid_days$uid <- anonymise.id(valid_days$uid,anonymise_table)
        walk_test_2_min_data$uid <- anonymise.id(walk_test_2_min_data$uid,anonymise_table)
        walk_test_6_min_data$uid <- anonymise.id(walk_test_6_min_data$uid,anonymise_table)
        walk_test_12_min_data$uid <- anonymise.id(walk_test_12_min_data$uid,anonymise_table)
        walk_test_30_s_data$uid <- anonymise.id(walk_test_30_s_data$uid,anonymise_table)

        bouts_breaks_data$uid <- anonymise.id(bouts_breaks_data$uid,anonymise_table)
        sedentary_data$uid <- anonymise.id(sedentary_data$uid,anonymise_table)
        upright_data$uid <- anonymise.id(upright_data$uid,anonymise_table)
        stepping_data$uid <- anonymise.id(stepping_data$uid,anonymise_table)
        daily_stepping_data$uid <- anonymise.id(daily_stepping_data$uid,anonymise_table)
        travel_data$uid <- anonymise.id(travel_data$uid,anonymise_table)
        mvpa_data$uid <- anonymise.id(mvpa_data$uid,anonymise_table)
        time_first_step_data$uid <- anonymise.id(time_first_step_data$uid,anonymise_table)
        if(!is.null(median_rise_time_data)){
          median_rise_time_data$uid <- anonymise.id(median_rise_time_data$uid,anonymise_table)
        }
        median_cadence_data$uid <- anonymise.id(median_cadence_data$uid,anonymise_table)
        non_wear_data$uid <- anonymise.id(non_wear_data$uid,anonymise_table)
      }

      graphics.off()

      #####
      valid_day_summary <- valid_days %>%
        dplyr::group_by(uid, category = .data$valid) %>%
        dplyr::summarise(days = n()) %>%
        tidyr::pivot_wider(names_from = "category", values_from = "days", values_fill = list(days = 0))
      sedentary_summary <- sedentary_data %>%
        dplyr::select("uid", "bout_length", "bout_duration") %>%
        tidyr::pivot_wider(names_from = "bout_length", names_expand = TRUE, values_from = "bout_duration", values_fill = list(bout_duration = 0))
      upright_summary <- upright_data %>%
        dplyr::select("uid", "bout_length", "bout_duration") %>%
        tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration", values_fill = list(bout_duration = 0))
      if(ncol(non_wear_data) == 4){
        non_wear_data$days <- 0
        non_wear_data$duration_by_day <- 0
      }
      non_wear_summary <- non_wear_data[,c(1,2,6)] %>%
        tidyr::pivot_wider(names_from = "activity", values_from = "duration_by_day", values_fill = list(duration_by_day = 0))
      daily_stepping_summary <- daily_stepping_data %>%
        dplyr::group_by(uid) %>%
        dplyr::summarise(lower_quartile_daily_steps = quantile(.data$steps, 0.25),
                         median_daily_steps = stats::median(.data$steps),
                         upper_quartile_daily_steps = quantile(.data$steps, 0.75))
      preferred_cadence_data <- median_cadence_data
      preferred_cadence_data$group <- paste("Preferred cadence (",preferred_cadence_data$group,")",sep="")
      preferred_cadence_summary <- preferred_cadence_data %>%
        tidyr::pivot_wider(names_from="group",values_from = "median_cadence", values_fill = list(median_cadence = 0))
      peak_30_s <- walk_test_30_s_data %>% dplyr::mutate(category = "Max 30s step") %>%
        dplyr::group_by(uid) %>%
        dplyr::filter("steps" == max("steps")) %>%
        dplyr::filter("duration" == min("duration")) %>%
        dplyr::distinct() %>%
        dplyr::select("uid", "category", "steps", "duration") %>%
        tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
      peak_2_min <- walk_test_2_min_data %>%
        dplyr::mutate(category = "Walk 2 Min") %>%
        dplyr::group_by(uid) %>%
        dplyr::filter("steps" == max("steps")) %>%
        dplyr::filter("duration" == min("duration")) %>%
        dplyr::distinct() %>%
        dplyr::select("uid", "category", "steps", "duration") %>%
        tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
      peak_6_min <- walk_test_6_min_data %>%
        dplyr::mutate(category = "Walk 6 Min") %>%
        dplyr::group_by(uid) %>%
        dplyr::filter("steps" == max("steps")) %>%
        dplyr::filter("duration" == min("duration")) %>%
        dplyr::distinct() %>%
        dplyr::select("uid", "category", "steps", "duration") %>%
        tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
      peak_12_min <- walk_test_12_min_data %>%
        dplyr::mutate(category = "Walk 12 Min") %>%
        dplyr::group_by(uid) %>%
        dplyr::filter("steps" == max("steps")) %>%
        dplyr::filter("duration" == min("duration")) %>%
        dplyr::distinct() %>%
        dplyr::select("uid", "category", "steps", "duration") %>%
        tidyr::pivot_wider(names_from = "category", values_from = c("steps", "duration"), values_fill = list(steps = 0, duration = 0))
      travel_data_summary <- travel_data %>%
        dplyr::select("uid", "bout_length", "bout_duration") %>%
        tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration")
      mvpa_summary <- mvpa_data %>%
        tidyr::pivot_wider(names_from = "category", values_from = "time") %>%
        tidyr::replace_na(list(LPA = 0, MPA = 0, MVPA = 0, VPA = 0))

      overall_summary <- dplyr::inner_join(valid_day_summary,sedentary_summary, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,upright_summary, by = "uid")
      overall_summary <- dplyr::left_join(overall_summary,non_wear_summary, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,daily_stepping_summary, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,preferred_cadence_summary, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,peak_30_s, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,peak_2_min, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,peak_6_min, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,peak_12_min, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,travel_data_summary, by = "uid")
      overall_summary <- dplyr::inner_join(overall_summary,mvpa_summary, by = "uid")

      numeric_columns <- c(1:ncol(overall_summary))[-c(which(colnames(overall_summary) == "uid"),which(colnames(overall_summary) == "duration"))]
      overall_summary[,numeric_columns] <- round(overall_summary[,numeric_columns], 1)
      overall_summary <- overall_summary %>% pivot_wider(names_from = 31, values_from = c(33,34,35,36))

      message("Generating summary outcome PDF")
      if(sort_order == "ALL"){
        for(i in sort_order_groups){
          generate.devices.summary(sedentary_data, upright_data,
                                   walk_test_2_min_data, walk_test_6_min_data, walk_test_12_min_data, walk_test_30_s_data,
                                   stepping_data, travel_data, mvpa_data, valid_days, daily_stepping_data, bouts_breaks_data,
                                   no_valid_days, median_rise_time_data, time_first_step_data, median_cadence_data,
                                   chart_title, output_folder,
                                   standard_scales, i)
        }
      }else{
        generate.devices.summary(sedentary_data, upright_data,
                                 walk_test_2_min_data, walk_test_6_min_data, walk_test_12_min_data, walk_test_30_s_data,
                                 stepping_data, travel_data, mvpa_data, valid_days, daily_stepping_data, bouts_breaks_data,
                                 no_valid_days, median_rise_time_data, time_first_step_data, median_cadence_data,
                                 chart_title, output_folder,
                                 standard_scales, sort_order)
      }
      if(length(no_valid_days) > 0){
        all_lines <- list()
        all_lines[[1]] <- grid::textGrob("The following files were excluded from the summary as they did not contain any days of valid activity data:",
                                         gp = grid::gpar(fontface = "bold"), just = "center")
        for (j in (1:length(no_valid_days))){
          all_lines[[j + 1]] <- grid::textGrob(paste("\n",no_valid_days[j],sep=""), gp = grid::gpar(fontface = "bold"), just = "center")
        }
        curr_pos <- 1
        while(curr_pos < length(all_lines)){
          page_lines <- all_lines[curr_pos:min(curr_pos + 49, length(all_lines))]
          gridExtra::grid.arrange(grobs = page_lines,
                                  layout_matrix = t(rbind(c(seq(1,length(page_lines),1),rep(NA,55 - length(page_lines))))),
                                  ncol = 1,
                                  nrow = 55,
                                  heights = rep(1,55))
          curr_pos <- curr_pos + 50
        }
      }
      graphics.off()
      if(output_folder == ""){
        message(paste("Summary outcome PDF saved to ",
                      paste(getwd(), "/", chart_title, "_Full_Summary_" ,sort_order, ".pdf", sep=""),
                      sep=""))
      }else{
        message(paste("Summary outcome PDF saved to ",
                      paste(output_folder, "/", chart_title, "_Full_Summary_" ,sort_order, ".pdf", sep=""),
                      sep=""))
      }
    }
  }

generate.sort.order <-
  function(list, sort_order){
    #' @import dplyr
    if(sort_order == "UID"){
      list <- list %>% dplyr::filter(.data$category == "Stepping")
      list <- list %>% dplyr::arrange(uid)
      return(list)
    }

    sort_order_groups <- c("SEDENTARY_TIME", "TIME_IN_BED", "UPRIGHT_TIME", "STEPPING_TIME",
                           "DAILY_SEDENTARY_BOUTS","DAILY_TIB_INTERRUPTIONS","STEPPING_BOUTS_UNDER_1_MIN",
                           "PEAK_2_MIN_STEPPING", "PEAK_6_MIN_STEPPING", "PEAK_12_MIN_STEPPING",
                           "DAILY_PEAK_30_SECOND_STEPPING", "MEDIAN_DAILY_STEP_COUNT","MEDIAN_RISE_TIME",
                           "MEDIAN_CADENCE_TO_1_MINUTE","MEDIAN_CADENCE_1_MINUTE_PLUS", "STEPPING_INTENSITY")
    sort_order_names <- c("Sedentary","Time in Bed", "Standing", "Stepping",
                          "Sedentary Bouts", "TiB Breaks", "Stepping Under 1 Min",
                          "Walk 2 Min", "Walk 6 Min", "Walk 12 Min", "Walk 30 s", "Median Daily Steps", "Median Rise Time",
                          "Median Cadence Short", "Median Cadence Long","Stepping Intensity")
    sort_order_ascending <- c(1,1,1,1,
                              1,1,1,
                              1,1,1,
                              1,1,1,
                              1,1,1)

    list <- list %>% dplyr::filter(.data$category == sort_order_names[which(sort_order == sort_order_groups)])
    if (sort_order_ascending[which(sort_order == sort_order_groups)] == 0){
      list <- list %>% dplyr::arrange(.data$duration)
    }else{
      list <- list %>% dplyr::arrange(desc(.data$duration))
    }
    return(list)
  }

generate.devices.summary <-
  function(sedentary_data, upright_data, walk_test_2_min_data, walk_test_6_min_data, walk_test_12_min_data, walk_test_30_s_data,
           stepping_data, travel_data, mvpa_data, valid_days, daily_stepping_data, bouts_breaks_data, no_valid_days,
           median_rise_time_data, median_first_step_data, median_cadence_data,
           chart_title, output_folder, standard_scales = FALSE, sort_order){
    #' @import dplyr
    #' @import tidyr

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

    activity_group <- c("Sedentary (4 hours +)", "Sedentary (2 - 4 hours)", "Sedentary (1 - 2 hours)", "Sedentary (30 min - 1 hour)", "Sedentary (< 30 min)",
                        "Quiet Standing", "Stepping (1 minute +)", "Stepping (< 1 minute)",
                        "Time in Bed (4 hours +)", "Time in Bed (2 - 4 hours)", "Time in Bed (1 - 2 hours)", "Time in Bed (30 min - 1 hour)", "Time in Bed (< 30 min)")

    chart_data$bout_length <- factor(chart_data$bout_length, levels = c(activity_group))
    chart_data$category <- factor(chart_data$category, levels = c("Time in Bed","Standing","Sedentary"))

    chart_summary <- chart_data %>%
      dplyr::group_by(uid, .data$category, .drop = FALSE) %>%
      dplyr::summarise(duration = sum(.data$bout_duration)) %>%
      dplyr::group_by(.data$category) %>%
      dplyr::filter(.data$duration == max(.data$duration)) %>%
      dplyr::group_by(.data$category) %>%
      dplyr::slice_head() %>%
      dplyr::select("category", "duration") %>%
      dplyr::transmute(duration = ceiling(.data$duration))
    chart_summary$proportion <- chart_summary$duration / sum(chart_summary$duration)
    chart_summary$category <- as.character(chart_summary$category)
    chart_summary[4,]$category <- "2 min stepping"
    chart_summary[4,]$duration <- max(walk_test_2_min_data$steps) - max(walk_test_2_min_data$steps) %% 100 + 100
    chart_summary[5,]$category <- "6 min stepping"
    chart_summary[5,]$duration <- max(walk_test_6_min_data$steps) - max(walk_test_6_min_data$steps) %% 200 + 200
    chart_summary[6,]$category <- "twelve min stepping"
    chart_summary[6,]$duration <- max(walk_test_12_min_data$steps) - max(walk_test_12_min_data$steps) %% 500 + 500
    chart_summary[7,]$category <- "daily steps"
    chart_summary[7,]$duration <- max(daily_stepping_data$steps) - max(daily_stepping_data$steps) %% 5000 + 5000
    chart_summary[8,]$category <- "valid days"
    chart_summary[8,]$duration <- max((valid_days %>% dplyr::group_by(uid) %>% dplyr::summarise(days = n()))$days) + 7 -
      (max((valid_days %>% dplyr::group_by(uid) %>% dplyr::summarise(days = n()))$days) %% 7)
    chart_summary[9,]$category <- "30 s stepping"
    chart_summary[9,]$duration <- max(walk_test_30_s_data$steps) - max(walk_test_30_s_data$steps) %% 20 + 20
    chart_summary[10,]$category <- "median rise time"
    if(is.null(median_rise_time_data)){
      chart_summary[10,]$duration <- 3
    }else{
      chart_summary[10,]$duration <- floor(max(median_rise_time_data$median_rise_time)) + 1
    }
    chart_summary[11,]$category <- "median time first step"
    chart_summary[11,]$duration <- floor(max(median_first_step_data$time_first_step)) + 1

    chart_summary[12,]$category <- "median cadence max"
    chart_summary[12,]$duration <- max(median_cadence_data$median_cadence)
    chart_summary[13,]$category <- "median cadence min"
    chart_summary[13,]$duration <- min(median_cadence_data$median_cadence)
    chart_summary[14,]$category <- "max intensity"
    mvpa_max <- mvpa_data %>%
      dplyr::mutate(val = .data$time / abs(.data$time)) %>%
      dplyr::group_by(uid, .data$val) %>%
      dplyr::summarise(total_time = abs(sum(.data$time))) %>%
      dplyr::arrange(desc(.data$total_time))
    chart_summary[14,]$duration <- max(mvpa_max$total_time)
    chart_summary[14,]$duration <- chart_summary[14,]$duration - (chart_summary[14,]$duration %% 20) + 20

    travel_summary <- travel_data %>%
      dplyr::select("uid", "bout_length", "bout_duration") %>%
      tidyr::pivot_wider(names_from = "bout_length", values_from = "bout_duration") %>%
      dplyr::group_by(uid) %>%
      dplyr::summarise(seated_transport = sum(.data$Seated_Transport), active_travel = sum(.data$Active_Walking + .data$Cycling))

    chart_summary[15,]$category <- "active travel"
    chart_summary[15,]$duration <- floor(max(travel_summary$active_travel)) + 1
    chart_summary[16,]$category <- "seated transport"
    chart_summary[16,]$duration <- floor(max(travel_summary$seated_transport)) + 1

    mvpa_summary <- mvpa_data %>%
      dplyr::group_by(uid, .data$duration) %>%
      dplyr::summarise(sum_time = sum(.data$time) / 60) %>%
      dplyr::group_by(.data$duration) %>%
      dplyr::summarise(time = max(.data$sum_time))

    chart_summary[17,]$category <- "short"
    if(length(round(mvpa_summary[which(mvpa_summary$duration == "short"),]$time, 2)) == 0){
      chart_summary[17,]$duration <- round(mvpa_summary[which(mvpa_summary$duration == "short"),]$time, 2)
    }else{
      chart_summary[17,]$duration <- 0
    }
    chart_summary[18,]$category <- "long"
    if(length(round(mvpa_summary[which(mvpa_summary$duration == "long"),]$time, 2)) == 0){
      chart_summary[18,]$duration <- 0
    }else{
      chart_summary[18,]$duration <- round(mvpa_summary[which(mvpa_summary$duration == "long"),]$time, 2)
    }
    chart_summary[19,]$category <- "min cadence"
    chart_summary[19,]$duration <- min(walk_test_12_min_data$cadence)
    chart_summary[20,]$category <- "peak cadence"
    chart_summary[20,]$duration <- max(walk_test_30_s_data$cadence)

    chart_element <- chart_data[grep("Stepping",chart_data$bout_length),]
    chart_element$bout_length <- factor(chart_element$bout_length, levels = activity_group)

    posture <- chart_data %>%
      dplyr::group_by(uid, category = as.character(.data$category)) %>%
      dplyr::summarise(duration = sum(.data$bout_duration))
    stepping <- chart_data[grep("Stepping",chart_data$bout_length),] %>%
      dplyr::mutate(category = "Stepping") %>%
      dplyr::group_by(uid, .data$category) %>%
      dplyr::summarise(duration = sum(.data$bout_duration))
    sedentary_bouts <- bouts_breaks_data[,c(1,4)] %>%
      dplyr::mutate(category = "Sedentary Bouts") %>%
      dplyr::select("uid", "category", duration = "sedentary_bout_per_day")
    tib_breaks <- bouts_breaks_data[,c(1,6)] %>%
      dplyr::mutate(category = "TiB Breaks") %>%
      dplyr::select("uid", "category", duration = "breaks_per_day")
    peak_2_min <- walk_test_2_min_data %>%
      dplyr::mutate(category = "Walk 2 Min") %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>%
      dplyr::select("uid", "category", duration = "steps")
    peak_6_min <- walk_test_6_min_data %>%
      dplyr::mutate(category = "Walk 6 Min") %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>%
      dplyr::select("uid", "category", duration = "steps")
    peak_12_min <- walk_test_12_min_data %>%
      dplyr::mutate(category = "Walk 12 Min") %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>%
      dplyr::select("uid", "category", duration = "steps")
    peak_30_s <- walk_test_30_s_data %>%
      dplyr::mutate(category = "Walk 30 s") %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps)) %>%
      dplyr::distinct() %>%
      dplyr::select("uid", "category", duration = "steps")
    median_steps <- daily_stepping_data %>%
      dplyr::mutate(category = "Median Daily Steps") %>%
      dplyr::group_by(uid, .data$category) %>%
      dplyr::summarise(duration = stats::median(.data$steps))
    below_1_minute <- chart_element %>%
      dplyr::filter(.data$bout_length == "Stepping (< 1 minute)") %>%
      dplyr::group_by(uid) %>%
      dplyr::summarise(short_duration = sum(.data$bout_duration))
    travel_summary <- travel_data %>%
      dplyr::select("uid", category = "bout_length", duration = "bout_duration")
    mvpa_summary <- mvpa_data %>%
      dplyr::group_by(uid, .data$category) %>%
      dplyr::summarise(total_time = sum(abs(.data$time))) %>%
      dplyr::select("uid", "category", duration = "total_time")
    if(!is.null(median_rise_time_data)){
      median_rise_time <- median_rise_time_data %>%
        dplyr::mutate(category = "Median Rise Time") %>%
        dplyr::group_by(uid, .data$category) %>%
        dplyr::summarise(duration = min(.data$median_rise_time))
    }else{
      median_rise_time <- NULL
    }
    median_cadence_max <- median_cadence_data %>%
      dplyr::filter(.data$group == "1 minute +") %>%
      dplyr::mutate(category = "Median Cadence Long") %>%
      dplyr::group_by(uid, .data$category) %>%
      dplyr::summarise(duration = max(.data$median_cadence))
    median_cadence_min <- median_cadence_data %>%
      dplyr::filter(.data$group == "< 1 minute") %>%
      dplyr::mutate(category = "Median Cadence Short") %>%
      dplyr::group_by(uid, .data$category) %>%
      dplyr::summarise(duration = min(.data$median_cadence))
    duration <- dplyr::inner_join(below_1_minute,stepping, by = "uid")
    duration$percent <- round((duration$short_duration / duration$duration * 100),0)
    duration <- duration[,c(1,3,5)]
    colnames(duration)[3] <- "duration"
    duration$category <- "Stepping Under 1 Min"
    if(is.null(median_rise_time)){
      sort_data <- dplyr::bind_rows(posture, stepping, sedentary_bouts, tib_breaks, travel_summary, mvpa_summary,
                             peak_2_min, peak_6_min, peak_12_min, peak_30_s,
                             median_steps, duration, median_cadence_max, median_cadence_min)
    }else{
      sort_data <- dplyr::bind_rows(posture, stepping, sedentary_bouts, tib_breaks, travel_summary, mvpa_summary,
                             peak_2_min, peak_6_min, peak_12_min, peak_30_s,
                             median_steps, duration, median_rise_time,
                             median_cadence_max, median_cadence_min)
    }

    individual_summary <- generate.sort.order(sort_data, sort_order)

    walk_test_12_min_data <- walk_test_12_min_data %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps))
    walk_test_6_min_data <- walk_test_6_min_data %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps))
    walk_test_2_min_data <- walk_test_2_min_data %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(steps == max(.data$steps))
    walk_test_30_s_data <- walk_test_30_s_data %>%
      dplyr::group_by(uid) %>%
      dplyr::filter(.data$steps == max(.data$steps))

    generate.all.outcomes.report(valid_days, chart_data, daily_stepping_data, travel_data, mvpa_data, median_rise_time_data, median_first_step_data,
                                 median_cadence_data, walk_test_30_s_data, walk_test_2_min_data, walk_test_6_min_data, walk_test_12_min_data,
                                 bouts_breaks_data, chart_summary, individual_summary, median_rise_time, sort_order,
                                 chart_title, output_folder, standard_scales)
  }
