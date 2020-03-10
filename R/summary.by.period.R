activpal.summary.by.period <-
  function(location_folder, output_folder, window_duration = 24){
    #' Generate event file summaries from a folder of activPAL events files
    #'
    #' @description activity.summary.by.period processes all the events files (format *Events.csv)
    #'     in a folder and produces a summary table giving a breakdown across consecutive
    #'     periods of a specified duration during the day. The output generated matches the
    #'     output format obtained from using the Daily summaries export in PALanalysis / PALbatch
    #'
    #'     The generated summary activity table for each events file is saved to a csv file,
    #'     while an additional csv file containing the summaries across all the processed
    #'     events files is also saved.
    #' @param location_folder The filepath for the folder where the events files to be processed
    #'     are saved
    #' @param output_folder The filepath for the folder where the csv summary files are saved
    #' @param window_duration The size of each bucket in hours that the data is to be summarised.
    #'     Default - 24 hours.

    #' @importFrom devtools load_all
    #' @import utils
    #' @export
    if(!valid.folder.path(location_folder)){
      stop("The location of the required input file has not been provided / does not exist.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    file_list <- list.files(location_folder, pattern = "[e|E]vents")
    all_summary <- list()
    for(i in (1:length(file_list))){
      single_file_summary <- activpal.summary.by.period.single.file(location_folder, file_list[i],window_duration)
      all_summary[[i]] <- single_file_summary
    }
    all_summary <- bind_rows(all_summary)
    all_summary <- format.summary(all_summary)
    write.csv(all_summary,paste(output_folder,"daily_summary_",window_duration,"hour_window.csv",sep=""),row.names = FALSE)
    return(all_summary)
  }

activpal.summary.by.period.single.file <-
  function(folder, file_name, epoch_size){
    events_file <- pre.process.detailed.events.file(folder, file_name)
    epochs <- calculate.period.boundaries(events_file, epoch_size)
    events_file$end_time <- events_file$time + events_file$interval
    epoch_summary <- list()
    for(i in (1:nrow(epochs))){
      file_summary <- build.file.summary(folder, file_name)
      data_in_single_epoch <- activpal.filter.single.epoch.data(events_file,epochs[i,]$epoch_start,epochs[i,]$epoch_end)
      if(!is.null(data_in_single_epoch)){
        epoch_time_summary <- single.epoch.time.summary(events_file,epochs[i,]$epoch_start,epochs[i,]$epoch_end)
        posture_summary <- single.epoch.posture.summary(data_in_single_epoch)
        transition_energy_summary <- single.epoch.transition.energy.summary(data_in_single_epoch)
        sedentary_summary <- single.epoch.sedentary.summary(data_in_single_epoch)
        stepping_summary <- single.epoch.stepping.summary(data_in_single_epoch)

        single_epoch_summary <- cbind(file_summary,epoch_time_summary,posture_summary,transition_energy_summary,sedentary_summary,stepping_summary)
        epoch_summary[[i]] <- single_epoch_summary
      }
    }
    return(bind_rows(epoch_summary))
  }

build.file.summary <-
  function (folder, file_name){
    serial_number <- get.device.serial(file_name)
    file_code <- ""
    download_identifier <- ""

    file_summary <- data.frame(folder, file_name, serial_number, file_code, download_identifier, stringsAsFactors = FALSE)
  }

format.summary <-
  function(data){
    data[,c(10:19,25,26,28:32,38,39,42,43)] <- data[,c(10:19,25,26,28:32,38,39,42,43)] / 60
    data[,c(10:19,25,26,28:32,38,39,42,43)] <- round(data[,c(10:19,25,26,28:32,38,39,42,43)], 2)

    return(data)
  }

activpal.filter.single.epoch.data <-
  function(events_file, epoch_start, epoch_end) {
    rows_to_include <- which(events_file$time < epoch_end & events_file$end_time >= epoch_start)
    if(length(rows_to_include) == 0){
      return(NULL)
    }
    epoch_data <- events_file[rows_to_include,]
    if(epoch_start > epoch_data[1,]$time){
      epoch_data[1,]$interval <- as.numeric(epoch_data[1,]$end_time) - as.numeric(epoch_start)
    }
    if(epoch_data[nrow(epoch_data),]$end_time > epoch_end){
      epoch_data[nrow(epoch_data),]$interval <- as.numeric(epoch_end) - as.numeric(epoch_data[nrow(epoch_data),]$time)
    }

    return(epoch_data)
  }

single.epoch.time.summary <-
  function(events_file, epoch_start, epoch_end){
    day_of_week <- format(epoch_start,"%a")
    date <- format(epoch_start, "%d-%b-%y")
    period <- paste(format(epoch_start,"%H:%M")," to ",format(epoch_end,"%H:%M"),sep="")

    events_file$date <- as.Date(events_file$time)
    events_file <- events_file[which(events_file$date == as.Date(epoch_start)),]
    events_file <- events_file[which(events_file$activity != 4),]
    valid_wear_time <- sum(events_file$interval)
    valid_day <- valid_wear_time >= 72000

    time_summary <- data.frame(day_of_week,date,period,valid_day, stringsAsFactors = FALSE)
    return(time_summary)
  }

single.epoch.posture.summary <-
  function(epoch_data){
    # Calculates the summary stats for row N to row V
    #' @import dplyr
    posture_summary <- epoch_data %>%
      dplyr::group_by(.data$activity) %>%
      dplyr::summarise(bouts = n(), duration = sum(.data$interval))

    upright_time <- sum(posture_summary[which(posture_summary$activity %in% c(1,2,2.1)),]$duration)
    standing_time <- sum(posture_summary[which(posture_summary$activity == 1),]$duration)
    stepping_time <- sum(posture_summary[which(posture_summary$activity == 2),]$duration)
    cycling_time <- sum(posture_summary[which(posture_summary$activity == 2.1),]$duration)

    sitting_time <- sum(posture_summary[which(posture_summary$activity %in% c(0,5)),]$duration)
    seated_travel_time <- sum(posture_summary[which(posture_summary$activity == 5),]$duration)

    primary_lying_time <- sum(posture_summary[which(posture_summary$activity %in% c(3,3.1)),]$duration)
    secondary_lying_time <- sum(posture_summary[which(posture_summary$activity == 3.2),]$duration)

    non_wear_time <- sum(posture_summary[which(posture_summary$activity == 4),]$duration)

    wear_time <- sum(posture_summary$duration) - non_wear_time

    posture_output <- data.frame(wear_time, upright_time, standing_time, stepping_time, cycling_time,
                                 sitting_time, seated_travel_time, primary_lying_time, secondary_lying_time, non_wear_time)

    return(posture_output)
  }

single.epoch.stepping.summary <-
  function(epoch_data){
    #' @import dplyr
    stepping_summary <- epoch_data %>%
      dplyr::filter(.data$stepping_bout > 0) %>%
      dplyr::group_by(.data$stepping_bout) %>%
      dplyr::summarise(steps_count = max(.data$steps_count),
                step_duration = max(.data$step_duration),
                cadence = max(.data$cadence),
                full_duration = sum(.data$full_duration))
    step_count <- sum(stepping_summary$steps_count)
    stepping_time_under_1_min <- sum(stepping_summary[which(stepping_summary$full_duration <= 60),]$step_duration)
    stepping_time_1_min_to_5_min <- sum(stepping_summary[which(stepping_summary$full_duration > 60 & stepping_summary$full_duration <= 300),]$step_duration)
    stepping_time_5_min_to_10_min <- sum(stepping_summary[which(stepping_summary$full_duration > 300 & stepping_summary$full_duration <= 600),]$step_duration)
    stepping_time_10_min_to_20_min <- sum(stepping_summary[which(stepping_summary$full_duration > 600 & stepping_summary$full_duration <= 1200),]$step_duration)
    stepping_time_20_min_plus <- sum(stepping_summary[which(stepping_summary$full_duration > 1200),]$step_duration)

    steps_bout_under_1_min <- sum(stepping_summary[which(stepping_summary$full_duration <= 60),]$steps_count)
    steps_bout_1_min_to_5_min <- sum(stepping_summary[which(stepping_summary$full_duration > 60 & stepping_summary$full_duration <= 300),]$steps_count)
    steps_bout_5_min_to_10_min <- sum(stepping_summary[which(stepping_summary$full_duration > 300 & stepping_summary$full_duration <= 600),]$steps_count)
    steps_bout_10_min_to_20_min <- sum(stepping_summary[which(stepping_summary$full_duration > 600 & stepping_summary$full_duration <= 1200),]$steps_count)
    steps_bout_20_min_plus <- sum(stepping_summary[which(stepping_summary$full_duration > 1200),]$steps_count)

    stepping_time_cadence_75_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 75 & stepping_summary$steps_count > 10),]$step_duration)
    steps_bout_cadence_75_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 75 & stepping_summary$steps_count > 10),]$steps_count)
    stepping_time_cadence_75_plus_bout_1_min_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 75 & stepping_summary$full_duration >= 60),]$step_duration)
    steps_bout_cadence_75_plus_bout_1_min_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 75 & stepping_summary$full_duration >= 60),]$steps_count)

    stepping_time_cadence_100_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 100 & stepping_summary$steps_count > 10),]$step_duration)
    steps_bout_cadence_100_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 100 & stepping_summary$steps_count > 10),]$steps_count)
    stepping_time_cadence_100_plus_bout_1_min_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 100 & stepping_summary$full_duration >= 60),]$step_duration)
    steps_bout_cadence_100_plus_bout_1_min_plus <- sum(stepping_summary[which(stepping_summary$cadence >= 100 & stepping_summary$full_duration >= 60),]$steps_count)

    stepping_output <- data.frame(step_count,
                                  stepping_time_under_1_min, stepping_time_1_min_to_5_min,
                                  stepping_time_5_min_to_10_min, stepping_time_10_min_to_20_min, stepping_time_20_min_plus,
                                  steps_bout_under_1_min, steps_bout_1_min_to_5_min, steps_bout_5_min_to_10_min,
                                  steps_bout_10_min_to_20_min, steps_bout_20_min_plus,
                                  stepping_time_cadence_75_plus, stepping_time_cadence_75_plus_bout_1_min_plus,
                                  steps_bout_cadence_75_plus, steps_bout_cadence_75_plus_bout_1_min_plus,
                                  stepping_time_cadence_100_plus, stepping_time_cadence_100_plus_bout_1_min_plus,
                                  steps_bout_cadence_100_plus, steps_bout_cadence_100_plus_bout_1_min_plus)

    return(stepping_output)
  }

single.epoch.sedentary.summary <-
  function(epoch_data){
    #' @import dplyr
    sedentary_summary <- epoch_data %>% dplyr::filter(.data$activity == 0)
    sitting_bouts_over_30_min <- length(which(sedentary_summary$full_duration > 1800))
    sitting_bouts_over_60_min <- length(which(sedentary_summary$full_duration > 3600))
    sitting_duration_in_bouts_over_30_min <- sum(sedentary_summary[which(sedentary_summary$full_duration > 1800),]$interval)
    sitting_duration_in_bouts_over_60_min <- sum(sedentary_summary[which(sedentary_summary$full_duration > 3600),]$interval)

    sitting_output <- data.frame(sitting_bouts_over_30_min, sitting_bouts_over_60_min,
                                 sitting_duration_in_bouts_over_30_min, sitting_duration_in_bouts_over_60_min)

    return(sitting_output)
  }

single.epoch.transition.energy.summary <-
  function(epoch_data){
    epoch_data$MET.h.part <- epoch_data$MET.h * (epoch_data$interval / epoch_data$full_duration)
    activity_score <- sum(epoch_data$MET.h.part)

    first <- c(-1,epoch_data$activity)
    second <- c(epoch_data$activity,-1)

    sit_to_stand_transitions <- which(first %in% c(0) & second == 1)
    sit_to_stand_transitions <- length(sit_to_stand_transitions)
    stand_to_sit_transitions <- which(first == 1 & second %in% c(0))
    stand_to_sit_transitions <- length(stand_to_sit_transitions)

    transition_summary <- data.frame(sit_to_stand_transitions,stand_to_sit_transitions,activity_score)

    return(transition_summary)
  }

calculate.period.boundaries <-
  function(events_file, epoch_size){
    epoch_duration <- 86400 / 24 * epoch_size
    epoch_boundaries <- seq(as.POSIXct(as.numeric(events_file[1,]$time)  - as.numeric(events_file[1,]$time) %% 86400,origin = "1970-01-01", tz = "UTC"),
                            as.POSIXct(as.numeric(events_file[nrow(events_file),]$time)  - as.numeric(events_file[nrow(events_file),]$time) %% 86400,origin = "1970-01-01", tz = "UTC"),
                            epoch_duration)
    epoch_start <- epoch_boundaries[1:(length(epoch_boundaries)-1)]
    epoch_end <- epoch_boundaries[2:(length(epoch_boundaries))]
    epoch_list <- data.frame(epoch_start, epoch_end)

    return(epoch_list)
  }
########################

pre.process.detailed.events.file <-
  function(folder, file_name = ""){
    events_file <- load.events.file(folder,file_name)
    events_file <- activpal.detailed.file.process(events_file)
    return(events_file)
  }

activpal.detailed.file.process<-
  function(data){
    # takes in an unprocessed activpal file, formatting and processing the file to allow further analysis
    # data = an unprocessed activpal event file
    # wear.time.minimum = minimum wear time required for a day to be considered valid
    process_data<-data
    if(ncol(process_data)==6){
      process_data$abs.sum <- 0
    }
    process_data$Time <- as.POSIXct(process_data$Time*86400,origin="1899-12-30",tz="UTC")
    process_data <- process_data[,1:7]
    process_data <- process_data[which(process_data$Interval..s.>0),]

    process_data <- activpal.file.process.rename.row(process_data)

    process_data$steps <- 0
    process_data[which(process_data$activity %in% c(2,2.1)),]$steps <- rep(2,length(which(process_data$activity %in% c(2,2.1))))
    process_data$full_duration <- process_data$interval

    process_data <- activpal.file.process.split.day(process_data,c(7,8))

    process_data <- activpal.add.stepping.data(process_data)
    return(process_data)
  }

activpal.add.stepping.data <-
  function(data){
    #' @import dplyr
    first <- c(-1,data$activity)
    second <- c(data$activity,-1)

    start_pos <- which(first != 2 & second == 2)
    end_pos <- which(second != 2 & first == 2)-1

    stepping_bout <- rep(0,nrow(data))
    for (i in (1:length(start_pos))){
      stepping_bout[start_pos[i]:end_pos[i]] <- i
    }
    data$stepping_bout <- stepping_bout

    stepping_summary <- data %>%
      dplyr::filter(.data@stepping_bout > 0) %>%
      dplyr::group_by(.data@stepping_bout) %>%
      dplyr::summarise(steps_count = sum(.data@steps), step_duration = sum(.data@interval), cadence = .data@steps_count / (.data@step_duration / 60))

    data <- dplyr::left_join(data, stepping_summary)

    return(data)
  }

get.device.serial <-
  function(data){
    # extracts the serial number from a file (assumes that the serial has the format APXXXXXX)
    # returns a blank string if no device serial is found within data
    serial_start <- regexpr("AP[[:digit:]]{6}",data)
    if (serial_start > -1){
      return(substr(data,serial_start,serial_start+7))
    }else{
      return ("")
    }

  }
