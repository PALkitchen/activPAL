load.events.extended.file <-
  function(folder_name, file_name, daily_wear_time = 20){
    file_location <- paste(folder_name, file_name, sep = "")
    if(is.na(file.info(file_location)$size)){
      stop("There is no file at the location you have specified. Is the spelling of the folder path and filename correct? The folder path should end with a forward slash (/).")
    }
    file_data <- open.events.extended.file(file_location)
    if(!valid.extended.events.file(file_data)){
      return(NULL)
    }
    file_data <- events.extended.rename.rows(file_data)
    file_data <- events.extended.merge.adjacent.events(file_data, 2)
    file_data <- events.extended.merge.adjacent.events(file_data, 2.1)
    file_data <- events.extended.tag.upright.containers(file_data)
    prev_rows <- 0
    file_rows <- nrow(file_data)
    while(prev_rows != file_rows) {
      file_data <- events.extended.split.multiday.events(file_data)
      prev_rows <- file_rows
      file_rows <- nrow(file_data)
    }
    file_data <- filter.valid.days(file_data, daily_wear_time * 3600)
    return(file_data[,-c(ncol(file_data))])
  }

open.events.extended.file <-
  function(file_path){
    #' @import readr

    extended_events <- read.csv(file_path, nrows=2, header = FALSE)
    if(extended_events[2,1] == "**header**"){
      extended_events <- read.csv(file_path, header = FALSE)
      data_start <- grep("**data**",extended_events$V1,fixed=TRUE)
      if(length(data_start) == 0){
        return(NULL)
      }
      extended_events <- read.delim(file_path, skip = (data_start[1] + 1), row.names = NULL, sep = ";")
    }else {
      extended_events <- read.delim(file_path, skip = 1, row.names = NULL, sep = ";")
    }

    events_colnames <- colnames(extended_events)[2:ncol(extended_events)]
    extended_events <- extended_events[,c(1:(ncol(extended_events) - 1))]
    colnames(extended_events) <- events_colnames

    extended_events$Time.Right.Lying..s. <- substr(extended_events$Time.Right.Lying..s.,
                                                     1, nchar(extended_events$Time.Right.Lying..s.) - 13)
    extended_events$Time.Right.Lying..s. <- as.numeric(extended_events$Time.Right.Lying..s.)
    extended_events$Time <- as.POSIXct(as.numeric(extended_events$Time) * 86400,
                                       origin = "1899-12-30",
                                       tz = "UTC")
    return(extended_events)
  }

events.extended.rename.rows <-
  function(file_data){
    # remove the Time(approx) column
    file_data <- file_data[,-c(2)]
    colnames(file_data) <- c("time","samples","activity","interval","waking_day",
                             "cumulative_steps","met_h","abs_sum_diff_x",
                             "abs_sum_diff_y","abs_sum_diff_z",
                             "time_upright","time_upside_down","time_back_lying",
                             "time_front_lying","time_left_lying","time_right_lying")
    file_data$cumulative_steps <- file_data$cumulative_steps * 2
    file_data$steps <- 0
    file_data[which(file_data$activity == 2),]$steps <- 2
    file_data[which(file_data$activity == 2.1),]$steps <-
      rep(2,length(which(file_data$activity == 2.1)))
    file_data <- file_data[,c(1:5,17,6:16)]
    return(file_data)
  }

events.extended.merge.adjacent.events <-
  function(file_data, activity_code){
    #' @import dplyr
    if(length(which(file_data$activity == activity_code)) == 0){
      return(file_data)
    }

    process_data <- file_data

    # create offset lists of activity codes to allow adjacent activities to be measured
    one <- c(-1,process_data$activity)
    two <- c(process_data$activity,-1)

    one_date <- c(-1,as.numeric(as.Date(process_data$time)))
    two_date <- c(as.numeric(as.Date(process_data$time)),-1)

    # Calculate rows where stepping bouts commence
    bout_start <- which(one != activity_code & two == activity_code |
                          (one == activity_code & two == activity_code & one_date != two_date))
    # Calculate rows where stepping bout ends
    bout_end <- which((one == activity_code & two != activity_code) |
                        (one == activity_code & two == activity_code & one_date != two_date))-1

    bouts <- rep(0,nrow(process_data))
    for(i in (1:length(bout_start))){
      bouts[bout_start[i]:bout_end[i]] <- i
    }
    process_data$bouts <- bouts
    merged_bouts <- process_data %>%
      dplyr::filter(.data$bouts != 0) %>%
      dplyr::group_by(.data$bouts) %>%
      dplyr::summarise(time = min(.data$time), samples = min(.data$samples), activity = min(.data$activity),
                interval = sum(.data$interval), waking_day = min(.data$waking_day),
                steps = n() * 2, cumulative_steps = max(.data$cumulative_steps),
                met_h = sum(.data$met_h), abs_sum_diff_x = sum(.data$abs_sum_diff_x),
                abs_sum_diff_y = sum(.data$abs_sum_diff_y), abs_sum_diff_z = sum(.data$abs_sum_diff_z),
                time_upright = sum(.data$time_upright), time_upside_down = sum(.data$time_upside_down),
                time_back_lying = sum(.data$time_back_lying), time_front_lying = sum(.data$time_front_lying),
                time_left_lying = sum(.data$time_left_lying), time_right_lying = sum(.data$time_right_lying))

    merged_bouts <- merged_bouts[-c(1)]
    file_data <- file_data %>% dplyr::filter(.data$activity != activity_code)
    file_data <- dplyr::bind_rows(file_data, merged_bouts)
    file_data <- file_data %>% dplyr::arrange(.data$time)
    return(file_data)
  }

events.extended.tag.upright.containers <-
  function(file_data){
    file_data$upright_bout_number <- 0
    pre_transition <- head(file_data$activity, -1)
    post_transition <- tail(file_data$activity, -1)
    sit_to_stand <- which(pre_transition %in% c(0,3.1,3.2,5) & post_transition %in% c(1,2,2.1)) + 1
    file_data[sit_to_stand,]$upright_bout_number <- 1
    file_data$upright_bout_number <- cumsum(file_data$upright_bout_number)
    file_data[which(!file_data$activity %in% c(1,2,2.1)),]$upright_bout_number <- 0
    return(file_data)
  }

events.extended.split.multiday.events <-
  function(file_data){
    #' @import dplyr
    sample_frequency <- file_data[2,]$samples / file_data[1,]$interval
    file_data$day_start <- as.numeric(file_data$time) %% 86400
    file_data$day_end <- file_data$day_start + file_data$interval

    if(length(which((file_data$day_end > 86400.0005))) == 0){
      return(file_data[,c(1:18)])
    }

    pre_break <- file_data[which(file_data$day_end > 86400.0005),]
    post_break <- pre_break

    pre_break$part_time <- 86400 - pre_break$day_start
    post_break$part_time <- post_break$day_end - 86400

    pre_break$proportion <- pre_break$part_time / (pre_break$day_end - pre_break$day_start)
    post_break$proportion <- post_break$part_time / (post_break$day_end - post_break$day_start)

    post_break$samples <- post_break$samples + pre_break$part_time * sample_frequency
    post_break$time <- post_break$time + (86400 - as.numeric(post_break$time) %% 86400)

    pre_break[which(pre_break$steps >0),]$steps <-
      pre_break[which(pre_break$steps >0),]$steps * pre_break[which(pre_break$steps >0),]$proportion
    pre_break[which(pre_break$steps >0),]$steps <-
      pre_break[which(pre_break$steps >0),]$steps - (pre_break[which(pre_break$steps >0),]$steps %% 2) + 2
    post_break[which(post_break$steps >0),]$steps <-
      post_break[which(post_break$steps >0),]$steps * post_break[which(post_break$steps >0),]$proportion
    post_break[which(post_break$steps >0),]$steps <-
      post_break[which(post_break$steps >0),]$steps - (post_break[which(post_break$steps >0),]$steps %% 2)

    period_break <- dplyr::bind_rows(pre_break, post_break)

    period_break$interval <- period_break$part_time
    period_break$met_h <- period_break$met_h * period_break$proportion
    period_break$abs_sum_diff_x <- round(period_break$abs_sum_diff_x * period_break$proportion)
    period_break$abs_sum_diff_y <- round(period_break$abs_sum_diff_y * period_break$proportion)
    period_break$abs_sum_diff_z <- round(period_break$abs_sum_diff_z * period_break$proportion)

    period_break$time_upright = round(period_break$time_upright * period_break$proportion, 1)
    period_break$time_upside_down = round(period_break$time_upside_down * period_break$proportion, 1)
    period_break$time_back_lying = round(period_break$time_back_lying * period_break$proportion, 1)
    period_break$time_front_lying = round(period_break$time_front_lying * period_break$proportion, 1)
    period_break$time_left_lying = round(period_break$time_left_lying * period_break$proportion, 1)
    period_break$time_right_lying = round(period_break$time_right_lying * period_break$proportion, 1)

    file_data <- file_data[-which(file_data$day_end > 86400.005),c(1:18)]
    period_break <- period_break[,c(1:18)]
    file_data <- dplyr::bind_rows(file_data, period_break) %>% dplyr::arrange(.data$time)

    return(file_data)
  }
