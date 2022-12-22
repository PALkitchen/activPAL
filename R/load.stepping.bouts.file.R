load.stepping.bouts.file <-
  function(folder_name, file_name, daily_wear_time = 20){
    file_location <- paste(folder_name, file_name, sep = "/")
    if(is.na(file.info(file_location)$size)){
      stop("There is no file at the location you have specified. Is the spelling of the folder path and filename correct? The folder path should end with a forward slash (/).")
    }
    file_data <- open.stepping.bouts.file(file_location)
    if(!valid.stepping.bouts.file(file_data)){
      return(NULL)
    }
    file_data$Cadence <- file_data$`Num Steps` / (file_data$`Duration (s)` / 60)
    file_data <- stepping.bouts.rename.rows(file_data)
    prev_rows <- 0
    file_rows <- nrow(file_data)
    while(prev_rows != file_rows) {
      file_data <- stepping.bouts.split.multiday.events(file_data)
      prev_rows <- file_rows
      file_rows <- nrow(file_data)
    }
    file_data <- filter.valid.days(file_data, daily_wear_time * 3600)
    return(file_data)
  }

open.stepping.bouts.file <-
  function(file_path){
    #' @import readr
    stepping_bouts <- readr::read_delim(file_path, skip = 1, delim = ";")
    stepping_bouts$Time <- as.POSIXct(stepping_bouts$Time * 86400,
                                       origin = "1899-12-30",
                                       tz = "UTC")
    return(stepping_bouts)
  }

stepping.bouts.rename.rows <-
  function(file_data){
    # remove the Time(approx) column
    file_data <- file_data[,-c(2)]
    colnames(file_data) <- c("time","samples","activity","interval","upright_bout_number",
                             "upright_bout_duration","steps","cadence")
    return(file_data)
  }

stepping.bouts.split.multiday.events <-
  function(file_data){
    #' @import dplyr
    sample_frequency <- file_data[2,]$samples / file_data[1,]$interval
    file_data$day_start <- as.numeric(file_data$time) %% 86400
    file_data$day_end <- file_data$day_start + file_data$interval

    if(length(which((file_data$day_end > 86400.0005))) == 0){
      return(file_data[,c(1:8)])
    }

    pre_break <- file_data[which(file_data$day_end > 86400.0005),]
    post_break <- pre_break

    pre_break$part_time <- 86400 - pre_break$day_start
    post_break$part_time <- post_break$day_end - 86400

    pre_break$proportion <- pre_break$part_time / (pre_break$day_end - pre_break$day_start)
    post_break$proportion <- post_break$part_time / (post_break$day_end - post_break$day_start)

    post_break$samples <- post_break$samples + pre_break$part_time * sample_frequency
    post_break$time <- post_break$time + (86400 - as.numeric(post_break$time) %% 86400)

    period_break <- bind_rows(pre_break, post_break)
    period_break$interval <- period_break$part_time
    period_break$steps <- round(period_break$steps * period_break$proportion)
    period_break$cadence <- round(period_break$steps / (period_break$interval / 60))

    file_data <- file_data[-which(file_data$day_end > 86400.005),c(1:8)]
    period_break <- period_break[,c(1:8)]
    file_data <- dplyr::bind_rows(file_data, period_break) %>% dplyr::arrange(.data$time)

    return(file_data)
  }
