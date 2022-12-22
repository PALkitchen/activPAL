activpal.extended.events.file.process<-
  function(data, wear_time_minimum = 72000, pal_batch_format = FALSE){
    # takes in an unprocessed activpal file, formatting and processing the file to allow further analysis
    # data = an unprocessed activpal event file
    # wear.time.minimum = minimum wear time required for a day to be considered valid
    process_data <- data

    process_data <- extended.events.file.process.rename.row(process_data)
    process_data <- extended.events.file.process.split.day(process_data)
    process_data <- extended.events.file.process.exclude.days(process_data,(86400 - wear_time_minimum), pal_batch_format)
    if(nrow(process_data) != 0){
      process_data <- extended.events.file.process.merge.stepping(process_data)
    }
    return(process_data)
  }

extended.events.file.process.rename.row<-
  function(data){
    # Renames the initial row names of an imported activpal event file to facilitate easier processing
    # data = an unprocessed activpal event file
    data <- data[,c(1,3:8)]
    colnames(data)<-c("time","samples","activity","interval","waking_day","cumulative_steps","MET_h")

    return(data)
  }

extended.events.file.process.split.day<-
  function(data,column.split=NULL){
    process_data <- data

    prev_size <- nrow(process_data)
    process_data <- extended.events.file.process.split.day.run(process_data,column.split)
    curr_size <- nrow(process_data)

    while (prev_size != curr_size){
      # Continues to call stepping.split.day.run until no more rows are added
      # (all multi-day spanning events have been successfully split)
      process_data <- extended.events.file.process.split.day.run(process_data)
      prev_size <- curr_size
      curr_size <- nrow(process_data)
    }
    return(process_data)
  }

extended.events.file.process.split.day.run<-
  function(data,col.split=NULL){
    # Splits any entries that cross two or more days
    # data - an activpal data file.  The event datetime must be in column 1 and the duration of the event should be in column 2
    # col.split - a vector containing the column number of additional rows that should be split based on the duration

    transform_data <- data
    transform_data$interval_start <- round(as.numeric(transform_data$time) %% 86400,1)
    transform_data$intervel_end <- transform_data$interval_start + transform_data$interval

    spanning_events <- which(transform_data$intervel_end > 86400)

    for (i in spanning_events){
      if(transform_data[i,]$activity %in% c(2,2.1) ){
        # spanning event is a step.  Clip duration of step to end at midnight and add remaining duration to next event
        right <- round(transform_data[i,]$intervel_end - 86400,1)
        left <- round(transform_data[i,]$interval - right,1)
        transform_data[i,]$interval <- left
        transform_data[i + 1,]$time <- as.POSIXct(as.Date(transform_data[i + 1,]$time),tz="UTC")
        transform_data[i + 1,]$interval <- transform_data[i + 1,]$interval + right
      }else{
        # split event into two separate events
        transform_data[(nrow(transform_data)+1),] <- transform_data[i,]
        transform_data[nrow(transform_data),]$time <- as.POSIXct(as.Date(transform_data[nrow(transform_data),]$time) + 1,tz="UTC")

        right <- round(transform_data[i,]$intervel_end - 86400,1)
        left <- round(transform_data[i,]$interval - right,1)
        transform_data[i,]$interval <- left
        transform_data[nrow(transform_data),]$interval <- right
        transform_data[nrow(transform_data),]$samples <- transform_data[nrow(transform_data),]$samples + (left * 10)

        transform_data[i,]$MET_h <- transform_data[i,]$MET_h * (left / (left + right))
        transform_data[nrow(transform_data),]$MET_h <- transform_data[nrow(transform_data),]$MET_h * (right / (left + right))

      }
    }
    transform_data <- transform_data[order(transform_data$time),]
    return(transform_data[,c(1:(ncol(transform_data)-2))])
  }

extended.events.file.process.exclude.days<-
  function(data, exclude_time=14400, pal_batch_format = FALSE){
    # Removes days where the total time for non-valid events (either no information available or activity = 4)
    # data = the process activpal file
    # exclude.time = Threshold time for excluding days based on non-activity
    exclude_data <- data
    # Create a temporary date column to allow processing
    exclude_data$date <- as.Date(exclude_data$time)
    # Calculate the minimum activity time necessary for a day to be considered valid
    min_activity_time <- 86400 - exclude_time
    # Calculate the total wear time for each day
    daily_wear_times <- data.frame(tapply(exclude_data$interval,exclude_data$date,sum))
    colnames(daily_wear_times) <- c("duration")
    daily_wear_times$date <- as.Date(rownames(daily_wear_times),format = "%Y-%m-%d")
    daily_wear_times <- daily_wear_times[which(daily_wear_times$duration > 86340),]
    # Create a subset with only valid activity data
    exclude_data <- exclude_data[which(exclude_data$activity!=4),]
    exclude_data <- exclude_data[which(exclude_data$activity!=-1),]
    # Calculate the total activity time for each day
    daily_activity_times <- data.frame(tapply(exclude_data$interval,exclude_data$date,sum))
    colnames(daily_activity_times)<-c("duration")
    daily_activity_times$date <- as.Date(rownames(daily_activity_times),format = "%Y-%m-%d")
    # Select only those days with the pre-requisite level of activity and 24 hours of total wear
    daily_activity_times <- daily_activity_times[which(daily_activity_times$duration >= min_activity_time),]
    data <- data[which(as.Date(data$time) %in% daily_activity_times$date),]
    if(pal_batch_format){
      data <- data[which(as.Date(data$time) %in% daily_wear_times$date),]
    }
    return(data)
  }

extended.events.file.process.merge.stepping<-
  function(data){
    # Merges adjacent stepping events in an activpal event file that has been processed by activpal.file.process.rename.row
    # Adds an additional column called steps which records the total number of steps in each stepping bout
    # data - an activpal event file with standardised column names

    # Amend the number of steps to contain the correct number of steps
    # (One step in cumulative steps is equivalent to two actual steps)
    data$cumulative_steps <- data$cumulative_steps*2
    data$steps<-0

    # create offset lists of activity codes to allow adjacent activities to be measured
    one <- c(-1,data$activity)
    two <- c(data$activity,-1)

    one_date <- c(as.Date(data[1,]$time),as.Date(data$time))
    two_date <- c(as.Date(data$time),as.Date(data[nrow(data),]$time))

    # Calculate rows where stepping bouts commence
    stepping_bout_start <- which((one!=2 & two==2) | (one == 2 & two == 2 & one_date != two_date))
    # Calculate rows where stepping bout ends
    stepping_bout_end <- which(one==2 & two!=2 |(one == 2 & two == 2 & one_date != two_date))-1

    stepping_bouts <- length(stepping_bout_start)

    # Build rows for each of the each of the combined stepping bouts
    stepping_bout_interval <- mapply(function(interval,x,y) sum(interval[x:y]), stepping_bout_start, stepping_bout_end, MoreArgs = list(interval = data$interval))
    stepping_bout_steps <- mapply(function(x,y) ((y-x)*2)+2, stepping_bout_start, stepping_bout_end)
    stepping_bout_met_h <- mapply(function(interval,x,y) sum(interval[x:y]), stepping_bout_start, stepping_bout_end, MoreArgs = list(interval = data$met_h))
    data[stepping_bout_start,]$interval <- stepping_bout_interval
    data[stepping_bout_start,]$steps <- stepping_bout_steps
    data[stepping_bout_start,]$MET_h <- stepping_bout_met_h

    if(length(which(data$activity == 2 & data$steps == 0)) > 0){
      data <- data[-which(data$activity == 2 & data$steps == 0),]
    }
    return(data)
  }
