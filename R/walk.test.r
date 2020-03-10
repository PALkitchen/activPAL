activpal.stepping.test.folder <-
  function(folder_location, validation_data, window_size = 360, max_bout_size = 86400, wear_time_minimum = 72000, daily_summary = FALSE){
    # Assumes that the name of the events files contain the text Events in the file name
    #' @importFrom Rcpp evalCpp
    #' @useDynLib activPAL
    #' @exportPattern ^[[:alpha:]]+
    file_names <- list.files(folder_location, pattern="*.csv",recursive = FALSE)
    file_names <- file_names[grep("EventsEx",file_names)]
    # Create an empty data frame to load the data into
    stepping.summary <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(stepping.summary) <- c("date","Time","steps","duration","cadence","uid")

    for (i in file_names){
      # Change substr to get the prefix of the filename that matches the File code field (column 2) in the daily validation file
      # break_point <- regexpr(" |-",i)[1]
      curr_uid <- parse.file.name(i)

      # Load the file
      events_file <- read.csv(paste(folder_location,i,sep=""), row.names = NULL, sep=";", skip = 1, stringsAsFactors = FALSE)
      colnames(events_file) <- c(tail(colnames(events_file),-1),"")
      events_file <- events_file[,-ncol(events_file)]
      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400, origin = "1899-12-30", tz = "UTC")

      valid_days <- validation_data %>% dplyr::filter(uid == .data$curr_uid) %>% filter(.data$valid == "valid")
      valid_days <- valid_days$Date
      events_file <- events_file %>% dplyr::filter(as.Date(.data$Time) %in% valid_days)
      if(nrow(events_file)>0){
        events_file <- activpal.remove.longer.bouts(events_file,max_bout_size)
        file.stepping.summary <- activpal.stepping.test.file(events_file,validation.data,window_size)
        file.stepping.summary <- file.stepping.summary %>% filter(.data$steps > 0)
        file.stepping.summary <- file.stepping.summary[which(file.stepping.summary$date %in% c(valid_days)),]
        if(nrow(file.stepping.summary)>0){
          file.stepping.summary$uid <- curr_uid
          if(daily_summary){
            file.stepping.summary <- file.stepping.summary %>%
              dplyr::group_by(.data$date) %>%
              dplyr::filter(.data$steps == max(.data$steps)) %>%
              dplyr::filter(.data$duration == min(.data$duration)) %>%
              dplyr::filter(.data$Time == min(.data$Time))
          }else{
            file.stepping.summary <- file.stepping.summary %>%
              dplyr::filter(.data$steps == max(.data$steps)) %>%
              dplyr::filter(.data$duration == min(.data$duration)) %>%
              dplyr::filter(.data$Time == min(.data$Time))
          }
          stepping.summary <- dplyr::bind_rows(stepping.summary,file.stepping.summary)
        }
      }
    }
    stepping.summary <- stepping.summary[,c(6,1:5)]
    return(stepping.summary)
  }

activpal.stepping.process.file <-
  function(folder_location, file_name, validation_data, window_size = 360, max_bout_size = 86400, wear_time_minimum = 72000, daily_summary = FALSE){
    # Change substr to get the prefix of the filename that matches the File code field (column 2) in the daily validation file
    # break_point <- regexpr(" |-",i)[1]
    curr_uid <- parse.file.name(file_name)

    # Load the file
    events_file <- read.csv(paste(folder_location,file_name,sep=""), row.names = NULL, sep=";", skip = 1, stringsAsFactors = FALSE)
    colnames(events_file) <- c(tail(colnames(events_file),-1),"")
    events_file <- events_file[,-ncol(events_file)]
    events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400, origin = "1899-12-30", tz = "UTC")

    valid_days <- validation_data %>% dplyr::filter(.data$uid == curr_uid) %>% dplyr::filter(.data$valid == "valid")
    valid_days <- valid_days$Date
    events_file <- events_file %>% dplyr::filter(as.Date(.data$Time) %in% valid_days)
    if(nrow(events_file)>0){
      events_file <- activpal.remove.longer.bouts(events_file,max_bout_size)
      file.stepping.summary <- activpal.stepping.test.file(events_file,validation_data,window_size)
      file.stepping.summary <- file.stepping.summary %>% filter(.data$steps > 0)
      file.stepping.summary <- file.stepping.summary[which(file.stepping.summary$date %in% c(valid_days)),]
      if(nrow(file.stepping.summary)>0){
        file.stepping.summary$uid <- curr_uid
        if(daily_summary){
          file.stepping.summary <- file.stepping.summary %>%
            dplyr::group_by(.data$date) %>%
            dplyr::filter(.data$steps == max(.data$steps)) %>%
            dplyr::filter(.data$duration == min(.data$duration)) %>%
            dplyr::filter(.data$Time == min(.data$Time))
        }else{
          file.stepping.summary <- file.stepping.summary %>%
            dplyr::filter(.data$steps == max(.data$steps)) %>%
            dplyr::filter(.data$duration == min(.data$duration)) %>%
            dplyr::filter(.data$Time == min(.data$Time))
        }
      }
    }
    file.stepping.summary <- file.stepping.summary[,c(6,1:5)]
    return(file.stepping.summary)
  }

activpal.remove.longer.bouts <-
  function(file_data, upper_bout_length){
    rownames(file_data) <- 1:nrow(file_data)
    one <- c(-1,file_data$Event.Type)
    two <- c(file_data$Event.Type,-1)

    stepping_start <- which(one!=2 & two==2)
    stepping_end <- which(one==2 & two!=2)-1

    file_data$group <- 0
    group_id <- 1:length(stepping_start)
    group_val <- rep(0,nrow(file_data))

    for (i in (1:length(stepping_start))){
      # Tag each bout of stepping
      group_val[(stepping_start[i]:stepping_end[i])] <- i
    }

    file_data$group <- group_val
    bouts_to_exclude <- file_data %>%
      filter(.data$group > 0) %>%
      group_by(.data$group) %>%
      summarise(time = sum(.data$Duration..s.)) %>%
      filter(.data$time > upper_bout_length)

    if(nrow(bouts_to_exclude) > 0){
      file_data[which(file_data$group %in% bouts_to_exclude$group),]$Event.Type <- 1
    }

    file_data <- file_data[,-ncol(file_data)]
    return(file_data)
  }

activpal.stepping.test.file<-
  function(file.data,validation.data,window.size){
    # extract the activpal code from the filename.  Assumes the filename is stored in the format AP#######

    # process the event files using the list of valid dates
    # Split events that cross two days
    file.data <- file.data[order(file.data$Time),]
    rownames(file.data) <- 1:nrow(file.data)
    file.data$date<-as.Date(file.data$Time)
    file.data$diff <- (difftime(file.data$Time,file.data$date,tz="UTC",units="secs") + file.data$Duration..s.) - 86400
    cross.days <- which(file.data$diff > 0)
    file.data <- rbind(file.data,file.data[cross.days,])
    file.data[cross.days,]$Duration..s. <- round(file.data[cross.days,]$Duration..s. - file.data[cross.days,]$diff,1)
    file.data[(nrow(file.data)-length(cross.days)+1):nrow(file.data),]$Duration..s. <- round(file.data[(nrow(file.data)-length(cross.days)+1):nrow(file.data),]$diff,1)
    file.data[(nrow(file.data)-length(cross.days)+1):nrow(file.data),]$Time <- file.data[(nrow(file.data)-length(cross.days)+1):nrow(file.data),]$date + 1
    file.data <- file.data[order(file.data$Time),]
    rownames(file.data) <- 1:nrow(file.data)
    file.data$date<-as.Date(file.data$Time)

    # Format the events file
    file.data$seq <- (1:nrow(file.data))
    colnames(file.data)[5:4] <- c("interval","activity")
    file.data <- file.data[,c(18,20,1,4,5)]

    ans <- bout_end(file.data$seq,file.data$activity,file.data$interval,nrow(file.data),window.size)

    stepping.summary <- file.data[,c(1,3)]
    stepping.summary$steps <- as.integer(ans[(nrow(file.data)+1):(nrow(file.data)*2)])
    stepping.summary$duration <- ans[((2*nrow(file.data))+1):(nrow(file.data)*3)]
    stepping.summary$cadence <- stepping.summary$steps / (stepping.summary$duration / 60)
    return(stepping.summary)

  }

activpal.stepping.test.day<-
  function(file.data,window.size){
    file.data$seconds <- round(as.numeric(difftime(file.data$Time,file.data$date,units="secs")),1)
    stepping <- which(file.data$activity == 2)
    stepping.time <- file.data[stepping,]$seconds
    file.seconds <- file.data$seconds

    ans <- bout_end(file.data$seq,file.data$activity,file.data$interval,nrow(file.data),window.size)

    stepping.summary <- file.data[,c(1,3)]
    stepping.summary$steps <- as.integer(ans[(nrow(file.data)+1):(nrow(file.data)*2)])
    stepping.summary$duration <- ans[((2*nrow(file.data))+1):(nrow(file.data)*3)]
    stepping.summary$cadence <- stepping.summary$steps / (stepping.summary$duration / 60)
    return(stepping.summary)
  }

