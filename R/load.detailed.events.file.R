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
      dplyr::filter(.data$stepping_bout > 0) %>%
      dplyr::group_by(.data$stepping_bout) %>%
      dplyr::summarise(steps_count = sum(.data$steps), step_duration = sum(.data$interval), cadence = .data$steps_count / (.data$step_duration / 60))

    data <- dplyr::left_join(data, stepping_summary)

    return(data)
  }

