calculate.peak.stepping <-
  function(input_folder, window_size = 360, max_bout_size = 86400, wear_time_minimum = 72000, daily_summary = FALSE){
    #' @description Processes a folder of extended events files to calculate the maximum
    #'     number of steps taken within a given time duration
    #' @param input_folder The filepath for the folder where the events files to be processed are saved
    #'
    #' @export
    #'
    # Assumes that the name of the events files contain the text Events in the file name
    file_names <- list.files(input_folder, pattern="*.csv",recursive = FALSE)
    file_names <- file_names[grep("EventsEx",file_names)]
    # Create an empty data frame to load the data into
    stepping.summary <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(stepping.summary)<-c("date","Time","steps","duration","cadence","uid")

    for (i in file_names){
      # Change substr to get the prefix of the filename that matches the File code field (column 2) in the daily validation file
      # break_point <- regexpr(" |-",i)[1]
      curr_uid <- parse.file.name(i)

      # Load the file
      events_file <- read.csv(paste(input_folder,i,sep=""), row.names = NULL, sep=";", skip = 1, stringsAsFactors = FALSE)
      colnames(events_file) <- c(tail(colnames(events_file),-1),"")
      events_file <- events_file[,-ncol(events_file)]
      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400, origin = "1899-12-30", tz = "UTC")

      if(nrow(events_file)>0){
        events_file <- activpal.remove.longer.bouts(events_file,max_bout_size)
        file.stepping.summary <- file.stepping.summary %>% filter(dplyr::steps > 0)
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
