activity.summary.by.custom.period <-
  function(location_file,output_folder){
    #' Summarise activity data from a folder of activPAL events files
    #' in user defined periods
    #' @description activity.summary.custom.period reads in a csv file containing a list
    #'     of events files and csv files with time periods of interest (the first two
    #'     columns should have the start date and end date in the format dd-mm-yy HH:MM).
    #'     A summary table is saved to a csv file for each events file.  A further csv
    #'     file giving a summary for all the events file is also generated.  The generated
    #'     summary activity table for each events file is saved to a csv file, while an
    #'     additional csv file containing the summaries across all the processed events
    #'     files is also saved.
    #' @param location_file The filepath to a csv file containing the path for a group
    #'     of csv files to be processed
    #' @param output_folder The filepath for the folder where the csv summary files are saved

    #' @importFrom devtools load_all
    #' @import utils
    #' @export

    if(!valid.file.path(location_file)){
      stop("The location of the required input file has not been provided / does not exist.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    file_list <- read.csv(location_file)
    all_summary <- data.frame(matrix(ncol = 6, nrow = 0))
    for(i in (1:nrow(file_list))){
      file_summary <- data.frame(matrix(ncol = 5, nrow = 0))

      file_name <- substr(as.character(file_list[i,1]),1,gregexpr("Event",as.character(file_list[i,1]))[[1]][1]-1)
      file_name <- substr(as.character(file_name),
                                 gregexpr("/",file_name)[[1]][length(gregexpr("/",file_name)[[1]])]+1,
                                 nchar(as.character(file_name)))

      summary_periods <- read.csv(as.character(file_list[i,2]))
      colnames(summary_periods)[1:2] <- c("start_time","end_time")

      if(length(which(summary_periods$end_time == "")) > 0){
        summary_periods <- summary_periods[-which(summary_periods$end_time == ""),]
      }
      summary_periods$start_time <-
        as.numeric(as.POSIXct(summary_periods$start_time,
                              format = "%d/%m/%Y %H:%M", tz="UTC"))
      summary_periods$end_time <-
        as.numeric(as.POSIXct(summary_periods$end_time,
                              format = "%d/%m/%Y %H:%M", tz="UTC"))
      events_file <- pre.process.events.file(as.character(file_list[i,1]))
      events_file$event_start <- as.numeric(events_file$time)
      events_file$event_end <- events_file$event_start + events_file$interval
      step_start <- load.step.times(as.character(file_list[i,1]))
      step_start <- as.numeric(step_start)

      for(j in (1:nrow(summary_periods))){
        events_file$start_count <- pmax(events_file$event_start,rep(summary_periods[j,]$start_time,nrow(events_file)))
        events_file$end_count <- pmin(events_file$event_end,rep(summary_periods[j,]$end_time,nrow(events_file)))

        events_file$duration <- round(events_file$end_count - events_file$start_count,1)

        summary_data <- events_file %>%
          dplyr::filter(.data$duration > 0) %>%
          dplyr::group_by(.data$activity) %>%
          dplyr::summarise(events = dplyr::n(), duration = sum(.data$duration))
        summary_data$steps <- length(which(step_start >= summary_periods[j,]$start_time &
                                             step_start <= summary_periods[j,]$end_time))*2
        summary_data$periods <- paste(as.POSIXct(summary_periods[j,]$start_time, origin = "1970-01-01",tz="UTC"),
                                      as.POSIXct(summary_periods[j,]$end_time, origin = "1970-01-01",tz="UTC"),
                                      sep=" to ")
        file_summary <- rbind(file_summary,summary_data)
      }
      # Write out the file summary
      summary_occurrences <- tidyr::spread(file_summary[,c(1:2,5,4)],1,2,fill=0)
      colnames(summary_occurrences)[3:ncol(summary_occurrences)] <- paste(colnames(summary_occurrences)[3:ncol(summary_occurrences)],"events",sep=" - ")
      summary_duration <- tidyr::spread(file_summary[,c(1,3,5)],1,2,fill=0)
      colnames(summary_duration)[2:ncol(summary_duration)] <- paste(colnames(summary_duration)[2:ncol(summary_duration)],"duration",sep=" - ")

      summary_data <- merge(summary_occurrences,summary_duration)

      summary_data <- activity.summary.column.headers(summary_data)
      print(paste("Saving summary for events file ",file_name,sep=""))
      write.csv(summary_data,file = paste(output_folder,file_name,"_custom_summary.csv",sep=""),
                row.names = FALSE)

      file_summary$id <- file_name
      all_summary <- rbind(all_summary,file_summary)
    }
    summary_occurrences <- tidyr::spread(all_summary[,c(1:2,6,5,4)],1,2,fill=0)
    colnames(summary_occurrences)[4:ncol(summary_occurrences)] <- paste(colnames(summary_occurrences)[4:ncol(summary_occurrences)],"events",sep=" - ")
    summary_duration <- tidyr::spread(all_summary[,c(1,3,6,5)],1,2,fill=0)
    colnames(summary_duration)[3:ncol(summary_duration)] <- paste(colnames(summary_duration)[3:ncol(summary_duration)],"duration",sep=" - ")

    summary_data <- merge(summary_occurrences,summary_duration)

    summary_data <- activity.summary.column.headers(summary_data)
    write.csv(summary_data,paste(output_folder,"custom_activity_summary_all_files.csv",sep=""),row.names = FALSE)
    return(summary_data)
  }

activity.summary.file.single.custom.period <-
  function(events_file,start_period,end_period){
    #' @import magrittr
    events_file$start_count <- start_period
    events_file$end_count <- end_period

    events_file$start_count <- pmax(events_file$start_count,events_file$event_start)
    events_file$end_count <- pmin(events_file$end_count,events_file$event_end)

    events_file$duration <- round(events_file$end_count - events_file$start_count,1)

    summary_data <- events_file %>%
      dplyr::filter(.data$duration > 0) %>%
      dplyr::group_by(.data$date,.data$activity) %>%
      dplyr::summarise(events = dplyr::n(), duration = sum(.data$duration), steps = sum(.data$steps))

    summary_occurrences <- tidyr::spread(summary_data[,c(1:3)],2,3,fill=0)
    colnames(summary_occurrences)[2:ncol(summary_occurrences)] <- paste(colnames(summary_occurrences)[2:ncol(summary_occurrences)],"events",sep=" - ")
    summary_duration <- tidyr::spread(summary_data[,c(1,2,4)],2,3,fill=0)
    colnames(summary_duration)[2:ncol(summary_duration)] <- paste(colnames(summary_duration)[2:ncol(summary_duration)],"duration",sep=" - ")
    summary_steps <- summary_data[which(summary_data$steps > 0),c(1,5)]

    summary_data <- merge(summary_occurrences,summary_duration)
    summary_data <- merge(summary_data,summary_steps,all.x=TRUE)

    return(summary_data)
  }

###########################################################

activity.summary.by.window.duration <-
  function(input_folder,output_folder,window_duration = 2){
    #' Summarise activity data from a folder of activPAL events files
    #' into equal sized periods
    #' @description activity.summary.window processes all the events files (format *Events.csv)
    #'     in a folder and produces a summary table giving a breakdown of the number and
    #'     duration of each event type across consecutive periods of a specified duration
    #'     during the day. If the window_size is not a factor of 24, the final period will
    #'     be shorter than the other reporting periods, generating a warning to advise of
    #'     this difference in reporting period duration.  The generated summary activity
    #'     table for each events file is saved to a csv file, while an additional csv file
    #'     containing the summaries across all the processed events files is also saved.
    #' @param input_folder The filepath for the folder where the events files (format *Events.csv)
    #'     to be processed are saved
    #' @param output_folder The filepath for the folder where the csv summary files are to be saved
    #' @param window_duration The size of each bucket in hours that the data is to be summarised.
    #'     Default - 2 hours.

    #' @import utils
    #' @export
    #' @examples input_folder <- paste(system.file("extdata", "", package = "activPAL"),"/",sep="")
    #' output_folder <- paste(tempdir(),"/",sep="")
    #'
    #' activPAL::activity.summary.by.window.duration(input_folder,output_folder,2)
    #'
    #' # Omitting a value for window_duration sets the default window size to 2 hours
    #' activPAL::activity.summary.by.window.duration(input_folder,output_folder)
    #'
    #' # You can set the window_duration to a time period that is not evenly divisible
    #' # by 24 hours.  In this case the last reporting period for each day will be
    #' # shorter than the specified window_duration.
    #' activPAL::activity.summary.by.window.duration(input_folder,output_folder,3.5)

    if(!valid.folder.path(input_folder)){
      stop("A valid folder to search for events files has not been provided.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    if ((24 %% window_duration) != 0){
      warning("Window size is not a factor of 24.  The final bin will be smaller than other bins")
    }
    start_seconds <- seq(0,86399,window_duration * 3600)
    end_seconds <- c(tail(start_seconds,-1),86400)
    labels <- paste(format(as.POSIXct(start_seconds,origin="1970-01-01",tz="UTC"),"%H%M"),"to",
                    format(as.POSIXct(end_seconds,origin="1970-01-01",tz="UTC"),"%H%M"))
    time_periods <- data.frame(labels,start_seconds,end_seconds)
    time_periods$labels <- factor(time_periods$labels, levels = labels)

    all_data <- activity.summary.folder(input_folder,output_folder,time_periods)
    write.csv(all_data,paste(output_folder,"activity_summary_all_files.csv",sep=""),row.names = FALSE)
    return(all_data)
  }

activity.summary.folder <-
  function(input_folder,output_folder,time_periods){
    file_list <- list.files(input_folder,pattern = "Events*.csv")
    all_data <- list(length=length(file_list))
    for (i in (1:length(file_list))){
      file_name <- file_list[i]
      activity_summary <- activity.summary.file(file_path = paste(input_folder,file_name,sep=""),time_periods)
      activity_summary <- activity.summary.column.headers(activity_summary)
      if(is.null(activity_summary)){
        print(paste("Processed Events File: ",file_name,". No output generated.",sep=""))
      }else{
        file_name <- substr(file_name,1,gregexpr("Event",file_name)[[1]][1]-1)
        write.csv(activity_summary,file = paste(output_folder,file_name,"_",round(24 / nrow(time_periods),2),
                                                "_hours_summary.csv",sep=""),row.names = FALSE)
        activity_summary$id <- file_name
        activity_summary <- activity_summary[,c(ncol(activity_summary),(1:(ncol(activity_summary)-1)))]
        all_data[[i]] <- activity_summary
        print(paste("Processed Events File: ",file_name,sep=""))
      }
    }
    all_data <- dplyr::bind_rows(all_data)
    all_data[is.na(all_data)] <- 0

    return(all_data)
  }

activity.summary.file <-
  function(file_path,time_periods){

    events_file <- pre.process.events.file(file_path)
    if(nrow(events_file) > 0 ){

      step_start <- load.step.times(file_path)
      step_date <- as.Date(step_start)
      step_time <- as.numeric(difftime(step_start,step_date,units="secs"))
      step_data <- data.frame(step_start,step_date,step_time)

      events_file$event_start <- round(as.numeric(events_file$time) %% 86400,1)
      events_file$event_end <- events_file$event_start + events_file$interval
      events_file$date <- as.Date(events_file$time)

      summary_data <- list(length=nrow(time_periods))
      for (i in (1:nrow(time_periods))){
        single_period <- activity.summary.file.single.period(events_file,step_data,time_periods[i,]$start_seconds,time_periods[i,]$end_seconds)
        single_period$period <- time_periods[i,]$labels
        summary_data[[i]] <- single_period
      }
      summary_data <- dplyr::bind_rows(summary_data)
      summary_data <- summary_data[order(summary_data$date,summary_data$period),order(colnames(summary_data))]
      summary_data <- summary_data[,c(((ncol(summary_data)-2):ncol(summary_data)),1:(ncol(summary_data)-3))]
      summary_data[is.na(summary_data)] <- 0

      return(summary_data)
    }else{
      return(NULL)
    }
  }

activity.summary.file.single.period <-
  function(events_file,step_times,start_period,end_period){
    #' @import magrittr
    events_file$start_count <- start_period
    events_file$end_count <- end_period

    events_file$start_count <- pmax(events_file$start_count,events_file$event_start)
    events_file$end_count <- pmin(events_file$end_count,events_file$event_end)

    events_file$duration <- round(events_file$end_count - events_file$start_count,1)

    summary_steps <- step_times %>%
      dplyr::filter(.data$step_time >= start_period & .data$step_time <= end_period) %>%
      dplyr::group_by(date = .data$step_date) %>%
      dplyr::summarise (steps = dplyr::n()*2)

    summary_data <- events_file %>%
      dplyr::filter(.data$duration > 0) %>%
      dplyr::group_by(.data$date,.data$activity) %>%
      dplyr::summarise(events = dplyr::n(), duration = sum(.data$duration))

    summary_occurrences <- tidyr::spread(summary_data[,c(1:3)],2,3,fill=0)
    colnames(summary_occurrences)[2:ncol(summary_occurrences)] <- paste(colnames(summary_occurrences)[2:ncol(summary_occurrences)],"events",sep=" - ")
    summary_duration <- tidyr::spread(summary_data[,c(1,2,4)],2,3,fill=0)
    colnames(summary_duration)[2:ncol(summary_duration)] <- paste(colnames(summary_duration)[2:ncol(summary_duration)],"duration",sep=" - ")

    summary_data <- merge(summary_occurrences,summary_duration)
    summary_data <- merge(summary_data,summary_steps,all.x=TRUE)
    summary_data$steps <- tidyr::replace_na(summary_data$steps,0)

    return(summary_data)
  }

activity.summary.column.headers <-
  function (data){
    # rename the column headers to convert activity codes to activity names
    col_headers <- colnames(data)
    col_headers <- gsub("0 -","Sedentary -", col_headers)
    col_headers <- gsub("3.1 -","Primary Lying -", col_headers)
    col_headers <- gsub("3.2 -","Seconday Lying -", col_headers)
    col_headers <- gsub("1 -","Standing -", col_headers)
    col_headers <- gsub("2 -","Stepping -", col_headers)
    col_headers <- gsub("3 -","Lying -", col_headers)
    col_headers <- gsub("4 -","Non Wear -", col_headers)
    col_headers <- gsub("5 -","Cycling -", col_headers)
    col_headers <- gsub("6 -","Travelling -", col_headers)
    colnames(data) <- col_headers
    return(data)
  }
