activity.summary.by.custom.period.revised <-
  function(input_folder, period_file = NULL, output_folder,
           prefix_delimiter = NULL, prefix_length = NULL, fill_gaps = FALSE,
           minimum_valid_wear = 20){
    #' Summarise activity data from a folder of activPAL events files
    #' in user defined periods.  This is an updated version of
    #' activity.summary.by.custom.period to support the specification of
    #' custom reporting periods used in generate.physical.behaviour.summary()
    #' and the include details on the number of events occurring within
    #' each epoch.
    #' @description activity.summary.custom.period.revised reads in a csv file containing a list
    #'     of events files and csv files with time periods of interest (the first two
    #'     columns should have the start date and end date in the format dd-mm-yy HH:MM).
    #'     A summary table is saved to a csv file for each events file.  A further csv
    #'     file giving a summary for all the events file is also generated.  The generated
    #'     summary activity table for each events file is saved to a csv file, while an
    #'     additional csv file containing the summaries across all the processed events
    #'     files is also saved.
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

    #' @importFrom devtools load_all
    #' @import utils
    #' @export

    .Deprecated("generate.physical.behaviour.summary")
    if(!valid.file.path(input_folder) & input_folder != ""){
      stop("A valid folder location to read events files from has not been provided.")
    }
    if(!valid.folder.path(output_folder) & output_folder != ""){
      stop("A valid folder to save the generated output has not been provided.")
    }

    file_names <- list.files(input_folder, pattern="*.csv",recursive = FALSE)
    file_names <- file_names[grep("EventsEx",file_names)]

    if(length(file_names) == 0){
      stop(paste("The summary outcome report was not generated as the selected folder contains no events extended files.",
                 "If there are events extended files in sub-folders you should move these files into the parent folder.", sep = " "))
    }

    lookup_data <- load.lookup.period.file(period_file, FALSE)

    all_summary <- list()
    for(i in (1:length(file_names))){
      tryCatch({
        events_import <- process.events.file(input_folder, file_names[i], NULL, minimum_valid_wear, prefix_delimiter, prefix_length)
        file_uid <- parse.file.name(file_names[i], prefix_delimiter, prefix_length)

        events_file_data <- events_import[[1]]

        if(!is.null(events_file_data)){
          full_events_file <- load.full.events.file(input_folder, file_names[i])

          if(!is.null(lookup_data)){
            custom_periods <- lookup_data %>% dplyr::filter(id == file_uid)
            if(nrow(custom_periods) > 0){
              events_file_data_custom <- set.custom.periods(events_file_data,custom_periods)

              file_summary <- events_file_data_custom %>%
                dplyr::group_by(period_date, period_name) %>%
                dplyr::summarise(period_duration = sum(interval), steps = sum(steps))
              epoch_summary <- events_file_data_custom %>%
                dplyr::group_by(period_date, period_name, activity) %>%
                dplyr::summarise(events = n())
              duration_summary <- events_file_data_custom %>%
                dplyr::group_by(period_date, period_name, activity) %>%
                dplyr::summarise(duration_minutes = round(sum(interval)/60,2))

              activity_summary <- dplyr::inner_join(file_summary, epoch_summary,
                                             by = c("period_date", "period_name"))
              activity_summary <- dplyr::inner_join(activity_summary, duration_summary,
                                             by = c("period_date", "period_name", "activity"))
              activity_summary$uid <- file_uid
              all_summary[[i]] <- activity_summary
            }
          }
        }
        message(paste("Processed File ",i," of ",length(file_names),sep=""))
      },
      error = function(c){
        message(paste("File ",file_names[i]," encoutered an error and was not processed.",sep=""))
      })
    }
    all_summary <- bind_rows(all_summary)

    if(nrow(all_summary) == 0){
      message("No summary data was generated as there were no valid files to process.")
      return(NULL)
    }

    all_summary$activity <- as.character(all_summary$activity)
    all_summary[which(all_summary$activity == "0"),]$activity <- rep("Sedentary",length(which(all_summary$activity == "0")))
    all_summary[which(all_summary$activity == "1"),]$activity <- rep("Upright",length(which(all_summary$activity == "1")))
    all_summary[which(all_summary$activity == "2"),]$activity <- rep("Stepping",length(which(all_summary$activity == "2")))
    all_summary[which(all_summary$activity == "2.1"),]$activity <- rep("Cycling",length(which(all_summary$activity == "2.1")))
    all_summary[which(all_summary$activity == "3"),]$activity <- rep("Lying",length(which(all_summary$activity == "3")))
    all_summary[which(all_summary$activity == "3.1"),]$activity <- rep("Primary Lying",length(which(all_summary$activity == "3.1")))
    all_summary[which(all_summary$activity == "3.2"),]$activity <- rep("Secondary Lying",length(which(all_summary$activity == "3.2")))
    all_summary[which(all_summary$activity == "4"),]$activity <- rep("Non-Wear",length(which(all_summary$activity == "4")))
    all_summary[which(all_summary$activity == "5"),]$activity <- rep("Travelling",length(which(all_summary$activity == "5")))

    all_summary$activity <- factor(all_summary$activity,
                                   levels =c("Sedentary","Upright","Stepping","Cycling","Lying",
                                             "Primary Lying","Secondary Lying","Non-Wear","Travelling"))
    all_summary <- all_summary %>%
      tidyr::pivot_wider(names_from = "activity", names_sort = TRUE,
                         values_from = c("events","duration_minutes"))
    col_refs <- c(rbind(6:(((ncol(all_summary) + 7) / 2)-1),((ncol(all_summary) + 7) / 2):ncol(all_summary)))
    all_summary <- all_summary[,c(5,1,2,3,4,col_refs)]
    all_summary[is.na(all_summary)] <- 0
    write.csv(all_summary,paste(output_folder,"custom_activity_summary_all_files.csv",sep=""),row.names = FALSE)
    return(all_summary)
  }

activity.summary.by.custom.period <-
  function(location_file,output_folder,minimum_valid_wear = 20){
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
    #' @param minimum_valid_wear The minimum duration of valid wear time required for
    #'     a day to be included in the analysis.
    #'     Valid daily wear time is the total daily wear time, excluding periods of wear
    #'     classified as non-wear (activity code - 4)
    #'     Default - 20 hours

    #' @importFrom devtools load_all
    #' @import utils
    #' @export

    .Deprecated("generate.physical.behaviour.summary")

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
      events_file <- pre.process.events.file(as.character(file_list[i,1]), "", minimum_valid_wear)
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
    #' @import dplyr
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
  function(input_folder,output_folder,window_duration = 2,minimum_valid_wear = 20){
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
    #' @param minimum_valid_wear The minimum valid wear time required for a day to be included
    #'     in the analysis
    #'     Default - 20 hours.

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

    all_data <- activity.summary.folder(input_folder,output_folder,time_periods,minimum_valid_wear = 20)
    write.csv(all_data,paste(output_folder,"activity_summary_all_files.csv",sep=""),row.names = FALSE)
    return(all_data)
  }

activity.summary.folder <-
  function(input_folder,output_folder,time_periods,minimum_valid_wear = 20){
    file_list <- list.files(input_folder,pattern = "Events[[:alnum:]]{0,2}.csv")
    to_remove <- grep("[[:alnum:]]+.csv[[:graph:]]+",file_list)
    if(length(to_remove) > 0){
      file_list <- file_list[-grep("[[:alnum:]]+.csv[[:graph:]]+",file_list)]
    }
    all_data <- list(length=length(file_list))
    for (i in (1:length(file_list))){
      file_name <- file_list[i]
      activity_summary <- activity.summary.file(file_path = paste(input_folder,file_name,sep=""),time_periods,minimum_valid_wear = 20)
      activity_summary <- activity.summary.column.headers(activity_summary)
      if(is.null(activity_summary)){
        warning(paste("Processed Events File: ",file_name,". No output generated.",sep=""))
      }else{
        file_name <- substr(file_name,1,gregexpr("Event",file_name)[[1]][1]-1)
        write.csv(activity_summary,file = paste(output_folder,file_name,"_",round(24 / nrow(time_periods),2),
                                                "_hours_summary.csv",sep=""),row.names = FALSE)
        activity_summary$id <- file_name
        activity_summary <- activity_summary[,c(ncol(activity_summary),(1:(ncol(activity_summary)-1)))]
        all_data[[i]] <- activity_summary
      }
    }
    all_data <- dplyr::bind_rows(all_data)
    all_data[is.na(all_data)] <- 0

    return(all_data)
  }

activity.summary.file <-
  function(file_path,time_periods,minimum_valid_wear = 20){

    events_file <- pre.process.events.file(file_path,minimum_valid_wear = 20)
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
    #' @import dplyr
    #' @import tidyr
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
    col_headers <- gsub("2.1 -","Cycling -", col_headers)
    col_headers <- gsub("2 -","Stepping -", col_headers)
    col_headers <- gsub("3 -","Lying -", col_headers)
    col_headers <- gsub("4 -","Non Wear -", col_headers)
    col_headers <- gsub("5 -","Travelling -", col_headers)
    colnames(data) <- col_headers
    return(data)
  }
