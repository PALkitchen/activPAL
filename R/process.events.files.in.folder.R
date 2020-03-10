process.events.file.folder <-
  function(folder){
    # Assumes that the name of the events files contain the text Events in the file name
    #    file.names <-list.files(process.data, pattern="Events*.csv",recursive = TRUE)
    file_names <- list.files(folder, pattern="*.csv",recursive = FALSE)
    file_names <- file_names[grep("EventsEx",file_names)]
    events_data <- list()
    events_days <- list()
    for(i in 1:length(file_names)){
      events_file <- read.csv(paste(folder,file_names[i],sep=""), row.names = NULL, sep=";", skip = 1, stringsAsFactors = FALSE)
      colnames(events_file) <- c(tail(colnames(events_file),-1),"")
      events_file <- events_file[,-ncol(events_file)]
      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400, origin = "1899-12-30", tz = "UTC")

      events_file_days <- as.data.frame(unique(as.Date(events_file$Time)))
      colnames(events_file_days) <- "Date"

      events_file <- activpal.extended.events.file.process(events_file, wear_time_minimum = 20 * 3600)
      if(nrow(events_file) > 0){
        events_file$Date <- as.Date(events_file$time)

        uid <- parse.file.name(file_names[i])

        events_file$uid <- uid
        events_file_days$uid <- uid

        events_file_days$valid <- "invalid"
        events_file_days[which(events_file_days$Date %in% unique(events_file$Date)),]$valid <- "valid"

        events_data[[i]] <- events_file
        events_days[[i]] <- events_file_days
      }
    }
    events_data <- dplyr::bind_rows(events_data)
    events_days <- dplyr::bind_rows(events_days)

    return (list(events_data, events_days))
  }

process.events.file <-
  function(folder, file_name, validation_list){
    # Assumes that the name of the events files contain the text Events in the file name
    events_file <- read.csv(paste(folder,file_name,sep=""), row.names = NULL, sep=";", skip = 1, stringsAsFactors = FALSE)
    colnames(events_file) <- c(tail(colnames(events_file),-1),"")
    events_file <- events_file[,-ncol(events_file)]
    events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400, origin = "1899-12-30", tz = "UTC")

    events_file_days <- as.data.frame(unique(as.Date(events_file$Time)))
    colnames(events_file_days) <- "Date"

    events_file <- activpal.extended.events.file.process(events_file, wear_time_minimum = 20 * 3600)

    if(nrow(events_file) > 0){
      events_file$Date <- as.Date(events_file$time)
      uid <- parse.file.name(file_name)
      pal_serial <- parse.device.serial(file_name)

      events_file$uid <- uid
      events_file_days$uid <- uid
      events_file$serial <- pal_serial

      if(!is.null(validation_list)){
        # A parsed DailyValidation file is available.
        validation_list <- validation_list[which(validation_list$serial == pal_serial),]
        if(nrow(validation_list) > 0){
          validation_list <- validation_list[which(validation_list$valid == 1),]
          events_file <- events_file[which(events_file$Date %in% validation_list$date),]
        }
      }

      events_file_days$valid <- "invalid"
      events_file_days[which(events_file_days$Date %in% unique(events_file$Date)),]$valid <- "valid"
      return (list(events_file, events_file_days))
    }
    return(NULL)
  }
