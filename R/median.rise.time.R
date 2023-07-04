process.rise.time <-
  function(folder, prefix_delimiter, prefix_length){
    #' @import dplyr

    # Assumes that the name of the events files contain the text Events in the file name
    file.names <- list.files(folder, pattern="*.csv",recursive = FALSE)
    file.names <- file.names[grep("RiseSitData",file.names)]

    rise.data <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(rise.data) <- c("uid","median_rise_time")
    if(length(file.names) == 0){
      return(NULL)
    }
    for(i in (1:length(file.names))){
      rise.file <- read.csv(paste(folder,file.names[i],sep=""), sep = ";", skip = 1)
      first_date <- format(as.POSIXct(min(rise.file$Time)*86400,origin = "1899-12-30", tz = "UTC"),"%b")
      rise.file <- rise.file[,c(1:3)]
      colnames(rise.file) <- c("time","transition","duration")
      rise.file <- rise.file[which(rise.file$transition == 0),]
      rise.file$time <- as.POSIXct(rise.file$time * 86400, origin = "1899-12-30", tz ="UTC")
      rise.file$Date <- as.Date(rise.file$time)

      uid <- parse.file.name(file.names[i], prefix_delimiter, prefix_length)

      rise.file <- rise.file %>%
        dplyr::summarise(median_rise_time = stats::median(.data$duration)) %>%
        dplyr::mutate(uid = uid)
      rise.file <- rise.file[,c(2,1)]

      if(nrow(rise.data) == 0){
        rise.data <- rise.file
      }else{
        rise.data <- dplyr::bind_rows(rise.data,rise.file)
      }
    }
    return(rise.data)
  }

process.daily.rise.time <-
  function(folder){
    #' @import dplyr

    # Assumes that the name of the events files contain the text Events in the file name
    file.names <- list.files(folder, pattern="*.csv",recursive = FALSE)
    file.names <- file.names[grep("RiseSitData",file.names)]

    # Searches for an activPAL validation file
    validation.file <- list.files(folder, pattern=".csv")
    validation.file <- validation.file[grep("DailyValidation",validation.file)]
    if (length(validation.file) > 0){
      # Validation file found.  Load validation file and extract the date
      validation.data <- read.csv(paste(folder,validation.file,sep=""),
                                                        stepping.delimiter=",",
                                                        stepping.startrow=1)
      if(ncol(validation.data) == 1){
        validation.data <- read.csv(paste(folder,validation.file,sep=""),
                                                          stepping.delimiter=";",
                                                          stepping.startrow=2)
      }
      validation.data$Date <- as.Date(validation.data$Date, tryFormats = c("%d-%b-%y","%d %b %Y"),origin="2010-01-01")
      validation.data <- validation.data[which(validation.data$Valid == 1),c(2,5,6)]
      colnames(validation.data)[1] <- "uid"
    }else{
      validation.data <- NULL
    }

    rise.data <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(rise.data) <- c("uid","date","median_rise_time","transitions")
    for(i in (1:length(file.names))){
      rise.file <- read.csv(paste(folder,file.names[i],sep=""), sep = ";", skip = 1)
      first_date <- format(as.POSIXct(min(rise.file$Time)*86400,origin = "1899-12-30", tz = "UTC"),"%b")
      rise.file <- rise.file[,c(1:3)]
      colnames(rise.file) <- c("time","transition","duration")
      rise.file <- rise.file[which(rise.file$transition == 0),]
      rise.file$time <- as.POSIXct(rise.file$time * 86400, origin = "1899-12-30", tz ="UTC")
      rise.file$Date <- as.Date(rise.file$time)

      # Change substr to get the prefix of the filename that matches the File code field (column 2) in the daily validation file
      break_point <- regexpr("-",file.names[i])[1]
      if(break_point == -1){
        break_point <- nchar(file.names[i])
      } else{
        break_point <- break_point - 1
      }

      uid <- substr(file.names[i],1,break_point)

      rise.file <- rise.file %>%
        dplyr::group_by(date = .data$Date) %>%
        dplyr::summarise(median_rise_time = stats::median(.data$duration), transitions = n()) %>%
        # Change substr to get the prefix of the filename that matches the File code field (column 2) in the daily validation file
        dplyr::mutate(uid = .data$uid)

      rise.file <- rise.file[,c(4,1,2,3)]

      rise.data <- dplyr::bind_rows(rise.data,rise.file)
    }
    return (rise.data)
  }
