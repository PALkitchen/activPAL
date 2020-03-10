process.valid.days <-
  function(folder){
    # Assumes that the name of the events files contain the text Events in the file name
    #    file.names <-list.files(process.data, pattern="Events*.csv",recursive = TRUE)
    validation.file <- list.files(folder, pattern=".csv")
    validation.file <- validation.file[grep("DailyValidation",validation.file)]

    if (length(validation.file) > 0){
      # A validation file has been found.  Load the validation file and extract the date
      validation.data <- read.csv(paste(folder,validation.file,sep=""),
                                                        stepping.delimiter=",",
                                                        stepping.startrow=1)
      if(ncol(validation.data) == 1){
        validation.data <- read.csv(paste(folder,validation.file,sep=""),
                                                          stepping.delimiter=";",
                                                          stepping.startrow=2)
      }
      validation.data$Date <- as.Date(validation.data$Date, tryFormats = c("%d-%b-%y","%d %b %Y"),origin="2010-01-01")
      validation.data <- validation.data[,c(2,5,6)]
    }else{
      return(NULL)
    }
    # validation.data$File.Code <- paste(validation.data$File.Code,format(validation.data$Date, "%b"),sep=" - ")

    valid_day_summary <- validation.data %>%
      dplyr::group_by(.data$File.Code) %>%
      dplyr::summarise(valid = length(which(.data$Valid == 1)), all_days = n())

    valid_day_summary$invalid <- valid_day_summary$all_days - valid_day_summary$valid
    valid_day_summary <- tidyr::pivot_longer(valid_day_summary[,c(1,2,4)],c(2,3),"category","days")
    valid_day_summary$category <- factor(valid_day_summary$category, levels = c("invalid","valid"))
    colnames(valid_day_summary)[1] <- "uid"
    return(valid_day_summary)
  }

process.individual.valid.days <-
  function(folder){
    # Assumes that the name of the events files contain the text Events in the file name
    #    file.names <-list.files(process.data, pattern="Events*.csv",recursive = TRUE)
    validation.file <- list.files(folder, pattern=".csv")
    validation.file <- validation.file[grep("DailyValidation",validation.file)]

    if (length(validation.file) > 0){
      # A validation file has been found.  Load the validation file and extract the date
      validation.data <- read.csv(paste(folder,validation.file,sep=""),
                                                        stepping.delimiter=",",
                                                        stepping.startrow=1)
      if(ncol(validation.data) == 1){
        validation.data <- read.csv(paste(folder,validation.file,sep=""),
                                                          stepping.delimiter=";",
                                                          stepping.startrow=2)
      }
      validation.data$Date <- as.Date(validation.data$Date, tryFormats = c("%d-%b-%y","%d %b %Y"),origin="2010-01-01")
      validation.data <- validation.data[,c(2,5,6)]

    }else{
      return(NULL)
    }
    colnames(validation.data)[1] <- "uid"

    return(validation.data)
  }
