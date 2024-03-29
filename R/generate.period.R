load.lookup.period.file <-
  function(file_path = NULL, fill = FALSE){
    #' @import dplyr
    if(is.null(file_path)){
      return (NULL)
    }
    if(!valid.file.path(file_path)){
      return (NULL)
    }

    lookup_file <- read.csv(file_path)
    colnames(lookup_file) <- c("id","start_date","end_date","category")
    lookup_file$start_date <- as.POSIXct(lookup_file$start_date,
                                         tryFormats = c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S",
                                                        "%Y-%d-%m %H:%M", "%Y-%d-%m %H:%M:%S",
                                                        "%d/%m/%Y %H:%M", "%d/%m/%Y %H:%M:%S",
                                                        "%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S"),
                                         tz = "UTC")
    lookup_file$end_date <- as.POSIXct(lookup_file$end_date,
                                       tryFormats = c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S",
                                                      "%Y-%d-%m %H:%M", "%Y-%d-%m %H:%M:%S",
                                                      "%d/%m/%Y %H:%M", "%d/%m/%Y %H:%M:%S",
                                                      "%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S"),
                                       tz = "UTC")

    if(fill){
      lookup_file <- lookup_file %>% dplyr::arrange(.data$id, .data$start_date)
      id_1 <- head(lookup_file$id, -1)
      id_2 <- tail(lookup_file$id, -1)
      end_1 <- head(lookup_file$end_date, -1)
      start_2 <- tail(lookup_file$start_date, -1)

      gaps <- which(id_1 == id_2 & end_1 < start_2)

      id <- lookup_file[gaps,]$id
      start_date <- lookup_file[gaps,]$end_date
      end_date <- lookup_file[(gaps + 1),]$start_date
      gaps_data <- data.frame(id, start_date, end_date)
      gaps_data$category <- "gap"

      lookup_file <- dplyr::bind_rows(lookup_file, gaps_data)
      lookup_file <- lookup_file %>% dplyr::arrange(.data$id, .data$start_date)
    }
    lookup_file$period_date <- as.Date(lookup_file$start_date, tz = "UTC")
    lookup_file <- lookup_file[,c(5,4,2,3,1)]
    colnames(lookup_file) <- c("period_date","category","start_date","end_date","id")
    lookup_file$id <- as.character(lookup_file$id)
    return(lookup_file)
  }

pad.period.data <-
  function(lookup_data, events_start, events_end){
    rows <- nrow(lookup_data) - 1
    if(rows > 0){
      for(i in (1:rows)){
        if(lookup_data[i,]$end_date < lookup_data[(i+1),]$start_date){
          lookup_data[nrow(lookup_data)+1,]$period_date <- as.Date(lookup_data[i,]$end_date)
          lookup_data[nrow(lookup_data),]$category <- paste("Padding_",i,sep="")
          lookup_data[nrow(lookup_data),]$start_date <- lookup_data[i,]$end_date
          lookup_data[nrow(lookup_data),]$end_date <- lookup_data[i+1,]$start_date
          lookup_data[nrow(lookup_data),]$id <- lookup_data[i,]$id
        }
      }
    }
    lookup_data <- lookup_data %>% dplyr::arrange(start_date)
    if(lookup_data[1,]$start_date > events_start){
      lookup_data[nrow(lookup_data)+1,]$period_date <- as.Date(events_start)
      lookup_data[nrow(lookup_data),]$category <- "Padding_0"
      lookup_data[nrow(lookup_data),]$start_date <- events_start
      lookup_data[nrow(lookup_data),]$end_date <- lookup_data[1,]$start_date
      lookup_data[nrow(lookup_data),]$id <- lookup_data[1,]$id
    }
    lookup_data <- lookup_data %>% dplyr::arrange(start_date)
    if(lookup_data[nrow(lookup_data),]$end_date < events_end){
      lookup_data[nrow(lookup_data)+1,]$period_date <- as.Date(lookup_data[nrow(lookup_data),]$end_date)
      lookup_data[nrow(lookup_data),]$category <- "Padding_n"
      lookup_data[nrow(lookup_data),]$start_date <- lookup_data[nrow(lookup_data)-1,]$end_date
      lookup_data[nrow(lookup_data),]$end_date <- events_end
      lookup_data[nrow(lookup_data),]$id <- lookup_data[nrow(lookup_data)-1,]$id
    }
    lookup_data <- lookup_data %>% dplyr::arrange(start_date)

    return(lookup_data)
  }

set.custom.periods <-
  function(events_file_data,full_events_file,lookup_data){
    library(magrittr)
    if(nrow(lookup_data) > 0){
      events_file_data$period_date <- NA
      events_file_data$period_name <- NA
      event_end <- events_file_data$time + events_file_data$interval

      events_file_data$end_time <- events_file_data$time + events_file_data$interval
      # Add padding if missing sections
      for(i in (1:nrow(lookup_data))){
        # Lookup period is completely within a single event
        event_end <- events_file_data$time + events_file_data$interval
        periods <- which(events_file_data$time < lookup_data[i,]$start_date & events_file_data$end_time > lookup_data[i,]$end_date)
        if(length(periods) > 0){
          start_event <- which(events_file_data$time < lookup_data[i,]$start_date & event_end > lookup_data[i,]$start_date)
          end_event <- which(events_file_data$time < lookup_data[i,]$end_date & event_end > lookup_data[i,]$end_date)

          events_file_data <- split.event(events_file_data, full_events_file, start_event, lookup_data[i,]$start_date, as.Date(lookup_data[i,]$start_date), lookup_data[i,]$category)
          events_file_data <- split.event(events_file_data, full_events_file, nrow(events_file_data), lookup_data[i,]$end_date, as.Date(lookup_data[i,]$start_date), lookup_data[i,]$category)
          events_file_data[nrow(events_file_data)-1,]$period_date <- lookup_data[i,]$period_date
          events_file_data[nrow(events_file_data)-1,]$period_name <- lookup_data[i,]$category
        }else{
          periods <- which(events_file_data$time >= lookup_data[i,]$start_date & events_file_data$time <= lookup_data[i,]$end_date)
          if(length(periods) > 0){
            start_event <- which(events_file_data$time <= lookup_data[i,]$start_date & event_end >= lookup_data[i,]$start_date)
            if(length(start_event) > 1){
              start_event <- min(start_event)
            }
            end_event <- which(events_file_data$time <= lookup_data[i,]$end_date & event_end >= lookup_data[i,]$end_date)
            if(length(end_event) > 1){
              end_event <- max(end_event)
            }
            events_file_data <- split.event(events_file_data, full_events_file, start_event, lookup_data[i,]$start_date, as.Date(lookup_data[i,]$start_date), lookup_data[i,]$category)
            events_file_data <- split.event(events_file_data, full_events_file, end_event, lookup_data[i,]$end_date, as.Date(lookup_data[i,]$start_date), lookup_data[i,]$category)
          }
        }
      }
      event_end <- events_file_data$time + events_file_data$interval
      for(i in (1:nrow(lookup_data))){
        periods <- which(events_file_data$time >= lookup_data[i,]$start_date & events_file_data$time < lookup_data[i,]$end_date)
        if(length(periods) > 0){
          events_file_data[periods,]$period_date <- as.Date(lookup_data[i,]$start_date, origin = "1970-01-01")
          events_file_data[periods,]$period_name <- lookup_data[i,]$category
        }
      }
    }
    events_file_data <- events_file_data %>%
      dplyr::arrange(time)
    events_file_data <- events_file_data %>%
      dplyr::filter(!is.na(period_name))
    return(events_file_data)
  }

set.waking.day.periods <-
  function(events_file_data){
    library(magrittr)
    tob <- which(head(events_file_data$waking_day,-1) == 0 & tail(events_file_data$waking_day,-1) == 1)
    if(length(tob) == 0 ){
      events_file_data$period_date <- as.Date(events_file_data[1,]$time, origin = "1970-01-01")
      events_file_data$period_name <- "Time in Bed"
      events_file_data[which(events_file_data$waking_day == 1),]$period_name <-
        rep("Waking Day",length(which(events_file_data$waking_day == 1)))
    }else{
      events_file_data$period_date <- NA
      events_file_data[tob,]$period_date <- as.Date(events_file_data[tob,]$Date, origin = "1970-01-01")
      events_file_data <- events_file_data %>%
        tidyr::fill(period_date, .direction = "updown")
      events_file_data$period_name <- "Time in Bed"
      events_file_data[which(events_file_data$waking_day == 1),]$period_name <-
        rep("Waking Day", length(which(events_file_data$waking_day == 1)))
    }
    return(events_file_data)
  }

get.waking.day.periods <-
  function(events_file_data, file_uid){
    waking_day_period <- events_file_data %>%
      dplyr::group_by(period_date, category = period_name) %>%
      dplyr::summarise(start_date = min(time), end_date = max(time)) %>% arrange(start_date)
    end_date <- c(tail(waking_day_period$start_date, -1),
                  events_file_data[nrow(events_file_data),]$time + events_file_data[nrow(events_file_data),]$interval)
    waking_day_period$end_date <- end_date
    waking_day_period$id <- file_uid
    return(waking_day_period)
  }

set.calendar.day.periods <-
  function(events_file_data){
    events_file_data$period_date <- as.Date(events_file_data$Date, origin = "1970-01-01")
    events_file_data$period_name <- "Calendar Day"
    return(events_file_data)
  }

get.calendar.day.periods <-
  function(events_file_data, file_uid){
    calendar_day_period <- events_file_data %>%
      dplyr::group_by(period_date, category = period_name) %>%
      dplyr::summarise(start_date = min(time), end_date = max(time)) %>% arrange(start_date)
    end_date <- c(tail(calendar_day_period$start_date, -1),
                  events_file_data[nrow(events_file_data),]$time + events_file_data[nrow(events_file_data),]$interval)
    calendar_day_period$end_date <- end_date
    calendar_day_period$id <- file_uid
    return(calendar_day_period)
  }

split.event <-
  function(data, full_data, pos, split_time, period_date, period_category){
    if(length(pos) == 0){
      return(data)
    }
    split_event <- data[pos,]
    time_diff <- round(as.numeric(difftime(split_time,data[pos,]$time,units = "secs")), 1)

    if(time_diff != split_event$interval & time_diff != 0){
      if(data[pos,]$activity %in% c(2,2.1)){
        pre_steps <- length(which(full_data$time >= split_event[1,]$time &
                                    (full_data$time < (split_time))))
        pre_steps <- pre_steps * 2
        data[pos,]$steps <- pre_steps
        data[pos,]$cumulative_steps <- data[pos,]$cumulative_steps - (split_event$steps - pre_steps)
        split_event$steps <- split_event$steps - pre_steps
      }

      split_event$time <- split_time
      split_event$interval <- split_event$interval - time_diff

      if(split_event$interval > 0){
        data[pos,]$interval <- time_diff

        data$period_date <- as.Date(data$period_date, origin = "1970-01-01")
        data <- dplyr::bind_rows(data, split_event)
      }
    }
    return(data)
  }
