parse.file.name <-
  function(file_name){
    device_serial <- ""
    serial_start <- regexpr("AP[[:digit:]]{6,7}",file_name)
    serial_length <- attr(regexpr("AP[[:digit:]]{6,7}",file_name),"match.length")

    break_point <- regexpr(" |-",file_name)[1]
    if(break_point == -1){
      break_point <- 10
    } else{
      break_point <- break_point
    }
    uid  <- substr(file_name, 1, (break_point - 1))
    
    recording_date <- ""
    date_start <- regexpr("[[:digit:]]{1,2}[[:alpha:]]{3}[[:digit:]]{2}",file_name)
    date_length <- attr(regexpr("[[:digit:]]{1,2}[[:alpha:]]{3}[[:digit:]]{2}",file_name),"match.length")
    if(date_start > -1){
      # device serial within file name
      recording_date <- substr(file_name, date_start, (date_start+date_length-1))
    }
    
    if(recording_date != ""){
      uid <- paste(uid,recording_date,sep=" ")
    }
    
    return (uid)
  }

parse.device.serial <-
  function(file_name){
    start_pos <- regexpr("AP[[:digit:]]{6}",file_name)[1]
    if(start_pos == -1){
      return(NULL)
    }
    return(substr(file_name,start_pos,start_pos+7))
  }