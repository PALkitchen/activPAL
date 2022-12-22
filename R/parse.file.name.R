parse.file.name <-
  function(file_name, prefix_delimiter = NULL, prefix_length = NULL){
    device_serial <- ""

    if(is.null(prefix_delimiter) & is.null(prefix_length)){
      return(substr(file_name, 1, 6))
    }
    if(is.null(prefix_length)){
      end_pos <- regexpr(prefix_delimiter,file_name)[1] - 1
      if(end_pos < 0){
        return(substr(file_name, 1, 6))
      }
      return(substr(file_name, 1, end_pos))
    }
    if(is.null(prefix_delimiter)){
      return(substr(file_name, 1, prefix_length))
    }
    if(prefix_length < 1){
      prefix_length <- 6
    }
    end_pos <- regexpr(prefix_delimiter,file_name)[1] - 1
    if(end_pos < 0){
      return(substr(file_name, 1, prefix_length))
    }
    return(substr(file_name, 1, min(end_pos, prefix_length)))
  }

parse.device.serial <-
  function(file_name){
    start_pos <- regexpr("AP[[:digit:]]{6}",file_name)[1]
    if(start_pos == -1){
      return(NULL)
    }
    return(substr(file_name,start_pos,start_pos+7))
  }
