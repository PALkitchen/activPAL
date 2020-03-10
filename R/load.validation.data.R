load.validation.data <-
  function(folder,file_name){
    if(length(file_name) == 0){
      return (NULL)
    }
    validation_data <- read.delim(paste(folder,file_name,sep=""), sep = ";", skip = 1)
    validation_data <- validation_data[,c(1,5,6)]
    colnames(validation_data) <- c("serial","date","valid")
    validation_data$date <- as.Date(validation_data$date,"%d %b %Y")
    return(validation_data)
  }