activity.with.overlay.chart.folder <-
  function(index_file_location,output_folder){
    #' Combines events file data with observational data across multiple events files
    #' to produce and save charts
    #' @description Reads in a csv file containing a list of events file locations and
    #'     overlay file locations, generating and saving spiral and linear charts for
    #'     each events file aligning the events file data with the corresponding overlay
    #'     data.
    #'     Where the overlay data is non-continuous data (such as sleeps diaries) the
    #'     csv file should have the columns start_time (dd-mm-YYYY HH:MM),
    #'     end_time (dd-mm-YYYY HH:MM) and category (text).
    #'     Where the overlay data is continuous data (such as continuous glucose monitoring)
    #'     the csv file should have the columns start_time (dd-mm-YYYY HH:MM) and
    #'     category (text).  In this case the end time coincides with the start time of
    #'     the subsequent observation.
    #'     The charts are saved as png images with two images generated for each events
    #'     file / overlay file pair (one spiral chart and one linear chart). \cr
    #'     \strong{Note}: Spiral plots are generated using ggplot2 and coord_polar() and can take
    #'     45 - 60 seconds to generate each spiral plot.
    #' @param index_file_location The filepath for the csv file with the following column names
    #'     file_id, events_file, overlay_file
    #' @param output_folder The filepath of the folder where the generated chart are to be saved to
    #' @export

    if(!valid.file.path(index_file_location)){
      stop("A valid file name for index_file_location has not been provided.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    index_file <- read.csv(paste(index_file_location,sep=""))
    for (i in (1:nrow(index_file))){
      overlay_file_folder <- substr(as.character(index_file[i,]$overlay_file),1,
                                   gregexpr("/",index_file[i,]$overlay_file)[[1]][length(gregexpr("/",index_file[i,]$overlay_file)[[1]])])
      overlay_file_name <- substr(as.character(index_file[i,]$overlay_file),
                                 gregexpr("/",index_file[i,]$overlay_file)[[1]][length(gregexpr("/",index_file[i,]$overlay_file)[[1]])]+1,
                                 nchar(as.character(index_file[i,]$overlay_file)))

      activity.with.overlay.chart(as.character(index_file[i,]$events_file),as.character(index_file[i,]$overlay_file),output_folder)
    }

  }

activity.with.overlay.chart <-
  function(events_file,overlay_file,output_folder){
    #' Combines events file data with observational data for a single events file
    #' @description Reads in an events file and overlay file, generating spiral and linear
    #'     charts aligning the events file data with the corresponding overlay data.
    #'     Where the overlay data is non-continuous data (such as sleeps diaries) the csv
    #'     file should have the columns start_time (dd-mm-YYYY HH:MM),
    #'     end_time (dd-mm-YYYY HH:MM) and category (text).
    #'     Where the overlay data is continuous data (such as continuous glucose monitoring)
    #'     the csv file should have the columns start_time(dd-mm-YYYY HH:MM) and category (text).
    #'     In this case the end time coincides with the start time of the subsequent observation.
    #'     The charts are saved as png images with two images generated
    #'     (one spiral chart and one linear chart). \cr
    #'     \strong{Note}: Spiral plots are generated using ggplot2 and coord_polar() and can take
    #'     45 - 60 seconds to generate each spiral plot.
    #' @param events_file The filepath of the events file. Must be a valid activPAL events csv file
    #' @param overlay_file The filepath of the csv file containing the overlay data csv file
    #' @param output_folder The filepath of the folder where the generated chart are to be saved to

    #' @export
    #' @examples events_file <- system.file("extdata", "Test_Events.csv", package = "activPAL")
    #' sleep_file <- system.file("extdata", "Sleep.csv", package = "activPAL")
    #' output_folder <- paste(tempdir(),"/",sep="")
    #'
    #' \donttest{activPAL::activity.with.overlay.chart(events_file,sleep_file,output_folder)}

    if(!valid.file.path(events_file)){
      stop("The location of the required input file has not been provided / does not exist.")
    }
    if(!valid.file.path(overlay_file)){
      stop("The location of the required input file has not been provided / does not exist.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    events_file_folder <- substr(events_file,1,
                                 gregexpr("/",events_file)[[1]][length(gregexpr("/",events_file)[[1]])])
    events_file_name <- substr(events_file,
                               gregexpr("/",events_file)[[1]][length(gregexpr("/",events_file)[[1]])]+1,
                               nchar(events_file))
    events_data <- pre.process.events.file(events_file_name,events_file_folder)
    overlay_data <- read.csv(paste(overlay_file,sep=""))
    blank_rows <- which(overlay_data[,1] != "")
    if(length(blank_rows)>0){
      overlay_data <- overlay_data[which(overlay_data[,1] != ""),]
    }
    if(ncol(overlay_data)==2){
      colnames(overlay_data)[1] <- "start_time"
      overlay_data$start_time <- as.POSIXct(overlay_data$start_time,tryFormat = c("%d/%m/%Y %H:%M"),tz="UTC")
      overlay_data$end_time <- as.POSIXct(c(tail(overlay_data$start_time,-1),tail(overlay_data$start_time,1)),tz="UTC")
      overlay_data$end_time <- overlay_data$start_time +
        as.numeric(difftime(overlay_data$end_time,overlay_data$start_time,units="secs"))
      overlay_data <- overlay_data[,c(1,3,2)]
    }else{
      colnames(overlay_data)[1:2] <- c("start_time","end_time")
      overlay_data$start_time <- as.POSIXct(overlay_data$start_time,tryFormat = c("%d/%m/%Y %H:%M","%d/%m/%Y %H:%M:%S"),tz="UTC")
      overlay_data$end_time <- as.POSIXct(overlay_data$end_time,tryFormat = c("%d/%m/%Y %H:%M","%d/%m/%Y %H:%M:%S"),tz="UTC")
    }

    activity.with.overlay.single.chart(events_data,overlay_data,events_file_name,output_folder)

  }

activity.with.overlay.single.chart<-
  function(activpal.data,overlay.data,file.name,out.path,match="both"){
    #' @importFrom grDevices gray palette
    #' @importFrom stats quantile

    # overlap_data should be of the format start_time / duration / category or start_time / end_time / category
    if (!match %in% c("single","both")){
      match <- "both"
    }

    # Restrict the activpal.data to the following columns: time,interval,activity,cumulative_steps,steps)
    activpal.data<-activpal.data[,c(1,3,4,5,8)]
    overlay.data <- process.overlay.file(overlay.data)

    if (match == "single"){
      # calculate the plotting start time and end time to include all activity and overlay data
      start.time<-min(min(activpal.data$time),min(overlay.data$time))
      end.time<-max(max((activpal.data$time+activpal.data$interval)),max(overlay.data$time+overlay.data$duration))
    }else{
      # calculate the plotting start time and end time to exclude leading / trailing data where the activity data or overlay data is missing
      start.time<-max(min(activpal.data$time),min(overlay.data$time))
      end.time<-min(max((activpal.data$time+activpal.data$interval)),max(overlay.data$time+overlay.data$duration))
    }

    # Caclulate the data.frame indexes where there is both activity and overlay data
    activpal.start<-max(1,(min(which(activpal.data$time>=start.time))-1))
    activpal.end<-min(max(which(activpal.data$time<=end.time))+1,nrow(activpal.data))
    overlap.start<-max(1,(min(which(overlay.data$time>=start.time))-1))
    overlap.end<-min(max(which(overlay.data$time<=end.time))+1,nrow(overlay.data))

    # Restrict the data to exclude data where only one of the stepping / overlay data is available
    activpal.data<-activpal.data[c(activpal.start:activpal.end),]
    overlay.data<-overlay.data[c(overlap.start:overlap.end),]

    # Categorise the activity and overlay data to allow correct colouring
    activpal.data<-characterise.activity(activpal.data)

    # Split any measurements that are greater than 5 minutes into 5 minute segments.
    # This is necessary to allow correct spiral production
    activpal.data<-split.events.file(activpal.data,2,300)
    overlay.data<-split.overlay(overlay.data,2,300)

    # Extract the time of day as the number of seconds following midnight for each of the data files.
    # This allows correct plotting of the data in regards to the x-axis
    activpal.data$DayTime<-as.numeric(format(activpal.data$time,"%H"))*3600
    activpal.data$DayTime<-activpal.data$DayTime+as.numeric(format(activpal.data$time,"%M"))*60
    activpal.data$DayTime<-activpal.data$DayTime+as.numeric(format(activpal.data$time,"%S"))

    overlay.data$DayTime<-as.numeric(format(overlay.data$time,"%H"))*3600
    overlay.data$DayTime<-overlay.data$DayTime+as.numeric(format(overlay.data$time,"%M"))*60
    overlay.data$DayTime<-overlay.data$DayTime+as.numeric(format(overlay.data$time,"%S"))

    # Identifiers and colours for chart production.  All categories used to classify activty,
    overlay_title <- colnames(overlay.data)[3]
    colnames(overlay.data)[3] <- "group"
    overlay.data$group <- factor(overlay.data$group)

    # Convert the posixct datetime to a numeric to allow correct spiral production.
    # This is necessary to allow the correct orientation of the spiral and to allow it to spiral inwards
    activpal.data$reverse.start<-as.numeric(activpal.data$time)
    activpal.data$reverse.end<-as.numeric(activpal.data$time+activpal.data$interval)
    overlay.data$reverse.start<-as.numeric(overlay.data$time)
    overlay.data$reverse.end<-as.numeric(overlay.data$time+overlay.data$duration)

    activity_colours <- c("blue","yellow","pink","green","purple","red","grey50")
    overlay_colours <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf")
    overlay_colours <- overlay_colours[c(1:length(levels(overlay.data$group)))]

    colour_categories <- c(overlay_title,levels(overlay.data$group),"Activity",levels(activpal.data$category))
    colour_values <- c("white",overlay_colours,"white",activity_colours)

    plot.dual.linear(activpal.data, overlay.data, file.name, out.path,colour_categories,colour_values)
    plot.dual.spiral(activpal.data, overlay.data, file.name, out.path,colour_categories,colour_values)
  }

plot.dual.linear<-
  function(activpal.data,overlay.data,file.name,out.path.linear,colour_categories,colour_values){
    #' @import ggplot2
    #' @importFrom graphics text
    activpal.data$date <- as.Date(activpal.data$time)
    overlay.data$date <- as.Date(overlay.data$time)

    # Find the minimum and maximum dates in the data to allocate correct plotting space
    min_date<-min(c(activpal.data$date,overlay.data$date))
    max_date<-max(c(activpal.data$date,overlay.data$date))

    # Calculate the start and end time that is being charted
    min_date_label<-min(c(activpal.data$time,overlay.data$time))
    max_date_label<-max(c(activpal.data$time,overlay.data$time))
    # Generate the labels for the y-axis from the minimum and maximum datetimes that the data covers
    y_label<-c(format(seq(as.Date(min_date_label,tz="UTC"),as.Date(max_date_label,tz="UTC"),"1 day"),"%b %e"),"")

    # Produce a linear graph for the stepping information
    if (are.levels.numeric(overlay.data,which(colnames(overlay.data) == "group"))){
      overlay.data <- add.numeric.levels(overlay.data)
      colour_categories <- c(colour_categories[1],levels(overlay.data$label),
                             colour_categories[c((length(colour_categories)-7):length(colour_categories))])
      colour_values <- c(colour_values[1],
                         palette(gray(seq(.8,.2,len = length(levels(overlay.data$label))))),
                         colour_values[c((length(colour_values)-7):length(colour_values))])
      overlay.data$group <- as.numeric(levels(overlay.data$group))[overlay.data$group]
      overlay.data$group <- overlay.data$group / max(overlay.data$group) * 0.25

      spiral<-ggplot2::ggplot()+
        ggplot2::geom_rect(data=activpal.data, ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$interval,
                                                            ymin=.data$date, ymax=.data$date+0.35,
                                                            color=.data$category,fill=.data$category),size=0.1) +

        ggplot2::geom_rect(data=overlay.data,ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$duration,
                                                          ymin=.data$date+0.35, ymax=.data$date+0.4+.data$group,
                                                          color=.data$label,fill=.data$label),size=0.1) +
        ggplot2::ggtitle(paste(min_date,"to",max_date)) +
        ggplot2::scale_x_continuous(limits=c(0,86400), breaks=seq(0,82800,3600), minor_breaks=seq(0,86400,3600),
                                    labels=paste(rep(c(12,1:11),2), rep(c("AM","PM"),each=12))) +
        ggplot2::scale_y_date(limits=c(min_date,max_date+1),
                              breaks=seq(min_date, max_date+1,1), labels=y_label) +
        ggplot2::scale_fill_manual(name="",limits=colour_categories, values=colour_values) +
        ggplot2::scale_colour_manual(name="",limits=colour_categories, values=colour_values,guide=FALSE) +
        ggplot2::theme_bw(base_size=10) +
        ggplot2::labs(x="Hour",y="Day",color="Orientation") +
        ggplot2::theme(panel.grid.minor.x=ggplot2::element_line(colour="grey60", size=0.3),
                       legend.text=ggplot2::element_text(size=12),
                       axis.text.y=ggplot2::element_text(size=12))
    }else{
      spiral<-ggplot2::ggplot()+
        ggplot2::geom_rect(data=activpal.data, ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$interval,
                                                            ymin=.data$date, ymax=.data$date+0.35,
                                                            color=.data$category,fill=.data$category),size=0.1) +
        ggplot2::geom_rect(data=overlay.data,ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$duration,
                                                          ymin=date+0.35, ymax=date+0.7,
                                                          color=.data$group,fill=.data$group),size=0.1) +
        ggplot2::ggtitle(paste(min_date,"to",max_date)) +
        ggplot2::scale_x_continuous(limits=c(0,86400), breaks=seq(0,82800,3600), minor_breaks=seq(0,86400,3600),
                                    labels=paste(rep(c(12,1:11),2), rep(c("AM","PM"),each=12))) +
        ggplot2::scale_y_date(limits=c(min_date,max_date+1),
                              breaks=seq(min_date, max_date+1,1), labels=y_label) +
        ggplot2::scale_fill_manual(name="",limits=colour_categories, values=colour_values) +
        ggplot2::scale_colour_manual(name="",limits=colour_categories, values=colour_values,guide=FALSE) +
        ggplot2::theme_bw(base_size=10) +
        ggplot2::labs(x="Hour",y="Day",color="Orientation") +
        ggplot2::theme(panel.grid.minor.x=ggplot2::element_line(colour="grey60", size=0.3),
                       legend.text=ggplot2::element_text(size=12),
                       axis.text.y=ggplot2::element_text(size=12))
    }

    # Outputs the spiral chart to a png file
    ggplot2::ggsave(paste(out.path.linear,file.name,"_linear.png",sep=""), plot = spiral, height = 8, width = 15)
  }

plot.dual.spiral<-
  function(activpal.data,overlay.data,file.name,out.path.spiral,colour_categories,colour_values){
    # Find the minimum and maximum dates in the data to allocate correct plotting space
    min_date<-min(c(activpal.data$reverse.start,overlay.data$reverse.start))
    max_date<-max(c(activpal.data$reverse.end,overlay.data$reverse.end))

    # Calculate the offset to ensure the spiral does not terminate in the centre of the plot to reduce plotting distortions
    central_space<-max(172800,floor((max_date-min_date)*3/4))

    # Produce a spiral graph for the stepping information.
    # This is achieve by producing a cartesian bar plot of the stepping information and converting
    # the coordinate system to polar coordinates to produce the spiral effect.
    # The cartesian version of the chart can be generated by commenting out the coord_polar() command
    message("Generating spiral chart.  This action may take some time.")
    if (are.levels.numeric(overlay.data,which(colnames(overlay.data) == "group"))){
      overlay.data <- add.numeric.levels(overlay.data)
      colour_categories <- c(colour_categories[1],levels(overlay.data$label),
                             colour_categories[c((length(colour_categories)-7):length(colour_categories))])
      colour_values <- c(colour_values[1],
                         palette(gray(seq(.8,.2,len = length(levels(overlay.data$label))))),
                         colour_values[c((length(colour_values)-7):length(colour_values))])
      overlay.data$group <- as.numeric(levels(overlay.data$group))[overlay.data$group]
      overlay.data$group <- 40000 - overlay.data$group / max(overlay.data$group) * 30000

      spiral<-ggplot2::ggplot() +
        ggplot2::geom_rect(data=activpal.data, ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$interval,
                                                            ymin=.data$reverse.start+50001, ymax=.data$reverse.start+78000,
                                                            color=.data$category,fill=.data$category),size=0.1) +
        ggplot2::geom_rect(data=overlay.data,ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$duration,
                                                          ymin=.data$reverse.start+50000, ymax=.data$reverse.start+.data$group,
                                                          color = .data$label, fill = .data$label),size=0.1) +
        ggplot2::ggtitle(paste(as.Date(as.POSIXct(min_date,origin="1970-01-01")),"to",as.Date(as.POSIXct(max_date,origin="1970-01-01")))) +
        ggplot2::scale_x_continuous(limits=c(0,86400), breaks=seq(0,82800,3600), minor_breaks=seq(0,86400,3600),
                                    labels=paste0(rep(c(12,1:11),2), rep(c("AM","PM"),each=12))) +
        ggplot2::scale_y_reverse(limits=c(max_date,min_date) + c(central_space,0),
                                 breaks=seq(min_date, max_date,86400), labels=NULL) +
        ggplot2::scale_fill_manual(name = "",limits=colour_categories, values = colour_values) +
        ggplot2::scale_colour_manual(name = "",limits=colour_categories, values = colour_values,guide=FALSE) +
        ggplot2::coord_polar() +
        ggplot2::theme_bw(base_size=10) +
        ggplot2::theme(panel.grid.minor.x=ggplot2::element_line(colour="grey60", size=0.3),
                       legend.text=ggplot2::element_text(size=12),
                       axis.text.y=ggplot2::element_text(size=12))
    }else{
      spiral<-ggplot2::ggplot() +
        ggplot2::geom_rect(data=activpal.data, ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$interval,
                                                            ymin=.data$reverse.start+50001, ymax=.data$reverse.start+78000,
                                                            color=.data$category,fill=.data$category),size=0.1) +
        ggplot2::geom_rect(data=overlay.data,ggplot2::aes(xmin=as.numeric(.data$DayTime), xmax=as.numeric(.data$DayTime)+.data$duration,
                                                          ymin=.data$reverse.start+50000, ymax=.data$reverse.start+22000,
                                                          color=.data$group, fill = .data$group),size=0.1) +
        ggplot2::ggtitle(paste(as.Date(as.POSIXct(min_date,origin="1970-01-01")),"to",as.Date(as.POSIXct(max_date,origin="1970-01-01")))) +
        ggplot2::scale_x_continuous(limits=c(0,86400), breaks=seq(0,82800,3600), minor_breaks=seq(0,86400,3600),
                                    labels=paste0(rep(c(12,1:11),2), rep(c("AM","PM"),each=12))) +
        ggplot2::scale_y_reverse(limits=c(max_date,min_date) + c(central_space,0),
                                 breaks=seq(min_date, max_date,86400), labels=NULL) +
        ggplot2::scale_fill_manual(name = "",limits=colour_categories, values = colour_values) +
        ggplot2::scale_colour_manual(name = "",limits=colour_categories, values = colour_values,guide=FALSE) +
        ggplot2::coord_polar() +
        ggplot2::theme_bw(base_size=10) +
        ggplot2::theme(panel.grid.minor.x=ggplot2::element_line(colour="grey60", size=0.3),
                       legend.text=ggplot2::element_text(size=12),
                       axis.text.y=ggplot2::element_text(size=12))
    }

    # Outputs the spiral chart to a png file
    ggplot2::ggsave(paste(out.path.spiral,file.name,"_spiral.png",sep=""), plot = spiral, height = 10, width = 10)
  }

characterise.activity<-
  function(data){
    # Takes in a processed activpal activity file and adds a column classifying the activity by activity.code
    # Returns a data.frame with following columns time, interval, activity, cumulative_steps, steps, category
    process.data<-data
    process.data$category<-(-1)
    process.data[which(process.data$activity==0),6]<-"sedentary"
    process.data[which(process.data$activity==1),6]<-"standing"
    process.data[which(process.data$activity==2),6]<-"stepping"
    process.data[which(process.data$activity %in% c(3,3.1,3.2)),6]<-"lying"
    process.data[which(process.data$activity==4),6]<-"non-wear"
    process.data[which(process.data$activity==5),6]<-"cycling"
    process.data[which(process.data$activity==6),6]<-"travel"

    process.data$category <- factor(process.data$category, levels = c("lying","sedentary","travel","standing","cycling","stepping","non-wear"))

    return(process.data)
  }

split.events.file<-
  function(data,duration.pos,duration=900){
    # Splits any activpal events with duration greater than the threshold into a number of events covering the event duration
    # Any produced event will have maximum length equaling a specified length
    process.data<-data
    # Find all the events that have duration greater than the specified length
    to.split<-which(process.data[,duration.pos]>duration)
    for (i in to.split){
      # Calculate the duration of the event
      start.time<-process.data[i,1]
      end.time<-process.data[i,1]+process.data[i,duration.pos]
      time.diff<-as.numeric(difftime(end.time,start.time,units="secs"))

      # Create a data.frame with the number of events needed to replicate the original event, but with duration less than the specified limit.
      # Assigns the correct start time for each of the new events
      to.add<-data.frame(seq(start.time,end.time,duration)); colnames(to.add)<-"time"

      # Assign the duration of each event to the specified maximum event length
      to.add$interval<-duration
      # Copy the information for each event from the original event
      to.add$activity<-process.data[i,3]
      to.add$cumulative_steps<-process.data[i,4]
      to.add$steps<-process.data[i,5]
      to.add$category<-process.data[i,6]
      # Calculate the actual duration of the final event so that the end of this event coincides with the end of the original event
      to.add[nrow(to.add),2]<- time.diff %% duration
      process.data<-rbind(process.data,to.add)
    }
    # Remove the original events to avoid duplication
    process.data<-process.data[which(process.data$interval<=duration),]
    process.data<-process.data[order(process.data$time),]
    rownames(process.data) <- (1:nrow(process.data))
    return(process.data)
  }

split.overlay<-
  function(data,duration.pos,duration=900){
    events.to.split <- which(data$duration > duration)
    for (i in events.to.split){
      copies <- ceiling(data[i,]$duration / duration)
      new.events <- data[rep(i,copies),]
      time_to_add <- seq(0,data[i,]$duration,duration)
      time_to_add <- time_to_add[1:nrow(new.events)]
      new.events$time <- new.events$time + time_to_add
      new.events$duration <- duration
      new.events[nrow(new.events),]$duration <- data[i,]$duration %% duration
      if(new.events[nrow(new.events),]$duration == 0){
        new.events[nrow(new.events),]$duration <- duration
      }
      data <- rbind(data,new.events)
    }
    data <- data[-events.to.split,]
    return(data)
  }

process.overlay.file <-
  #' @importFrom  methods is
  function(data){
    if (nrow(data)==0){
      return (NULL)
    }
    if(is(data[[1,1]],"POSIXct") & is(data[[1,2]],"POSIXct") ){
      # First two columns are event start time and end time
      colnames(data)[1:2] <- c("time","end_time")
      data$duration <- as.numeric(difftime(data$end_time,data$time,units = "secs"))
      data <- data[,c(1,ncol(data),3:(ncol(data)-1))]
    }else if(is(data[[1,1]],"POSIXct") & is(data[[1,2]],"numeric")){
      # First two columns are event start time and event duration
      colnames(data)[1:2] <- c("time","duration")
    }
    data$end_period <- (as.numeric(data$time) %% 86400) + data$duration
    to_split <- which(data$end_period > 86400)

    pre_split <- data[to_split,]
    pre_split$duration <- 86400 - (as.numeric(pre_split$time) %% 86400)

    post_split <- data[to_split,]
    post_split$duration <- post_split$end_period - 84000
    post_split$time <- post_split$time + (86400 - as.numeric(post_split$time) %% 86400)

    if(length(to_split) > 0){
      data <- data[-to_split,]
      data <- rbind(data,pre_split,post_split)
    }
    data <- data[order(data$time),1:3]
    rownames(data) <- 1:nrow(data)
    return(data)
  }

are.levels.numeric <-
  function(data, col_pos){
    # tests if the levels of a given column (col_pos) of a data.frame are numeric
    col_levels <- levels(data[,col_pos])

    tryCatch({
      col_levels <- as.numeric(col_levels)
      return(TRUE)
    }, warning = function(war) {
      return(FALSE)
    }, finally = {
    })
  }

add.numeric.levels <-
  function(data){
    group_values <- as.numeric(levels(data$group))[data$group]
    categories <- unique(c(min(group_values),
                           round(as.numeric(quantile(group_values,c(0.1,0.3,0.5,0.7,0.9))),1),
                           max(group_values)))
    lower <- head(categories,-1)
    upper <- tail(categories,-1)
    label <- paste(lower, " - ", upper, sep = "")
    groupings <- data.frame(label,lower,upper)
    groupings$label <- factor(groupings$label,levels = groupings[order(groupings$lower),]$label)
    data$label <- levels(groupings$label)[length(levels(groupings$label))]

    for (i in (1:nrow(groupings))){
      locations_to_add <- which(group_values >= groupings[i,]$lower & group_values < groupings[i,]$upper)
      if(length(locations_to_add) > 0){
        data[locations_to_add,]$label <- levels(groupings[i,]$label)[groupings[i,]$label]
      }
    }
    data$label <- factor(data$label,levels = groupings[order(groupings$lower),]$label)
    return(data)
  }
