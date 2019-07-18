stepping.cadence.bands.folder.two.stepping.groups <-
  function(input_folder, output_folder,generate_charts=TRUE){
    #' Processes events files to produce histograms showing the distribution of stepping and
    #'     weighted median stepping cadence across two groups of stepping bout duration.
    #' @description Processes a folder of events files and generates a faceted set of histograms
    #'     for each events file showing the duration of stepping in different cadence bands (each
    #'     cadence band has a width of 10 steps per minute) for stepping bouts of duration 10
    #'     seconds to 1 minute and 1 minute plus.  The weighted median cadence of stepping at
    #'     each stepping bout duration is also calculated and indicated on the histogram.
    #'     The values of the weighted median cadence for each stepping duration across all the
    #'     processed events files is also returned as a data.frame.
    #' @param input_folder The filepath for the folder where the events files to be processed are saved
    #' @param output_folder The filepath for the folder where the generated files are to be saved
    #' @param generate_charts Set TRUE if stacked histograms showing the distribution of stepping
    #'     cadences are to be generated for each events file. Default = TRUE

    #' @export
    #' @examples input_folder <- paste(system.file("extdata", "", package = "activPAL"),"/",sep="")
    #' output_folder <- paste(tempdir(),"/",sep="")
    #'
    #' activPAL::stepping.cadence.bands.folder.two.stepping.groups(input_folder,output_folder,TRUE)
    #' # Omitting a value for generate_charts results in the charts being saved in the output folder.
    #' activPAL::stepping.cadence.bands.folder.two.stepping.groups(input_folder,output_folder)

    if(!valid.folder.path(input_folder)){
      stop("A valid folder to search for events files has not been provided.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    # set the minimum and maximum duration for each stepping duration
    lower_limit <- c(10,60)
    upper_limit <- c(60,86400)
    stepping.cadence.bands.folder(input_folder,lower_limit,upper_limit,output_folder,generate_charts)
  }

stepping.cadence.bands.folder.four.stepping.groups <-
  function(input_folder, output_folder,generate_charts=TRUE){
    #' Processes events files to produce histograms showing the distribution of stepping and
    #'     weighted median stepping cadence across four groups of stepping bout duration.
    #' @description Processes a folder of events files and generates a faceted set of histograms
    #'     for each events file showing the duration of stepping in different cadence bands
    #'     (each cadence band has a width of 10 steps per minute) for stepping bouts of duration
    #'     10 seconds to 1 minute, 1 minute to 5 minutes, 5 minutes to 10 minutes and 10 minutes plus.
    #'     The weighted median cadence of stepping at each stepping bout duration is also calculated
    #'     and indicated on the histogram.  The values of the weighted median cadence for each
    #'     stepping duration across all the processed events files is also returned as a data.frame.
    #' @param input_folder The filepath for the folder where the events files to be processed are saved
    #' @param output_folder The filepath for the folder where the generated files are to be saved
    #' @param generate_charts Set TRUE if stacked histograms showing the distribution of stepping cadences are to be generated for each events file

    #' @export
    #' @examples input_folder <- paste(system.file("extdata", "", package = "activPAL"),"/",sep="")
    #' output_folder <- paste(tempdir(),"/",sep="")
    #'
    #' activPAL::stepping.cadence.bands.folder.four.stepping.groups(input_folder,output_folder,TRUE)
    #' # Omitting a value for generate_charts results in the charts being saved in the output folder.
    #' activPAL::stepping.cadence.bands.folder.four.stepping.groups(input_folder,output_folder)

    if(!valid.folder.path(input_folder)){
      stop("A valid folder to search for events files has not been provided.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    # set the minimum and maximum duration for each stepping duration
    lower_limit <- c(10,60,300,600)
    upper_limit <- c(60,300,600,86400)
    stepping.cadence.bands.folder(input_folder,lower_limit,upper_limit,output_folder,generate_charts)
  }

stepping.cadence.bands.folder <-
  function(input_folder,lower_bound,upper_bound,output_folder,generate_charts=FALSE){
    # Draw a stacked histogram showing the distribution of stepping cadence and median cadence
    # for different stepping durations
    file_list <- list.files(input_folder,pattern = "Events*.csv")
    cadence_summary <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(cadence_summary) <- c("bout_duration", "weighted_median_cadence", "file_id")
    for (i in file_list){
        file_name <- substr(i,1,gregexpr("Event",i)[[1]][1]-1)
        events_data <- pre.process.events.file(i,input_folder)
        if(nrow(events_data) > 0){
          stepping_summary <- stepping.cadence.bands.file(events_data,lower_bound,upper_bound)
          median_cadence_by_group <- stepping_summary %>% dplyr::group_by(.data$group) %>%
            dplyr::summarise(median_cadence = weighted.median(.data$cadence,.data$interval))
          if(generate_charts){
            stepping.cadence.bands.generate.histogram(stepping_summary,median_cadence_by_group,output_folder,file_name)
          }
          median_cadence_by_group$file_id <- file_name
          cadence_summary <- rbind(cadence_summary,median_cadence_by_group)
        }
    }
    cadence_summary <- cadence_summary[,c(ncol(cadence_summary),1:(ncol(cadence_summary)-1))]
    cadence_summary <- tidyr::spread(cadence_summary,2,3)
    write.csv(cadence_summary,paste(output_folder,"median_cadence_summary.csv",sep=""),row.names = FALSE)
    return(cadence_summary)
  }

stepping.cadence.bands.file <-
  function(events_file,lower_bound,upper_bound){
    events_file <- events_file[which(events_file$activity == 2 & events_file$interval >= lower_bound[1]),]
    events_file$cadence <- events_file$steps / (events_file$interval / 60)
    events_file$group <- ""
    for (i in (1:length(lower_bound))){
      in_group <- which(events_file$interval >= lower_bound[i] & events_file$interval < upper_bound[i])
      if(length(in_group) > 0 ){
        # Convert the maximum and minimum duration into a label
        duration_label <- paste(lubridate::seconds_to_period(lower_bound[i]),"to",lubridate::seconds_to_period(upper_bound[i]))
        duration_label <- gsub("d"," day",duration_label)
        duration_label <- gsub("S"," seconds",duration_label)
        duration_label <- gsub("M"," minutes",duration_label)
        duration_label <- gsub("H"," hours",duration_label)
        duration_label <- gsub(" 0 seconds","",duration_label)
        duration_label <- gsub(" 0 minutes","",duration_label)
        duration_label <- gsub(" 0 hours","",duration_label)
        duration_label <- gsub(" 1 minutes"," 1 minute",duration_label)
        duration_label <- gsub(" 1 hours"," 1 hour",duration_label)
        events_file[in_group,]$group <- duration_label
      }
    }
    return(events_file)
  }

stepping.cadence.bands.generate.histogram <-
  function(events_file,median_cadence_list,output_folder,file_name){
    box_colour <- c("#d7191c","#fdae61","#ffffbf","#abd9e9","#2c7bb6")
    line_colour <- c("#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33")
    events_file$group <- factor(events_file$group,
                                levels = unique(events_file[order(events_file$interval),]$group))
    median_cadence_list$group <- factor(median_cadence_list$group,levels = levels(events_file$group))

    events_file$interval <- events_file$interval / 60
    output_file <- paste(output_folder,file_name,"-cadence-histogram.png",sep="")
    graph_data <- ggplot2::ggplot(events_file,ggplot2::aes(x = .data$cadence,fill = .data$group)) +
      ggplot2::geom_histogram(ggplot2::aes(weight = .data$interval),breaks = seq(0, 160, 10)) +
      ggplot2::geom_vline(data = median_cadence_list, ggplot2::aes(xintercept = .data$median_cadence,colour=.data$group),size = 2) +
      ggplot2::scale_x_continuous(breaks = seq(0, 160, 10)) +
      ggplot2::scale_fill_manual("Bout Duration",values = box_colour) +
      ggplot2::scale_color_manual("Bout Duration\nMedian Cadence",values = line_colour) +
      ggplot2::xlab("Stepping Cadence") +
      ggplot2::ylab("Stepping Duration (minutes)") +
      ggplot2::facet_grid(rows = vars(.data$group), scales = "fixed")
    ggplot2::ggsave(filename = output_file, plot = graph_data, width = 10, height = 8)
  }
