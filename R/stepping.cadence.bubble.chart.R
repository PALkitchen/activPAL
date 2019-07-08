daily.stepping.activity.bubble.chart <-
  function(input_folder,output_folder){
    #' Processes a folder of events files to output bubble charts showing the daily
    #' distribution of stepping cadences
    #' @description Processes a folder of events files to output bubble charts showing
    #'     the daily distribution of stepping cadences.
    #'     A bubble chart shows daily patterns in stepping behaviour.  For each
    #'     valid day of activity data a bubble is plotted on the chart.  The
    #'     centre of bubble indicates the weighted median cadence of all stepping
    #'     bouts of duration between 10 seconds and one minute (x-axis position)
    #'     and the weighted median cadence of all stepping bouts of duration greater
    #'     than or equal to one minute (y-axis position).  The height of a bubble
    #'     indicates the weighted upper quartile  (y - max) and lower quartile
    #'     (y - min) cadence across the stepping bouts of duration greater than or
    #'     equal to one minute occurring during the given day.  The width of a bubble
    #'     indicates the weighted upper quartile  (x - right) and lower quartile
    #'     (x - left) cadence across the stepping bouts of duration between 10 seconds
    #'     and one minute occurring during the given day.
    #'     Each bubble chart is saved as a .png image.
    #' @param input_folder The filepath for the folder where the events files to be processed are saved
    #' @param output_folder The filepath for the folder where the bubble charts are to be saved

    #' @export
    #' @examples input_folder <- paste(system.file("extdata", "", package = "activPAL"),"/",sep="")
    #' output_folder <- paste(tempdir(),"/",sep="")
    #'
    #' activPAL::daily.stepping.activity.bubble.chart(input_folder,output_folder)

    if(!valid.folder.path(input_folder)){
      stop("A valid folder to search for events files has not been provided.")
    }
    if(!valid.folder.path(output_folder)){
      stop("A valid folder to save the generated output has not been provided.")
    }

    file_list <- list.files(input_folder,pattern = "Events*.csv")
    for (i in (1:length(file_list))){
      bubble_chart_data <- pre.process.events.file(file_list[i],input_folder)
      daily.stepping.activity.bubble.chart.file(bubble_chart_data,output_folder,file_list[i])
    }
  }

daily.stepping.activity.bubble.chart.file <-
  function(events_file,output_folder,file_name){
    # Draw a bubble chart showing the daily distribution of stepping cadences
    file_id <- substr(file_name,1,gregexpr("Event",file_name)[[1]][1]-1)

    print(paste("Processing file: ",file_id,sep=""))

    chart_data <- activpal.produce.bubble.chart.data(events_file)
    if(!is.null(chart_data)){
      activpal.bubble.chart(data = chart_data, output_folder = output_folder, title = file_id,
                            xlab_text = "cadence (bout duration - 10s to 60s)",
                            ylab_text = "cadence (bout duration - 1 minute +)")
    }
  }

activpal.produce.bubble.chart.data <-
  function(events_file){
    # Process an events file and generate a dataframe that can be used to generate a bubble chart
    # Each bubble has the following dimensions
    # Centre (x - weighted median cadence of bouts - 10s to 60s, y - weighted median cadence of bouts - 60s +)
    # Top - weighted upper quartile cadence of bouts - 60s +
    # Bottom - weighted lower quartile cadence of bouts - 60s +
    # Left - weighted lower quartile cadence of bouts - 10s to 60s
    # Right - weighted upper quartile cadence of bouts - 10s to 60
    if(nrow(events_file) == 0){
      return(NULL)
    }
    events_file <- events_file[which(events_file$activity == 2),]
    if(nrow(events_file) == 0){
      return(NULL)
    }
    events_file$date <- as.Date(events_file$time)
    events_file$cadence <- events_file$steps / (events_file$interval / 60)

    # Calculate the weighted median / upper quartile / lower quartile cadence
    # of stepping longer than 1 minute and shorter than 1 minute
    under_60s_summary <- events_file %>% dplyr::filter(.data$interval >= 10 & .data$interval < 60) %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(x = weighted.median(.data$cadence,.data$interval),
                       xleft = weighted.quantile(.data$cadence,.data$interval,0.25),
                       xright = weighted.quantile(.data$cadence,.data$interval,0.75))
    over_60s_summary <- events_file %>% dplyr::filter(.data$interval >= 60) %>%
      dplyr::group_by(.data$date) %>%
      dplyr::summarise(y = weighted.median(.data$cadence,.data$interval),
                       ybottom = weighted.quantile(.data$cadence,.data$interval,0.25),
                       ytop = weighted.quantile(.data$cadence,.data$interval,0.75))

    summary_data <- events_file %>%
      dplyr::filter(.data$interval >= 10) %>%
      dplyr::group_by(.data$date) %>%
      dplyr::select(date) %>%
      dplyr::distinct()

    chart_data <- merge(summary_data,under_60s_summary,all.x = TRUE)
    chart_data <- merge(chart_data,over_60s_summary,all.x = TRUE)

    chart_data <- tidyr::replace_na(chart_data, list(x = 0, xleft = 0, xright = 0,
                                                     y = 0, ybottom = 0, ytop = 0))
    # Map the day of the week to weekday / weekend
    chart_data$group <- format(chart_data$date,"%a")
    chart_data[which(chart_data$group %in% c("Mon","Tue","Wed","Thu","Fri")),]$group <- rep("Weekday",length(which(chart_data$group %in% c("Mon","Tue","Wed","Thu","Fri"))))
    chart_data[which(chart_data$group %in% c("Sat","Sun")),]$group <- rep("Weekend",length(which(chart_data$group %in% c("Sat","Sun"))))

    return (chart_data)
  }

activpal.bubble.chart <-
  function(data,output_folder,title="output",xlab_text="",ylab_text="",group_colour = NULL){
    # takes a data.frame containing the following columns and generates a bubble chart
    # x, y, xleft, ytop
    # it may also contain the following columns
    # xright, ybottom, group
    # scale - scaling factor for xleft / ytop / xright / ybottom
    #' @importFrom grDevices graphics.off png col2rgb

    if(nrow(data) == 0){
      warning(paste("File: ",title," has no valid days of stepping data. No bubble chart has been generated",sep=""))
      return(NULL)
    }

    col_names <- c("x","y","xleft","ytop")
    if(length(which(colnames(data) %in% col_names))!=4){
      return (NULL)
    }
    x_col <- which(colnames(data) == "x")
    y_col <- which(colnames(data) == "y")
    xleft_col <- which(colnames(data) == "xleft")
    ytop_col <- which(colnames(data) == "ytop")
    chart_table <- data[,c(x_col,y_col,xleft_col,ytop_col)]
    if(length(which(colnames(data) == "xright")) == 0){
      chart_table$xright <- chart_table$xleft * scale
    }else{
      chart_table$xright <- data[,which(colnames(data) == "xright")]
    }

    if(length(which(colnames(data) == "ybottom")) == 0){
      chart_table$ybottom <- chart_table$ytop * scale
    }else{
      chart_table$ybottom <- data[,which(colnames(data) == "ybottom")]
    }

    if(length(which(colnames(data) == "group")) == 0){
      chart_table$group <- ""
    }else{
      chart_table$group <- data[,which(colnames(data) == "group")]
      chart_table$group <- as.character(chart_table$group)
    }

    ellipses <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(ellipses) <- c("x","y","id","group")
    for (i in (1:nrow(chart_table))){
      single_ellipse <- distorted.ellipse(chart_table[i,]$x,chart_table[i,]$y,chart_table[i,]$xleft,
                                          chart_table[i,]$xright,chart_table[i,]$ytop,chart_table[i,]$ybottom)
      single_ellipse$id <- i
      single_ellipse$group <- rep(chart_table[i,]$group,nrow(single_ellipse))

      ellipses <- rbind(ellipses,single_ellipse)
    }
    colnames(ellipses)[1:2] <- c("x","y")
    rownames(ellipses) <- 1:nrow(ellipses)

    if(!is.null(group_colour) & !(FALSE %in% areColors(group_colour))){
      group_colour <- rep(group_colour,ceiling(length(unique(chart_table$group))/length(group_colour)))
      plot.data <- ggplot2::ggplot() +
        ggplot2::geom_point(data=chart_table,ggplot2::aes_string(x="x",y="y",colour = "group")) +
        ggplot2::geom_polygon(data = ellipses,ggplot2::aes_string(x="x",y="y",group="id",fill="group"),alpha = 0.25, colour = "black") +
        ggplot2::scale_fill_manual(values=group_colour)
    }else{
      plot.data <- ggplot2::ggplot() +
        ggplot2::geom_point(data=chart_table,ggplot2::aes_string(x="x",y="y",colour = "group")) +
        ggplot2::geom_polygon(data = ellipses,ggplot2::aes_string(x="x",y="y",group="id",colour="group",fill="group"),alpha = 0.25)
    }
    # Calculate common elements to the plot
    x_min <- min(ellipses$x) - min(ellipses$x) %% 10
    x_max <- max(ellipses$x) - max(ellipses$x) %% 10 + 10
    x_min <- max(0,x_min - ((x_max - x_min) / 4))
    y_min <- min(ellipses$y) - min(ellipses$y) %% 10
    y_max <- max(ellipses$y) - max(ellipses$y) %% 10 + 10
    y_min <- max(0,y_min - ((y_max - y_min) / 4))

    # Add common elements to the plot
    plot.data <- plot.data +
      ggplot2::xlab(xlab_text) +
      ggplot2::xlim(c(x_min,x_max)) +
      ggplot2::ylab(ylab_text)  +
      ggplot2::ylim(c(y_min,y_max)) +
      ggplot2::theme(legend.title = ggplot2::element_blank())

    # Build the custom legend in the bottom left of the chart
    x_right <- x_min + (x_max - x_min) / 5
    y_top <- y_min + (y_max - y_min) / 5

    legend_box <- data.frame(x=c(x_min,x_right,x_right,x_min,x_min),y=c(y_min,y_min,y_top,y_top,y_min))
    legend_point <- data.frame(x=mean(c(x_min,x_min,x_right)),y=mean(c(y_min,y_top)))
    legend_ellipse <- distorted.ellipse(x_min + (x_right - x_min)/3,
                                        y_min + (y_top - y_min)/2,
                                        x_min + (x_right - x_min)/10,
                                        x_min + (x_right - x_min)*(3/5),
                                        y_min + (y_top - y_min)*(7/8),
                                        y_min + (y_top - y_min)/3)
    colnames(legend_ellipse) <- c("x","y")
    legend_label_x <- data.frame(x = (x_min + (x_right - x_min) * 0.35),
                                 y = y_min + (y_top - y_min) / 3 - (y_top - y_min) * 0.2,
                                 text = "interquartile range for\nstepping bouts < 60s")
    legend_label_y <- data.frame(x = (x_right - (x_right - x_min) * 0.15),
                                 y = y_min + (y_top - y_min) * 0.6,
                                 text = "interquartile range for\nstepping bouts > 60s")
    legend_label_title <- data.frame(x = x_min + (x_right - x_min)/2, y = y_top - (y_top - y_min) / 15, text = "Legend")

    legend_arrow_x <- data.frame(x=c(x_min + (x_right - x_min)/10, x_min + (x_right - x_min)*(3/5)),
                                 y=c(y_min + (y_top - y_min)/3-(y_top - y_min)*0.05, y_min + (y_top - y_min)/3-(y_top - y_min)*0.05))
    legend_arrow_y <- data.frame(x=c(x_min + (x_right - x_min)*(7/10),x_min + (x_right - x_min)*(7/10)),
                                 y=c(y_min + (y_top - y_min)*(7/8),y_min + (y_top - y_min)/3))

    plot.data <- plot.data +
      ggplot2::geom_path(data = legend_box,ggplot2::aes_string(x="x",y="y")) +
      ggplot2::geom_point(data=legend_point,ggplot2::aes_string(x="x",y="y"),colour="black") +
      ggplot2::geom_polygon(data = legend_ellipse,ggplot2::aes_string(x="x",y="y"),colour="blue",fill="blue",alpha = 0.25) +
      ggplot2::geom_text(data = legend_label_title,ggplot2::aes_string(x="x",y="y",label = "text"),size = 4) +
      ggplot2::geom_text(data = legend_label_x,ggplot2::aes_string(x="x",y="y",label = "text"),size = 3) +
      ggplot2::geom_text(data = legend_label_y,ggplot2::aes_string(x="x",y="y",label = "text"),angle=270,size = 3) +
      ggplot2::geom_line(data = legend_arrow_x,ggplot2::aes_string(x="x",y="y"),arrow = ggplot2::arrow(angle = 15, ends = "both", type = "closed")) +
      ggplot2::geom_line(data = legend_arrow_y,ggplot2::aes_string(x="x",y="y"),arrow = ggplot2::arrow(angle = 15, ends = "both", type = "closed"))

    ggplot2::ggsave(filename = paste(output_folder,title,"_bubble_chart.png",sep=""),plot = plot.data, width=12, height = 10)
  }

distorted.ellipse <-
  function(centre.x,centre.y,left,right,top,bottom){
    # Calculate a series of points needed to draw a distorted ellipse
    # Returns an object containing the coordinaties of the ellipse ($x, $y)
    # @centre.x / centre.y - coordinates of the centre of the shape.
    # @left / right - x-coordinate of the leftmost and rightmost points on the ellipse
    # @top / bottom - y-coordinates of the top and bottom points on the ellipse
    major_radius <- centre.x - left
    minor_radius <- top - centre.y
    t <- seq(0,pi/2, pi/200)
    x1 <- centre.x - major_radius*cos(t)
    y1 <- centre.y + minor_radius*sin(t)

    major_radius <- right - centre.x
    minor_radius <- top - centre.y
    t <- seq(pi/2, pi, pi/200)
    x2 <- centre.x - major_radius*cos(t)
    y2 <- centre.y + minor_radius*sin(t)

    major_radius <- right - centre.x
    minor_radius <- centre.y - bottom
    t <- seq(pi, 3*pi/2, pi/200)
    x3 <- centre.x - major_radius*cos(t)
    y3 <- centre.y + minor_radius*sin(t)

    major_radius <- centre.x - left
    minor_radius <- centre.y - bottom
    t <- seq(3*pi/2, 2*pi ,pi/200)
    x4 <- centre.x - major_radius*cos(t)
    y4 <- centre.y + minor_radius*sin(t)

    x <- c(x1,x2,x3,x4)
    y <- c(y1,y2,y3,y4)
    return(as.data.frame(cbind(as.numeric(x),as.numeric(y))))
  }

areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}
