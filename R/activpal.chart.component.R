generate.valid.day.sequence.chart <-
  function(validation_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import dplyr
    #' @import ggplot2
    validation_data <- validation_data[which(validation_data$uid %in% individual_summary$uid),]
    validation_data$uid <- factor(validation_data$uid, levels = individual_summary$uid)
    validation_data$Date <- factor(validation_data$Date,levels = sort(unique(validation_data$Date),decreasing = TRUE))
    validation_data$height <- 1

    max_days <- chart_summary[grep("valid days",chart_summary$category),]$duration

    valid_days <- validation_data %>%
      dplyr::filter(.data$valid == "valid") %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(valid_days = n())
    valid_days$uid <- factor(valid_days$uid, levels = individual_summary$uid)

    plot_data <- ggplot(data = validation_data, ggplot2::aes(x = .data$uid, y = .data$height)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=.data$max_days, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=1, alpha=0, fill="white") +
      ggplot2::geom_col(ggplot2::aes(fill = .data$valid, group = .data$Date)) +
      ggplot2::geom_text(data = valid_days, ggplot2::aes(x = .data$uid, y = 1,
                                                         label = paste("(",.data$valid_days,")",sep="")),
                         hjust = 0, size = 3, color = "#000000") +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_fill_manual("",labels = c("invalid","valid"), values = c("#C8C8C8","#FF9800")) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,21), breaks = seq(0,21,7),
                                                  labels = rep("",4), minor_breaks = seq(0,21,1))
    } else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,max_days), breaks = seq(0,max_days,7),
                                                  minor_breaks = seq(0,max_days,1))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.uid.valid.day.chart <-
  function(validation_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import dplyr
    #' @import ggplot2
    #' @import tidyr
    validation_data <- validation_data[which(validation_data$uid %in% individual_summary$uid),]
    validation_data$uid <- factor(validation_data$uid, levels = individual_summary$uid)
    validation_data$Date <- factor(validation_data$Date,levels = sort(unique(validation_data$Date),decreasing = TRUE))
    validation_data$height <- 1
    validation_data$valid <- factor(validation_data$valid, levels = c("valid","invalid"))

    max_days <- chart_summary[grep("valid days",chart_summary$category),]$duration

    valid_days <- validation_data %>%
      dplyr::select(.data$uid, .data$valid, .data$height) %>%
      dplyr::group_by(.data$uid, .data$valid) %>%
      dplyr::summarise(days = n()) %>%
      tidyr::pivot_wider(names_from = .data$valid, values_from = .data$days, names_expand = TRUE) %>%
      tidyr::replace_na(list(invalid = 0, valid = 0)) %>%
      dplyr::mutate(total_days = .data$invalid + .data$valid)

    valid_days$uid <- factor(valid_days$uid, levels = individual_summary$uid)
    valid_days$id <- paste(valid_days$uid," (",valid_days$valid,"d)",sep ="")
    for(i in (1:nrow(valid_days))){
      if(nchar(valid_days[i,]$id) > 12){
        valid_days[i,]$id <- paste(substr(valid_days[i,]$id,1,4),
                                      "...",
                                      substr(valid_days[i,]$id,nchar(valid_days[i,]$id)-7,nchar(valid_days[i,]$id)),
                                   sep="")
      }
    }
    valid_days$id <- factor(valid_days$id, levels = unique(valid_days$id))

    plot_data <- ggplot2::ggplot(data = valid_days, aes(x = .data$id, y = 1)) +
      ggplot2::geom_point(color = "white")+
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=0, alpha=0.2, fill="grey85") +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_y_continuous(limit = c(0,1), breaks = seq(0,1,1),
                         labels = c("",""), minor_breaks = seq(0,1,1)) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.ticks = element_line(colour = "white"),
            axis.text.x = element_text(size = 7),
            panel.background = element_rect(fill = NA),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    return(plot_data)
  }

generate.lying.chart <-
  function(chart_data, chart_summary, bouts_breaks_data, individual_summary, standard_scale = FALSE, include_labels = TRUE, simplified_colors = FALSE){
    #' @import dplyr
    #' @import ggplot2
    chart_element <- chart_data[grep("Time in Bed",chart_data$category),]
    max_val <- chart_summary[grep("Time in Bed",chart_summary$category),]$duration + (chart_summary[grep("Time in Bed",chart_summary$category),]$duration %% 2)

    if(nrow(chart_element) == 0){
      chart_element <- bouts_breaks_data[,c(1,3,4,6)]
      chart_element$bout_length <- 0
      chart_element$bout_duration <- 0
      chart_element$bouts <- 0
      chart_element$dates <- chart_element$valid_days
      chart_element$category <- "Time in Bed"
      chart_element$uid <- factor(chart_element$uid, levels = individual_summary$uid)
    }else{
      chart_element <- dplyr::inner_join(chart_element,bouts_breaks_data[,c(1,4,6)], by = "uid")
      chart_element$uid <- factor(chart_element$uid, levels = individual_summary$uid)
    }

    plot_data <- ggplot2::ggplot(data = chart_element, ggplot2::aes(x = .data$uid, y = .data$bout_duration)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=max_val, alpha=0.2, fill="grey85") +
      ggplot2::geom_col(fill = "#009EE2") +
      ggplot2::geom_text(ggplot2::aes(y = 0, label = paste("[",round(.data$breaks_per_day, 0),"]",sep="")), size = 3, hjust = 0, color = "white") +
      ggplot2::xlab("") +
      ggplot2::ylab("time (h)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    if(standard_scale){
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limit = c(0,24), breaks = seq(0,24,2), labels = seq(0,24,2))
    } else {
      if(max_val > 16){
        plot_data <- plot_data +
          ggplot2::scale_y_continuous(limits = c(0,max_val),
                                      breaks = seq(0,max_val,4),
                                      minor_breaks = seq(0,max_val,1),
                                      labels = seq(0,max_val,4))
      }else{
        plot_data <- plot_data +
          ggplot2::scale_y_continuous(limits = c(0,max_val),
                                      breaks = seq(0,max_val,2),
                                      minor_breaks = seq(0,max_val,1),
                                      labels = seq(0,max_val,2))
      }
    }
    return(plot_data)
  }

generate.sedentary.standing.chart <-
  function(chart_data, chart_summary, bouts_breaks_data, individual_summary, standard_scale = FALSE, include_labels = TRUE, simplified_colors = FALSE){
    #' @import dplyr
    #' @import ggplot2
    activity_group <- c("Sedentary (4 hours +)", "Sedentary (2 - 4 hours)", "Sedentary (1 - 2 hours)", "Sedentary (30 min - 1 hour)", "Sedentary (< 30 min)",
                        "Stepping (1 minute +)","Stepping (< 1 minute)","Quiet Standing")

    activity_colour <- c("#c17100","#f18c00","#ffa700","#ffc451","#f2f200",
                         "#CB181D","#FEE5D9","#38D305")
    chart_data$sub_category <- as.character(chart_data$bout_length)
    chart_data$sub_category <- factor(chart_data$sub_category, levels = activity_group)

    sedentary_element <- chart_data[grep("Sedentary",chart_data$sub_category),] %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(sedentary_duration = sum(.data$bout_duration))
    activity_element <- chart_data[grep("Standing|Sedentary|Stepping",chart_data$sub_category),] %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(activity_duration = sum(.data$bout_duration))
    activity_element <- dplyr::inner_join(activity_element, sedentary_element, by = "uid")
    activity_element$sedentary_index <- activity_element$sedentary_duration / activity_element$activity_duration
    activity_element$sedentary_index <- round(activity_element$sedentary_index, 2)
    activity_element$uid <- factor(activity_element$uid, levels = individual_summary$uid)

    chart_element <- chart_data[grep("Standing|Sedentary|Stepping",chart_data$sub_category),]
    chart_element[grep("Sedentary",chart_element$category),]$bout_duration <- - (chart_element[grep("Sedentary",chart_element$category),]$bout_duration)

    chart_element <- dplyr::inner_join(chart_element,bouts_breaks_data[,c(1,4,6)], by = "uid")
    chart_element$uid <- factor(chart_element$uid, levels = individual_summary$uid)

    if(standard_scale){
      lower_val <- (-24)
      upper_val <- 24
    } else {
      lower_val <- -(chart_summary[grep("Sedentary",chart_summary$category),]$duration +
        (chart_summary[grep("Sedentary",chart_summary$category),]$duration %% 2))
      upper_val <- chart_summary[grep("Standing",chart_summary$category),]$duration +
        (chart_summary[grep("Standing",chart_summary$category),]$duration %% 2)
    }

    plot_data <- ggplot2::ggplot(data = chart_element, ggplot2::aes(x = .data$uid)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=lower_val, ymax=upper_val, alpha=0.2, fill="grey85") +
      ggplot2::geom_col(ggplot2::aes(y = .data$bout_duration, fill = .data$sub_category)) +
      ggplot2::geom_text(ggplot2::aes(y = -2, label = paste("[",round(.data$sedentary_bout_per_day, 0),"]",sep="")), hjust = 0.5, size = 2.75, col = "black") +
      ggplot2::xlab("") +
      ggplot2::ylab("time (h)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    plot_data <- plot_data + ggplot2::scale_fill_manual("activity classification",labels = activity_group, values = activity_colour, drop = FALSE)

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    if(standard_scale){
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limit = c(-24,24), breaks = seq(-24,24,2), labels = c(seq(24,0,-2),seq(2,24,2)))
    } else {
      if((upper_val - lower_val) <= 30){
        plot_data <- plot_data +
          ggplot2::scale_y_continuous(limits = c(lower_val, upper_val),
                             breaks = seq(lower_val, upper_val, 2), labels = c(seq(-lower_val, 0, -2),seq(2,upper_val,2)),
                             minor_breaks = seq(lower_val, upper_val, 1))
      } else {
        plot_data <- plot_data +
          ggplot2::scale_y_continuous(limits = c(lower_val, upper_val),
                             breaks = c(rev(seq(0,lower_val,-4)),tail(seq(0,upper_val,4),-1)),
                             labels = c(-rev(seq(0,lower_val,-4)),tail(seq(0,upper_val,4),-1)),
                             minor_breaks = seq(lower_val, upper_val, 1))
      }
    }
    return(plot_data)
  }

generate.sedentary.index.chart <-
  function(chart_data, chart_summary, bouts_breaks_data, individual_summary, standard_scale = FALSE, include_labels = TRUE, simplified_colors = FALSE){
    #' @import dplyr
    #' @import ggplot2
    sedentary_element <- chart_data[grep("Sedentary",chart_data$category),] %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(sedentary_duration = sum(.data$bout_duration))
    activity_element <- chart_data[grep("Standing|Sedentary|Stepping",chart_data$category),] %>%
      dplyr::group_by(.data$uid) %>% dplyr::summarise(activity_duration = sum(.data$bout_duration))
    activity_element <- dplyr::inner_join(activity_element, sedentary_element, by = "uid")
    activity_element$sedentary_index <- activity_element$sedentary_duration / activity_element$activity_duration
    activity_element$sedentary_index <- round(activity_element$sedentary_index, 2)
    activity_element$uid <- factor(activity_element$uid, levels = individual_summary$uid)

    chart_element <- chart_data[grep("Standing|Sedentary|Stepping",chart_data$category),]
    chart_element[grep("Sedentary",chart_element$category),]$bout_duration <- - (chart_element[grep("Sedentary",chart_element$category),]$bout_duration)

    chart_element <- dplyr::inner_join(chart_element,bouts_breaks_data[,c(1,4,6)], by = "uid")
    chart_element <- dplyr::inner_join(chart_element, activity_element, by = "uid")
    chart_element$uid <- factor(chart_element$uid, levels = individual_summary$uid)

    if(standard_scale){
      lower_val <- (-24)
      upper_val <- 24
    } else {
      lower_val <- -(chart_summary[grep("Sedentary",chart_summary$category),]$duration +
                       (chart_summary[grep("Sedentary",chart_summary$category),]$duration %% 2))
      upper_val <- chart_summary[grep("Standing",chart_summary$category),]$duration +
        (chart_summary[grep("Standing",chart_summary$category),]$duration %% 2)
    }

    plot_data <- ggplot2::ggplot(data = chart_element, ggplot2::aes(x = .data$uid, y = 1.5)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=3, alpha=0.2, fill="white") +
      ggplot2::geom_text(ggplot2::aes(label = paste(.data$sedentary_index * 100, "%", sep = "")), hjust = 0.5, size = 3, col = "black") +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_y_continuous(limits = c(0,3), breaks = c(0,3), labels = c("","")) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.spacing = unit(1, "lines"),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())

    return(plot_data)
  }

########################

generate.daily.stepping.summary.chart <-
  function(daily_stepping_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import dplyr
    #' @import ggplot2
    daily_stepping_data$uid <- factor(daily_stepping_data$uid, levels = individual_summary$uid)
    stepping_max <- chart_summary[grep("daily steps",chart_summary$category),]$duration
    stepping_max <- max(stepping_max,10000)

    mean_valid_days <- daily_stepping_data %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::tally() %>%
      dplyr::summarise(total = mean(.data$n))

    plot_data <- ggplot2::ggplot(data = daily_stepping_data, aes(x = .data$uid, y = .data$steps)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=stepping_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=10000, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(color = "black", fill = grDevices::rgb(0.25,0.25,0.25,0.25), shape = 21, size = 2, stroke = 0.5) +
      ggplot2::xlab("") +
      ggplot2::ylab("steps") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,30000), breaks = seq(0,30000,10000), minor_breaks = seq(0,30000,2500))
    } else{
      break_size <- 5000
      if(stepping_max > 30000){
        break_size <- 10000
      }
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,stepping_max),
                                                  labels = paste(seq(0,stepping_max,break_size)/1000,"k",sep = ""),
                                                  breaks = seq(0,stepping_max,break_size),
                                                  minor_breaks = seq(0,stepping_max,1000))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.indoor.walking.chart <-
  function(mvpa_data, chart_summary, individual_summary){
    #' @import ggplot2
    mvpa_data$uid <- factor(mvpa_data$uid, levels = individual_summary$uid)

    plot_data <- ggplot2::ggplot(data = mvpa_data, ggplot2::aes(x = .data$uid, y = 1.5)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=3, alpha=0.2, fill="white") +
      ggplot2::geom_text(aes(label = paste(round(.data$short_percent,0), "%", sep="")), size = 3, hjust = 0.5) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_y_continuous(limit = c(0,3), breaks = seq(0,3,3),
                         labels = c("",""), minor_breaks = seq(0,3,3)) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = NA),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")
    return(plot_data)
  }

generate.stepping.intensity.chart <-
  function(mvpa_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import ggplot2
    mvpa_data$uid <- factor(mvpa_data$uid, levels = individual_summary$uid)
    mvpa_data$category <- factor(mvpa_data$category, levels = c("VPA (> 125 spm)","MVPA (100 - 125 spm)",
                                                                "MPA (75 - 100 spm)","LPA (< 75 spm)"))

    mvpa_data$time <- abs(mvpa_data$time)

    stepping_max <- max(1,chart_summary[which(chart_summary$category %in% c("long","short")),]$duration) * 60
    stepping_max <- stepping_max - (stepping_max %% 20) + 20

    plot_data <- ggplot2::ggplot(data = mvpa_data, ggplot2::aes(x = .data$uid, y = .data$time)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=0, alpha=0.2, fill="grey85") +
      ggplot2::geom_col(ggplot2::aes(fill = .data$category), position = "stack")
    plot_data <- plot_data +
      ggplot2::scale_fill_manual(values = c("#ff2121","#ffa526","#ffcc80","#e6ed9d"), drop = FALSE) +
      ggplot2::ylab("time (h)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,120),
                                                  breaks = seq(0,120,60), labels = seq(0,2,1),
                                                  minor_breaks = seq(0,120,20))
    } else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,stepping_max),
                                                  breaks = seq(0,stepping_max,60), labels = seq(0,(stepping_max/60),1),
                                                  minor_breaks = seq(0,stepping_max,20))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.travel.chart <-
  function(chart_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE, simplified_colors = FALSE){
    #' @import ggplot2
    activity_group <- c("Active_Walking","Cycling","Seated_Transport")

    activity_colour <- c("#CB181D","#7916B1","#F4858D")

    chart_data[grep("Seated_Transport",chart_data$bout_length),]$bout_duration <- - (chart_data[grep("Seated_Transport",chart_data$bout_length),]$bout_duration)

    chart_data$uid <- factor(chart_data$uid, levels = individual_summary$uid)

    if(standard_scale){
      lower_val <- (-24)
      upper_val <- 24
    } else {
      lower_val <- -floor(chart_summary[grep("seated transport",chart_summary$category),]$duration)
      upper_val <- floor(chart_summary[grep("active travel",chart_summary$category),]$duration)
    }

    plot_data <- ggplot2::ggplot(data = chart_data, ggplot2::aes(x = .data$uid)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=lower_val, ymax=upper_val, alpha=0.2, fill="grey85") +
      ggplot2::geom_col(aes(y = .data$bout_duration, fill = .data$bout_length)) +
      ggplot2::xlab("") +
      ggplot2::ylab("time (h)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_fill_manual(values = activity_colour, drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    if(standard_scale){
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limit = c(-24,24), breaks = seq(-24,24,2), labels = c(seq(24,0,-2),seq(2,24,2)))
    } else {
      if((upper_val - lower_val) >= 8){
        plot_data <- plot_data +
          ggplot2::scale_y_continuous(limits = c(lower_val - 0.1, upper_val + 0.1),
                             breaks = c(rev(seq(0,lower_val,-2)),tail(seq(0,upper_val,2),-1)),
                             minor_breaks = seq(lower_val, upper_val, 1),
                             labels = c(-rev(seq(0,lower_val,-2)),tail(seq(0,upper_val,2),-1)))
      }else{
        plot_data <- plot_data +
          ggplot2::scale_y_continuous(limits = c(lower_val - 0.1, upper_val + 0.1),
                             breaks = seq(lower_val, upper_val, 1), labels = c(seq(-lower_val, 0, -1),seq(1,upper_val,1)))
      }
    }
    return(plot_data)
  }

########################

generate.time.to.first.step.chart <-
  function(first_step_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import ggplot2
    first_step_data <- first_step_data[which(first_step_data$uid %in% individual_summary$uid),]
    first_step_data$uid <- factor(first_step_data$uid, levels = individual_summary$uid)
    first_step_data$group <- "full"
    if(length(which(first_step_data$time_first_step > 5)) != 0){
      first_step_data[which(first_step_data$time_first_step > 5),]$group <- "short"
      first_step_data[which(first_step_data$time_first_step > 5),]$time_first_step <- 5
    }

    plot_data <- ggplot2::ggplot(data = first_step_data, ggplot2::aes(x = .data$uid, y = .data$time_first_step)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=5, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=1, ymax=1, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(ggplot2::aes(fill = .data$group), color = "black", shape = 21, size = 3) +
      ggplot2::xlab("") +
      ggplot2::ylab("time (s)") +
      ggplot2::scale_fill_manual(values = c("black","red")) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_y_continuous(limit = c(0,5), breaks = seq(0,5,1), minor_breaks = seq(0,5,0.5)) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

generate.rise.time.chart <-
  function(rise_time_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import ggplot2
    rise_time_data <- rise_time_data[which(rise_time_data$uid %in% individual_summary$uid),]
    rise_time_data$uid <- factor(rise_time_data$uid, levels = individual_summary$uid)
    rise_time_max <-  max_days <- chart_summary[grep("valid median rise time",chart_summary$category),]$duration

    plot_data <- ggplot2::ggplot(data = rise_time_data, ggplot2::aes(x = .data$uid, y = .data$median_rise_time)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=rise_time_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=1, ymax=1, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(size = 3) +
      ggplot2::xlab("") +
      ggplot2::ylab("time (s)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.spacing = unit(1, "lines"))

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,3),
                                                  breaks = seq(0,3,1),
                                                  minor_breaks = seq(0,3,0.5))
    } else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,rise_time_max),
                                                  breaks = seq(0,rise_time_max,1),
                                                  minor_breaks = seq(0,rise_time_max,0.5))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.median.rise.time.chart <-
  function(median_rise_time_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import ggplot2
    median_rise_time_max <- chart_summary[grep("median rise time",chart_summary$category),]$duration

    median_rise_time_data$proportion <- "All"
    if(length(which(median_rise_time_data$duration < 25)) > 0){
      median_rise_time_data[median_rise_time_data$duration < 25,]$proportion <- "Partial"
    }
    median_rise_time_data$proportion <- factor(median_rise_time_data$proportion, levels = c("All","Partial"))
    median_rise_time_data$uid <- factor(median_rise_time_data$uid, levels = individual_summary$uid)

    plot_data <- ggplot2::ggplot(data = median_rise_time_data, ggplot2::aes(x = .data$uid, y = .data$median_rise_time)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=median_rise_time_max, alpha=0.2, fill="grey85") +
      ggplot2::geom_point(fill = "black", size = 4, shape = 20) +
      ggplot2::xlab("") +
      ggplot2::ylab("time (s)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,3),
                                                  breaks = seq(0,3,1),
                                                  minor_breaks = seq(0,3,0.2))
    }else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,median_rise_time_max),
                                                  breaks = seq(0,median_rise_time_max,1),
                                                  minor_breaks = seq(0,median_rise_time_max,0.2))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

generate.peak.stepping.chart <-
  function(median_cadence_data, walk_test_30s_data, walk_test_2min_data, walk_test_6min_data, walk_test_12min_data,
           chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import dplyr
    #' @import ggplot2
    cadence_data <- median_cadence_data[which(median_cadence_data$group == "1 minute +"),]
    cadence_data$uid <- factor(cadence_data$uid, levels = individual_summary$uid)

    walk_test_30s_data$period <- "30s"
    walk_test_30s_data$proportion <- "30s"
    if(length(which(walk_test_30s_data$duration < 25)) > 0){
      walk_test_30s_data[which(walk_test_30s_data$duration < 25),]$proportion <- "Partial"
    }
    walk_test_2min_data$period <- "2min"
    walk_test_2min_data$proportion <- "2min"
    if(length(which(walk_test_2min_data$duration < 115)) > 0){
      walk_test_2min_data[which(walk_test_2min_data$duration < 115),]$proportion <- "Partial"
    }
    walk_test_6min_data$period <- "6min"
    walk_test_6min_data$proportion <- "6min"
    if(length(which(walk_test_6min_data$duration < 345)) > 0){
      walk_test_6min_data[which(walk_test_6min_data$duration < 345),]$proportion <- "Partial"
    }
    walk_test_12min_data$period <- "12min"
    walk_test_12min_data$proportion <- "12min"
    if(length(which(walk_test_12min_data$duration < 705)) > 0){
      walk_test_12min_data[which(walk_test_12min_data$duration < 705),]$proportion <- "Partial"
    }

    max_steps <- chart_summary[which(chart_summary$category == "twelve min stepping"),]$duration
    walk_test_data <- dplyr::bind_rows(walk_test_30s_data, walk_test_2min_data, walk_test_6min_data, walk_test_12min_data)
    walk_test_data$uid <- factor(walk_test_data$uid, levels = individual_summary$uid)
    walk_test_data$period <- factor(walk_test_data$period,
                                    levels = c("30s","2min","6min","12min"))
    walk_test_data$proportion <- factor(walk_test_data$proportion,
                                        levels = c("30s","2min","6min","12min","Partial"))

    plot_data <- ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=0, alpha=0.2, fill="grey85") +
      ggplot2::geom_point(data = walk_test_data, ggplot2::aes(x = .data$uid, y = .data$steps, color = .data$period, fill = .data$proportion),
                          shape = 21, size = 2, stroke = 1.5) +
      ggplot2::xlab("") +
      ggplot2::ylab("step count") +
      ggplot2::scale_color_manual(values = c("#d9d9d9","#969696","#525252","#000000"), drop = FALSE) +
      ggplot2::scale_fill_manual(values = c("#d9d9d9","#969696","#525252","#000000",grDevices::rgb(1,1,1,1)), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::scale_y_continuous(limit = c(0,max_steps),
                         breaks = seq(0,max_steps,500),
                         minor_breaks = seq(0,max_steps,100)) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

generate.peak.stepping.cadence.chart <-
  function(median_cadence_data, walk_test_30s_data, walk_test_2min_data, walk_test_6min_data, walk_test_12min_data,
           chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import dplyr
    #' @import ggplot2
    cadence_data <- median_cadence_data[which(median_cadence_data$group == "1 minute +"),]
    cadence_data$uid <- factor(cadence_data$uid, levels = individual_summary$uid)
    short_cadence_data <- median_cadence_data[which(median_cadence_data$group == "< 1 minute"),]
    short_cadence_data$uid <- factor(short_cadence_data$uid, levels = individual_summary$uid)

    walk_test_30s_data$period <- "30s"
    walk_test_30s_data$cadence <- walk_test_30s_data$steps / (walk_test_30s_data$duration / 60)
    walk_test_30s_data$proportion <- "30s"
    if(length(which(walk_test_30s_data$duration < 25)) > 0){
      walk_test_30s_data[which(walk_test_30s_data$duration < 25),]$proportion <- "Partial"
    }
    walk_test_2min_data$period <- "2min"
    walk_test_2min_data$cadence <- walk_test_2min_data$steps / (walk_test_2min_data$duration / 60)
    walk_test_2min_data$proportion <- "2min"
    if(length(which(walk_test_2min_data$duration < 115)) > 0){
      walk_test_2min_data[which(walk_test_2min_data$duration < 115),]$proportion <- "Partial"
    }
    walk_test_6min_data$period <- "6min"
    walk_test_6min_data$cadence <- walk_test_6min_data$steps / (walk_test_6min_data$duration / 60)
    walk_test_6min_data$proportion <- "6min"
    if(length(which(walk_test_6min_data$duration < 345)) > 0){
      walk_test_6min_data[which(walk_test_6min_data$duration < 345),]$proportion <- "Partial"
    }
    walk_test_12min_data$period <- "12min"
    walk_test_12min_data$cadence <- walk_test_12min_data$steps / (walk_test_12min_data$duration / 60)
    walk_test_12min_data$proportion <- "12min"
    if(length(which(walk_test_12min_data$duration < 705)) > 0){
      walk_test_12min_data[which(walk_test_12min_data$duration < 705),]$proportion <- "Partial"
    }

    walk_test_data <- dplyr::bind_rows(walk_test_30s_data, walk_test_2min_data, walk_test_6min_data, walk_test_12min_data)
    walk_test_data$uid <- factor(walk_test_data$uid, levels = individual_summary$uid)
    walk_test_data$period <- factor(walk_test_data$period,
                                    levels = c("30s","2min","6min","12min"))
    walk_test_data$proportion <- factor(walk_test_data$proportion,
                                    levels = c("30s","2min","6min","12min","Partial"))

    walk_test_data <- dplyr::inner_join(walk_test_data,cadence_data, by = "uid")
    walk_test_data <- walk_test_data %>% dplyr::group_by(.data$uid) %>% dplyr::mutate(min_cadence = min(.data$cadence))

    min_cadence <- chart_summary[which(chart_summary$category == "min cadence"),]$duration
    max_cadence <- chart_summary[which(chart_summary$category == "peak cadence"),]$duration

    min_val <- min_cadence - (min_cadence %% 25)
    max_val <- max_cadence - (max_cadence %% 25) + 25

    plot_data <- ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=min_val, ymax=max_val, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=min_val, ymax=100, alpha=0.2, fill="grey60") +
      ggplot2::geom_segment(data = walk_test_data,
                            ggplot2::aes(x = .data$uid, y = .data$cadence, xend = .data$uid, yend = .data$min_cadence,
                                              color = .data$period, size = .data$period)) +
      ggplot2::geom_point(data = walk_test_data,
                          ggplot2::aes(x = .data$uid, y = .data$cadence, fill = .data$period),
                          shape = 21, size = 3) +
      ggplot2::xlab("") +
      ggplot2::ylab("cadence (steps/min)") +
      ggplot2::scale_size_manual(values = c(0.75,1,1.25,1.5), drop = FALSE) +
      ggplot2::scale_fill_manual(values = c("#d9d9d9","#969696","#525252","#000000"), drop = FALSE) +
      ggplot2::scale_color_manual(values = c("#dadaeb","#bcbddc","#807dba","#54278f"), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
        panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
        panel.spacing = unit(1, "lines"),
        legend.position = "none")

    plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(min_val,max_val),
                                                  breaks = seq(min_val,max_val,25),
                                                  minor_breaks = seq(min_val,max_val,5))

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.indoor.stepping.cadence.chart <-
  function(median_cadence_data,
           chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import ggplot2
    cadence_data <- median_cadence_data[which(median_cadence_data$group == "1 minute +"),]
    cadence_data$uid <- factor(cadence_data$uid, levels = individual_summary$uid)
    short_cadence_data <- median_cadence_data[which(median_cadence_data$group == "< 1 minute"),]
    short_cadence_data$uid <- factor(short_cadence_data$uid, levels = individual_summary$uid)

    min_cadence <- chart_summary[which(chart_summary$category == "median cadence min"),]$duration
    max_cadence <- chart_summary[which(chart_summary$category == "median cadence max"),]$duration

    min_val <- min_cadence - (min_cadence %% 25)
    max_val <- max_cadence - (max_cadence %% 25) + 25

    plot_data <- ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=min_val, ymax=max_val, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=min_val, ymax=100, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(data = cadence_data, ggplot2::aes(x = .data$uid, y = .data$median_cadence),
                          shape = 23, size = 3, color = "black", fill = "#CB181D") +
      ggplot2::geom_point(data = short_cadence_data, ggplot2::aes(x = .data$uid, y = .data$median_cadence),
                          shape = 23, size = 3, color = "black", fill = "#FEE5D9") +
      ggplot2::xlab("") +
      ggplot2::ylab("steps/min") +
      ggplot2::scale_size_manual(values = c(0.75,1,1.25,1.5), drop = FALSE) +
      ggplot2::scale_fill_manual(values = c("#d9d9d9","#969696","#525252","#000000"), drop = FALSE) +
      ggplot2::scale_color_manual(values = c("#dadaeb","#bcbddc","#807dba","#54278f"), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(min_val,max_val),
                                                breaks = seq(min_val,max_val,25),
                                                minor_breaks = seq(min_val,max_val,5))

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.median.cadence.chart <-
  function(median_cadence_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    #' @import ggplot2
    median_cadence_data$uid <- factor(median_cadence_data$uid, levels = individual_summary$uid)
    median_cadence_max <- chart_summary[grep("median cadence max",chart_summary$category),]$duration
    median_cadence_min <- chart_summary[grep("median cadence min",chart_summary$category),]$duration

    plot_data <- ggplot2::ggplot(data = median_cadence_data, ggplot2::aes(x = .data$uid, y = .data$median_cadence)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=median_cadence_min, ymax=median_cadence_max, alpha=0.2, fill="grey85") +
      ggplot2::geom_point(ggplot2::aes(fill = .data$group), size = 3, shape = 23) +
      ggplot2::xlab("") +
      ggplot2::ylab("median steps / minute") +
      ggplot2::scale_fill_manual(values = c("#FEE5D9","#CB181D"), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
        panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
        panel.spacing = unit(1, "lines"),
        legend.position = "none")

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(40,160),
                                                  breaks = seq(40,160,20),
                                                  minor_breaks = seq(40,160,5))
    }else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(median_cadence_min,median_cadence_max),
                                                  breaks = seq(median_cadence_min,median_cadence_max,20),
                                                  minor_breaks = seq(median_cadence_min,median_cadence_max,5))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
}

##########

load.logo <-
  function(file_name, width, v_just = 0.5, h_just = 0.5){
    #' @import png
    #' @import grid
    logo <- png::readPNG(system.file(file_name, package = "activPAL"))
    logo <- grid::rasterGrob(logo, vjust = v_just, hjust = h_just, width = unit(width, "points"))
    return(logo)
  }
