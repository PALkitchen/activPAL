generate.sedentary.lying.chart <-
  function(chart_data, chart_summary, bouts_breaks_data, individual_summary, standard_scale = FALSE, include_labels = TRUE, simplified_colors = FALSE){
    activity_group <- c("Sedentary (4 hours +)", "Sedentary (2 - 4 hours)", "Sedentary (1 - 2 hours)", "Sedentary (30 min - 1 hour)", "Sedentary (< 30 min)",
                        "Lying")

    activity_colour <- c("#c17100","#f18c00","#ffa700","#ffc451","#f2f200",
                         "#009EE2")
    chart_data$sub_category <- as.character(chart_data$bout_length)
    chart_data[grep("Time in Bed",chart_data$sub_category),]$sub_category <- "Lying"
    chart_data$sub_category <- factor(chart_data$sub_category, levels = activity_group)

    chart_element <- chart_data[grep("Lying|Sedentary",chart_data$sub_category),]
    chart_element[grep("Sedentary",chart_element$category),]$bout_duration <- - (chart_element[grep("Sedentary",chart_element$category),]$bout_duration)

    chart_element <- inner_join(chart_element,bouts_breaks_data[,c(1,4,6)], by = "uid")
    chart_element$uid <- factor(chart_element$uid, levels = individual_summary$uid)

    if(standard_scale){
      lower_val <- (-24)
      upper_val <- 24
    } else {
      lower_val <- -(chart_summary[grep("Sedentary",chart_summary$category),]$duration +
        (chart_summary[grep("Sedentary",chart_summary$category),]$duration %% 2))
      upper_val <- chart_summary[grep("Time in Bed",chart_summary$category),]$duration +
        (chart_summary[grep("Time in Bed",chart_summary$category),]$duration %% 2)
    }

    plot_data <- ggplot2::ggplot(data = chart_element, aes(x = .data$uid)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin = lower_val, ymax = upper_val, alpha=0.2, fill="grey85") +
      ggplot2::geom_col(aes(y = .data$bout_duration, fill = .data$sub_category)) +
      ggplot2::geom_text(aes(y = 0.5, label = paste("< ",round(.data$breaks_per_day, 0)," >",sep="")), hjust = 0, size = 3, col = "white") +
      ggplot2::geom_text(aes(y = -0.5, label = paste("[",round(.data$sedentary_bout_per_day, 0),"]",sep="")), hjust = 1, size = 3, col = "black") +
      ggplot2::xlab("") +
      ggplot2::ylab("average daily time (hours)") +
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

    plot_data <- plot_data + ggplot2::scale_fill_manual("activity classification",
                                                        labels = activity_group,
                                                        values = activity_colour,
                                                        drop = FALSE)

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    if(standard_scale){
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limit = c(-24,24), breaks = seq(-24,24,2), labels = c(seq(24,0,-2),seq(2,24,2)))
    } else {
      min <- (chart_summary[grep("Sedentary",chart_summary$category),]$duration %% 2)
      max <- (chart_summary[grep("Time in Bed",chart_summary$category),]$duration %% 2)
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limits = c(-chart_summary[grep("Sedentary",chart_summary$category),]$duration-min,chart_summary[grep("Time in Bed",chart_summary$category),]$duration+max),
                           breaks = seq(-chart_summary[grep("Sedentary",chart_summary$category),]$duration-min,chart_summary[grep("Time in Bed",chart_summary$category),]$duration+max,2),
                           labels = c(seq(chart_summary[grep("Sedentary",chart_summary$category),]$duration+min,0,-2),seq(2,chart_summary[grep("Time in Bed",chart_summary$category),]$duration+max,2)))
    }
    return(plot_data)
  }

generate.standing.chart <-
  function(chart_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE, simplified_colors = FALSE){
    activity_group <- c("Quiet Standing","Stepping (1 minute +)","Stepping (< 1 minute)")

    activity_colour <- c("#38D305","#CB181D","#FEE5D9")

    sub_category_group <- c("Sedentary","Standing","Stepping")

    sub_category_color <- c("#FFDF00","#38D305","#FF0000")

    chart_data$sub_category <- "Sedentary"
    chart_data[grep("Quiet Standing",chart_data$bout_length),]$sub_category <- "Standing"
    chart_data[grep("Stepping",chart_data$bout_length),]$sub_category <- "Stepping"
    chart_data$sub_category <- factor(chart_data$sub_category, levels = sub_category_group)

    chart_element <- chart_data[grep("Standing",chart_data$category),]
    chart_element$bout_length <- factor(chart_element$bout_length, levels = activity_group)

    if(standard_scale){
      upper_val <- 24
    }else{
      upper_val <- chart_summary[grep("Standing",chart_summary$category),]$duration +
        (chart_summary[grep("Standing",chart_summary$category),]$duration %% 2)
    }

    total_duration <- chart_element %>%
      dplyr::filter(.data$sub_category == "Stepping") %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(total_duration = sum(.data$bout_duration))

    below_1_minute <- chart_element %>%
      dplyr::filter(.data$bout_length == "Stepping (< 1 minute)") %>%
      dplyr::group_by(.data$uid) %>%
      dplyr::summarise(short_duration = sum(.data$bout_duration))

    duration <- dplyr::inner_join(below_1_minute,total_duration, by = "uid")
    duration$percent <- round((duration$short_duration / duration$total_duration * 100),0)

    chart_element <- dplyr::inner_join(chart_element,duration, by = "uid")
    chart_element$uid <- factor(chart_element$uid, levels = individual_summary$uid)

    plot_data <- ggplot2::ggplot(data = chart_element, aes(x = .data$uid, y = .data$bout_duration, fill = .data$bout_length)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax= upper_val, alpha=0.2, fill="grey85") +
      ggplot2::geom_col() +
      ggplot2::geom_text(aes(y = -1.2, label = paste(.data$percent,"%",sep="")), size = 3, color = "#FF0000") +
      ggplot2::xlab("") +
      ggplot2::ylab("average daily time (hours)") +
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

    if(simplified_colors){
      plot_data <- plot_data +
        ggplot2::scale_fill_manual("activity classification",labels = sub_category_group, values = sub_category_color, drop = FALSE)
    }else{
      plot_data <- plot_data +
        ggplot2::scale_fill_manual("activity classification",labels = activity_group, values = activity_colour, drop = FALSE)
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    if(standard_scale){
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limit = c(-2,24), breaks = seq(0,24,2), labels = seq(0,24,2))
    } else {
      max_val <- chart_summary[grep("Standing",chart_summary$category),]$duration + (chart_summary[grep("Standing",chart_summary$category),]$duration %% 2)
      plot_data <- plot_data +
        ggplot2::scale_y_continuous(limits = c(-2,max_val),
                           breaks = seq(0,max_val,2),
                           minor_breaks = seq(0,max_val,1),
                           labels = seq(0,max_val,2))
    }
    return(plot_data)
  }

generate.peak.2.min.stepping.chart <-
  function(walk_test_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    walk_test_data$uid <- factor(walk_test_data$uid, levels = individual_summary$uid)
    walk_test_max <- chart_summary[grep("2 min stepping",chart_summary$category),]$duration

    walk_test_data$proportion <- "All"
    if(length(which(walk_test_data$duration < 115)) > 0){
      walk_test_data[walk_test_data$duration < 115,]$proportion <- "Partial"
    }
    walk_test_data$proportion <- factor(walk_test_data$proportion, levels = c("All","Partial"))

    plot_data <- ggplot2::ggplot(data = walk_test_data, aes(x = .data$uid, y = .data$steps)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=walk_test_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=200, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(aes(fill = .data$proportion), shape = 23, size = 3) +
      ggplot2::xlab("") +
      ggplot2::ylab("steps") +
      ggplot2::scale_fill_manual(values = c("black","white"), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(#plot.background = element_rect(colour = "black"),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.grid.minor.x = element_line(colour = "grey85", linetype = 2),
            panel.spacing = unit(1, "lines"),
            legend.position = "none")

    if(standard_scale){
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,400),
                                                  breaks = seq(0,400,100),
                                                  minor_breaks = seq(0,400,20))
    }else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,walk_test_max),
                                                  breaks = seq(0,walk_test_max,100),
                                                  minor_breaks = seq(0,walk_test_max,20))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

generate.peak.6.min.stepping.chart <-
  function(walk_test_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    walk_test_data$uid <- factor(walk_test_data$uid, levels = individual_summary$uid)
    walk_test_max <- chart_summary[grep("6 min stepping",chart_summary$category),]$duration

    walk_test_data$proportion <- "All"
    if(length(which(walk_test_data$duration < 345)) > 0){
      walk_test_data[walk_test_data$duration < 345,]$proportion <- "Partial"
    }
    walk_test_data$proportion <- factor(walk_test_data$proportion, levels = c("All","Partial"))

    plot_data <- ggplot2::ggplot(data = walk_test_data, aes(x = .data$uid, y = .data$steps)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=walk_test_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=600, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(aes(fill = .data$proportion), size = 3, shape = 23) +
      ggplot2::xlab("") +
      ggplot2::ylab("steps") +
      ggplot2::scale_fill_manual(values = c("black","white"), drop = FALSE) +
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
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,800),
                                                  breaks = seq(0,800,200),
                                                  minor_breaks = seq(0,800,50))
    }else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,walk_test_max),
                                                  breaks = seq(0,walk_test_max,200),
                                                  minor_breaks = seq(0,walk_test_max,50))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

generate.peak.10.min.stepping.chart <-
  function(walk_test_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    walk_test_data$uid <- factor(walk_test_data$uid, levels = individual_summary$uid)
    walk_test_max <- chart_summary[grep("10 min stepping",chart_summary$category),]$duration

    walk_test_data$proportion <- "All"
    if(length(which(walk_test_data$duration < 585)) > 0){
      walk_test_data[walk_test_data$duration < 585,]$proportion <- "Partial"
    }
    walk_test_data$proportion <- factor(walk_test_data$proportion, levels = c("All","Partial"))

    plot_data <- ggplot2::ggplot(data = walk_test_data, aes(x = .data$uid, y = .data$steps)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax= walk_test_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=1000, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(aes(fill = .data$proportion), size = 3, shape = 23) +
      ggplot2::xlab("") +
      ggplot2::ylab("steps") +
      ggplot2::scale_fill_manual(values = c("black","white"), drop = FALSE) +
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
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,1500),
                                                  breaks = seq(0,1500,300),
                                                  minor_breaks = seq(0,1500,100))
    }else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,walk_test_max),
                                                  breaks = seq(0,walk_test_max,500),
                                                  minor_breaks = seq(0,walk_test_max,100))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

generate.daily.30.s.stepping.chart <-
  function(walk_test_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    walk_test_data$uid <- factor(walk_test_data$uid, levels = individual_summary$uid)
    walk_test_max <- chart_summary[grep("30 s stepping",chart_summary$category),]$duration

    walk_test_data$proportion <- "All"
    if(length(which(walk_test_data$duration < 25)) > 0){
      walk_test_data[walk_test_data$duration < 25,]$proportion <- "Partial"
    }
    walk_test_data$proportion <- factor(walk_test_data$proportion, levels = c("All","Partial"))

    plot_data <- ggplot2::ggplot(data = walk_test_data, aes(x = .data$uid, y = .data$steps)) +
      ggplot2::annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = walk_test_max, alpha = 0.2, fill = "grey85")

    if(walk_test_max >= 50){
      plot_data <- plot_data + ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=50, alpha=0.2, fill="grey60")
    }
      plot_data <- plot_data + ggplot2::geom_point(aes(fill = .data$proportion), size = 3, shape = 23) +
        ggplot2::xlab("") +
        ggplot2::ylab("steps") +
        ggplot2::scale_fill_manual(values = c("black","white"), drop = FALSE) +
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
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,100),
                                                  breaks = seq(0,100,20),
                                                  minor_breaks = seq(0,100,20))
    }else{
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,walk_test_max),
                                                  breaks = seq(0,walk_test_max,20),
                                                  minor_breaks = seq(0,walk_test_max,5))
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
    median_rise_time_data$uid <- factor(median_rise_time_data$uid, levels = individual_summary$uid)
    median_rise_time_max <- chart_summary[grep("median rise time",chart_summary$category),]$duration

    median_rise_time_data$proportion <- "All"
    if(length(which(median_rise_time_data$duration < 25)) > 0){
      median_rise_time_data[median_rise_time_data$duration < 25,]$proportion <- "Partial"
    }
    median_rise_time_data$proportion <- factor(median_rise_time_data$proportion, levels = c("All","Partial"))

    plot_data <- ggplot2::ggplot(data = median_rise_time_data, aes(x = .data$uid, y = .data$median_rise_time)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=median_rise_time_max, alpha=0.2, fill="grey85") +
      ggplot2::geom_point(fill = "black", size = 3, shape = 20) +
      ggplot2::xlab("") +
      ggplot2::ylab("rise time (s)") +
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

generate.median.cadence.chart <-
  function(median_cadence_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    median_cadence_data$uid <- factor(median_cadence_data$uid, levels = individual_summary$uid)
    median_cadence_max <- chart_summary[grep("median cadence max",chart_summary$category),]$duration
    median_cadence_min <- chart_summary[grep("median cadence min",chart_summary$category),]$duration

    plot_data <- ggplot2::ggplot(data = median_cadence_data, aes(x = .data$uid, y = .data$median_cadence)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=median_cadence_min, ymax=median_cadence_max, alpha=0.2, fill="grey85") +
      ggplot2::geom_point(aes(fill = .data$group), size = 3, shape = 23) +
      ggplot2::xlab("") +
      ggplot2::ylab("median steps / min") +
      ggplot2::scale_fill_manual(values = c("#FEE5D9","#CB181D"), drop = FALSE) +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(#plot.background = element_rect(colour = "black"),
        strip.background = element_blank(),
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

generate.daily.stepping.summary.chart <-
  function(daily_stepping_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    daily_stepping_data$uid <- factor(daily_stepping_data$uid, levels = individual_summary$uid)
    stepping_max <- chart_summary[grep("daily steps",chart_summary$category),]$duration
    stepping_max <- max(stepping_max,10000)

    mean_valid_days <- daily_stepping_data %>% group_by(.data$uid) %>% tally() %>% summarise(total = mean(n))

    plot_data <- ggplot2::ggplot(data = daily_stepping_data, aes(x = .data$uid, y = .data$steps)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=stepping_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=10000, alpha=0.2, fill="grey60")
    if(mean_valid_days >= 5){
      plot_data <- plot_data + ggplot2::geom_boxplot(fill = "grey65")
    }else{
      plot_data <- plot_data + ggplot2::geom_point(colour = "black")
    }
    plot_data <- plot_data +
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
      plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,stepping_max),
                                                  labels = paste(seq(0,stepping_max,5000)/1000,"k",sep = ""),
                                                  breaks = seq(0,stepping_max,5000),
                                                  minor_breaks = seq(0,stepping_max,2500))
    }

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }
    return(plot_data)
  }

generate.valid.day.sequence.chart <-
  function(validation_data, chart_summary, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    library(dplyr)
    validation_data <- validation_data[which(validation_data$uid %in% individual_summary$uid),]
    validation_data$uid <- factor(validation_data$uid, levels = individual_summary$uid)
    validation_data$Date <- factor(validation_data$Date,levels = sort(unique(validation_data$Date),decreasing = TRUE))
    validation_data$height <- 1

    max_days <- chart_summary[grep("valid days",chart_summary$category),]$duration

    valid_days <- validation_data %>% filter(.data$valid == "valid") %>% group_by(.data$uid) %>% summarise(valid_days = n())
    valid_days$uid <- factor(valid_days$uid, levels = individual_summary$uid)

    plot_data <- ggplot2::ggplot(data = validation_data, aes(x = .data$uid, y = .data$height)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=max_days, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=1, alpha=0, fill="white") +
      ggplot2::geom_col(aes(fill = .data$valid, group = .data$Date)) +
      ggplot2::geom_text(data = valid_days, aes(x = .data$uid, y = 1, label = paste("(",.data$valid_days,")",sep="")), hjust = 0, size = 3, color = "#000000") +
      ggplot2::xlab("") +
      ggplot2::ylab("days") +
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

generate.legend <-
  function(){
    x_left <- c(0,5,
                0,5,10,15,20,25,
                0,5,10)
    x_right <- c(1,6,
                 1,6,11,16,21,26,
                 1,6,11)
    y_bottom <- c(3.5,3.5,
                  2.75,2.75,2.75,2.75,2.75,2.75,
                  2,2,2)
    y_top <- c(3.1,3.1,
               2.35,2.35,2.35,2.35,2.35,2.35,
               1.6,1.6,1.6)
    code <- seq(1,11,1)
    color <- c("#ff9800","#c8c8c8",
               "#c17100","#f18c00","#ffa700","#ffc451","#f2f200",
               "#009EE2", "#FEE5D9","#CB181D","#38D305")

    text_x <- c(1.2,6.2,
                1.2,6.2,11.2,16.2,21.2,26.2,
                1.2,6.2,11.2,16.2,21.2,26.2,
                1.2,6.2)
    text_y <- c(3.3,3.3,
                2.55,2.55,2.55,2.55,2.55,2.55,
                1.8,1.8,1.8,1.8,1.8,1.8,
                1.05,1.05)
    text_label <- c("Valid day\n(Total valid days)","Invalid day",
                    "Sedentary\n(4 hours +)", "Sedentary\n(2 - 4 hours)", "Sedentary\n(1 - 2 hours)", "Sedentary\n(30 min - 1 hour)", "Sedentary (< 30 min) \n[Average daily sedentary events]",
                    "Time in Bed \n<Average daily Time out of Bed events>",
                    "Stepping\n(< 1 minute)", "Stepping\n(1 minute +)", "Quiet standing", "Stepping time\nin bouts < 1 minute",
                    "Preferred Cadence (bouts < 1 minute)","Preferred Cadence (bouts > 1 minute)","Broken stepping","Continuous stepping")

    insert_x <- c(0.5,20.5,25.5,
                  15.5)
    insert_y <- c(3.3,2.55,2.55,
                  1.8)
    insert_label <- c("(3)","[15]","< 6 >","92%")
    insert_color <- c("#000000","#000000","#ffffff","#ff0000")

    shape_x <- c(20.5,25.5,
                 0.5,5.5)
    shape_y <- c(1.8,1.8,
                 1.1,1.1)
    shape_type <- c(23,23,
                    5,23)

    plot_data <- ggplot2::ggplot() +
      ggplot2::geom_text(aes(label = text_label, x = text_x, y = text_y), size = 2.5, hjust = 0) +
      ggplot2::geom_rect(aes(xmin = x_left, ymin = y_bottom, xmax = x_right, ymax = y_top, fill = factor(code))) +
      ggplot2::geom_point(aes(x = shape_x[1], y = shape_y[1], shape = shape_type[1]), size = 3, fill = "#FEE5D9") +
      ggplot2::geom_point(aes(x = shape_x[2], y = shape_y[2], shape = shape_type[2]), size = 3, fill = "#CB181D") +
      ggplot2::geom_point(aes(x = shape_x[3:4], y = shape_y[3:4], shape = shape_type[3:4]), size = 3, fill = "black") +
      ggplot2::geom_text(aes(label = insert_label, x = insert_x, y = insert_y, colour = insert_label), size = 3, hjust = 0.5) +
      ggplot2::scale_x_continuous(limits = c(0,29)) +
      ggplot2::scale_color_manual(values = insert_color) +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::scale_shape_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    return(plot_data)
  }

################################

generate.rise.time.chart <-
  function(rise_time_data, individual_summary, standard_scale = FALSE, include_labels = TRUE){
    rise_time_data <- rise_time_data[which(rise_time_data$uid %in% individual_summary$uid),]
    rise_time_data$uid <- factor(rise_time_data$uid, levels = individual_summary$uid)
    rise_time_max <- round(max(rise_time_data$median_rise_time) - max(rise_time_data$median_rise_time) %% 0.5 + 0.5,1)

    plot_data <- ggplot2::ggplot(data = rise_time_data, aes(x = .data$uid, y = .data$median_rise_time)) +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=0, ymax=rise_time_max, alpha=0.2, fill="grey85") +
      ggplot2::annotate("rect", xmin=0, xmax=Inf, ymin=1, ymax=3, alpha=0.2, fill="grey60") +
      ggplot2::geom_point(size = 3) +
      ggplot2::xlab("") +
      ggplot2::ylab("median (seconds)") +
      ggplot2::scale_x_discrete(drop = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::theme(#plot.background = element_rect(colour = "black"),
            strip.background = element_blank(),
            strip.text.x = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_rect(fill = NA),
            panel.grid.major.x = element_line(colour = "grey75", linetype = 2),
            panel.spacing = unit(1, "lines"))

    plot_data <- plot_data + ggplot2::scale_y_continuous(limit = c(0,3), breaks = seq(0,3,0.5))

    if(!include_labels){
      plot_data <- plot_data + ggplot2::theme(axis.title.y = element_blank(),
                                     axis.text.y = element_blank(),
                                     axis.ticks.y = element_blank())
    }

    return(plot_data)
  }

load.logo <-
  function(file_name, width, v_just = 0.5, h_just = 0.5){
    logo <- png::readPNG(file_name)
    logo <- grid::rasterGrob(logo, vjust = v_just, hjust = h_just, width = unit(width, "points"))
    return(logo)
  }
