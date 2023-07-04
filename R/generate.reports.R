generate.all.outcomes.report <-
  function(valid_days, chart_data, daily_stepping_data, travel_data, mvpa_data, bouted_stepping_data,
           median_rise_time_data, median_first_step_data, median_cadence_data, walk_test_30s_data,
           walk_test_2_min_data, walk_test_6_min_data, walk_test_12_min_data,
           bouts_breaks_data, chart_summary, individual_summary, median_rise_time, sort_order,
           chart_title, output_folder, standard_scales = FALSE){
    #' @import dplyr
    #' @import ggplot2
    #' @import grid
    #' @importFrom gridExtra grid.arrange

    items_per_page <- 65

    if(output_folder == ""){
      grDevices::pdf(paste(chart_title,"_Full_Summary_",sort_order,".pdf",sep=""), width = 16.5, height = 11.75)
    }else{
      grDevices::pdf(paste(output_folder,"/",chart_title,"_Full_Summary_",sort_order,".pdf",sep=""), width = 16.5, height = 11.75)
    }

    pages <- nrow(individual_summary) %/% items_per_page
    if((nrow(individual_summary) %% items_per_page) != 0){
      pages <- pages + 1
    }

    for(i in (1:pages)){
      # Plot and save the page on the chart
      page_individual_summary <- individual_summary

      page_individual_summary <- page_individual_summary[((i-1)*items_per_page+1):min((i*items_per_page),nrow(page_individual_summary)),]
      page_individual_summary <- page_individual_summary %>% arrange(desc(as.numeric(row.names(page_individual_summary))))
      page_uid <- page_individual_summary$uid

      plot_data_validation_data <- generate.uid.valid.day.chart(valid_days[which(valid_days$uid %in% page_uid),],
                                                                chart_summary, page_individual_summary,standard_scales,TRUE)
      plot_data_lying <- generate.lying.chart(chart_data[which(chart_data$uid %in% page_uid),],
                                              chart_summary, bouts_breaks_data, page_individual_summary,standard_scales,FALSE,FALSE)
      plot_data_sedentary_index <- generate.sedentary.index.chart(chart_data[which(chart_data$uid %in% page_uid),],
                                                                  chart_summary, bouts_breaks_data, page_individual_summary,standard_scales,FALSE,FALSE)
      plot_data_sedentary_standing <- generate.sedentary.standing.chart(chart_data[which(chart_data$uid %in% page_uid),],
                                                                        chart_summary, bouts_breaks_data, page_individual_summary,standard_scales,FALSE,FALSE)
      plot_data_daily_stepping <- generate.daily.stepping.summary.chart(daily_stepping_data[which(daily_stepping_data$uid %in% page_uid),],
                                                                        chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_travel <- generate.travel.chart(travel_data[which(travel_data$uid %in% page_uid),],
                                                chart_summary, page_individual_summary,standard_scales,FALSE)

      plot_indoor_walking <- generate.indoor.walking.chart(mvpa_data[which(mvpa_data$uid %in% page_uid & mvpa_data$duration == "short (< 60s)"),],
                                                           chart_summary, page_individual_summary)
      plot_data_short_mvpa_stepping <- generate.stepping.intensity.chart(mvpa_data[which(mvpa_data$uid %in% page_uid & mvpa_data$duration == "short (< 60s)"),],
                                                                         chart_summary, page_individual_summary, standard_scales, FALSE)
      plot_data_long_mvpa_stepping <- generate.stepping.intensity.chart(mvpa_data[which(mvpa_data$uid %in% page_uid & mvpa_data$duration == "long (>= 60s)"),],
                                                                        chart_summary, page_individual_summary, standard_scales, FALSE)
      plot_data_bouted_stepping <- generate.bouted.stepping.chart(bouted_stepping_data[which(bouted_stepping_data$uid %in% page_uid),],
                                                                     chart_summary, page_individual_summary,standard_scales,FALSE)

      plot_time_to_first_step <- generate.time.to.first.step.chart(median_first_step_data[which(median_first_step_data$uid %in% page_uid),],
                                                                   chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_median_cadence <- generate.median.cadence.chart(median_cadence_data[which(median_cadence_data$uid %in% page_uid),],
                                                                chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_peak_stepping <- generate.peak.stepping.chart(median_cadence_data[which(median_cadence_data$uid %in% page_uid),],
                                                              walk_test_30s_data[which(walk_test_30s_data$uid %in% page_uid),],
                                                              walk_test_2_min_data[which(walk_test_2_min_data$uid %in% page_uid),],
                                                              walk_test_6_min_data[which(walk_test_6_min_data$uid %in% page_uid),],
                                                              walk_test_12_min_data[which(walk_test_12_min_data$uid %in% page_uid),],
                                                              chart_summary, page_individual_summary, standard_scales, FALSE)
      plot_data_peak_cadence_stepping <- generate.peak.stepping.cadence.chart(median_cadence_data[which(median_cadence_data$uid %in% page_uid),],
                                                                              walk_test_30s_data[which(walk_test_30s_data$uid %in% page_uid),],
                                                                              walk_test_2_min_data[which(walk_test_2_min_data$uid %in% page_uid),],
                                                                              walk_test_6_min_data[which(walk_test_6_min_data$uid %in% page_uid),],
                                                                              walk_test_12_min_data[which(walk_test_12_min_data$uid %in% page_uid),],
                                                                              chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_indoor_outdoor_cadence_stepping <- generate.indoor.stepping.cadence.chart(median_cadence_data[which(median_cadence_data$uid %in% page_uid),],
                                                                              chart_summary, page_individual_summary,standard_scales,FALSE)
      plot_data_legend <- generate.all.outcomes.legend()

      # Add right margins to the plots for correct formatting
      plot_data_validation_data <- plot_data_validation_data + ggplot2::theme(plot.margin = margin(4,2,4,0, "pt"))
      plot_data_lying <- plot_data_lying + ggplot2::theme(plot.margin = margin(4,4,4,6, "pt"))
      plot_data_sedentary_standing <- plot_data_sedentary_standing + ggplot2::theme(plot.margin = margin(4,0,4,2, "pt"))
      plot_data_sedentary_index <- plot_data_sedentary_index + ggplot2::theme(plot.margin = margin(4,2,4,0, "pt"))

      plot_data_daily_stepping <- plot_data_daily_stepping + ggplot2::theme(plot.margin = margin(4,4,4,6, "pt"))
      plot_data_travel <- plot_data_travel + ggplot2::theme(plot.margin = margin(4,8,4,2, "pt"))

      plot_indoor_walking <- plot_indoor_walking + ggplot2::theme(plot.margin = margin(4,0,4,0, "pt"))
      plot_data_short_mvpa_stepping <- plot_data_short_mvpa_stepping + ggplot2::theme(plot.margin = margin(4,2,4,2, "pt"))
      plot_data_long_mvpa_stepping <- plot_data_long_mvpa_stepping + ggplot2::theme(plot.margin = margin(4,8,4,0, "pt"))

      if(!is.null(median_rise_time)){
        plot_data_rise_time <- generate.median.rise.time.chart(median_rise_time_data[which(median_rise_time_data$uid %in% page_uid),],
                                                               chart_summary, page_individual_summary,standard_scales,FALSE)
        plot_data_rise_time <- plot_data_rise_time + ggplot2::theme(plot.margin = margin(4,4,4,6, "pt"))
      }
      plot_time_to_first_step <- plot_time_to_first_step + ggplot2::theme(plot.margin = margin(4,8,4,2, "pt"))

      plot_data_peak_stepping <- plot_data_peak_stepping + ggplot2::theme(plot.margin = margin(4,4,4,6, "pt"))
      plot_data_peak_cadence_stepping <- plot_data_peak_cadence_stepping + ggplot2::theme(plot.margin = margin(4,8,4,2, "pt"))
      plot_indoor_outdoor_cadence_stepping <- plot_indoor_outdoor_cadence_stepping + ggplot2::theme(plot.margin = margin(4,8,4,2, "pt"))

      plot_data_median_cadence <- plot_data_median_cadence + ggplot2::theme(plot.margin = margin(4,8,4,6, "pt"))

      title_list <- list()
      font_size <- 9.5
      header_font_size <- 12
      header_font <- "Montserrat Thin"
      large_header_font_size <- 12
      text_font <- "Rubik"
      legend_font_size <- 7
      title_list[[1]] <- grid::textGrob("accumulation / participation", hjust = 1,
                                        gp = grid::gpar(family = text_font, fontsize = large_header_font_size, fontface = "bold"))
      title_list[[2]] <- grid::textGrob("", hjust = 1.15,
                                        gp = grid::gpar(family = text_font, fontsize = large_header_font_size, fontface = "bold"))
      title_list[[3]] <- grid::textGrob("ability", hjust = 6.25,
                                        gp = grid::gpar(family = text_font, fontsize = large_header_font_size, fontface = "bold"))
      title_list[[4]] <- grid::textGrob("", gp = grid::gpar(family = text_font, fontsize = header_font_size, fontface = "bold"))
      title_list[[5]] <- grid::textGrob("user ID", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[6]] <- grid::textGrob("time in bed", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[7]] <- grid::textGrob("sedentary", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[8]] <- grid::textGrob("upright", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[9]] <- grid::textGrob("SI", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[10]] <- grid::textGrob("daily step count", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[11]] <- grid::textGrob("average step count", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[12]] <- grid::textGrob("preferred cadence", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[13]] <- grid::textGrob("travel", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[14]] <- grid::textGrob("rise time", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[15]] <- grid::textGrob("time to 1st step", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))
      title_list[[16]] <- grid::textGrob("peak cadence (0.5 / 2 / 6 minutes)", gp = grid::gpar(family = text_font, fontsize = font_size, fontface = "bold"))

      footer_list <- list()
      footer_list[[1]] <- grid::textGrob(as.numeric(as.Date(Sys.time())) + 25569, hjust = 0, vjust = 1)
      footer_list[[2]] <- grid::textGrob(paste("Page ",i," of ",pages,sep=""), vjust = 1)

      header_padding <- grid::rectGrob(gp=grid::gpar(fill="orange", col="orange"))
      header_www <- grid::grobTree( grid::rectGrob(gp=grid::gpar(fill="orange", col="orange")),
                                    grid::textGrob("www.palt.com", gp=grid::gpar(family=header_font, fontsize=15, col="white", fontface="bold"), vjust = 0.5, hjust = 0.1))

      # footer_image <- grid::grobTree( load.logo("logos/BWPALIconStrip.png",width = 250, h_just = 0.2))
      legend_panel <- grid::grobTree( load.logo("logos/Legend_Panel.png", width = 1190))
      footer_image <- grid::grobTree( load.logo("logos/IconStripActivPAL_BW.png",width = 250, h_just = 0.2))
      plot_data_legend <- plot_data_legend + ggplot2::theme(plot.margin = margin(0,12,0,12, "pt"))
      if(!is.null(median_rise_time)){
        header_image <- grid::grobTree( grid::rectGrob(gp=grid::gpar(fill="orange", col="orange")), load.logo("logos/PALbatch_icon_inverted.png", width = 40, h_just = 1))

        header_details <- grid::grobTree( grid::rectGrob(gp=grid::gpar(fill="orange", col="orange")),
                                          grid::textGrob("PALsummary - Participation and Ability Outcomes", gp=grid::gpar(family=header_font, fontsize=15, col="white", fontface="bold"),
                                             vjust = 0.5, hjust = 0.8))
        layout_matrix <- rbind(c(rep(1,1),rep(2,10),rep(3,3),rep(4,5)), # Header
                               c(rep(5,5),rep(6,8),rep(7,6)), # Title 1
                               c(seq(8,13,1),c(13,14,14,15,15,16,17,18,18),rep(19,4)), # Title 3
                               c(20,21,22,22,23,24,24,25,25,26,26,27,28,29,29,30,30,30,30), # Data
                               c(rep(31,19)), # Legend
                               c(rep(NA,19)), # Space
                               c(32,rep(33,16),rep(34,2)), # Footer
                               c(rep(NA,19))) # Space
        col_widths <- c(4,5,6,4,1.5,4,4,4,4,4,3.5,5,5,2,3,3,3,3,3)

        full_charts <- gridExtra::grid.arrange(grobs = list(header_image, header_details, header_padding, header_www,  # Header 1 - 4
                                                 title_list[[1]], title_list[[2]], title_list[[3]],  # Title 1 5 - 7
                                                 title_list[[5]], title_list[[6]], title_list[[7]], title_list[[8]], title_list[[9]], # Title 2 - Posture 8 - 12
                                                 title_list[[10]], title_list[[11]], title_list[[12]], # Title 2 - Participation 13 - 15
                                                 title_list[[13]], title_list[[14]], title_list[[15]], title_list[[16]], # Title 2 - Ability 16 - 19
                                                 plot_data_validation_data, plot_data_lying, plot_data_sedentary_standing, plot_data_sedentary_index, # Data - Posture 20 - 23
                                                 plot_data_daily_stepping, plot_data_bouted_stepping, plot_indoor_outdoor_cadence_stepping, plot_data_travel, # Data - Participation 24 - 27
                                                 plot_data_rise_time, plot_time_to_first_step, plot_data_peak_cadence_stepping, # Data - Ability 28 - 30
                                                 legend_panel, # plot_data_legend, # Legend 31
                                                 footer_list[[1]], footer_image, footer_list[[2]]), # Footer 32 - 35
                                    ncol = 19,
                                    heights = c(3,2,2,(4 + nrow(page_individual_summary)),8,(items_per_page - nrow(page_individual_summary)),2,1),
                                    widths = col_widths,
                                    layout_matrix = layout_matrix)

        grid_lines <- list()
        grid_height_units <- sum(c(3,2,(4 + nrow(page_individual_summary)),8,(items_per_page - nrow(page_individual_summary)),2,1))
        grid_lines[[1]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                     y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-3)/grid_height_units), "npc"))
        grid_lines[[2]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                     y = unit(c((grid_height_units-5)/grid_height_units,(grid_height_units-5)/grid_height_units), "npc"))
        grid_lines[[3]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                     y = unit(c((grid_height_units-7)/grid_height_units,(grid_height_units-7)/grid_height_units), "npc"))
        grid_lines[[4]] <- grid::linesGrob(x = unit(c(0,0), "npc"),
                                     y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        grid_lines[[5]] <- grid::linesGrob(x = unit(c(20.6/sum(col_widths),20.6/sum(col_widths)), "npc"),
                                     y = unit(c((grid_height_units-5)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        grid_lines[[6]] <- grid::linesGrob(x = unit(c(49/sum(col_widths),49/sum(col_widths)), "npc"),
                                     y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        grid_lines[[7]] <- grid::linesGrob(x = unit(c(59/sum(col_widths),59/sum(col_widths)), "npc"),
                                      y = unit(c((grid_height_units-5)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"),
                                      gp = grid::gpar(color = "grey75"))
        grid_lines[[8]] <- grid::linesGrob(x = unit(c(1,1), "npc"),
                                      y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        if(nrow(page_individual_summary) > 25){
          grid_lines[[9]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                             y = unit(c((grid_height_units-(9.5+nrow(page_individual_summary)))/grid_height_units,(grid_height_units-(9.5+nrow(page_individual_summary)))/grid_height_units), "npc"))
        }else{
          grid_lines[[9]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                             y = unit(c((grid_height_units-(10.5+nrow(page_individual_summary)))/grid_height_units,(grid_height_units-(10.5+nrow(page_individual_summary)))/grid_height_units), "npc"))
        }
        grid_lines[[10]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                            y = unit(c((grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
      }else{
        header_image <- grid::grobTree( grid::rectGrob(gp=grid::gpar(fill="orange", col="orange")), load.logo("logos/PALbatch_icon_inverted.png", width = 40, h_just = 1))

        header_details <- grid::grobTree( grid::rectGrob(gp=grid::gpar(fill="orange", col="orange")),
                                          grid::textGrob("PALsummary - Participation and Ability Outcomes", gp=grid::gpar(family=header_font, fontsize=15, col="white", fontface="bold"),
                                                         vjust = 0.5, hjust = 0.8))
        title_list[[3]] <- grid::textGrob("ability", hjust = 6,
                                          gp = grid::gpar(family = text_font, fontsize = large_header_font_size, fontface = "bold"))
        layout_matrix <- rbind(c(rep(1,1),rep(2,10),rep(3,3),rep(4,4)), # Header
                               c(rep(5,5),rep(6,8),rep(7,5)), # Title 1
                               c(seq(8,13,1),c(13,14,14,15,15,16,17,17),rep(18,4)), # Title 3
                               c(19,20,21,21,22,23,23,24,24,25,25,26,27,27,28,28,28,28), # Data
                               c(rep(29,18)), # Legend
                               c(rep(NA,18)), # Space
                               c(30,rep(31,15),rep(32,2)), # Footer
                               c(rep(NA,18))) # Space
        col_widths <- c(4,5,6,4,1.5,4,4,4,4,4,3.5,5,5,3,3,3,3,3)

        full_charts <- gridExtra::grid.arrange(grobs = list(header_image, header_details, header_padding, header_www,  # Header 1 - 4
                                                            title_list[[1]], title_list[[2]], title_list[[3]],  # Title 1 5 - 7
                                                            title_list[[5]], title_list[[6]], title_list[[7]], title_list[[8]], title_list[[9]], # Title 2 - Posture 8 - 12
                                                            title_list[[10]], title_list[[11]], title_list[[12]], # Title 2 - Participation 13 - 15
                                                            title_list[[13]], title_list[[15]], title_list[[16]], # Title 2 - Ability 16 - 18
                                                            plot_data_validation_data, plot_data_lying, plot_data_sedentary_standing, plot_data_sedentary_index, # Data - Posture 19 - 22
                                                            plot_data_daily_stepping, plot_data_bouted_stepping, plot_indoor_outdoor_cadence_stepping, plot_data_travel, # Data - Participation 23 - 26
                                                            plot_time_to_first_step, plot_data_peak_cadence_stepping, # Data - Ability 27 - 28
                                                            legend_panel, # plot_data_legend, # Legend 29
                                                            footer_list[[1]], footer_image, footer_list[[2]]), # Footer 30 - 33
                                               ncol = 18,
                                               heights = c(3,2,2,(4 + nrow(page_individual_summary)),8,(items_per_page - nrow(page_individual_summary)),2,1),
                                               widths = col_widths,
                                               layout_matrix = layout_matrix)

        grid_lines <- list()
        grid_height_units <- sum(c(3,2,(4 + nrow(page_individual_summary)),8,(items_per_page - nrow(page_individual_summary)),2,1))
        grid_lines[[1]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                           y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-3)/grid_height_units), "npc"))
        grid_lines[[2]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                           y = unit(c((grid_height_units-5)/grid_height_units,(grid_height_units-5)/grid_height_units), "npc"))
        grid_lines[[3]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                           y = unit(c((grid_height_units-7)/grid_height_units,(grid_height_units-7)/grid_height_units), "npc"))
        grid_lines[[4]] <- grid::linesGrob(x = unit(c(0,0), "npc"),
                                           y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        grid_lines[[5]] <- grid::linesGrob(x = unit(c(20.6/sum(col_widths),20.6/sum(col_widths)), "npc"),
                                           y = unit(c((grid_height_units-5)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        grid_lines[[6]] <- grid::linesGrob(x = unit(c(49/sum(col_widths),49/sum(col_widths)), "npc"),
                                           y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        grid_lines[[7]] <- grid::linesGrob(x = unit(c(57/sum(col_widths),57/sum(col_widths)), "npc"),
                                           y = unit(c((grid_height_units-5)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"),
                                           gp = grid::gpar(color = "grey75"))
        grid_lines[[8]] <- grid::linesGrob(x = unit(c(1,1), "npc"),
                                           y = unit(c((grid_height_units-3)/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))
        if(nrow(page_individual_summary) > 25){
          grid_lines[[9]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                             y = unit(c((grid_height_units-(9.5+nrow(page_individual_summary)))/grid_height_units,(grid_height_units-(9.5+nrow(page_individual_summary)))/grid_height_units), "npc"))
        }else{
          grid_lines[[9]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                             y = unit(c((grid_height_units-(10.5+nrow(page_individual_summary)))/grid_height_units,(grid_height_units-(10.5+nrow(page_individual_summary)))/grid_height_units), "npc"))
        }
        grid_lines[[10]] <- grid::linesGrob(x = unit(c(0,1), "npc"),
                                            y = unit(c((grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units,(grid_height_units-(19+nrow(page_individual_summary)))/grid_height_units), "npc"))

      }
      #####
      # print(full_charts)
      for(j in(1:length(grid_lines))){
        grid::grid.draw(grid_lines[[j]])
      }
    }
  }
