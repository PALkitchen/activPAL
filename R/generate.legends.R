generate.all.outcomes.legend <-
  function(){
    #' @import ggplot2
    x_left <- c(0.5, rep(5.5,5), rep(11.5,3), rep(21.5,4), rep(30,3))
    x_right <- c(1, rep(6,5), rep(12,3), rep(22,4), rep(30.5,3))
    y_bottom <- c(9,seq(9,3,-1.5),seq(9,6,-1.5),seq(9,4.5,-1.5),seq(9,6,-1.5))
    y_top <- c(10,seq(10,4,-1.5),seq(10,7,-1.5),seq(10,5.5,-1.5),seq(10,7,-1.5))
    code <- seq(1,16,1)
    color <- c("#009EE2",
               "#C17100","#F18C00","#FFA700","#FFC451","#F2F200",
               "#38D305","#FEE5D9","#CB181D",
               "#e6ed9d","#ffcc80","#ffa526","#ff2121",
               "#F4858D","#7916B1","#CB181D")

    text_x <- c(1.2,0,
                rep(6.2,6),
                rep(12.2,4),
                rep(22.2,4),
                rep(30.7,3),
                rep(38.2,2),
                rep(48.2,2))
    text_y <- c(9.5,8,
                seq(9.5,1,-1.5),
                seq(9.5,5,-1.5),
                seq(9.5,5,-1.5),
                seq(9.5,6.5,-1.5),
                seq(9.5,8,-1.5),
                seq(9.5,8,-1.5))
    text_label <- c("Time in Bed", "[Average Time out of Bed events]",
                    "Sedentary (4 hours +)", "Sedentary (2 - 4 hours)", "Sedentary (1 - 2 hours)", "Sedentary (30 min - 1 hour)", "Sedentary (< 30 min)", "[Average daily sedentary events]",
                    "Quiet standing", "Stepping (< 1 minute)", "Stepping (1 minute +)",  "Stepping time in bouts < 1 minute",
                    "< 75 steps / minute", "75 - 100 step / minute", "100 - 125 steps / minute", "> 125 steps / minute",
                    "Seated Transport","Cycling","Stepping (1 minute +)",
                    "Broken stepping","Continuous stepping",
                    "Preferred Cadence (bouts < 1 minute)","Preferred Cadence (bouts > 1 minute)")

    insert_x <- c(0.5,20.5,25.5,
                  15.5)
    insert_y <- c(3.3,2.55,2.55,
                  1.8)
    insert_label <- c("(3)","[15]","< 6 >","92%")
    insert_color <- c("#000000","#000000","#ffffff","#ff0000")

    shape_x <- c(47.8,47.8,37.8,37.8)
    shape_y <- c(9.5,8,9.5,8)
    shape_type <- c(23,23,1,21)

    plot_data <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(label = text_label, x = text_x, y = text_y), size = 2.5, hjust = 0) +
      ggplot2::geom_rect(ggplot2::aes(xmin = x_left, ymin = y_bottom, xmax = x_right, ymax = y_top, fill = factor(code))) +
      ggplot2::geom_point(ggplot2::aes(x = shape_x[1], y = shape_y[1], shape = shape_type[1]), size = 3, fill = "#FEE5D9") +
      ggplot2::geom_point(ggplot2::aes(x = shape_x[2], y = shape_y[2], shape = shape_type[2]), size = 3, fill = "#CB181D") +
      ggplot2::geom_point(ggplot2::aes(x = shape_x[3:4], y = shape_y[3:4], shape = shape_type[3:4]), size = 3, fill = "black") +
      ggplot2::scale_x_continuous(limits = c(0,52)) +
      ggplot2::scale_y_continuous(limits = c(0,11)) +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::scale_shape_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    return(plot_data)
  }

##########

generate.posture.outcomes.legend <-
  function(){
    #' @import ggplot2
    x_left <- c(3.5, rep(25.5,5), rep(40.5,3))
    x_right <- c(4, rep(26,5), rep(41,3))
    y_bottom <- c(9,seq(9,3,-1.5),seq(9,6,-1.5))
    y_top <- c(10,seq(10,4,-1.5),seq(10,7,-1.5))
    code <- seq(1,9,1)
    color <- c("#009EE2",
               "#C17100","#F18C00","#FFA700","#FFC451","#F2F200",
               "#38D305","#FEE5D9","#CB181D")

    text_x <- c(4.5,4.5,
                rep(26.5,6),
                rep(41.5,3))
    text_y <- c(9.5,8,
                seq(9.5,1,-1.5),
                seq(9.5,6.5,-1.5))
    text_label <- c("Time in Bed", "[Average Time out of Bed events]",
                    "Sedentary (4 hours +)", "Sedentary (2 - 4 hours)", "Sedentary (1 - 2 hours)", "Sedentary (30 min - 1 hour)", "Sedentary (< 30 min)", "[Average daily sedentary events]",
                    "Quiet standing", "Stepping (< 1 minute)", "Stepping (1 minute +)")

    plot_data <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(label = text_label, x = text_x, y = text_y), size = 2.5, hjust = 0) +
      ggplot2::geom_rect(ggplot2::aes(xmin = x_left, ymin = y_bottom, xmax = x_right, ymax = y_top, fill = factor(code))) +
      ggplot2::scale_x_continuous(limits = c(0,52)) +
      ggplot2::scale_y_continuous(limits = c(0,11)) +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::scale_shape_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    return(plot_data)
  }

##########

generate.participation.outcomes.legend <-
  function(){
    #' @import ggplot2
    x_left <- c(rep(40.5,4))
    x_right <- c(rep(41,4))
    y_bottom <- c(seq(9,4.5,-1.5))
    y_top <- c(seq(10,5.5,-1.5))
    code <- seq(1,4,1)
    color <- c("#e6ed9d","#ffcc80","#ffa526","#ff2121")

    text_x <- c(rep(41.2,4))
    text_y <- c(seq(9.5,5,-1.5))
    text_label <- c("< 75 steps / minute", "75 - 100 step / minute", "100 - 125 steps / minute", "> 125 steps / minute")

    plot_data <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(label = text_label, x = text_x, y = text_y), size = 2.5, hjust = 0) +
      ggplot2::geom_rect(ggplot2::aes(xmin = x_left, ymin = y_bottom, xmax = x_right, ymax = y_top, fill = factor(code))) +
      ggplot2::scale_x_continuous(limits = c(0,52)) +
      ggplot2::scale_y_continuous(limits = c(0,11)) +
      ggplot2::scale_fill_manual(values = color) +
      ggplot2::scale_shape_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    return(plot_data)
  }

##########

generate.ability.outcomes.legend <-
  function(){
    #' @import ggplot2
    text_x <- c(rep(22.2,2),
                rep(42.2,2))
    text_y <- c(seq(9.5,8,-1.5),
                seq(9.5,8,-1.5))
    text_label <- c("Broken stepping","Continuous stepping",
                    "Preferred Cadence (bouts < 1 minute)","Preferred Cadence (bouts > 1 minute)")

    shape_x <- c(20.8,20.8,40.8,40.8)
    shape_y <- c(9.5,8,9.5,8)
    shape_type <- c(1,21,23,23)

    plot_data <- ggplot2::ggplot() +
      ggplot2::geom_text(ggplot2::aes(label = text_label, x = text_x, y = text_y), size = 2.5, hjust = 0) +
      ggplot2::geom_point(ggplot2::aes(x = shape_x[1:2], y = shape_y[1:2], shape = shape_type[1:2]), size = 3, fill = "black") +
      ggplot2::geom_point(ggplot2::aes(x = shape_x[3], y = shape_y[3], shape = shape_type[3]), size = 3, fill = "#FEE5D9") +
      ggplot2::geom_point(ggplot2::aes(x = shape_x[4], y = shape_y[4], shape = shape_type[4]), size = 3, fill = "#CB181D") +
      ggplot2::scale_x_continuous(limits = c(0,52)) +
      ggplot2::scale_y_continuous(limits = c(0,11)) +
      ggplot2::scale_shape_identity() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none")

    return(plot_data)
  }
