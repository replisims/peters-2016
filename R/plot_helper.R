# Plotting helper function

power_plot <- function(data, file_path){

  plot <- data %>%
            ggplot(data=., aes(x = ma_size,
                             y = power,
                             color = factor(odds_ratio),
                             shape = factor(odds_ratio))) +
            facet_grid(heterogeneity~test_type,
                       labeller = labeller(test_type = test_type.labs,
                                           heterogeneity = heterogeneity.labs)) +
            geom_point(aes(x = ma_size,
                               y = power,
                               color = factor(odds_ratio),
                               shape = factor(odds_ratio)),
                       size = 3)+
            scale_shape_manual(values = c(17, 8 ,19, 18, 15)) +
            geom_line(size = 1) +
            geom_hline(yintercept = 0.1,
                       linetype = "dotted",
                       color = "black",
                       size = 0.9) +
            labs(color = "Odds Ratio",
                 shape = "Odds Ratio") +
            scale_y_continuous(name = "Power, %",
                               breaks = c(0.20, 0.40, 0.60, 0.80, 1.00),
                               labels = c("20", "40", "60", "80", "100")) +
            xlab("No. of Studies") +
            ggtitle("Power") +
            theme_classic() +
            theme(strip.background = element_rect(colour = NA,
                                                  fill = NA),
                  panel.spacing = unit(1, "lines"))

  ggsave(file_path,
          plot = plot,
          device = "png",
          scale = 1,
          dpi = 300,
          limitsize = TRUE)
}
