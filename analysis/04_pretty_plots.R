
# Type 1 error plot -------------------------------------------------------

  plot_data <- results_h0_true %>%
    group_by(test_type, ma_size, odds_ratio, heterogeneity) %>%
      summarise(error_rate = mean(error_rate)) %>% ungroup()

   test_type.labs <- c("egger" = "Egger's Regression Test",
                       "peters" = "Peters' Regression Test")

   heterogeneity.labs <- c("0" = "No Heterogeneity",
                           "0.2" = "20% Heterogeneity",
                           "1.5" = "150% Heterogeneity",
                           "5" = "500% Heterogeneity")

type_1_plot <-   plot_data %>%
                  ggplot(data=., aes(x = ma_size,
                                         y = error_rate,
                                         color = factor(odds_ratio),
                                         shape = factor(odds_ratio))) +
                      facet_grid(heterogeneity~test_type,
                                 labeller = labeller(test_type = test_type.labs,
                                                     heterogeneity = heterogeneity.labs)) +
                      geom_point(aes(x = ma_size,
                                         y = error_rate,
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
                      #ylab("Type 1 error rate") +
                      scale_y_continuous(name = "Type 1 Error Rate, %",
                                         breaks = c(0.20, 0.40, 0.60, 0.80, 1.00),
                                         labels = c("20", "40", "60", "80", "100")) +
                      xlab("No. of Studies") +
                      ggtitle("Type 1 error rate") +
                      theme_classic() +
                      theme(strip.background = element_rect(colour = NA,
                                                            fill = NA),
                            panel.spacing = unit(1, "lines"))
                      #scale_color_manual(values = c("#00AFBB"),# "#E7B800", "#FC4E07"),
                      #        name = "Odds Ratio", #,
                      #        #breaks = c("0.5", "1", "2"),
                     #         labels = c("OR = 1", "OR = 1.5", "OR = 3", "OR = 5")) +
                     # scale_shape_manual(name = "Odds Ratio")

ggsave("analysis/figures/type_1_error.png",
        plot = type_1_plot,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)



plot_data_power <- results_h0_false %>% mutate(bias_detail = case_when(bias_type == "p" & bias_strength == "moderate"~ "p_moderate",
                                                      bias_type == "p" & bias_strength == "severe"~ "p_severe",
                                                      bias_type == "es" & bias_percentage == 0.14 ~ "es_moderate",
                                                      bias_type == "es" & bias_percentage == 0.4 ~ "es_severe")) %>%
    group_by(test_type, ma_size, odds_ratio, heterogeneity, bias_detail) %>%
      summarise(power = mean(error_rate)) %>%
        ungroup()

power_plot2 <-   plot_data_power %>%
                  dplyr::filter(bias_detail == "p_severe") %>%
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
                      #ylab("Power") +
                      scale_y_continuous(name = "Power, %",
                                         breaks = c(0.20, 0.40, 0.60, 0.80, 1.00),
                                         labels = c("20", "40", "60", "80", "100")) +
                      xlab("No. of Studies") +
                      ggtitle("Power") +
                      theme_classic() +
                      theme(strip.background = element_rect(colour = NA,
                                                            fill = NA),
                            panel.spacing = unit(1, "lines"))

ggsave("analysis/figures/power_p_severe.png",
        plot = power_plot2,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)

power_plot3 <-   plot_data_power %>%
                  dplyr::filter(bias_detail == "p_moderate") %>%
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
                      #ylab("Power") +
                      scale_y_continuous(name = "Power, %",
                                         breaks = c(0.20, 0.40, 0.60, 0.80, 1.00),
                                         labels = c("20", "40", "60", "80", "100")) +
                      xlab("No. of Studies") +
                      ggtitle("Power") +
                      theme_classic() +
                      theme(strip.background = element_rect(colour = NA,
                                                            fill = NA),
                            panel.spacing = unit(1, "lines"))

ggsave("analysis/figures/power_p_moderate.png",
        plot = power_plot3,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)

power_plot4 <-   plot_data_power %>%
                  dplyr::filter(bias_detail == "es_moderate") %>%
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
                      scale_shape_manual(values = c(17, 8 ,19, 18, 15)) +                      geom_line(size = 1) +
                      geom_hline(yintercept = 0.1,
                                 linetype = "dotted",
                                 color = "black",
                                 size = 0.9) +
                      labs(color = "Odds Ratio",
                           shape = "Odds Ratio") +
                      #ylab("Power") +
                      scale_y_continuous(name = "Power, %",
                                         breaks = c(0.20, 0.40, 0.60, 0.80, 1.00),
                                         labels = c("20", "40", "60", "80", "100")) +
                      xlab("No. of Studies") +
                      ggtitle("Power") +
                      theme_classic() +
                      theme(strip.background = element_rect(colour = NA,
                                                            fill = NA),
                            panel.spacing = unit(1, "lines"))

ggsave("analysis/figures/power_es_moderate.png",
        plot = power_plot4,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)


power_plot5 <-   plot_data_power %>%
                  dplyr::filter(bias_detail == "es_severe") %>%
                  ggplot(data=., aes(x = ma_size,
                                         y = power,
                                         color = factor(odds_ratio),
                                         shape = factor(odds_ratio))) +
                      facet_grid(heterogeneity ~ test_type,
                                 labeller = labeller(test_type = test_type.labs,
                                                     heterogeneity = heterogeneity.labs)) +
                      geom_point(aes(x = ma_size,
                                         y = power,
                                         color = factor(odds_ratio),
                                         shape = factor(odds_ratio)),
                                 size = 3)+
                      scale_shape_manual(values = c(17, 8 ,19, 18, 15)) +                      geom_line(size = 1) +
                      geom_hline(yintercept = 0.1,
                                 linetype = "dotted",
                                 color = "black",
                                 size = 0.9) +
                      labs(color = "Odds Ratio",
                           shape = "Odds Ratio") +
                      #ylab("Power") +
                      scale_y_continuous(name = "Power, %",
                                         breaks = c(0.20, 0.40, 0.60, 0.80, 1.00),
                                         labels = c("20", "40", "60", "80", "100")) +
                      xlab("No. of Studies") +
                      ggtitle("Power") +
                      theme_classic() +
                      theme(strip.background = element_rect(colour = NA,
                                                            fill = NA),
                            panel.spacing = unit(1, "lines"))

ggsave("analysis/figures/power_es_severe.png",
        plot = power_plot5,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)
