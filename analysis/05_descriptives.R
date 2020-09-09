# descriptives

results_data_descr <- data_paths %>%
  map_df(~{
    import_data <- readRDS(.x)

    import_data %>% dplyr::group_by(scenario_id) %>% summarise(mean_k = mean(original_k),
                                                                       mean_i_squared_unbiased = mean(i_squared_unbiased),
                                                                       mean_i_squared_biased = mean(i_squared_biased)) %>%
        left_join(scenarios, by = "scenario_id")}) %>%
  group_by(scenario_id) %>% summarize(mean_k = mean(mean_k),
                                      mean_i_squared_unbiased = mean(mean_i_squared_unbiased),
                                      mean_i_squared_biased = mean(mean_i_squared_biased))

plot_data_descr <- results_data_descr %>%
  dplyr::left_join(scenarios, by = "scenario_id") %>%
  dplyr::mutate(bias_detail = dplyr::case_when(bias_type == "p" & bias_strength == "moderate"~ "p_moderate",
                                                bias_type == "p" & bias_strength == "severe"~ "p_severe",
                                                bias_type == "es" & bias_percentage == 0.14 ~ "es_moderate",
                                                bias_type == "es" & bias_percentage == 0.4 ~ "es_severe")) %>%
  dplyr::filter(bias_detail %in% c("p_moderate", "p_severe"))

bias.labs <- c("p_moderate" = "Moderate bias (p)",
               "p_severe" = "Severe bias (p)",
               "es_moderate" = "Moderate Bias (es)",
               "es_severe" = "Severe bias (es)")


plot_selection <-   plot_data_descr %>%
                  ggplot(data=., aes(x = ma_size,
                                         y = mean_k,
                                         color = factor(odds_ratio),
                                         shape = factor(odds_ratio))) +
                      xlim(c(0, 100)) +
                      ylim(c(0, max(plot_data_descr$mean_k))) +
                      #coord_fixed(ratio = 1) +
                      facet_grid(heterogeneity~bias_detail,
                                 labeller = labeller(heterogeneity = heterogeneity.labs,
                                                     bias_detail = bias.labs)) +
                      geom_point(size=3) +
                      geom_line(size = 1) +
                      geom_hline(yintercept = 0.1,
                                 linetype = "dotted",
                                 color = "black",
                                 size = 0.9) +
                      labs(color = "Odds Ratio",
                           shape = "Odds Ratio") +
                      ylab("Original Number of Studies") +
                      xlab("No. of Studies after selection bias") +
                      #ggtitle("Mean number of studies generated to obtain the number of studies \n required under 'moderate' and 'severe' bias") +
                      theme_classic() +
                      theme(strip.background = element_rect(colour = NA,
                                                            fill = NA),
                            panel.spacing = unit(1, "lines"))

ggsave("analysis/figures/studies_generated.png",
        plot = plot_selection,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)
