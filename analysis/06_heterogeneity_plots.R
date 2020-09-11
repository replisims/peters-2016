# Plotting intended vs observed heterogeneity

plot_data_descr2 <- results_data_descr %>%
  dplyr::left_join(scenarios, by = "scenario_id") %>%
  dplyr::mutate(bias_detail = dplyr::case_when(bias_type == "p" & bias_strength == "moderate"~ "p_moderate",
                                                bias_type == "p" & bias_strength == "severe"~ "p_severe",
                                                bias_type == "es" & bias_percentage == 0.14 ~ "es_moderate",
                                                bias_type == "es" & bias_percentage == 0.4 ~ "es_severe",
                                                bias_type == "es" & bias_percentage == 0 ~ "no_pub_bias"))

df_summary <- plot_data_descr2 %>%
  group_by(heterogeneity, bias_detail) %>%
  summarise(
    low = quantile(mean_i_squared_biased, probs = 0.25),
    med = median(mean_i_squared_biased),
    up = quantile(mean_i_squared_biased, probs = 0.75)
                 )

df_summary2 <- plot_data_descr2 %>%
  group_by(heterogeneity, bias_detail) %>%
  summarise(
    low = quantile(mean_i_squared_unbiased, probs = 0.25),
    med = median(mean_i_squared_unbiased),
    up = quantile(mean_i_squared_unbiased, probs = 0.75)
                 )

biased_boxplot <- plot_data_descr2 %>%
    boxplot_biased

ggsave("analysis/figures/i_squared_biased.png",
        plot = biased_boxplot,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)

unbiased_boxplot <- plot_data_descr2 %>%
    bloxplot_unbiased

ggsave("analysis/figures/i_squared_unbiased.png",
        plot = unbiased_boxplot,
        device = "png",
        scale = 1,
        dpi = 300,
        limitsize = TRUE)
