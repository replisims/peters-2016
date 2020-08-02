# Plotting
library(patchwork)

biased_boxplot <- compile_plot_data(sim_data = sim_data, scenarios = scenarios) %>%
  boxplot_biased

unbiased_boxplot <- compile_plot_data(sim_data = sim_data, scenarios = scenarios) %>%
  bloxplot_unbiased

pdf("i_squared_plot.pdf", width = 16, height = 8)

biased_boxplot + unbiased_boxplot +
  plot_layout(guides = 'collect')

dev.off()


# # Load analysed data ------------------------------------------------------
#
results_h0_true <- readRDS("results_h0_true.rds")
results_h0_false <- readRDS("results_h0_false.rds")


# Type I error rate plot --------------------------------------------------

plot_type_1 <- plot_error_rate(df = results_h0_true,
                  titel = "Type 1 error rate",
                  yaxis = "Type 1 error rate",
                  facet1 = ma_size,
                  facet2 = odds_ratio,
                  facet3 = heterogeneity,
                  facet4 = bias_type)$plots

# Power plot --------------------------------------------------------------

plot_power <- plot_error_rate(df = results_h0_false,
                  titel = "Power",
                  yaxis = "Power",
                  facet1 = ma_size,
                  facet2 = odds_ratio,
                  facet3 = heterogeneity,
                  facet4 = bias_type)$plots

pdf("error_rate_plot.pdf", width = 16, height = 8)

  ((plot_type_1[[1]] / plot_type_1[[2]]) | wrap_plots(plot_power)) +
    plot_layout(guides = 'collect')

  dev.off()
