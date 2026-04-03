library(dplyr)
library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript plot_break_even_example.R /path/to/output_dir /path/to/figure_dir")
}

output_dir <- normalizePath(args[1], mustWork = TRUE)
figure_dir <- args[2]
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
figure_dir <- normalizePath(figure_dir, mustWork = TRUE)

results_df <- read.csv(file.path(output_dir, "break_even_results.csv"), stringsAsFactors = FALSE, check.names = FALSE)
sensitivity_df <- read.csv(file.path(output_dir, "sensitivity_summary.csv"), stringsAsFactors = FALSE, check.names = FALSE)

main_thresholds <- unique(results_df$threshold_name)[1:min(2, length(unique(results_df$threshold_name)))]

figure1_df <- results_df %>%
  filter(threshold_name %in% main_thresholds)

figure1 <- ggplot(
  figure1_df,
  aes(x = entity, y = required_reduction_pct, color = threshold_name)
) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_linerange(
    aes(ymin = required_reduction_pct_low, ymax = required_reduction_pct_high),
    position = position_dodge(width = 0.3)
  ) +
  facet_wrap(~group, scales = "free_x") +
  scale_y_log10() +
  labs(
    x = "Entity",
    y = "Required reduction (%)",
    color = "Threshold"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(
  filename = file.path(figure_dir, "minimal_break_even_plot.pdf"),
  plot = figure1,
  width = 10,
  height = 7,
  units = "in"
)

figure2 <- ggplot(
  sensitivity_df,
  aes(x = mean_delta_pp, y = reorder(parameter_label, mean_delta_pp), color = bound)
) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0) +
  facet_wrap(~group, scales = "free_y") +
  labs(
    x = "Mean change in required reduction (percentage points)",
    y = "Parameter",
    color = "Bound"
  ) +
  theme_bw()

ggsave(
  filename = file.path(figure_dir, "minimal_sensitivity_plot.pdf"),
  plot = figure2,
  width = 10,
  height = 6,
  units = "in"
)

message("Wrote figures to: ", figure_dir)
