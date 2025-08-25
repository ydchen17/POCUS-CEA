library(triangle)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

set.seed(2025)
n_sim <- 10000
T_horizon <- 5

pv_factor <- function(r) sum((1 + r)^(-seq_len(T_horizon)))

run_psa <- function(FC_equip, FC_train, VC_scan,
                    volume, delta_death, rho,
                    scenario_name) {
  equip_sim <- rtriangle(n_sim,
    a = FC_equip["min"],
    b = FC_equip["max"],
    c = FC_equip["median"]
  )

  train_sim <- rtriangle(n_sim,
    a = FC_train["min"],
    b = FC_train["max"],
    c = FC_train["median"]
  )

  vc_sim <- rtriangle(n_sim,
    a = VC_scan["min"],
    b = VC_scan["max"],
    c = VC_scan["median"]
  )

  alpha_par <- delta_death * 1000
  beta_par <- (1 - delta_death) * 1000
  death_sim <- rbeta(n_sim, alpha_par, beta_par)

  pv_s <- rep(pv_factor(rho), n_sim)
  fc_total <- equip_sim + train_sim
  incr_cost <- fc_total + vc_sim * volume * pv_s
  incr_eff <- death_sim * volume * pv_s

  data.frame(
    dCost    = incr_cost,
    dEffect  = incr_eff,
    scenario = scenario_name
  )
}

params_Global <- list(
  FC_equip    = c(min = 12022.60253, median = 22924.40506, max = 51211.27111),
  FC_train    = c(min = 204.94151, median = 3929.07305, max = 19640.03222),
  VC_scan     = c(min = 7.84310, median = 27.34721, max = 84.90665),
  volume      = 4000,
  delta_death = 0.0193,
  rho         = 0.03,
  year        = 5
)

params_SSA <- list(
  FC_equip    = c(min = 12022.60253, median = 18260.78618, max = 51211.27111),
  FC_train    = c(min = 2231.26824, median = 5626.87787, max = 13703.30186),
  VC_scan     = c(min = 7.84310, median = 27.34721, max = 84.90665),
  volume      = 4000,
  delta_death = 0.0363,
  rho         = 0.03,
  year        = 5
)

# RUN THE PSA SIMULATIONS - THIS WAS MISSING!
df_global <- run_psa(
  FC_equip = params_Global$FC_equip,
  FC_train = params_Global$FC_train,
  VC_scan = params_Global$VC_scan,
  volume = params_Global$volume,
  delta_death = params_Global$delta_death,
  rho = params_Global$rho,
  scenario_name = "Global"
)

df_ssa <- run_psa(
  FC_equip = params_SSA$FC_equip,
  FC_train = params_SSA$FC_train,
  VC_scan = params_SSA$VC_scan,
  volume = params_SSA$volume,
  delta_death = params_SSA$delta_death,
  rho = params_SSA$rho,
  scenario_name = "SSA"
)

# COMBINE THE RESULTS AND ADD ICER CALCULATION
df_all <- bind_rows(df_global, df_ssa) %>%
  mutate(ICER = dCost / dEffect)

# CREATE ICER SUMMARY TABLE
icer_summary <- df_all %>%
  group_by(scenario) %>%
  summarise(
    median_icer = median(ICER, na.rm = TRUE),
    lower_95 = quantile(ICER, 0.025, na.rm = TRUE),
    upper_95 = quantile(ICER, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    median_icer = round(median_icer, 2),
    lower_95    = round(lower_95, 2),
    upper_95    = round(upper_95, 2)
  )

# NOW THE PLOTTING CODE WILL WORK
p1 <- ggplot(df_all, aes(x = dEffect, y = dCost, colour = scenario)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_colour_manual(values = c(Global = "skyblue", SSA = "pink")) +
  labs(
    x      = "Incremental deaths averted",
    y      = "Incremental cost (Int$)",
    colour = "Scenario",
    title  = "A) Cost-effectiveness Planes"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

wtp <- seq(500, 10000, by = 10)
ceac_df <- df_all %>%
  group_by(scenario) %>%
  do({
    tibble(
      wtp     = wtp,
      ce_prob = sapply(wtp, function(lambda) mean(.$ICER < lambda))
    )
  })

# CREATE CEAC SELECTION TABLE
ceac_sel <- ceac_df %>%
  filter(wtp %in% c(1000, 2000, 3000, 5000, 10000)) %>%
  mutate(ce_prob = round(ce_prob, 3))

p2 <- ggplot(ceac_df, aes(x = wtp, y = ce_prob, colour = scenario)) +
  geom_line(size = 1) +
  scale_x_log10(
    breaks = c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 10000),
    labels = c("1k", "2k", "3k", "4k", "5k", "6k", "7k", "8k", "10k")
  ) +
  scale_colour_manual(values = c(Global = "skyblue", SSA = "pink")) +
  labs(
    x      = "Willingness to pay (Int$ per death averted)",
    y      = "Probability ICER < threshold",
    colour = "Scenario",
    title  = "B) Cost-effectiveness Acceptability Curves"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

tv_factor <- function(r, T_horizon = 5) {
  sum((1 + r)^(-seq_len(T_horizon)))
}

compute_icer <- function(vals) {
  pv <- tv_factor(vals$rho, vals$year)
  fc <- vals$FC_equip + vals$FC_train
  vc <- vals$VC_scan * vals$volume * pv
  eff <- vals$delta_death * vals$volume * pv
  (fc + vc) / eff
}

param_grid <- data.frame(
  name = c("FC_equip", "FC_train", "volume", "delta_death", "rho", "year"),
  label = c(
    "Equipment Cost", "Training Cost", "Volume",
    "Delta Death %", "Discount Rate", "Service Life (years)"
  ),
  stringsAsFactors = FALSE
)

make_df_plot <- function(p) {
  baseline <- list(
    FC_equip    = p$FC_equip["median"],
    FC_train    = p$FC_train["median"],
    VC_scan     = p$VC_scan["median"],
    volume      = p$volume,
    delta_death = p$delta_death,
    rho         = p$rho,
    year        = p$year
  )
  grid <- param_grid %>%
    mutate(
      low = case_when(
        name == "FC_equip" ~ p$FC_equip["min"] * 0.5,
        name == "FC_train" ~ p$FC_train["min"] * 0.5,
        name == "volume" ~ p$volume * 0.5,
        name == "delta_death" ~ p$delta_death * 0.5,
        name == "rho" ~ 0.01,
        name == "year" ~ 1
      ),
      high = case_when(
        name == "FC_equip" ~ p$FC_equip["max"] * 2,
        name == "FC_train" ~ p$FC_train["max"] * 2,
        name == "volume" ~ p$volume * 2,
        name == "delta_death" ~ p$delta_death * 2,
        name == "rho" ~ 0.05,
        name == "year" ~ 10
      )
    )

  res <- grid
  res$icer_low <- NA_real_
  res$icer_high <- NA_real_
  for (i in seq_len(nrow(grid))) {
    nm <- grid$name[i]
    low_vals <- baseline
    high_vals <- baseline
    low_vals[[nm]] <- grid$low[i]
    high_vals[[nm]] <- grid$high[i]
    res$icer_low[i] <- compute_icer(low_vals)
    res$icer_high[i] <- compute_icer(high_vals)
  }

  base_icer <- compute_icer(baseline)
  res2 <- res %>%
    mutate(
      diff_low  = icer_low - base_icer,
      diff_high = icer_high - base_icer,
      range     = abs(diff_low) + abs(diff_high),
      label     = factor(label, levels = label[order(range, decreasing = TRUE)])
    )

  df_plot <- res2 %>%
    select(label, diff_low, diff_high) %>%
    pivot_longer(
      cols = c(diff_low, diff_high),
      names_to = "case", values_to = "delta"
    ) %>%
    mutate(case = if_else(case == "diff_low", "Decrease", "Increase"))
  df_plot
}

make_tornado <- function(df_plot, scn, x_lim = NULL) {
  expr_labels <- sapply(levels(df_plot$label), function(l) {
    if (l == "Delta Death %") {
      'Delta~Death~"%"'
    } else if (l == "Discount Rate") {
      '"Discount Rate"'
    } else if (l == "Service Life (years)") {
      '"Service Life (years)"'
    } else {
      paste0('"', l, '"')
    }
  })
  p <- ggplot(df_plot, aes(x = delta, y = label, fill = case)) +
    geom_col(width = 0.6) +
    geom_vline(xintercept = 0, colour = "black") +
    scale_fill_manual(values = c(Decrease = "skyblue", Increase = "pink")) +
    scale_y_discrete(labels = parse(text = expr_labels)) +
    labs(
      x     = "Change in ICER (Int$ per death averted)",
      y     = NULL,
      fill  = "Value",
      title = scn
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  if (!is.null(x_lim)) p <- p + scale_x_continuous(limits = x_lim)
  p
}

df_g <- make_df_plot(params_Global)
df_s <- make_df_plot(params_SSA)
x_lim <- range(c(df_g$delta, df_s$delta))

plot_Global <- make_tornado(df_g, "C) One-way Sensitivity Analysis (Global Average)", x_lim) +
  theme(legend.position = "none")
plot_SSA <- make_tornado(df_s, "D) One-way Sensitivity Analysis (Sub-Saharan Africa)", x_lim)

combined <- (p1 + p2) / (plot_Global + plot_SSA)
ggsave("CEA_plots.pdf", combined, width = 12)

tornado_global <- df_g %>%
  pivot_wider(names_from = case, values_from = delta) %>%
  rename(
    Decrease = Decrease,
    Increase = Increase
  ) %>%
  mutate(Parameter = label) %>%
  select(Parameter, Decrease, Increase)

tornado_ssa <- df_s %>%
  pivot_wider(names_from = case, values_from = delta) %>%
  rename(
    Decrease = Decrease,
    Increase = Increase
  ) %>%
  mutate(Parameter = label) %>%
  select(Parameter, Decrease, Increase)

tornado_combined <- bind_rows(
  tornado_global %>% mutate(Scenario = "Global"),
  tornado_ssa %>% mutate(Scenario = "Sub-Saharan Africa")
) %>%
  select(Scenario, everything()) %>%
  mutate(
    Decrease = round(Decrease, 2),
    Increase = round(Increase, 2)
  )

library(officer)
library(flextable)

ft_icer <- flextable(icer_summary) %>%
  set_header_labels(
    scenario    = "Scenario",
    median_icer = "Median ICER",
    lower_95    = "Lower 95% CI",
    upper_95    = "Upper 95% CI"
  ) %>%
  autofit()

ft_ceac <- flextable(ceac_sel) %>%
  set_header_labels(
    scenario = "Scenario",
    wtp      = "WTP (Int$)",
    ce_prob  = "Pr(ICER < WTP)"
  ) %>%
  autofit()

ft_tornado <- flextable(tornado_combined) %>%
  set_header_labels(
    Scenario  = "Scenario",
    Parameter = "Parameter",
    Decrease  = "Change in ICER (low)",
    Increase  = "Change in ICER (high)"
  ) %>%
  font(fontname = "Arial Unicode MS", part = "all") %>%
  autofit()

doc <- read_docx() %>%
  body_add_par("Table 1. ICER summary by scenario", style = "heading 2") %>%
  body_add_flextable(ft_icer) %>%
  body_add_par("Table 2. CEAC probabilities at selected WTP thresholds", style = "heading 2") %>%
  body_add_flextable(ft_ceac) %>%
  body_add_par("Table 3. One-way sensitivity analysis (both contexts)", style = "heading 2") %>%
  body_add_flextable(ft_tornado)

print(doc, target = "CEA_tables.docx")
