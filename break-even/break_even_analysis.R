library(dplyr)
data_dir <- "data"
outputs_dir <- "outputs"

dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

threshold_path <- file.path(data_dir, "thresholds.csv")
structural_path <- file.path(data_dir, "structural_parameters.csv")
cost_path <- file.path(data_dir, "cost_inputs.csv")

if (!file.exists(threshold_path)) stop("Missing file: data/thresholds.csv")
if (!file.exists(structural_path)) stop("Missing file: data/structural_parameters.csv")
if (!file.exists(cost_path)) stop("Missing file: data/cost_inputs.csv")

threshold_df <- read.csv(threshold_path, stringsAsFactors = FALSE, check.names = FALSE)
structural_df <- read.csv(structural_path, stringsAsFactors = FALSE, check.names = FALSE)
cost_df <- read.csv(cost_path, stringsAsFactors = FALSE, check.names = FALSE)

required_threshold_cols <- c(
  "country",
  "income_label",
  "model_cost_scenario",
  "threshold_label",
  "ce_threshold",
  "baseline_burden"
)

required_structural_cols <- c("name", "base", "low", "high")
required_cost_cols <- c("name", "scenario", "base", "low", "high")

missing_threshold_cols <- setdiff(required_threshold_cols, names(threshold_df))
missing_structural_cols <- setdiff(required_structural_cols, names(structural_df))
missing_cost_cols <- setdiff(required_cost_cols, names(cost_df))

if (length(missing_threshold_cols) > 0) {
  stop(sprintf("thresholds.csv is missing required columns: %s", paste(missing_threshold_cols, collapse = ", ")))
}

if (length(missing_structural_cols) > 0) {
  stop(sprintf("structural_parameters.csv is missing required columns: %s", paste(missing_structural_cols, collapse = ", ")))
}

if (length(missing_cost_cols) > 0) {
  stop(sprintf("cost_inputs.csv is missing required columns: %s", paste(missing_cost_cols, collapse = ", ")))
}

threshold_df$ce_threshold <- suppressWarnings(as.numeric(threshold_df$ce_threshold))
threshold_df$baseline_burden <- suppressWarnings(as.numeric(threshold_df$baseline_burden))

for (col in c("base", "low", "high")) {
  structural_df[[col]] <- suppressWarnings(as.numeric(ifelse(trimws(structural_df[[col]]) == "", NA, structural_df[[col]])))
  cost_df[[col]] <- suppressWarnings(as.numeric(ifelse(trimws(cost_df[[col]]) == "", NA, cost_df[[col]])))
}

required_structural_names <- c("throughput", "useful_life", "uptake", "units_per_person")
required_cost_names <- c("equipment_cost_package", "training_cost_package", "per_unit_cost")

missing_structural_names <- setdiff(required_structural_names, unique(structural_df$name))
missing_cost_names <- setdiff(required_cost_names, unique(cost_df$name))

if (length(missing_structural_names) > 0) {
  stop(sprintf("structural_parameters.csv is missing required parameter names: %s", paste(missing_structural_names, collapse = ", ")))
}

if (length(missing_cost_names) > 0) {
  stop(sprintf("cost_inputs.csv is missing required cost names: %s", paste(missing_cost_names, collapse = ", ")))
}

get_value <- function(df, ..., key = "base") {
  rows <- df
  filters <- list(...)

  for (nm in names(filters)) {
    rows <- rows[rows[[nm]] == filters[[nm]], , drop = FALSE]
  }

  if (nrow(rows) == 0) stop("Requested value not found in input table")

  value <- rows[[key]][1]

  if (is.na(value)) stop("Requested value is missing")

  value
}

program_cost <- function(structural_df, cost_df, scenario,
                         equipment_key = "base",
                         training_key = "base",
                         per_unit_key = "base",
                         throughput_key = "base",
                         life_key = "base") {
  equipment_cost <- if (any(cost_df$name == "equipment_cost_package" & cost_df$scenario == scenario)) {
    get_value(cost_df, name = "equipment_cost_package", scenario = scenario, key = equipment_key)
  } else {
    get_value(cost_df, name = "equipment_cost_package", scenario = "all", key = equipment_key)
  }

  training_cost <- if (any(cost_df$name == "training_cost_package" & cost_df$scenario == scenario)) {
    get_value(cost_df, name = "training_cost_package", scenario = scenario, key = training_key)
  } else {
    get_value(cost_df, name = "training_cost_package", scenario = "all", key = training_key)
  }

  per_unit_cost <- if (any(cost_df$name == "per_unit_cost" & cost_df$scenario == scenario)) {
    get_value(cost_df, name = "per_unit_cost", scenario = scenario, key = per_unit_key)
  } else {
    get_value(cost_df, name = "per_unit_cost", scenario = "all", key = per_unit_key)
  }

  throughput <- get_value(structural_df, name = "throughput", key = throughput_key)
  useful_life <- get_value(structural_df, name = "useful_life", key = life_key)
  uptake <- get_value(structural_df, name = "uptake", key = "base")
  units_per_person <- get_value(structural_df, name = "units_per_person", key = "base")

  fixed_cost <- (equipment_cost + training_cost) / (useful_life * throughput)
  variable_cost <- uptake * units_per_person * per_unit_cost

  fixed_cost + variable_cost
}

threshold_df$income_panel <- ifelse(
  threshold_df$income_label %in% c("Low income", "Not classified"),
  "Low income",
  ifelse(
    threshold_df$income_label == "Lower middle income",
    "Lower-middle income",
    ifelse(
      threshold_df$income_label == "Upper middle income",
      "Upper-middle income",
      threshold_df$income_label
    )
  )
)

panel_levels <- c("Low income", "Lower-middle income", "Upper-middle income")
threshold_df$income_panel <- factor(threshold_df$income_panel, levels = panel_levels)

scenario_df <- data.frame(
  model_cost_scenario = sort(unique(threshold_df$model_cost_scenario)),
  stringsAsFactors = FALSE
)

scenario_df$incremental_cost <- sapply(
  scenario_df$model_cost_scenario,
  function(x) program_cost(structural_df, cost_df, x)
)

break_even_country_df <- merge(
  threshold_df,
  scenario_df,
  by = "model_cost_scenario",
  all.x = TRUE,
  sort = FALSE
)

break_even_country_df$break_even_burden_reduction <- break_even_country_df$incremental_cost / break_even_country_df$ce_threshold
break_even_country_df$break_even_relative_reduction <- break_even_country_df$break_even_burden_reduction / break_even_country_df$baseline_burden
break_even_country_df$break_even_relative_reduction_pct <- 100 * break_even_country_df$break_even_relative_reduction

break_even_country_df <- break_even_country_df[order(
  break_even_country_df$income_panel,
  break_even_country_df$country,
  break_even_country_df$threshold_label
), ]

summary_df <- break_even_country_df %>%
  group_by(income_panel, threshold_label) %>%
  summarise(
    n_countries = dplyr::n_distinct(country),
    median_break_even_relative_reduction_pct = median(break_even_relative_reduction_pct, na.rm = TRUE),
    p25_break_even_relative_reduction_pct = quantile(break_even_relative_reduction_pct, 0.25, na.rm = TRUE),
    p75_break_even_relative_reduction_pct = quantile(break_even_relative_reduction_pct, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

country_examples <- c(
  "Kenya",
  "Ethiopia",
  "Uganda",
  "Cambodia",
  "Congo, Dem. Rep.",
  "Guatemala",
  "Philippines"
)

country_examples_df <- break_even_country_df[break_even_country_df$country %in% country_examples, ]

country_examples_df$country[country_examples_df$country == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"

country_order <- c(
  "Kenya",
  "Ethiopia",
  "Uganda",
  "Cambodia",
  "Democratic Republic of the Congo",
  "Guatemala",
  "Philippines"
)

country_examples_df$country <- factor(country_examples_df$country, levels = country_order)

country_examples_df <- country_examples_df %>%
  select(
    country,
    income_panel,
    threshold_label,
    model_cost_scenario,
    incremental_cost,
    ce_threshold,
    baseline_burden,
    break_even_burden_reduction,
    break_even_relative_reduction,
    break_even_relative_reduction_pct
  ) %>%
  arrange(country, threshold_label)

sensitivity_defs <- data.frame(
  parameter_key = c(
    "device_cost", "device_cost",
    "training_cost", "training_cost",
    "per_unit", "per_unit",
    "throughput", "throughput",
    "device_life", "device_life",
    "base"
  ),
  parameter_label = c(
    "Device cost", "Device cost",
    "Training cost", "Training cost",
    "Per-unit cost", "Per-unit cost",
    "Throughput", "Throughput",
    "Useful life", "Useful life",
    "Base-case"
  ),
  bound = c("low", "high", "low", "high", "low", "high", "low", "high", "low", "high", "base"),
  stringsAsFactors = FALSE
)

available_scenarios <- sort(unique(cost_df$scenario))
available_scenarios <- available_scenarios[available_scenarios != "all"]
if (length(available_scenarios) == 0) available_scenarios <- "all"

cost_sensitivity_list <- list()
k <- 1

for (scenario in available_scenarios) {
  for (i in seq_len(nrow(sensitivity_defs))) {
    parameter_key <- sensitivity_defs$parameter_key[i]
    bound <- sensitivity_defs$bound[i]

    if (parameter_key == "base") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario)
    } else if (parameter_key == "device_cost") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario, equipment_key = bound)
    } else if (parameter_key == "training_cost") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario, training_key = bound)
    } else if (parameter_key == "per_unit") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario, per_unit_key = bound)
    } else if (parameter_key == "throughput") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario, throughput_key = bound)
    } else if (parameter_key == "device_life") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario, life_key = bound)
    } else {
      stop("Unknown sensitivity parameter")
    }

    cost_sensitivity_list[[k]] <- data.frame(
      model_cost_scenario = scenario,
      parameter_key = parameter_key,
      parameter_label = sensitivity_defs$parameter_label[i],
      bound = bound,
      incremental_cost = incremental_cost,
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
}

cost_sensitivity_df <- do.call(rbind, cost_sensitivity_list)

baseline_threshold_label <- if ("Ochalek low" %in% threshold_df$threshold_label) {
  "Ochalek low"
} else {
  unique(threshold_df$threshold_label)[1]
}

sensitivity_country_df <- merge(
  threshold_df[threshold_df$threshold_label == baseline_threshold_label, ],
  cost_sensitivity_df,
  by = "model_cost_scenario",
  all = FALSE,
  sort = FALSE
)

sensitivity_country_df$break_even_relative_reduction_pct <- 100 * sensitivity_country_df$incremental_cost / sensitivity_country_df$ce_threshold / sensitivity_country_df$baseline_burden

sensitivity_summary_raw <- sensitivity_country_df %>%
  group_by(income_panel, parameter_key, parameter_label, bound) %>%
  summarise(
    median_break_even_relative_reduction_pct = median(break_even_relative_reduction_pct, na.rm = TRUE),
    p25_break_even_relative_reduction_pct = quantile(break_even_relative_reduction_pct, 0.25, na.rm = TRUE),
    p75_break_even_relative_reduction_pct = quantile(break_even_relative_reduction_pct, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

base_break_even_by_income <- sensitivity_summary_raw %>%
  filter(bound == "base") %>%
  transmute(
    income_panel,
    base_break_even_relative_reduction_pct = median_break_even_relative_reduction_pct
  )

sensitivity_summary_df <- sensitivity_summary_raw %>%
  filter(bound != "base") %>%
  left_join(base_break_even_by_income, by = "income_panel") %>%
  mutate(
    delta_pp = median_break_even_relative_reduction_pct - base_break_even_relative_reduction_pct,
    delta_p25 = p25_break_even_relative_reduction_pct - base_break_even_relative_reduction_pct,
    delta_p75 = p75_break_even_relative_reduction_pct - base_break_even_relative_reduction_pct
  ) %>%
  select(
    income_panel,
    parameter_label,
    bound,
    delta_pp,
    delta_p25,
    delta_p75
  )

write.csv(
  break_even_country_df,
  file.path(outputs_dir, "break_even_country_results.csv"),
  row.names = FALSE
)

write.csv(
  summary_df,
  file.path(outputs_dir, "break_even_summary_by_income_and_threshold.csv"),
  row.names = FALSE
)

write.csv(
  country_examples_df,
  file.path(outputs_dir, "break_even_country_examples.csv"),
  row.names = FALSE
)

write.csv(
  sensitivity_summary_df,
  file.path(outputs_dir, "break_even_sensitivity_summary.csv"),
  row.names = FALSE
)