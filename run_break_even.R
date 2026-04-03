library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript run_break_even_example.R /path/to/input_dir /path/to/output_dir")
}

input_dir <- normalizePath(args[1], mustWork = TRUE)
output_dir <- args[2]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

threshold_path <- file.path(input_dir, "thresholds.csv")
structural_path <- file.path(input_dir, "structural_inputs.csv")
cost_path <- file.path(input_dir, "cost_inputs.csv")

if (!file.exists(threshold_path)) stop("Missing thresholds.csv")
if (!file.exists(structural_path)) stop("Missing structural_inputs.csv")
if (!file.exists(cost_path)) stop("Missing cost_inputs.csv")

threshold_df <- read.csv(threshold_path, stringsAsFactors = FALSE, check.names = FALSE)
structural_df <- read.csv(structural_path, stringsAsFactors = FALSE, check.names = FALSE)
cost_df <- read.csv(cost_path, stringsAsFactors = FALSE, check.names = FALSE)

required_threshold_cols <- c("entity", "group", "scenario", "threshold_name", "threshold_value", "baseline_burden")
required_structural_cols <- c("name", "base", "low", "high")
required_cost_cols <- c("name", "scenario", "base", "low", "high")

missing_threshold_cols <- setdiff(required_threshold_cols, names(threshold_df))
missing_structural_cols <- setdiff(required_structural_cols, names(structural_df))
missing_cost_cols <- setdiff(required_cost_cols, names(cost_df))

if (length(missing_threshold_cols) > 0) stop(sprintf("thresholds.csv is missing: %s", paste(missing_threshold_cols, collapse = ", ")))
if (length(missing_structural_cols) > 0) stop(sprintf("structural_inputs.csv is missing: %s", paste(missing_structural_cols, collapse = ", ")))
if (length(missing_cost_cols) > 0) stop(sprintf("cost_inputs.csv is missing: %s", paste(missing_cost_cols, collapse = ", ")))

for (col in c("threshold_value", "baseline_burden")) {
  threshold_df[[col]] <- as.numeric(threshold_df[[col]])
}

for (col in c("base", "low", "high")) {
  structural_df[[col]] <- as.numeric(structural_df[[col]])
  cost_df[[col]] <- as.numeric(cost_df[[col]])
}

required_structural_names <- c("throughput", "useful_life", "discount_rate", "units_per_person")
required_cost_names <- c("equipment_cost", "training_cost", "unit_cost")

missing_structural_names <- setdiff(required_structural_names, unique(structural_df$name))
missing_cost_names <- setdiff(required_cost_names, unique(cost_df$name))

if (length(missing_structural_names) > 0) stop(sprintf("structural_inputs.csv is missing parameter names: %s", paste(missing_structural_names, collapse = ", ")))
if (length(missing_cost_names) > 0) stop(sprintf("cost_inputs.csv is missing cost names: %s", paste(missing_cost_names, collapse = ", ")))

get_value <- function(df, ..., key = "base") {
  rows <- df
  filters <- list(...)
  for (nm in names(filters)) {
    rows <- rows[rows[[nm]] == filters[[nm]], , drop = FALSE]
  }
  if (nrow(rows) == 0) stop("Requested value not found")
  value <- rows[[key]][1]
  if (is.na(value)) stop("Requested value is missing")
  value
}

annualize_fixed_cost <- function(upfront_cost, useful_life, discount_rate) {
  if (is.na(discount_rate) || discount_rate <= 0) {
    return(upfront_cost / useful_life)
  }
  upfront_cost * discount_rate / (1 - (1 + discount_rate)^(-useful_life))
}

program_cost <- function(structural_df, cost_df, scenario,
                         equipment_key = "base",
                         training_key = "base",
                         unit_key = "base",
                         throughput_key = "base",
                         life_key = "base",
                         discount_key = "base") {
  equipment_cost <- get_value(cost_df, name = "equipment_cost", scenario = scenario, key = equipment_key)
  training_cost <- get_value(cost_df, name = "training_cost", scenario = scenario, key = training_key)
  unit_cost <- get_value(cost_df, name = "unit_cost", scenario = scenario, key = unit_key)

  throughput <- get_value(structural_df, name = "throughput", key = throughput_key)
  useful_life <- get_value(structural_df, name = "useful_life", key = life_key)
  discount_rate <- get_value(structural_df, name = "discount_rate", key = discount_key)
  units_per_person <- get_value(structural_df, name = "units_per_person", key = "base")

  annualized_fixed_cost <- annualize_fixed_cost(equipment_cost + training_cost, useful_life, discount_rate)
  annualized_fixed_cost / throughput + units_per_person * unit_cost
}

scenario_df <- data.frame(
  scenario = sort(unique(threshold_df$scenario)),
  stringsAsFactors = FALSE
)

scenario_df$incremental_cost <- sapply(scenario_df$scenario, function(x) program_cost(structural_df, cost_df, x))
scenario_df$incremental_cost_low <- sapply(scenario_df$scenario, function(x) program_cost(structural_df, cost_df, x, equipment_key = "low", training_key = "low", unit_key = "low"))
scenario_df$incremental_cost_high <- sapply(scenario_df$scenario, function(x) program_cost(structural_df, cost_df, x, equipment_key = "high", training_key = "high", unit_key = "high"))

results_df <- merge(threshold_df, scenario_df, by = "scenario", all.x = TRUE, sort = FALSE)

results_df$min_health_gain <- results_df$incremental_cost / results_df$threshold_value
results_df$required_reduction <- results_df$min_health_gain / results_df$baseline_burden
results_df$required_reduction_pct <- 100 * results_df$required_reduction
results_df$required_reduction_pct_low <- 100 * (results_df$incremental_cost_low / results_df$threshold_value / results_df$baseline_burden)
results_df$required_reduction_pct_high <- 100 * (results_df$incremental_cost_high / results_df$threshold_value / results_df$baseline_burden)
results_df$target_relative_risk <- 1 - results_df$required_reduction

summary_df <- results_df %>%
  group_by(group, threshold_name) %>%
  summarise(
    n = dplyr::n_distinct(entity),
    median_required_reduction_pct = median(required_reduction_pct, na.rm = TRUE),
    .groups = "drop"
  )

sensitivity_defs <- data.frame(
  parameter_key = c(
    "equipment_cost", "equipment_cost",
    "training_cost", "training_cost",
    "unit_cost", "unit_cost",
    "throughput", "throughput",
    "useful_life", "useful_life",
    "discount_rate", "discount_rate",
    "base"
  ),
  parameter_label = c(
    "Equipment cost", "Equipment cost",
    "Training cost", "Training cost",
    "Unit cost", "Unit cost",
    "Throughput", "Throughput",
    "Useful life", "Useful life",
    "Discount rate", "Discount rate",
    "Base-case"
  ),
  bound = c("low", "high", "low", "high", "low", "high", "low", "high", "low", "high", "low", "high", "base"),
  multiplier = c(0.5, 1.5, 0.5, 1.5, 0.5, 1.5, 0.5, 1.5, 0.5, 1.5, 1.0, 1.0, 1.0),
  stringsAsFactors = FALSE
)

cost_sensitivity_list <- list()
k <- 1

for (scenario in unique(threshold_df$scenario)) {
  for (i in seq_len(nrow(sensitivity_defs))) {
    parameter_key <- sensitivity_defs$parameter_key[i]
    bound <- sensitivity_defs$bound[i]
    multiplier <- sensitivity_defs$multiplier[i]

    if (parameter_key == "base") {
      incremental_cost <- program_cost(structural_df, cost_df, scenario)
    } else if (parameter_key == "equipment_cost") {
      source_key <- if (bound == "low") "low" else "high"
      equipment_override <- get_value(cost_df, name = "equipment_cost", scenario = scenario, key = source_key) * multiplier
      annualized_fixed_cost <- annualize_fixed_cost(
        equipment_override + get_value(cost_df, name = "training_cost", scenario = scenario, key = "base"),
        get_value(structural_df, name = "useful_life", key = "base"),
        get_value(structural_df, name = "discount_rate", key = "base")
      )
      incremental_cost <- annualized_fixed_cost / get_value(structural_df, name = "throughput", key = "base") +
        get_value(structural_df, name = "units_per_person", key = "base") * get_value(cost_df, name = "unit_cost", scenario = scenario, key = "base")
    } else if (parameter_key == "training_cost") {
      source_key <- if (bound == "low") "low" else "high"
      training_override <- get_value(cost_df, name = "training_cost", scenario = scenario, key = source_key) * multiplier
      annualized_fixed_cost <- annualize_fixed_cost(
        get_value(cost_df, name = "equipment_cost", scenario = scenario, key = "base") + training_override,
        get_value(structural_df, name = "useful_life", key = "base"),
        get_value(structural_df, name = "discount_rate", key = "base")
      )
      incremental_cost <- annualized_fixed_cost / get_value(structural_df, name = "throughput", key = "base") +
        get_value(structural_df, name = "units_per_person", key = "base") * get_value(cost_df, name = "unit_cost", scenario = scenario, key = "base")
    } else if (parameter_key == "unit_cost") {
      source_key <- if (bound == "low") "low" else "high"
      unit_override <- get_value(cost_df, name = "unit_cost", scenario = scenario, key = source_key) * multiplier
      annualized_fixed_cost <- annualize_fixed_cost(
        get_value(cost_df, name = "equipment_cost", scenario = scenario, key = "base") + get_value(cost_df, name = "training_cost", scenario = scenario, key = "base"),
        get_value(structural_df, name = "useful_life", key = "base"),
        get_value(structural_df, name = "discount_rate", key = "base")
      )
      incremental_cost <- annualized_fixed_cost / get_value(structural_df, name = "throughput", key = "base") +
        get_value(structural_df, name = "units_per_person", key = "base") * unit_override
    } else if (parameter_key == "throughput") {
      source_key <- if (bound == "low") "low" else "high"
      throughput_override <- get_value(structural_df, name = "throughput", key = source_key) * multiplier
      annualized_fixed_cost <- annualize_fixed_cost(
        get_value(cost_df, name = "equipment_cost", scenario = scenario, key = "base") + get_value(cost_df, name = "training_cost", scenario = scenario, key = "base"),
        get_value(structural_df, name = "useful_life", key = "base"),
        get_value(structural_df, name = "discount_rate", key = "base")
      )
      incremental_cost <- annualized_fixed_cost / throughput_override +
        get_value(structural_df, name = "units_per_person", key = "base") * get_value(cost_df, name = "unit_cost", scenario = scenario, key = "base")
    } else if (parameter_key == "useful_life") {
      source_key <- if (bound == "low") "low" else "high"
      life_override <- get_value(structural_df, name = "useful_life", key = source_key) * multiplier
      annualized_fixed_cost <- annualize_fixed_cost(
        get_value(cost_df, name = "equipment_cost", scenario = scenario, key = "base") + get_value(cost_df, name = "training_cost", scenario = scenario, key = "base"),
        life_override,
        get_value(structural_df, name = "discount_rate", key = "base")
      )
      incremental_cost <- annualized_fixed_cost / get_value(structural_df, name = "throughput", key = "base") +
        get_value(structural_df, name = "units_per_person", key = "base") * get_value(cost_df, name = "unit_cost", scenario = scenario, key = "base")
    } else if (parameter_key == "discount_rate") {
      source_key <- if (bound == "low") "low" else "high"
      discount_override <- get_value(structural_df, name = "discount_rate", key = source_key) * multiplier
      annualized_fixed_cost <- annualize_fixed_cost(
        get_value(cost_df, name = "equipment_cost", scenario = scenario, key = "base") + get_value(cost_df, name = "training_cost", scenario = scenario, key = "base"),
        get_value(structural_df, name = "useful_life", key = "base"),
        discount_override
      )
      incremental_cost <- annualized_fixed_cost / get_value(structural_df, name = "throughput", key = "base") +
        get_value(structural_df, name = "units_per_person", key = "base") * get_value(cost_df, name = "unit_cost", scenario = scenario, key = "base")
    }

    cost_sensitivity_list[[k]] <- data.frame(
      scenario = scenario,
      parameter_label = sensitivity_defs$parameter_label[i],
      bound = bound,
      incremental_cost = incremental_cost,
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
}

cost_sensitivity_df <- do.call(rbind, cost_sensitivity_list)

baseline_threshold_name <- unique(threshold_df$threshold_name)[1]

sensitivity_entity_df <- merge(
  threshold_df[threshold_df$threshold_name == baseline_threshold_name, ],
  cost_sensitivity_df,
  by = "scenario",
  all = FALSE,
  sort = FALSE
)

sensitivity_entity_df$required_reduction_pct <- 100 * (
  sensitivity_entity_df$incremental_cost / sensitivity_entity_df$threshold_value / sensitivity_entity_df$baseline_burden
)

base_by_entity <- sensitivity_entity_df %>%
  filter(bound == "base") %>%
  select(entity, group, scenario, base_required_reduction_pct = required_reduction_pct)

sensitivity_delta_df <- sensitivity_entity_df %>%
  left_join(base_by_entity, by = c("entity", "group", "scenario")) %>%
  mutate(delta_pp = required_reduction_pct - base_required_reduction_pct)

sensitivity_summary_df <- sensitivity_delta_df %>%
  filter(bound != "base") %>%
  group_by(group, parameter_label, bound) %>%
  summarise(
    n = sum(!is.na(delta_pp)),
    mean_delta_pp = mean(delta_pp, na.rm = TRUE),
    sd_delta_pp = sd(delta_pp, na.rm = TRUE),
    se_delta_pp = sd_delta_pp / sqrt(n),
    t_crit = qt(0.975, df = pmax(n - 1, 1)),
    ci_low = mean_delta_pp - t_crit * se_delta_pp,
    ci_high = mean_delta_pp + t_crit * se_delta_pp,
    .groups = "drop"
  )

write.csv(results_df, file.path(output_dir, "break_even_results.csv"), row.names = FALSE)
write.csv(summary_df, file.path(output_dir, "break_even_summary.csv"), row.names = FALSE)
write.csv(sensitivity_summary_df, file.path(output_dir, "sensitivity_summary.csv"), row.names = FALSE)

message("Wrote outputs to: ", output_dir)
