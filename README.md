# Break-even Example Code

This folder contains a minimal, reusable example of the break-even workflow.

It removes:
- project-specific data
- manuscript-specific wording
- custom plotting styles

It keeps only:
- input validation
- annualized fixed-cost calculation
- break-even calculation
- a simple sensitivity analysis
- minimal plots

## Files

- `run_break_even.R`
  - reads generic input files
  - computes break-even results
  - computes a simple one-way sensitivity summary
  - writes CSV outputs

- `plot_break_even.R`
  - reads the output CSVs
  - creates minimal Figure 1 and Figure 2 style plots

- `input_templates/thresholds_template.csv`
- `input_templates/structural_inputs_template.csv`
- `input_templates/cost_inputs_template.csv`
  - header-only templates with generic column names

## Required input files

Put your own CSVs in a folder and pass that folder as the first argument.

Expected file names:

- `thresholds.csv`
- `structural_inputs.csv`
- `cost_inputs.csv`

## Run

```bash
Rscript run_break_even.R /path/to/input_dir /path/to/output_dir
Rscript plot_break_even.R /path/to/output_dir /path/to/figure_dir
```

## Core logic

Base-case program cost per person:

```text
annualized_fixed_cost = upfront_cost * r / (1 - (1 + r)^(-n))
program_cost = annualized_fixed_cost / throughput + units_per_person * unit_cost
```

If `r <= 0`, fixed cost falls back to straight-line annualization:

```text
annualized_fixed_cost = upfront_cost / n
```

Break-even health gain:

```text
min_health_gain = incremental_cost / threshold_value
required_reduction = min_health_gain / baseline_burden
```

## Generic input structure

### `thresholds.csv`

- `entity`
- `group`
- `scenario`
- `threshold_name`
- `threshold_value`
- `baseline_burden`

### `structural_inputs.csv`

- `name`
- `base`
- `low`
- `high`

Required parameter names:

- `throughput`
- `useful_life`
- `discount_rate`
- `units_per_person`

### `cost_inputs.csv`

- `name`
- `scenario`
- `base`
- `low`
- `high`

Required cost names:

- `equipment_cost`
- `training_cost`
- `unit_cost`

## Notes

- This is a minimal example implementation, not a full economic evaluation.
- The uncertainty interval in the main analysis is a cost-scenario range, not a probability-based confidence interval.
- The sensitivity summary reports mean changes across entities.
