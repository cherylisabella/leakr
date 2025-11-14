# leakr: Universal Data Leakage Detector for R  <img src="man/figures/leakr-hex.png" align="right" width="175" />

[![CRAN version](https://www.r-pkg.org/badges/version/leakr)](https://CRAN.R-project.org/package=leakr)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/leakr)](https://cran.r-project.org/package=leakr)
[![Cite with Zenodo](http://img.shields.io/badge/DOI-10.5281/zenodo.1343417-1073c8?)](https://doi.org/10.5281/zenodo.17511513)

A comprehensive R package for detecting and diagnosing data leakage in machine learning workflows.

## Overview

Data leakage undermines model validity by allowing information from outside the training dataset to influence model development. **leakr** provides an automated, registry-based detection system that identifies:

- **Train/test contamination**: Overlapping observations between training and testing sets
- **Target leakage**: Features with suspicious relationships to the target variable
- **Temporal leakage**: Using future information to predict the past in time-series data
- **Duplicate records**: Exact and near-duplicate rows that can inflate performance metrics

**Key Features:**
- Automated leakage detection across multiple dimensions
- Rich visualisation and reporting capabilities
- Seamless integration with tidymodels, caret, and mlr3
- Comprehensive I/O support (CSV, Excel, JSON, Parquet, RDS)
- Data snapshots for reproducibility
- Optimised for large datasets with intelligent sampling

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("cherylisabella/leakr")
```

Install from CRAN:

```r
install.packages("leakr")
```

## Quick Start

```r
library(leakr)

# Basic audit on iris dataset
report <- leakr_audit(iris, target = "Species")
print(report)

# Audit with custom configuration
report <- leakr_audit(
  data = iris,
  target = "Species",
  config = list(
    sample_size = 50000,
    correlation_threshold = 0.8,
    contamination_threshold = 0.1,
    plot_results = TRUE
  )
)

# View detailed summary
leakr_summarise(report, top_n = 10, show_config = TRUE)
```

## Table of Contents

- [Core Functions](#core-functions)
  - [leakr_audit()](#leakr_audit)
  - [Built-in Detectors](#built-in-detectors)
- [Data Import/Export](#data-importexport)
- [Visualisation](#visualisation)
- [ML Framework Integration](#ml-framework-integration)
- [Data Snapshots](#data-snapshots)
- [Real-World Examples](#real-world-examples)
- [Best Practices](#best-practices)

---

## Core Functions

### `leakr_audit()`

The main function for comprehensive data leakage detection.

#### Parameters

| Parameter | Type | Description | Default |
|-----------|------|-------------|---------|
| `data` | data.frame | Dataset to audit | Required |
| `target` | character | Target variable name | `NULL` |
| `split` | character/vector | Split variable for train/test division | `NULL` |
| `id` | character | Unique identifier column | `NULL` |
| `detectors` | character vector | Detector names to run | `NULL` (all) |
| `config` | list | Configuration settings | `list()` |

#### Configuration Options

```r
config <- list(
  sample_size = 50000,              # Max rows to analyse
  correlation_threshold = 0.8,      # Suspicious correlation threshold
  contamination_threshold = 0.1,    # Contamination tolerance
  numeric_severity = TRUE,          # Use numeric severity scores
  plot_results = FALSE,             # Generate diagnostic plots
  parallel = FALSE,                 # Enable parallel processing
  seed = 123                        # Random seed for reproducibility
)
```

#### Basic Usage

```r
# Simple audit
report <- leakr_audit(data = my_data, target = "outcome")

# With train/test split
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  split = my_data$split_column
)

# With unique identifier
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  id = "customer_id"
)

# Select specific detectors
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  detectors = c("train_test_contamination", "target_leakage")
)

# List available detectors
list_registered_detectors()
#> [1] "file_format" "train_test_contamination" 
#> [3] "target_leakage" "duplication_detection"
```

#### Report Structure

```r
# Access report components
report$summary        # Issues dataframe with severity, detector, description
report$evidence       # Detector-specific evidence (overlap_ids, leakage_indices, etc.)
report$meta          # Metadata (timestamps, config, sampling info)

# Report metadata
report$meta$n_detectors          # Number of detectors run
report$meta$n_issues            # Total issues found
report$meta$data_shape          # Analysed data dimensions
report$meta$original_data_shape # Original data dimensions
report$meta$was_sampled         # Whether data was sampled
report$meta$detectors_run       # Names of detectors executed
report$meta$timestamp           # Analysis timestamp
report$meta$config_used         # Configuration used
```

### Built-in Detectors

#### 1. Train/Test Contamination Detector

Detects overlapping observations between training and testing sets using ID columns or row hashing.

```r
# Create detector
detector <- new_train_test_detector(threshold = 0.1)

# Run detector
result <- run_detector(
  detector = detector,
  data = my_data,
  split = split_vector,
  id = "customer_id"
)

# Access results
result$issues        # Issue information (severity, description)
result$evidence      # Evidence (overlap_ids or overlap_hashes)
result$detector      # Detector name
```

**Detection Strategy:**
- Compares IDs between train and test sets if `id` provided
- Falls back to row hashing for exact duplicate detection
- Severity levels: 
  - `critical`: ≥2 overlapping observations
  - `low`: <2 overlaps

**Example Output:**
```r
# Example with contamination
result$issues
#> $severity
#> [1] "critical"
#> 
#> $description
#> [1] "Detected 5 overlapping"

result$evidence$overlap_ids
#> [1] "CUST001" "CUST045" "CUST102" "CUST203" "CUST456"
```

#### 2. Temporal Leakage Detector

Identifies temporal misalignment where test data points fall before or too close to the training period.

```r
# Create temporal detector
detector <- new_temporal_detector(
  time_col = "transaction_date",
  lookahead_window = 7  # days
)

# Run detector
result <- run_detector(
  detector = detector,
  data = my_data,
  split = split_vector
)

# Access temporal evidence
result$evidence$max_train_time      # Latest training timestamp
result$evidence$lookahead_window    # Configured lookahead period
result$evidence$n_leak             # Number of leakage violations
result$evidence$leakage_indices    # Indices of problematic rows
```

**Detection Strategy:**
- Validates that test data comes after training data plus a lookahead window
- Severity levels:
  - `high`: ≥20 violations
  - `medium`: 5-19 violations
  - `low`: <5 violations
- Supports `Date` and `POSIXt` time formats

**Example Output:**
```r
result$issues
#> $severity
#> [1] "medium"
#> 
#> $description
#> [1] "Detected 12 test samples with temporal leakage"

result$evidence
#> $max_train_time
#> [1] "2023-12-31"
#> 
#> $lookahead_window
#> Time difference of 7 days
#> 
#> $n_leak
#> [1] 12
```

#### 3. Target Leakage Detector

Identifies features with suspicious correlations to the target variable.

*Automatically registered when package loads.*

```r
# Included in leakr_audit automatically
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  detectors = "target_leakage"
)
```

#### 4. Duplication Detector

Finds exact and near-duplicate rows within datasets.

*Automatically registered when package loads.*

```r
# Included in leakr_audit automatically
report <- leakr_audit(
  data = my_data,
  detectors = "duplication_detection"
)
```

---

## Data Import/Export

### Import Data

leakr provides flexible data import with automatic format detection and preprocessing.

```r
# Auto-detect format from extension
data <- leakr_import("path/to/data.csv")

# Specify format explicitly
data <- leakr_import("path/to/data.xlsx", format = "excel")

# Import with preprocessing
data <- leakr_import(
  "path/to/data.csv",
  preprocessing = list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    clean_column_names = TRUE,
    handle_dates = TRUE,
    remove_constant_cols = TRUE,
    convert_character_to_factor = FALSE,
    max_factor_levels = 100
  )
)

# Quick import (minimal preprocessing)
data <- leakr_quick_import("path/to/data.csv")

# Import from data.frame
data <- leakr_import(iris)
```

#### Supported Formats

| Format | Extensions | Required Package |
|--------|-----------|------------------|
| CSV | `.csv`, `.txt` | Base R |
| TSV | `.tsv` | Base R |
| Excel | `.xlsx`, `.xls` | `readxl` |
| RDS | `.rds` | Base R |
| JSON | `.json` | `jsonlite` |
| Parquet | `.parquet` | `arrow` |

#### Advanced Import Options

```r
# CSV with specific encoding
data <- leakr_import(
  "data.csv",
  encoding = "latin1",
  verbose = TRUE
)

# Excel with specific sheet
data <- leakr_import(
  "workbook.xlsx",
  format = "excel",
  sheet = "Sheet2"  # or sheet = 2
)

# Handle large files (automatic streaming)
# Files > 100MB are automatically limited to first 10k rows
large_data <- leakr_import("very_large_file.csv")
attr(large_data, "leakr_streaming")  # TRUE if streamed
```

#### Preprocessing Features

```r
# Default preprocessing (automatic)
data <- leakr_import(
  "data.csv",
  preprocessing = list(
    remove_empty_rows = TRUE,      # Remove all-NA rows
    remove_empty_cols = TRUE,      # Remove all-NA columns
    clean_column_names = TRUE,     # Standardise column names
    handle_dates = TRUE,           # Auto-detect and convert dates
    remove_constant_cols = FALSE   # Remove columns with single value
  )
)

# Check preprocessing provenance
attr(data, "leakr_provenance")
#> $clean_column_names
#> [1] TRUE
#> 
#> $removed_empty_rows
#> [1] 5
#> 
#> $removed_empty_cols
#> [1] 2
#> 
#> $handle_dates
#> [1] TRUE
```

**Date Detection**: Automatically detects and converts:
- ISO format (YYYY-MM-DD)
- UK format (DD/MM/YYYY)
- US format (MM/DD/YYYY)
- Unix timestamps (seconds and milliseconds)
- Long formats (DD Month YYYY)

### Export Data

```r
# Export to CSV
leakr_export_data(data, "output.csv", format = "csv")

# Export to Excel
leakr_export_data(data, "output.xlsx", format = "excel")

# Export to RDS (recommended for R)
leakr_export_data(data, "output.rds", format = "rds")

# Export to JSON
leakr_export_data(data, "output.json", format = "json")

# Export to Parquet
leakr_export_data(data, "output.parquet", format = "parquet")

# Silent export
leakr_export_data(data, "output.csv", verbose = FALSE)
```

---

## ML Framework Integration

### From mlr3

```r
library(mlr3)
library(leakr)

# Create mlr3 task
task <- TaskClassif$new(
  id = "iris_task",
  backend = iris,
  target = "Species"
)

# Convert to leakr format
leakr_data <- leakr_from_mlr3(task, include_target = TRUE)

# Access components
leakr_data$data          # Full dataset
leakr_data$target        # Target variable
leakr_data$target_name   # Target column name
leakr_data$feature_names # Feature names
leakr_data$task_type     # Task type (e.g., "TaskClassif")

# Run audit
report <- leakr_audit(
  data = leakr_data$data,
  target = leakr_data$target_name
)
```

### From caret

```r
library(caret)
library(leakr)

# Train model with caret
train_obj <- train(
  Species ~ .,
  data = iris,
  method = "rf"
)

# Convert to leakr format
leakr_data <- leakr_from_caret(
  train_obj = train_obj,
  original_data = iris,
  target_name = "Species"
)

# Access components
leakr_data$data          # Training data
leakr_data$method        # Model method
leakr_data$final_model   # Trained model

# Run audit
report <- leakr_audit(
  data = leakr_data$data,
  target = leakr_data$target_name
)
```

### From tidymodels

```r
library(tidymodels)
library(leakr)

# Create workflow
workflow <- workflow() %>%
  add_formula(Species ~ .) %>%
  add_model(decision_tree() %>% set_engine("rpart") %>% set_mode("classification"))

# Convert to leakr format
leakr_data <- leakr_from_tidymodels(workflow, data = iris)

# Access components
leakr_data$data                   # Dataset
leakr_data$has_preprocessor       # Has preprocessing?
leakr_data$preprocessor_type      # Preprocessor type
leakr_data$model_spec            # Model specification

# Run audit
report <- leakr_audit(
  data = leakr_data$data,
  target = "Species"
)
```

### Pipeline Integration Example

```r
library(tidymodels)
library(leakr)

# Safe preprocessing with leakage checks
safe_preprocess <- function(data, target_var, split_var) {
  # Run leakage audit
  audit_report <- leakr_audit(
    data = data,
    target = target_var,
    split = split_var,
    config = list(correlation_threshold = 0.9)
  )
  
  # Check for critical issues
  critical_issues <- audit_report$summary[
    audit_report$summary$severity == "critical", 
  ]
  
  if (nrow(critical_issues) > 0) {
    stop("Critical data leakage detected! Fix before modeling:\n",
         paste(critical_issues$description, collapse = "\n"))
  }
  
  # Warn on high severity
  high_issues <- audit_report$summary[
    audit_report$summary$severity == "high", 
  ]
  
  if (nrow(high_issues) > 0) {
    warning("High severity leakage detected:\n",
            paste(high_issues$description, collapse = "\n"))
  }
  
  return(audit_report)
}

# Use in workflow
data$split <- sample(c("train", "test"), nrow(data), replace = TRUE, prob = c(0.8, 0.2))
leakage_check <- safe_preprocess(data, "outcome", "split")

# Proceed only if no critical issues
if (sum(leakage_check$summary$severity == "critical") == 0) {
  # Continue with modeling
  message("✓ No critical leakage detected - safe to proceed")
}
```

---

## Data Snapshots

Create reproducible snapshots with full provenance tracking.

### Create Snapshot

```r
# Basic snapshot
snapshot_path <- leakr_create_snapshot(
  data = my_data,
  snapshot_name = "baseline_data"
)

# Snapshot with metadata
snapshot_path <- leakr_create_snapshot(
  data = my_data,
  output_dir = "data/snapshots",
  snapshot_name = "experiment_v1",
  metadata = list(
    description = "Initial dataset for fraud detection model",
    experiment_id = "EXP001",
    notes = "Removed duplicates and handled missing values"
  )
)

# Fast hashing for large datasets
snapshot_path <- leakr_create_snapshot(
  data = large_dataset,
  sample_for_hash = TRUE  # Use sampling for faster hashing
)
```

**What's Saved:**
- `data.csv`: Data in CSV format
- `data.rds`: Data in R binary format (recommended)
- `metadata.json`: Complete metadata and provenance
- `README.md`: Human-readable documentation

**Metadata Includes:**
- Creation timestamp
- leakr and R versions
- Data dimensions and types
- Column names and types
- Data hash for integrity checking
- Preprocessing provenance
- Custom user metadata

### Load Snapshot

```r
# Load snapshot (RDS format - recommended)
data <- leakr_load_snapshot("data/snapshots/baseline_data")

# Load from CSV
data <- leakr_load_snapshot(
  "data/snapshots/baseline_data",
  format = "csv"
)

# Verify integrity
data <- leakr_load_snapshot(
  "data/snapshots/baseline_data",
  verify_integrity = TRUE  # Checks hash
)
```

### List Snapshots

```r
# List all snapshots with metadata
snapshots <- leakr_list_snapshots("data/snapshots")
print(snapshots)
#>              name             created  rows cols leakr_version size_mb
#> 1  experiment_v2 2024-01-15 14:30:22  5000   25         0.1.0    2.45
#> 2  experiment_v1 2024-01-14 09:15:00  5000   23         0.1.0    2.31
#> 3  baseline_data 2024-01-10 08:00:00  4800   20         0.1.0    2.10

# Simple listing without metadata
snapshots <- leakr_list_snapshots(
  "data/snapshots",
  include_metadata = FALSE
)
```

---

## Visualization

### Automatic Plotting

```r
# Generate plots with audit
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  config = list(plot_results = TRUE)
)

# Access generated plots
report$plots
```

### Manual Plotting

```r
# Plot detector results
detector <- new_train_test_detector()
result <- run_detector(detector, my_data, split = my_data$split, id = "customer_id")

# Default plot
plot(result)

# Custom palette
plot(result, palette = "Set2")
```

### Report Visualisation

```r
# Print formatted report (automatically shows summary)
print(report)

# Detailed summary with configuration
leakr_summarise(report, top_n = 20, show_config = TRUE)

# Example output:
# Leakage Audit Report
# ===================
# Data shape: 1000 x 25
# Detectors run: train_test_contamination, target_leakage, duplication_detection
# Timestamp: 2024-01-15 14:30:22
# 
# Issues Summary:
#   Critical: 2
#   High: 1
#   Medium: 3
#   Low: 0
# 
# Top Issues:
#   detector                    severity  issue_type              description
#   train_test_contamination    critical  data_contamination      Detected 5 overlapping
#   target_leakage             high      suspicious_correlation   Feature 'total_purchases' ...
```

### Visual Examples

When you run `plot(result)` on a detector result with issues, you'll see:

**Severity Distribution Plot:**
```
┌─────────────────────────────────────┐
│ Leakage Issues by Severity          │
├─────────────────────────────────────┤
│                                     │
│        █████                        │
│        █████                        │
│        █████    ████                │
│        █████    ████    ███         │
│   ███  █████    ████    ███         │
│   ███  █████    ████    ███         │
└───┴────┴────────┴────────┴──────────┘
   low  medium   high   critical
```

**Train/Test Contamination Visualisation:**
Shows overlap patterns between training and testing datasets, highlighting:
- Number of overlapping IDs
- Percentage of contamination
- Distribution of contaminated observations

---

## Real-World Examples

### Example 1: Credit Card Fraud Detection

```r
library(leakr)

# Simulate credit card transaction data
set.seed(42)
fraud_data <- data.frame(
  transaction_id = 1:1000,
  customer_id = sample(1:200, 1000, replace = TRUE),
  amount = round(rnorm(1000, 100, 50), 2),
  transaction_date = seq(as.Date("2023-01-01"), by = "day", length.out = 1000),
  merchant_category = sample(c("retail", "grocery", "online", "restaurant"), 1000, replace = TRUE),
  is_fraud = sample(0:1, 1000, replace = TRUE, prob = c(0.98, 0.02)),
  split = c(rep("train", 700), rep("test", 300))
)

# Comprehensive audit
report <- leakr_audit(
  data = fraud_data,
  target = "is_fraud",
  split = "split",
  id = "transaction_id",
  config = list(
    correlation_threshold = 0.9,
    plot_results = TRUE
  )
)

# Review findings
leakr_summarise(report, top_n = 10)

# Check for temporal issues
temporal_detector <- new_temporal_detector(
  time_col = "transaction_date",
  lookahead_window = 7  # 7-day lookahead
)

temporal_result <- run_detector(
  temporal_detector,
  fraud_data,
  split = fraud_data$split
)

# Examine temporal evidence
cat("Max train date:", as.character(temporal_result$evidence$max_train_time), "\n")
cat("Leakage count:", temporal_result$evidence$n_leak, "\n")
cat("Severity:", temporal_result$issues$severity, "\n")

# Create snapshot for reproducibility
leakr_create_snapshot(
  fraud_data,
  snapshot_name = "fraud_detection_v1",
  metadata = list(
    project = "Fraud Detection",
    version = "1.0",
    split_date = "2023-10-01"
  )
)
```

### Example 2: Customer Churn Prediction

```r
# Simulate customer data
set.seed(123)
n_customers <- 5000

customer_data <- data.frame(
  customer_id = 1:n_customers,
  tenure_months = sample(1:60, n_customers, replace = TRUE),
  monthly_charges = round(rnorm(n_customers, 70, 20), 2),
  total_charges = NA,  # Potential leakage!
  contract_type = sample(c("month-to-month", "one_year", "two_year"), 
                        n_customers, replace = TRUE),
  churned = sample(0:1, n_customers, replace = TRUE, prob = c(0.7, 0.3))
)

# Create total_charges (potential leakage!)
customer_data$total_charges <- 
  customer_data$tenure_months * customer_data$monthly_charges

# Create train/test split
train_idx <- sample(1:n_customers, 0.7 * n_customers)
customer_data$split <- "test"
customer_data$split[train_idx] <- "train"

# Import and preprocess
customer_data_clean <- leakr_import(
  customer_data,
  preprocessing = list(
    clean_column_names = TRUE,
    handle_dates = TRUE,
    remove_constant_cols = TRUE
  )
)

# Audit for leakage
report <- leakr_audit(
  data = customer_data_clean,
  target = "churned",
  split = "split",
  id = "customer_id",
  detectors = c("train_test_contamination", "target_leakage"),
  config = list(correlation_threshold = 0.85)
)

# Check for ID-based contamination
contamination_check <- new_train_test_detector(threshold = 0.05)
contamination_result <- run_detector(
  contamination_check,
  customer_data_clean,
  split = customer_data_clean$split,
  id = "customer_id"
)

# View evidence
if (length(contamination_result$evidence$overlap_ids) > 0) {
  cat("⚠ Found overlapping customer IDs:\n")
  print(head(contamination_result$evidence$overlap_ids))
} else {
  cat("✓ No customer ID overlap detected\n")
}

# Export cleaned data
leakr_export_data(
  customer_data_clean[, !names(customer_data_clean) %in% c("split")],
  "customer_churn_clean.csv"
)
```

### Example 3: Time Series Forecasting

```r
# Simulate time series data
dates <- seq(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "day")
n <- length(dates)

set.seed(456)
timeseries_data <- data.frame(
  date = dates,
  sales = round(1000 + cumsum(rnorm(n, 2, 50))),
  temperature = round(rnorm(n, 20, 5), 1),
  day_of_week = weekdays(dates),
  is_holiday = sample(0:1, n, replace = TRUE, prob = c(0.95, 0.05))
)

# Add lagged features (check for leakage!)
timeseries_data$lag_1_sales <- c(NA, head(timeseries_data$sales, -1))
timeseries_data$lag_7_sales <- c(rep(NA, 7), head(timeseries_data$sales, -7))

# Create temporal split
timeseries_data$split <- ifelse(
  timeseries_data$date < as.Date("2023-10-01"),
  "train",
  "test"
)

# Import and preprocess
ts_clean <- leakr_import(
  timeseries_data,
  preprocessing = list(
    handle_dates = TRUE,
    clean_column_names = TRUE
  )
)

# Check for temporal leakage
temporal_det <- new_temporal_detector(
  time_col = "date",
  lookahead_window = 30  # 30-day lookahead
)

temporal_result <- run_detector(
  temporal_det,
  ts_clean,
  split = ts_clean$split
)

# Comprehensive audit
report <- leakr_audit(
  data = ts_clean,
  target = "sales",
  split = "split",
  config = list(
    correlation_threshold = 0.9,
    plot_results = FALSE
  )
)

# Print temporal analysis
cat("\n=== Temporal Leakage Analysis ===\n")
cat("Max train date:", as.character(temporal_result$evidence$max_train_time), "\n")
cat("Lookahead window:", temporal_result$evidence$lookahead_window, "\n")
cat("Leakage violations:", temporal_result$evidence$n_leak, "\n")
cat("Severity:", temporal_result$issues$severity, "\n")

# Create versioned snapshot
leakr_create_snapshot(
  ts_clean,
  snapshot_name = paste0("timeseries_", format(Sys.Date(), "%Y%m%d")),
  metadata = list(
    date_range = paste(range(ts_clean$date), collapse = " to "),
    features = setdiff(names(ts_clean), c("date", "sales", "split")),
    train_samples = sum(ts_clean$split == "train"),
    test_samples = sum(ts_clean$split == "test")
  )
)
```

### Example 4: Multi-format Data Pipeline

```r
# Complete data pipeline with leakr

# 1. Import from various sources
csv_data <- leakr_import("data/raw/customers.csv")
excel_data <- leakr_import("data/raw/transactions.xlsx", sheet = "2023")
json_data <- leakr_import("data/raw/metadata.json")

# 2. Merge datasets
merged_data <- merge(csv_data, excel_data, by = "customer_id")

# 3. Preprocess with full tracking
clean_data <- leakr_import(
  merged_data,
  preprocessing = list(
    remove_empty_rows = TRUE,
    remove_empty_cols = TRUE,
    clean_column_names = TRUE,
    handle_dates = TRUE,
    remove_constant_cols = TRUE,
    convert_character_to_factor = TRUE,
    max_factor_levels = 50
  )
)

# Check provenance
provenance <- attr(clean_data, "leakr_provenance")
cat("Removed empty rows:", provenance$removed_empty_rows, "\n")
cat("Removed empty cols:", provenance$removed_empty_cols, "\n")

# 4. Create train/test split
set.seed(42)
clean_data$split <- sample(
  c("train", "test"),
  nrow(clean_data),
  replace = TRUE,
  prob = c(0.8, 0.2)
)

# 5. Audit for leakage
report <- leakr_audit(
  data = clean_data,
  target = "target_variable",
  split = "split",
  id = "customer_id",
  config = list(
    correlation_threshold = 0.85,
    contamination_threshold = 0.05,
    plot_results = TRUE
  )
)

# 6. Create snapshot before modeling
snapshot_path <- leakr_create_snapshot(
  clean_data,
  snapshot_name = "model_ready_data",
  metadata = list(
    source_files = c("customers.csv", "transactions.xlsx"),
    preprocessing_steps = names(provenance),
    audit_timestamp = report$meta$timestamp,
    total_issues = report$meta$n_issues
  )
)

# 7. Export in multiple formats for downstream use
leakr_export_data(clean_data, "output/model_data.csv", format = "csv")
leakr_export_data(clean_data, "output/model_data.rds", format = "rds")
leakr_export_data(clean_data, "output/model_data.parquet", format = "parquet")

# 8. List all snapshots for audit trail
snapshots <- leakr_list_snapshots()
print(snapshots)
```

---

## Creating Custom Detectors

Extend leakr with domain-specific detectors:

```r
# Define custom detector function
my_custom_detector <- function(audit_data, config) {
  data <- audit_data$data
  
  # Initialise issues dataframe
  issues <- data.frame(
    severity = character(),
    issue_type = character(),
    description = character(),
    suggested_fix = character(),
    stringsAsFactors = FALSE
  )
  
  # Example: Check for suspicious column names
  suspicious_cols <- grep(
    "_after_|_post_|_future_|_outcome_", 
    names(data), 
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (length(suspicious_cols) > 0) {
    issues <- rbind(issues, data.frame(
      severity = "high",
      issue_type = "suspicious_features",
      description = paste(
        "Found", length(suspicious_cols), 
        "columns with temporal/outcome indicators:",
        paste(head(suspicious_cols, 3), collapse = ", ")
      ),
      suggested_fix = "Review feature engineering for temporal leakage and target-derived features",
      stringsAsFactors = FALSE
    ))
  }
  
  # Check for perfect correlations (potential proxy targets)
  if (!is.null(audit_data$target) && audit_data$target %in% names(data)) {
    numeric_features <- names(data)[sapply(data, is.numeric)]
    numeric_features <- setdiff(numeric_features, audit_data$target)
    
    if (length(numeric_features) > 0) {
      target_vals <- data[[audit_data$target]]
      
      for (feat in numeric_features) {
        if (all(!is.na(data[[feat]])) && all(!is.na(target_vals))) {
          correlation <- cor(data[[feat]], as.numeric(target_vals), 
                           use = "complete.obs")
          
          if (abs(correlation) > 0.95) {
            issues <- rbind(issues, data.frame(
              severity = "critical",
              issue_type = "perfect_correlation",
              description = paste(
                "Feature", feat, 
                "has near-perfect correlation with target:",
                round(correlation, 3)
              ),
              suggested_fix = paste("Remove", feat, "or investigate its derivation"),
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Return standardised result
  list(
    issues = issues,
    evidence = list(
      suspicious_columns = suspicious_cols,
      n_suspicious = length(suspicious_cols)
    )
  )
}

# Register the detector
register_detector(
  name = "custom_feature_check",
  fun = my_custom_detector,
  description = "Checks for suspicious feature patterns and perfect correlations"
)

# Verify registration
"custom_feature_check" %in% list_registered_detectors()
#> [1] TRUE

# Use in audit
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  detectors = c("train_test_contamination", "custom_feature_check")
)
```

---

## Advanced Configuration

### Large Dataset Handling

```r
# Automatic stratified sampling
large_report <- leakr_audit(
  data = large_dataset,  # e.g., 1M rows
  target = "outcome",
  config = list(
    sample_size = 100000,  # Sample to 100k rows
    seed = 42
  )
)

# Check if sampling occurred
if (large_report$meta$was_sampled) {
  cat("Data was sampled:\n")
  cat("  Original:", large_report$meta$original_data_shape[1], "rows\n")
  cat("  Analysed:", large_report$meta$data_shape[1], "rows\n")
}
```

### Parallel Processing

```r
# Enable parallel processing for large datasets
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  config = list(
    parallel = TRUE,
    sample_size = 200000
  )
)
```

### Custom Thresholds

```r
# Strict configuration
strict_report <- leakr_audit(
  data = my_data,
  target = "outcome",
  config = list(
    correlation_threshold = 0.7,       # More sensitive
    contamination_threshold = 0.05,    # Lower tolerance
    numeric_severity = TRUE            # Include severity scores
  )
)

# Lenient configuration (for exploratory analysis)
lenient_report <- leakr_audit(
  data = my_data,
  target = "outcome",
  config = list(
    correlation_threshold = 0.95,      # Less sensitive
    contamination_threshold = 0.2      # Higher tolerance
  )
)
```

### Filtering and Analysing Results

```r
# Filter by severity
critical_issues <- report$summary[report$summary$severity == "critical", ]
serious_issues <- report$summary[
  report$summary$severity %in% c("critical", "high"), 
]

# Filter by detector
train_test_issues <- report$summary[
  report$summary$detector == "train_test_contamination", 
]

# Sort by severity score (if numeric_severity = TRUE)
sorted_issues <- report$summary[
  order(-report$summary$severity_score), 
]

# Get top N most severe issues
top_5_issues <- head(sorted_issues, 5)

# Export issues for documentation
leakr_export_data(
  report$summary,
  "leakage_issues_report.csv"
)
```

---

## Best Practices

### 1. Run Audits Early and Often

```r
# ✓ Good: Audit immediately after data splitting
data$split <- create_split(data)
report <- leakr_audit(data, target = "outcome", split = "split")

# ✗ Bad: Audit after feature engineering and model training
```

### 2. Always Specify ID Columns

```r
# ✓ Good: Use unique identifiers when available
report <- leakr_audit(
  data = customer_data,
  target = "churn",
  split = "split",
  id = "customer_id"  # Explicit ID
)

# ✗ Bad: Rely on row hashing when IDs exist
report <- leakr_audit(
  data = customer_data,
  target = "churn",
  split = "split"
  # Missing id parameter
)
```

### 3. Configure Appropriate Thresholds

```r
# For financial/healthcare: Strict thresholds
report <- leakr_audit(
  data = medical_data,
  target = "diagnosis",
  config = list(
    correlation_threshold = 0.7,
    contamination_threshold = 0.01
  )
)

# For exploratory analysis: Lenient thresholds
report <- leakr_audit(
  data = exploration_data,
  target = "outcome",
  config = list(
    correlation_threshold = 0.95,
    contamination_threshold = 0.1
  )
)
```

### 4. Review Evidence, Not Just Summaries

```r
report <- leakr_audit(data, target = "outcome", split = "split", id = "id")

# Don't just look at summary
print(report$summary)

# ✓ Examine detailed evidence
report$evidence$train_test_contamination$overlap_ids
report$evidence$temporal$leakage_indices
report$evidence$duplication_detection$exact_duplicates
```

### 5. Use Snapshots for Reproducibility

```r
# Before any analysis
snapshot_path <- leakr_create_snapshot(
  raw_data,
  snapshot_name = "raw_data_v1",
  metadata = list(source = "production_db", date = Sys.Date())
)

# After preprocessing
snapshot_path <- leakr_create_snapshot(
  clean_data,
  snapshot_name = "clean_data_v1",
  metadata = list(preprocessing = "removed nulls, normalized")
)

# Before modeling
snapshot_path <- leakr_create_snapshot(
  model_ready_data,
  snapshot_name = "model_ready_v1",
  metadata = list(audit_passed = TRUE, issues = 0)
)
```

### 6. Document Leakage Checks

```r
# Export detailed report
leakr_export_data(report$summary, "audit_report.csv")

# Save configuration for reproducibility
config_used <- report$meta$config_used
saveRDS(config_used, "audit_config.rds")

# Create audit log
audit_log <- data.frame(
  timestamp = report$meta$timestamp,
  n_detectors = report$meta$n_detectors,
  n_issues = report$meta$n_issues,
  critical_issues = sum(report$summary$severity == "critical"),
  high_issues = sum(report$summary$severity == "high"),
  data_shape = paste(report$meta$data_shape, collapse = "x")
)
write.csv(audit_log, "audit_log.csv", row.names = FALSE)
```

### 7. Integrate into CI/CD

```r
# In your testing script
test_that("no data leakage in production pipeline", {
  data <- load_production_data()
  
  report <- leakr_audit(
    data = data,
    target = "target_var",
    split = "split",
    id = "id",
    config = list(
      correlation_threshold = 0.85,
      contamination_threshold = 0.05
    )
  )
  
  # Fail test if critical issues found
  critical_count <- sum(report$summary$severity == "critical")
  expect_equal(critical_count, 0, 
               info = paste("Found", critical_count, "critical leakage issues"))
})
```

### 8. Handle Temporal Data Carefully

```r
# ✓ Good: Proper temporal validation
detector <- new_temporal_detector(
  time_col = "date",
  lookahead_window = 30  # Allow 30-day gap
)

result <- run_detector(
  detector,
  timeseries_data,
  split = timeseries_data$split
)

# Verify temporal ordering
max_train <- max(timeseries_data$date[timeseries_data$split == "train"])
min_test <- min(timeseries_data$date[timeseries_data$split == "test"])
stopifnot(min_test > max_train)
```

---

## Troubleshooting

### Common Issues

#### Q: Why is my audit showing no issues when I know there's leakage?

**A:** Check these common causes:

```r
# 1. Verify split labels
table(data$split)  # Should show "train" and "test"

# 2. Check if target is specified
report <- leakr_audit(data, target = "outcome")  # Don't forget target!

# 3. Lower thresholds for more sensitivity
report <- leakr_audit(
  data,
  target = "outcome",
  config = list(
    correlation_threshold = 0.7,      # Lower = more sensitive
    contamination_threshold = 0.05
  )
)

# 4. Ensure ID column is correct
names(data)  # Verify column names
report <- leakr_audit(data, id = "correct_id_column")
```

#### Q: How do I interpret severity scores?

**A:** Severity levels indicate risk:

```r
# When numeric_severity = TRUE
report$summary$severity_score
# 4 = critical: Immediate action required
# 3 = high: Address before production
# 2 = medium: Investigate and monitor
# 1 = low: Minor issue or false positive

# Filter by severity
critical <- report$summary[report$summary$severity_score >= 4, ]
actionable <- report$summary[report$summary$severity_score >= 3, ]
```

#### Q: Can I use custom split labels?

**A:** Currently requires "train"/"test". Convert your labels:

```r
# Convert custom labels
data$split <- ifelse(data$fold == 1, "train", "test")

# Or use a mapping
split_map <- c("training" = "train", "validation" = "test", "testing" = "test")
data$split <- split_map[data$original_split]
```

#### Q: What if I get "time_col not found" errors?

**A:** Verify column existence and type:

```r
# Check column names
names(data)

# Check column type
class(data$date_column)  # Should be Date or POSIXt

# Convert if needed
data$date_column <- as.Date(data$date_column)

# Then create detector
detector <- new_temporal_detector(time_col = "date_column")
```

#### Q: How do I handle large files that won't import?

**A:** Use streaming or manual chunking:

```r
# Large files are automatically streamed (first 10k rows)
data <- leakr_import("very_large_file.csv")
attr(data, "leakr_streaming")  # Check if streamed

# Or manually limit
data <- leakr_import("large_file.csv", nrows = 50000)

# For full analysis, use sampling in audit
report <- leakr_audit(
  full_data,
  config = list(sample_size = 100000)
)
```

#### Q: Why do I get package dependency errors?

**A:** Install required packages for specific formats:

```r
# For Excel files
install.packages("readxl")

# For JSON
install.packages("jsonlite")

# For Parquet
install.packages("arrow")

# For better CSV performance
install.packages("data.table")

# For mlr3 integration
install.packages("mlr3")

# For tidymodels integration
install.packages("tidymodels")
```

---

## Package Structure

```
leakr/
├── R/
│   ├── core.R              # Main audit function, detector registry
│   ├── constructors.R      # Detector constructors (new_*_detector)
│   ├── pkg-detector.R      # Detector registration system
│   ├── report.R            # Report generation and formatting
│   ├── plot.R              # Plotting functions
│   ├── viz.R               # Visualisation methods
│   ├── io.R                # Import/export and snapshot functions
│   ├── imports.R           # Package imports declaration
│   ├── imports-utils.R     # Utility imports
│   └── zzz.R               # Package initialisation hooks
├── man/                    # Documentation
├── tests/                  # Unit tests
├── vignettes/             # Long-form documentation
└── DESCRIPTION            # Package metadata
```

---

## Roadmap

### Phase 1: Core Tabular Detectors ✅
- Train/test contamination detection
- Target leakage identification  
- Duplicate row detection
- Temporal misalignment checks
- Registry-based detector system
- Comprehensive I/O support
- Data snapshots with provenance

### Phase 2: Enhanced Detectors 🔄
- Feature importance-based leakage detection
- Statistical distribution comparison (KS test, etc.)
- Cross-validation leakage detection
- Grouped time-series validation
- Advanced visualisation dashboard

### Phase 3: Domain-Specific Extensions 📋
- NLP pipeline leakage detection
- Computer vision data leakage (image augmentation checks)
- Multi-modal data validation
- Hierarchical/nested data validation
- Spatial data leakage detection

### Phase 4: Integration & Ecosystem 📋
- Native tidymodels integration
- mlr3 deeper integration  
- Automatic HTML/PDF report generation
- Interactive Shiny dashboard
- Python interoperability via reticulate
- Automated CI/CD integration templates

---

## Contributing

Contributions are welcome! Areas where you can help:

### Adding New Detectors
```r
# Follow this template
my_detector <- function(audit_data, config) {
  # Your detection logic
  issues <- data.frame(
    severity = c("high", "medium"),
    issue_type = c("type1", "type2"),
    description = c("desc1", "desc2"),
    suggested_fix = c("fix1", "fix2")
  )
  
  list(issues = issues, evidence = list(...))
}

register_detector("my_detector", my_detector, "Description")
```

### Improving Documentation
- Add more real-world examples
- Create domain-specific vignettes
- Translate documentation

### Enhancing Visualisations
- New plot types for detector results
- Interactive visualisations
- Better report formatting

### Performance Optimisation
- Parallel processing improvements
- Memory-efficient algorithms
- Faster hashing methods

### Testing
- Edge case coverage
- Integration tests with ML frameworks
- Performance benchmarks

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

---

## Citation

If you use leakr in your research, please cite:

```bibtex
@software{leakr2025,
  title = {leakr: Data Leakage Detection for Machine Learning Workflows},
  author = {Cheryl Isabella},
  year = {2025},
  url = {https://github.com/cherylisabella/leakr},
  version = {0.1.0},
  note = {R package for automated data leakage detection}
}
```

---

## Resources

- **Documentation**: [Package Website](https://cherylisabella.github.io/leakr/)
- **Issue Tracker**: [GitHub Issues](https://github.com/cherylisabella/leakr/issues)
- **CRAN**: [leakr on CRAN](https://cran.r-project.org/package=leakr)

### Related Reading

- Kaufman et al. (2012): ["Leakage in Data Mining: Formulation, Detection, and Avoidance"](https://dl.acm.org/doi/10.1145/2382577.2382579)
- Kaggle: [Data Leakage Tutorial](https://www.kaggle.com/code/alexisbcook/data-leakage)
- Google: [Rules of Machine Learning](https://developers.google.com/machine-learning/guides/rules-of-ml)

---

## License

MIT License - see [LICENSE](LICENSE.md) file for details.

---

## Support

Having issues? Here's how to get help:

1. **Check the documentation**: Most common questions are answered in this README
2. **Search existing issues**: Someone may have encountered your problem
3. **Create a minimal reproducible example**: Use `reprex` package
4. **Open an issue**: [GitHub Issues](https://github.com/cherylisabella/leakr/issues)

For feature requests, use the "enhancement" label when creating an issue.

---

## Quick Reference Card

```r
# ===== IMPORT/EXPORT =====
leakr_import("file.csv")                    # Import with auto-detection
leakr_quick_import("file.xlsx")            # Quick import
leakr_export_data(data, "out.csv")         # Export data

# ===== AUDIT =====
leakr_audit(data, target = "y")            # Basic audit
leakr_audit(data, target = "y", 
           split = "split", id = "id")     # Full audit
list_registered_detectors()                # List detectors

# ===== DETECTORS =====
new_train_test_detector(threshold = 0.1)   # Train/test detector
new_temporal_detector("date", 
                     lookahead_window = 7) # Temporal detector
run_detector(detector, data, split)        # Run detector

# ===== RESULTS =====
report$summary                             # Issues dataframe
report$evidence                            # Detailed evidence
report$meta                               # Metadata
leakr_summarise(report, top_n = 10)       # Summary

# ===== SNAPSHOTS =====
leakr_create_snapshot(data, "name")       # Create snapshot
leakr_load_snapshot("path")               # Load snapshot
leakr_list_snapshots()                    # List snapshots

# ===== INTEGRATION =====
leakr_from_mlr3(task)                     # From mlr3
leakr_from_caret(train_obj, data)         # From caret
leakr_from_tidymodels(workflow, data)     # From tidymodels

# ===== CUSTOM DETECTORS =====
register_detector("name", func, "desc")   # Register
get_detector("name")                      # Retrieve
```
