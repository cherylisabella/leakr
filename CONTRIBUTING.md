# Contributing to leakr

Thank you for your interest in contributing to leakr! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)
- [Pull Request Process](#pull-request-process)
- [Issue Reporting](#issue-reporting)
- [Areas for Contribution](#areas-for-contribution)

---

## Code of Conduct

### Pledge

Contributors and maintainers are committed to providing a welcoming and inclusive environment for all contributors, regardless of experience level, gender, gender identity and expression, sexual orientation, disability, personal appearance, race, ethnicity, age, religion, or nationality.

### Standards

**Examples of behavior that contributes to a positive environment:**
- Using welcoming and inclusive language
- Being respectful of differing viewpoints and experiences
- Gracefully accepting constructive criticism
- Focusing on what is best for the community
- Showing empathy towards other community members

**Examples of unacceptable behavior:**
- Trolling, insulting/derogatory comments, and personal or political attacks
- Public or private harassment
- Publishing others' private information without explicit permission
- Other conduct which could reasonably be considered inappropriate in a professional setting

### Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting the project maintainers. All complaints will be reviewed and investigated promptly and fairly.

---

## Getting Started

### Prerequisites

- **R** ≥ 4.0.0
- **Git** for version control
- **RStudio** (recommended) or another R IDE
- Required packages for development:

```r
# Install development dependencies
install.packages(c(
  "devtools",      # Development tools
  "testthat",      # Testing framework
  "roxygen2",      # Documentation
  "covr",          # Code coverage
  "lintr",         # Code linting
  "styler",        # Code formatting
  "pkgdown",       # Website generation
  "usethis"        # Package development utilities
))

# Optional but recommended
install.packages(c(
  "goodpractice",  # Package quality checks
  "rcmdcheck",     # R CMD check
  "rhub",          # R-hub builder
  "spelling"       # Spell checking
))
```

### Fork and Clone

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:

```bash
git clone https://github.com/YOUR-USERNAME/leakr.git
cd leakr
```

3. **Add upstream remote**:

```bash
git remote add upstream https://github.com/cherylisabella/leakr.git
```

4. **Install the package** in development mode:

```r
devtools::load_all()
```

### Verify Your Setup

Run these checks to ensure your environment is ready:

```r
# Load the package
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()

# Build documentation
devtools::document()
```

---

## Development Workflow

### 1. Create a Branch

Always create a new branch for your work:

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-number-description
```

**Branch naming conventions:**
- `feature/` - New features
- `fix/` - Bug fixes
- `docs/` - Documentation changes
- `refactor/` - Code refactoring
- `test/` - Test additions or modifications
- `perf/` - Performance improvements

### 2. Make Your Changes

- Write clear, readable code
- Follow the [coding standards](#coding-standards)
- Add tests for new functionality
- Update documentation as needed
- Keep commits focused and atomic

### 3. Commit Your Changes

Write clear, descriptive commit messages:

```bash
git add .
git commit -m "Add feature: train/test contamination severity levels

- Implement severity score calculation
- Add tests for edge cases
- Update documentation with examples"
```

**Commit message format:**
```
<type>: <short summary>

<detailed description>

<optional footer>
```

**Types:**
- `feat:` - New feature
- `fix:` - Bug fix
- `docs:` - Documentation changes
- `style:` - Code style changes (formatting, etc.)
- `refactor:` - Code refactoring
- `test:` - Adding or updating tests
- `chore:` - Maintenance tasks

### 4. Keep Your Branch Updated

Regularly sync with the upstream repository:

```bash
git fetch upstream
git rebase upstream/main
```

### 5. Push and Create Pull Request

```bash
git push origin feature/your-feature-name
```

Then create a Pull Request on GitHub.

---

## Coding Standards

### R Code Style

Follow the [tidyverse style guide](https://style.tidyverse.org/) with some modifications:

#### Naming Conventions

```r
# Functions: snake_case
detect_leakage <- function(data) { ... }

# Variables: snake_case
contamination_rate <- 0.05

# Constants: SCREAMING_SNAKE_CASE
MAX_SAMPLE_SIZE <- 50000

# Classes: snake_case with suffix
new_train_test_detector <- function() {
  structure(list(...), class = c("train_test_detector", "detector"))
}

# Private functions: prefix with dot (but avoid when possible)
.calculate_severity <- function() { ... }
```

#### Spacing and Indentation

```r
# Use 2 spaces for indentation (never tabs)
if (condition) {
  do_something()
}

# Space after commas, not before
x <- c(1, 2, 3)

# Space around operators
y <- x + 1
z <- x == 2

# No space around :: or :::
dplyr::select()

# Function calls
result <- my_function(
  param1 = value1,
  param2 = value2,
  param3 = value3
)
```

#### Line Length

- Maximum **80 characters** per line (hard limit: 100)
- Break long function calls across multiple lines

```r
# Good
report <- leakr_audit(
  data = my_data,
  target = "outcome",
  split = "split_column",
  config = list(
    correlation_threshold = 0.8,
    contamination_threshold = 0.1
  )
)

# Bad
report <- leakr_audit(data = my_data, target = "outcome", split = "split_column", config = list(correlation_threshold = 0.8, contamination_threshold = 0.1))
```

#### Documentation

Use roxygen2 for function documentation:

```r
#' @title Detect data leakage in machine learning workflows
#'
#' @description
#' This function performs comprehensive leakage detection by running
#' multiple detectors on the provided dataset.
#'
#' @param data A data.frame containing the dataset to audit.
#' @param target Character string specifying the target variable name.
#'   If NULL, no target-specific checks are performed.
#' @param split Character string or vector specifying the train/test split.
#'   Can be a column name in \code{data} or a separate vector.
#' @param config A list of configuration parameters. See Details for options.
#'
#' @return A \code{leakr_report} object containing:
#'   \item{summary}{Data frame with detected issues}
#'   \item{evidence}{List of detector-specific evidence}
#'   \item{meta}{Metadata about the audit run}
#'
#' @details
#' The \code{config} parameter accepts the following options:
#' \itemize{
#'   \item \code{sample_size}: Maximum number of rows to analyse (default: 50000)
#'   \item \code{correlation_threshold}: Threshold for suspicious correlations (default: 0.8)
#'   \item \code{contamination_threshold}: Threshold for contamination detection (default: 0.1)
#' }
#'
#' @examples
#' # Basic audit
#' report <- detect_leakage(iris, target = "Species")
#'
#' # With custom configuration
#' report <- detect_leakage(
#'   iris,
#'   target = "Species",
#'   config = list(correlation_threshold = 0.9)
#' )
#'
#' @export
#' @seealso \code{\link{leakr_summarise}}, \code{\link{new_train_test_detector}}
detect_leakage <- function(data, target = NULL, split = NULL, config = list()) {
  # Function implementation
}
```

#### Error Handling

```r
# Use informative error messages
if (!is.data.frame(data)) {
  stop("'data' must be a data.frame, not ", class(data)[1])
}

# Validate inputs early
stopifnot(
  is.character(target),
  length(target) == 1,
  target %in% names(data)
)

# Use tryCatch for expected failures
result <- tryCatch({
  risky_operation()
}, error = function(e) {
  warning("Operation failed: ", e$message)
  return(NULL)
})
```

#### Code Organisation

```r
# Order within a file:
# 1. Roxygen header
# 2. Function definition
# 3. Input validation
# 4. Main logic
# 5. Return statement

#' @title Example function
#' @export
example_function <- function(x, y) {
  # 1. Input validation
  stopifnot(is.numeric(x), is.numeric(y))
  
  # 2. Early returns
  if (length(x) == 0) return(numeric(0))
  
  # 3. Main logic
  result <- x + y
  
  # 4. Return
  return(result)
}
```

### Linting and Formatting

Before submitting, run:

```r
# Auto-format code
styler::style_pkg()

# Check for style issues
lintr::lint_package()

# Fix common issues
goodpractice::gp()
```

---

## Testing

### Testing Philosophy

- **Every new function** must have tests
- **Every bug fix** must include a regression test
- **Aim for >80% code coverage**
- **Test edge cases** and error conditions

### Writing Tests

Please use `testthat` for testing. Place tests in `tests/testthat/`:

```r
# tests/testthat/test-contamination.R

test_that("train_test_detector identifies overlapping IDs", {
  # Setup
  data <- data.frame(
    id = c(1, 2, 3, 4, 5, 1, 2),  # IDs 1 and 2 overlap
    value = rnorm(7),
    split = c(rep("train", 5), rep("test", 2))
  )
  
  detector <- new_train_test_detector()
  
  # Execute
  result <- run_detector(detector, data, split = data$split, id = "id")
  
  # Assert
  expect_s3_class(result, "detector_result")
  expect_equal(result$issues$severity, "critical")
  expect_equal(length(result$evidence$overlap_ids), 2)
  expect_true(all(c(1, 2) %in% result$evidence$overlap_ids))
})

test_that("train_test_detector handles no overlap correctly", {
  data <- data.frame(
    id = 1:10,
    value = rnorm(10),
    split = c(rep("train", 7), rep("test", 3))
  )
  
  detector <- new_train_test_detector()
  result <- run_detector(detector, data, split = data$split, id = "id")
  
  expect_equal(result$issues$severity, "low")
  expect_equal(result$issues$description, "No overlap")
})

test_that("train_test_detector validates inputs", {
  detector <- new_train_test_detector()
  
  # Test missing split
  expect_error(
    run_detector(detector, iris, split = NULL, id = "Species"),
    "Split vector is required"
  )
  
  # Test invalid data
  expect_error(
    run_detector(detector, "not a dataframe", split = NULL),
    "data.frame"
  )
})
```

### Test Organisation

```
tests/
├── testthat/
│   ├── helper-functions.R          # Test utilities
│   ├── test-core.R                 # Tests for core.R
│   ├── test-detectors.R            # Tests for detector constructors
│   ├── test-contamination.R        # Tests for contamination detection
│   ├── test-temporal.R             # Tests for temporal detection
│   ├── test-io.R                   # Tests for import/export
│   ├── test-snapshots.R            # Tests for snapshot functionality
│   └── test-integration.R          # Integration tests
└── testthat.R                      # Test runner
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "contamination")

# Run with coverage
covr::package_coverage()

# Generate coverage report
covr::report()
```

### Test Coverage Goals

- **Core functions**: 90%+ coverage
- **Detectors**: 85%+ coverage
- **Utilities**: 75%+ coverage
- **Overall package**: 80%+ coverage

---

## Documentation

### Function Documentation

All exported functions must have complete roxygen2 documentation:

```r
#' @title Short title (one line)
#'
#' @description
#' Longer description explaining what the function does,
#' when to use it, and any important considerations.
#'
#' @param param1 Description of first parameter. Include type and
#'   any constraints or default values.
#' @param param2 Description of second parameter.
#'
#' @return Description of return value. For complex objects, use
#'   \code{\\item} to describe components:
#'   \\item{component1}{Description}
#'   \\item{component2}{Description}
#'
#' @details
#' Additional details, algorithm descriptions, or usage notes.
#'
#' @examples
#' # Basic example
#' result <- my_function(data, param1 = "value")
#'
#' # Advanced example
#' result <- my_function(
#'   data,
#'   param1 = "value",
#'   param2 = list(option1 = TRUE)
#' )
#'
#' @export
#' @seealso \code{\link{related_function}}
#' @family detector_functions
```

### Vignettes

For major features, create vignettes:

```r
usethis::use_vignette("detecting-temporal-leakage")
```

Vignette structure:

```r
---
title: "Detecting Temporal Leakage in Time Series Data"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Detecting Temporal Leakage in Time Series Data}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# Introduction

Brief introduction to the problem and what the vignette covers.

# Basic Usage

Simple examples showing common use cases.

# Advanced Techniques

More complex scenarios and configurations.

# Best Practices

Recommendations and common pitfalls to avoid.
```

### Update Documentation

After making changes:

```r
# Generate documentation
devtools::document()

# Build README
rmarkdown::render("README.Rmd")

# Build website (if using pkgdown)
pkgdown::build_site()
```

---

## Pull Request Process

### Before Submitting

Complete this checklist:

- [ ] Code follows style guide and passes `lintr::lint_package()`
- [ ] All tests pass: `devtools::test()`
- [ ] Code coverage is adequate: `covr::package_coverage()`
- [ ] Documentation is complete and accurate
- [ ] `devtools::check()` passes with no errors, warnings, or notes
- [ ] NEWS.md is updated with changes
- [ ] README.md is updated if needed
- [ ] Commit messages are clear and descriptive

### PR Template

When creating a PR, include:

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to change)
- [ ] Documentation update

## Related Issue
Closes #(issue number)

## Testing
Describe the tests you added/ran

## Checklist
- [ ] My code follows the style guidelines
- [ ] I have performed a self-review
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes
- [ ] Any dependent changes have been merged and published

## Screenshots (if applicable)

## Additional Context
```

### Review Process

1. **Automated checks** run first (CI/CD)
2. **Maintainer review** - typically within 3-5 days
3. **Address feedback** - make requested changes
4. **Approval** - once approved, your PR will be merged

### After Merge

1. Update your local repository:

```bash
git checkout main
git pull upstream main
```

2. Delete your feature branch:

```bash
git branch -d feature/your-feature-name
git push origin --delete feature/your-feature-name
```

---

## Issue Reporting

### Before Creating an Issue

1. **Search existing issues** to avoid duplicates
2. **Update to the latest version** to see if the issue persists
3. **Prepare a reproducible example** using the `reprex` package

### Bug Reports

Use this template:

```markdown
## Bug Description
A clear and concise description of the bug.

## To Reproduce
Steps to reproduce the behavior:
1. Load data with '...'
2. Run function '....'
3. See error

## Reproducible Example
```r
library(leakr)
library(reprex)

# Minimal reproducible example
data <- data.frame(...)
result <- leakr_audit(data, ...)
```

## Expected Behavior
What you expected to happen.

## Actual Behavior
What actually happened.

## Environment
- leakr version: [e.g. 0.1.0]
- R version: [e.g. 4.3.1]
- OS: [e.g. Windows 11, macOS 13, Ubuntu 22.04]

## Additional Context
Any other context about the problem.
```

### Feature Requests

Use this template:

```markdown
## Feature Description
Clear description of the feature you'd like to see.

## Use Case
Describe the problem this feature would solve.

## Proposed Solution
How you envision this feature working.

## Alternatives Considered
Other approaches you've considered.

## Additional Context
Any other context, examples, or references.
```

---

## Areas for Contribution

### High Priority

#### 1. New Detectors
Implement detectors for specific leakage patterns:

```r
# Example: Feature engineering leakage detector
new_feature_engineering_detector <- function() {
  structure(
    list(type = "feature_engineering"),
    class = c("feature_engineering_detector", "detector")
  )
}

run_detector.feature_engineering_detector <- function(detector, data, ...) {
  # Implementation
}
```

**Needed detectors:**
- Feature engineering leakage (using post-split statistics)
- Cross-validation leakage (data preprocessing inside CV folds)
- Group leakage (hierarchical/clustered data)
- Stratification leakage (imbalanced sampling issues)

#### 2. Enhanced Visualisations

```r
# Example: Interactive dashboard with plotly
plot_leakage_dashboard <- function(report) {
  # Interactive plots showing:
  # - Severity distribution
  # - Timeline of issues
  # - Feature importance vs leakage risk
  # - Network graph of data relationships
}
```

#### 3. Integration Tests
Add comprehensive tests for:
- tidymodels workflows
- caret pipelines
- mlr3 tasks
- Real-world datasets

#### 4. Performance Optimisation
- Parallel processing improvements
- Memory-efficient large file handling
- Faster similarity computations
- Caching mechanisms

### Medium Priority

#### 5. Documentation
- Domain-specific vignettes (finance, healthcare, NLP)
- Video tutorials
- Blog posts / case studies
- Translation to other languages

#### 6. Report Generation
- HTML report with interactive elements
- PDF report for compliance/auditing
- Automated email reports
- Integration with RMarkdown

#### 7. Additional File Formats
- SQLite/DuckDB support
- Apache Arrow/Feather
- HDF5 for large scientific data
- Cloud storage integration (S3, GCS, Azure)

### Nice to Have

#### 8. Shiny Dashboard
Interactive web application for:
- Upload data
- Configure detectors
- View results
- Download reports

#### 9. Python Integration
- reticulate-based interoperability
- Python package wrapper
- Jupyter notebook examples

#### 10. CI/CD Templates
- GitHub Actions workflows
- GitLab CI templates
- Jenkins pipelines
- Pre-commit hooks

---

## Development Tips

### Useful Commands

```r
# Quick development cycle
devtools::load_all()    # Load package
devtools::document()    # Update documentation
devtools::test()        # Run tests
devtools::check()       # Full package check

# Debugging
devtools::load_all()
debugonce(my_function)
my_function(test_data)

# Code coverage
covr::package_coverage()
covr::report()

# Spell checking
spelling::spell_check_package()

# Check on different platforms
rhub::check_for_cran()
```

### Common Pitfalls

1. **Forgetting to export functions**: Add `@export` to roxygen

2. **Not updating documentation**: Run `devtools::document()` after changes

3. **Global variables**: Declare them in `R/imports-utils.R`:
   ```r
   utils::globalVariables(c("variable_name"))
   ```

4. **Package dependencies**: Add to DESCRIPTION:
   ```r
   usethis::use_package("packagename")
   usethis::use_package("packagename", type = "Suggests")
   ```

5. **Long-running examples**: Wrap in `\donttest{}`:
   ```r
   #' @examples
   #' \donttest{
   #' # This example takes > 5 seconds
   #' result <- long_running_function()
   #' }
   ```

### Getting Help

- **Documentation**: `?function_name` or `help(function_name)`
- **Package development**: [R Packages book](https://r-pkgs.org/)
- **Style guide**: [Tidyverse style guide](https://style.tidyverse.org/)
- **Testing**: [testthat documentation](https://testthat.r-lib.org/)
- **Community**: [R for Data Science Online Learning Community](https://www.rfordatasci.com/)

---

## Release Process

(For maintainers)

### Version Numbering

Follow [Semantic Versioning](https://semver.org/):
- **MAJOR.MINOR.PATCH** (e.g., 1.2.3)
- **MAJOR**: Breaking changes
- **MINOR**: New features (backwards compatible)
- **PATCH**: Bug fixes (backwards compatible)

### Pre-release Checklist

- [ ] All tests pass on multiple R versions
- [ ] Code coverage >80%
- [ ] `R CMD check` passes with no errors/warnings/notes
- [ ] NEWS.md updated
- [ ] Version number bumped in DESCRIPTION
- [ ] Documentation up to date
- [ ] Examples run successfully
- [ ] Vignettes build correctly

### Release Steps

1. Update NEWS.md with changes
2. Bump version in DESCRIPTION
3. Run full check: `devtools::check()`
4. Build package: `devtools::build()`
5. Tag release: `git tag v0.1.0`
6. Push to GitHub: `git push --tags`
7. Submit to CRAN (if applicable)

---

**Questions?** Open an issue or contact the maintainer.

**Found a security vulnerability?** Please email the maintainer directly rather than opening a public issue.