---
title: "leakr: Universal Data Leakage Detector for R"
tags:
  - R
  - data science
  - machine learning
  - data leakage
  - reproducibility
  - model validation
authors:
  - name: Cheryl Isabella Lim
    orcid: 0009-0004-5766-1392
bibliography: paper.bib
---

# Summary

Data leakage, when information from outside the training dataset unintentionally influences a model, is a persistent and underdiagnosed problem in machine learning. It leads to deceptively high performance metrics and unreliable results. 

**leakr** is an R package that provides a unified, registry-based system for detecting, diagnosing, and preventing data leakage across common modeling workflows. It automatically scans datasets for train/test contamination, target leakage, temporal leakage, and duplication issues. 

The package integrates seamlessly with **tidymodels**, **caret**, and **mlr3**, making it compatible with modern R machine learning pipelines. Its design prioritises reproducibility, visual diagnostics, and minimal user configuration, enabling users to detect hidden data issues early in model development.

# Statement of Need

Despite increased attention to model validation, few open-source tools provide comprehensive, automated detection of data leakage in R. Existing solutions tend to be ad hoc, model-specific, or require substantial manual inspection.  

**leakr** fills this gap by offering:
- **Automated, multi-detector auditing:** Detects contamination, target correlation anomalies, temporal misalignment, and duplication.
- **Cross-framework compatibility:** Works with `tidymodels`, `caret`, and `mlr3`.
- **Reproducible snapshots:** Captures data provenance and metadata for transparency.
- **Scalable design:** Efficiently handles large datasets through intelligent sampling and parallelisation.

By unifying these capabilities, leakr helps data scientists, researchers, and practitioners produce more reliable, leak-free models without re-engineering existing pipelines.

# Features and Implementation

The core of **leakr** is the `leakr_audit()` function, which orchestrates a modular system of detectors. Each detector implements a standard interface (`new_*_detector()`, `run_detector()`), returning structured evidence and severity levels.  

Key components include:
- **Train/Test Contamination Detector:** Identifies overlapping IDs or hashed rows between training and testing sets.  
- **Target Leakage Detector:** Flags features with unusually high correlation to the target variable.  
- **Temporal Detector:** Detects future data leakage in time-series.  
- **Duplication Detector:** Finds exact and near-duplicate rows.  

Results are returned as a structured audit report containing summaries, evidence tables, plots, and metadata, allowing both programmatic use and human review.  

Performance-oriented design choices, such as lazy sampling, lightweight hashing, and optional parallel execution, ensure usability even for datasets exceeding millions of rows.

# Example

```r
library(leakr)

# Run an automated audit
report <- leakr_audit(
  data = iris,
  target = "Species",
  config = list(
    correlation_threshold = 0.8,
    plot_results = TRUE
  )
)

# Summarise and visualise findings
leakr_summarise(report)

# Reproducibility and Integration

**leakr** supports reproducible research through its snapshot registry, which records dataset versions, hashes, and metadata in portable formats. This allows leakage audits to be rerun and verified across environments and time.  

The package integrates directly with major R machine learning frameworks, enabling users to insert leakage detection steps anywhere in their modeling pipeline without refactoring code. Visual summaries and structured reports can be exported for publication or audit purposes.

# Acknowledgements

This work was inspired by recurring data leakage incidents encountered in both academic and applied machine learning projects. I thank the open-source R community and contributors to packages such as **mlr3**, **tidymodels**, and **caret**, whose ecosystems shaped the design of **leakr**.

# References

