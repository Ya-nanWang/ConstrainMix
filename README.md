# ConstrainMix

**Pseudorandomization with Flexible Constraints**

An R package for generating pseudo-random experimental stimulus lists for psycholinguistic and neurolinguistic experiments. ConstrainMix ensures that your stimuli satisfy multiple constraints while maintaining appropriate randomization.

---

## Table of Contents

- [Features](#features)
- [Background and Motivation](#background-and-motivation)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Core Functions](#core-functions)
- [Constraint Types](#constraint-types)
- [Example Scenarios](#example-scenarios)
- [How It Works](#how-it-works)
- [Advanced Usage](#advanced-usage)
- [Citation](#citation)
- [Requirements](#requirements)
- [License](#license)
- [Contact and Feedback](#contact-and-feedback)

---

## Features

- ✅ **Flexible constraint system**: Define hard (must satisfy) and soft (prefer to satisfy) constraints
- ✅ **Latin square counterbalancing**: Automatic cross-version balance for between-subject designs
- ✅ **Block-based optimization**: Handle experiments with rest breaks between blocks
- ✅ **Multi-factorial balancing**: Balance multiple factors simultaneously
- ✅ **Repetition tracking**: Monitor and control item repetition effects
- ✅ **Comprehensive validation**: Built-in diagnostics and visualization tools
- ✅ **Transparent feedback**: Explicit reporting of constraint violations

---

## Background and Motivation

My colleagues and I have previously used **Mix** (van Casteren & Davis, 2006) as a tool for pseudorandom stimulus ordering. Although Mix sometimes suffers from compatibility issues on modern systems, it was a small, convenient, and extremely useful program for controlling order-related confounds in experiments.

Recently, I noticed that Mix has been removed from the MRC-CBU website and is no longer available for download. This is understandable, given that software developed over 20 years ago may no longer be considered essential now that its functions can largely be replicated through custom coding or other modern means.

Nevertheless, as an experimental linguistics researcher, I realized that there currently does not seem to be an R package that provides comparable functionality for constraint-based pseudorandomization with multi-level stimulus balancing.

Since I needed to implement such functionality for my own project anyway, I decided to take one further step and package the code so that it could be shared with others who face similar challenges. This is how **ConstrainMix** came into being.

While it was conceptually inspired by Mix, the underlying algorithms are different. Based on my own experimental needs, this package is designed to maximize stimulus balance under multiple nested constraints. Unlike the "black box" nature of older executables, ConstrainMix provides explicit feedback mechanisms, allowing users to precisely diagnose which constraints are violated and where.

As the code was originally developed for my own picture–word interference (PWI) experiment, some parts may not be fully generalizable, although I have made efforts to avoid unnecessary task-specific assumptions. Since this package is currently maintained by a single developer, updates and fixes may not always be immediate. Feedback and suggestions are very welcome.

---

## Installation

```r
# Install from GitHub
devtools::install_github("Ya-nanWang/ConstrainMix")

# Load the package
library(ConstrainMix)
```

### Manual Installation

If you prefer to source the files directly:

```r
# Download the repository and source the files
source("R/constraints.R")
source("R/scorer.R")
source("R/solver.R")
source("R/utils.R")
```

---

## Quick Start

### Step 1: Prepare Your Data

Your data should be a data frame (or CSV) with:
- An **item identifier** column (e.g., "Item", "Picture", "Word")
- **Factor columns** you want to balance (e.g., "Condition", "Category")
- Any other properties you want to constrain (e.g., "Frequency", "Length")

```r
library(readr)
library(dplyr)

# Example: Load your experimental items
data <- read_csv("stimuli.csv")
head(data)
```

### Step 2: Define Constraints

```r
# Define what patterns to avoid in your experimental sequence
constraints <- list(
  # Hard constraints (must be 100% satisfied)
  hard_constraint("Category", "max_rep", 2),      # No more than 2 consecutive items from same category
  hard_constraint("Item", "min_dist", 15),        # Same item at least 15 trials apart
  
  # Soft constraints (preferably satisfied)
  soft_constraint("Frequency", "max_rep", 2)      # Prefer varied word frequency
)

# Preview constraints
print_constraint_summary(constraints)
```

### Step 3: Generate Balanced Lists

```r
results <- generate_balanced_lists(
  data = data,
  constraints = constraints,
  n_lists = 6,                    # Generate 6 list versions
  item_col = "Item",              # Column identifying unique items
  balance_cols = "Condition",     # Column(s) to balance across blocks
  n_blocks = 2,                   # 2 experimental blocks
  save_to = "output/"             # Save results to directory
)
```

### Step 4: Validate Results

```r
# Check block-internal balance
check_block_internal_balance(
  data = results[[1]]$data,
  balance_cols = "Condition",
  block_col = "Block"
)

# Check cross-version balance
check_cross_version_balance(
  results = results,
  item_col = "Item",
  balance_cols = "Condition",
  n_blocks = 2
)
```

---

## Core Functions

### Constraint Definition

- **`hard_constraint()`** - Create a hard constraint (must be 100% satisfied)
- **`soft_constraint()`** - Create a soft constraint (preferably satisfied)
- **`constraints_batch()`** - Batch create multiple similar constraints

### List Generation

- **`generate_balanced_lists()`** - Main function for generating balanced experimental lists
- **`constrain_mix()`** - Lower-level function for constraint-based randomization

### Validation & Diagnostics

- **`check_block_internal_balance()`** - Verify condition balance within each block
- **`check_cross_version_balance()`** - Verify Latin square balance across versions
- **`diagnose_violations()`** - Check constraint violations in generated lists
- **`validate_constraints()`** - Pre-check constraint feasibility

### Utilities

- **`add_occurrence_count()`** - Add repetition/occurrence counter
- **`analyze_repetition_distribution()`** - Analyze repetition patterns
- **`export_block_summary()`** - Export block assignment summary

---

## Constraint Types

### Maximum Consecutive Repetitions (`max_rep`)

Limits how many times the same value can appear consecutively.

```r
hard_constraint("Condition", "max_rep", 2)
# ✓ Acceptable: A, A, B, C, C, A
# ✗ Violation:  A, A, A, B, C (3 consecutive A's)
```

### Minimum Distance (`min_dist`)

Ensures minimum spacing between occurrences of the same value.

```r
hard_constraint("Item", "min_dist", 10)
# ✓ Acceptable: Item1 at position 5, next Item1 at position 20 (distance = 15)
# ✗ Violation:  Item1 at position 5, next Item1 at position 10 (distance = 5)
```

---

## Example Scenarios

### Scenario 1: Semantic Priming

```r
# 120 pictures: 60 animals, 60 artifacts
# Each appears twice: Related and Unrelated condition

constraints <- list(
  hard_constraint("Category", "max_rep", 2),     # Vary semantic categories
  hard_constraint("Item", "min_dist", 20),       # Space repetitions
  hard_constraint("Condition", "max_rep", 3)     # Vary conditions
)

results <- generate_balanced_lists(
  data = stimuli,
  constraints = constraints,
  n_lists = 6,
  item_col = "Picture",
  balance_cols = "Condition",
  n_blocks = 2
)
```

### Scenario 2: Multi-Factorial Design with Selective Balancing

```r
# Picture-word interference: 2 Categories × 2 Conditions × 5 SOAs
# Total: 20 combinations, but we only counterbalance SOA levels
# This avoids combinatorial explosion while ensuring SOA balance

# 100 items: 50 animals, 50 artifacts
# Each appears in one of 5 SOA levels (0, 100, 200, 300, 400 ms)
# Both Congruent and Incongruent conditions

constraints <- list(
  hard_constraint("Category", "max_rep", 2),     # Vary semantic categories
  hard_constraint("Condition", "max_rep", 2),    # Vary congruency
  hard_constraint("SOA", "max_rep", 2)           # Vary SOA levels
)

results <- generate_balanced_lists(
  data = stimuli,
  constraints = constraints,
  n_lists = 5,              # 5 versions (one per SOA pattern)
  item_col = "Picture",
  balance_cols = "SOA",     # Latin square ONLY on SOA (not all 20 combinations!)
  n_blocks = 5              # 5 blocks, each block gets one SOA level
)

# This generates C(5,1) = 5 patterns
# Practical and efficient while maintaining rigorous SOA counterbalancing
```

### Scenario 3: Repetition Priming

```r
# 40 items, each repeated 3 times (120 trials total)

constraints <- list(
  hard_constraint("Item", "min_dist", 15),       # Space repetitions
  hard_constraint("Category", "max_rep", 2)      # Vary categories
)

results <- generate_balanced_lists(
  data = stimuli,
  constraints = constraints,
  n_lists = 4,
  item_col = "Picture",
  balance_cols = "Category",
  n_blocks = 1,                                  # No block division
  track_occurrences = TRUE                       # Track 1st, 2nd, 3rd occurrence
)

# Analyze repetition effects
analyze_repetition_distribution(results[[1]]$data, "Occurrence")
```

---

## How It Works

### The Vertical Control Principle (Column-Based)
Following the design philosophy of **Mix** (van Casteren & Davis, 2006), ConstrainMix operates on a **vertical control principle**:
- **Constraint Scope**: All constraints (e.g., `max_rep`, `min_dist`) are applied to specific columns independently.
- **Feature Independence**: The algorithm optimizes the sequence based on the properties defined within those columns.
- **Implementation Note**: It does **not** automatically detect cross-item dependencies (e.g., phonological similarity between a target and the following distractor) unless those features are explicitly represented in a column and constrained.

### Item-Level Latin Square Balancing
Unlike tools that balance only at the condition level, ConstrainMix ensures rigorous **Item-level counterbalancing**:
- **Granular Rotation**: Each unique item (identified by `item_col`) is assigned to a specific rotation pattern.
- **True Counterbalancing**: This ensures that every individual item—regardless of its condition—is balanced across different list versions and experimental blocks over the full set of generated lists.
- **Statistical Rigor**: This approach minimizes item-specific artifacts, ensuring that your experimental effects are not driven by the specific placement of individual stimuli.

### Optimization Algorithm

ConstrainMix uses **simulated annealing** to find optimal orderings:

1. Start with a random shuffle of items
2. Iteratively swap pairs of trials
3. Accept swaps that improve constraint satisfaction
4. Continue until convergence or max iterations

### Latin Square Counterbalancing

For multi-block designs:

1. Divides conditions into blocks using combinatorial patterns
2. Different list versions use different block assignment patterns
3. Ensures each item appears in each pattern approximately equally across versions
4. Achieves between-subject balance

### Constraint Scoring

- **Hard constraints**: High penalty (weight = 10,000) for violations
- **Soft constraints**: Low penalty (weight = 10) for violations
- Total score = sum of weighted violations (lower is better)

---

## Advanced Usage

### Custom Constraint Descriptions

```r
hard_constraint(
  col_name = "PhonologicalNeighbors",
  type = "min_dist",
  val = 5,
  description = "Phonological neighbors should be at least 5 trials apart"
)
```

### Batch Constraint Creation

```r
# Apply same constraint to multiple columns
phonetic_features <- c("Onset", "Vowel", "Tone", "Coda")

all_constraints <- constraints_batch(
  col_names = phonetic_features,
  type = "max_rep",
  val = 1,
  level = "hard"
)

print_constraint_summary(all_constraints)
```

### Relaxing Constraints

If generation fails, try:

```r
# Convert hard constraints to soft
soft_constraint("Feature", "max_rep", 1)  # More lenient

# OR adjust threshold
hard_constraint("Feature", "max_rep", 2)  # Relax threshold

# OR increase optimization steps
results <- generate_balanced_lists(..., max_steps = 20000)
```

### Diagnosing Issues

```r
# Check if constraints are feasible
validate_constraints(data, constraints)

# Examine violations in detail
violations <- diagnose_violations(results[[1]]$data, constraints)
print(violations)
```

---

## Citation

If you use this package in your research, please cite:

```
Wang, Y. (2026). ConstrainMix: Pseudorandomization with Flexible Constraints. 
R package version 0.1.0. https://github.com/Ya-nanWang/ConstrainMix
```

**BibTeX:**
```bibtex
@software{wang2026constrainmix,
  author = {Wang, Yanan},
  title = {ConstrainMix: Pseudorandomization with Flexible Constraints},
  year = {2026},
  url = {https://github.com/Ya-nanWang/ConstrainMix},
  version = {0.1.0}
}
```

---

## Requirements

- R >= 4.0.0
- dplyr >= 1.0.0
- readr >= 2.0.0
- rlang >= 1.0.0

---

## License

MIT License

---

## Contact and Feedback

**Author:** Yanan Wang  
**Email:** ya-nan.wang@outlook.com

### Reporting Issues

If you encounter bugs or have feature requests, please open an issue on GitHub:  
[https://github.com/Ya-nanWang/ConstrainMix/issues](https://github.com/Ya-nanWang/ConstrainMix/issues)

### Contributing

Contributions are welcome! Please feel free to submit a Pull Request. As this package is currently maintained by a single developer, patience is appreciated, but all feedback and suggestions will be carefully considered.
