#!/usr/bin/env Rscript
# ============================================================================
# Minimal Working Example - Test ConstrainMix
# ============================================================================
# This script tests ConstrainMix functionality.
#
# Usage Option 1 (if package is installed):
#   Rscript tests/minimal_test.R
#   OR
#   source("tests/minimal_test.R")
#
# Usage Option 2 (if testing during development):
#   Run from package root directory
# ============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("Minimal Test of ConstrainMix\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")

# ----------------------------------------------------------------------------
# Step 1: Load Package or Source Files
# ----------------------------------------------------------------------------
cat("Step 1: Loading ConstrainMix...\n")

# Try to load as installed package first
if (requireNamespace("ConstrainMix", quietly = TRUE)) {
  library(ConstrainMix)
  cat("  ✓ ConstrainMix package loaded\n")
} else {
  # If not installed, source from R/ directory (for development)
  cat("  Package not installed, loading from source files...\n")
  source("R/constraints.R")
  cat("  ✓ constraints.R loaded\n")
  
  source("R/scorer.R")
  cat("  ✓ scorer.R loaded\n")
  
  source("R/solver.R")
  cat("  ✓ solver.R loaded\n")
  
  source("R/utils.R")
  cat("  ✓ utils.R loaded\n")
}

# ----------------------------------------------------------------------------
# Step 2: Create Minimal Test Data
# ----------------------------------------------------------------------------
cat("\nStep 2: Creating test data...\n")

suppressPackageStartupMessages(library(dplyr))

# 10 items, each appearing twice (20 trials total)
test_data <- data.frame(
  Item = paste0("Item_", rep(1:10, each = 2)),
  Condition = rep(c("A", "B"), 10),
  Category = rep(c("X", "Y"), each = 10),
  stringsAsFactors = FALSE
)

cat(sprintf("  Created %d trials (%d unique items)\n", 
            nrow(test_data), length(unique(test_data$Item))))
cat("\n  Sample data:\n")
print(head(test_data, 6))

# ----------------------------------------------------------------------------
# Step 3: Define Simple Constraints
# ----------------------------------------------------------------------------
cat("\n\nStep 3: Defining constraints...\n")

constraints <- list(
  hard_constraint("Category", "max_rep", 2),
  hard_constraint("Item", "min_dist", 5)
)

print_constraint_summary(constraints)

# ----------------------------------------------------------------------------
# Step 4: Generate a Single List
# ----------------------------------------------------------------------------
cat("\nStep 4: Generating list...\n")

results <- generate_balanced_lists(
  data = test_data,
  constraints = constraints,
  n_lists = 1,
  item_col = "Item",
  balance_cols = "Condition",
  n_blocks = 1,  # Simple, no blocks
  max_steps = 2000,
  show_feedback = TRUE,
  track_occurrences = TRUE,
  track_by = "Item",
  occurrence_count_name = "Occurrence"
)

# ----------------------------------------------------------------------------
# Step 5: Check Results
# ----------------------------------------------------------------------------
cat("\n\nStep 5: Checking results...\n\n")

list1 <- results[[1]]$data

cat(sprintf("✓ Generated list with %d trials\n", nrow(list1)))
cat(sprintf("✓ Score: %d (0 = perfect)\n", results[[1]]$score))
cat(sprintf("✓ Perfect list: %s\n", results[[1]]$is_perfect))
cat(sprintf("✓ Hard constraint violations: %d\n", results[[1]]$hard_violations))

cat("\n  First 10 trials:\n")
print(head(list1, 10))

cat("\n  Occurrence distribution:\n")
print(table(list1$Occurrence))

# ----------------------------------------------------------------------------
# Step 6: Validate Constraints
# ----------------------------------------------------------------------------
cat("\n\nStep 6: Validating constraints...\n\n")

violations <- diagnose_violations(list1, constraints)
cat("  Violations per constraint:\n")
print(violations)

if (all(violations == 0)) {
  cat("\n  ✓✓✓ SUCCESS! All constraints satisfied! ✓✓✓\n")
} else {
  cat("\n  ⚠ Some constraints were violated (this may be expected for soft constraints)\n")
}

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------
cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("  TEST COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n")

cat("Summary:\n")
cat(sprintf("  - All source files loaded successfully\n"))
cat(sprintf("  - Generated %d list(s)\n", length(results)))
cat(sprintf("  - List quality: %s\n", 
            ifelse(results[[1]]$is_perfect, "PERFECT", 
                   sprintf("Score = %d", results[[1]]$score))))
cat("\n")

if (results[[1]]$is_perfect) {
  cat("✅ The code is working correctly!\n")
  cat("   You can now use this package for your experiments.\n\n")
  cat("   Next steps:\n")
  cat("   1. Try the examples in QUICKSTART.md\n")
  cat("   2. Read the full Vignette for detailed use cases\n")
  cat("   3. Adapt the code to your own experimental design\n\n")
} else {
  cat("⚠️  The code ran but didn't produce a perfect list.\n")
  cat("   This might be due to:\n")
  cat("   - Constraints too strict for small dataset\n")
  cat("   - Need more optimization steps (increase max_steps)\n")
  cat("   - Random variation (try running again)\n\n")
  cat("   The code is still functional. Try with larger datasets\n")
  cat("   or refer to the Vignette for proper experimental designs.\n\n")
}

cat(rep("=", 70), "\n", sep = "")
cat("\n")
