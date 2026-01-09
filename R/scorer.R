# =============================================================================
# Scoring System - Calculate Constraint Violation Scores
# =============================================================================

#' Calculate Violation Score (Lightweight)
#'
#' Lightweight scoring function that only returns the total score. Used for 
#' fast evaluation during optimization iterations.
#'
#' @param df A data frame representing the current list sequence.
#' @param constraints A list of constraints.
#'
#' @return A numeric score (lower is better, 0 = perfect).
#' @keywords internal
calculate_score <- function(df, constraints) {
  constraints <- flatten_constraints(constraints)

  total_score <- 0
  n <- nrow(df)
  if (n < 2) return(0)

  for (rule in constraints) {
    if (!rule$col %in% names(df)) next

    vec <- df[[rule$col]]

    if (rule$type == "max_rep") {
      rle_res <- rle(as.character(vec))
      violations <- sum(rle_res$lengths > rule$val)
      if (violations > 0) {
        total_score <- total_score + (violations * rule$weight)
      }
    }

    if (rule$type == "min_dist") {
      for (v in unique(vec)) {
        idx <- which(vec == v)
        if (length(idx) > 1) {
          violations <- sum(diff(idx) < rule$val)
          if (violations > 0) {
            total_score <- total_score + (violations * rule$weight)
          }
        }
      }
    }
  }

  return(total_score)
}


#' Calculate Score with Detailed Violations
#'
#' Complete scoring function that returns all information in one pass: total score, 
#' hard constraint violations, and detailed violation report. Used for final 
#' evaluation after optimization to avoid redundant traversals.
#'
#' @param df A data frame representing the current list sequence.
#' @param constraints A list of constraints.
#'
#' @return A list with:
#'   - score: Total penalty score
#'   - hard_violations: Count of hard constraints violated
#'   - violations: Named vector of violation counts per constraint
#' @keywords internal
calculate_score_detailed <- function(df, constraints) {
  constraints <- flatten_constraints(constraints)

  n <- nrow(df)

  # Initialize report
  report <- integer(length(constraints))
  names(report) <- vapply(constraints, function(x) {
    paste(x$col, x$type, sep = "_")
  }, character(1))

  if (n < 2) {
    return(list(score = 0, hard_violations = 0, violations = report))
  }

  total_score <- 0
  hard_violations <- 0

  for (i in seq_along(constraints)) {
    rule <- constraints[[i]]

    if (!rule$col %in% names(df)) next

    vec <- df[[rule$col]]
    violations <- 0L

    if (rule$type == "max_rep") {
      rle_res <- rle(as.character(vec))
      violations <- sum(rle_res$lengths > rule$val)
    }

    if (rule$type == "min_dist") {
      for (v in unique(vec)) {
        idx <- which(vec == v)
        if (length(idx) > 1) {
          violations <- violations + sum(diff(idx) < rule$val)
        }
      }
    }

    report[i] <- violations

    if (violations > 0) {
      total_score <- total_score + (violations * rule$weight)
      if (rule$level == "hard") {
        hard_violations <- hard_violations + 1
      }
    }
  }

  list(score = total_score, hard_violations = hard_violations, violations = report)
}


#' Diagnose Violations
#'
#' Check if generated lists violate constraints, supports Block analysis.
#' This is a public API that users can call independently to diagnose any data.
#'
#' @param data The data frame to check.
#' @param constraints The list of constraint rules.
#' @param block_col Optional. Name of the block column for block-wise analysis.
#'
#' @return A named vector of violation counts.
#' @export
diagnose_violations <- function(data, constraints, block_col = NULL) {
  constraints <- flatten_constraints(constraints)

  # If block_col is specified, count by block
  if (!is.null(block_col)) {
    if (!block_col %in% names(data)) {
      stop(sprintf("Block column '%s' not found in data!", block_col))
    }

    blocks <- split(data, data[[block_col]])
    total_report <- numeric(length(constraints))
    names(total_report) <- vapply(constraints, function(x) {
      paste(x$col, x$type, sep = "_")
    }, character(1))

    for (b_data in blocks) {
      b_report <- diagnose_violations(b_data, constraints, block_col = NULL)
      total_report <- total_report + b_report
    }
    return(total_report)
  }

  # Diagnosis for single block/global - reuse calculate_score_detailed logic
  result <- calculate_score_detailed(data, constraints)
  return(result$violations)
}


#' Merge Violation Reports from Multiple Blocks
#'
#' Internal function: Merge violation reports from multiple blocks
#'
#' @param violation_list List of violation vectors from each block
#' @return Merged violation vector (sum of all blocks)
#' @keywords internal
merge_violations <- function(violation_list) {
  if (length(violation_list) == 0) return(NULL)
  Reduce(`+`, violation_list)
}
