# =============================================================================
# Constraint System - For Psycho/Neurolinguistic Experiments
# =============================================================================

#' Create a Hard Constraint (Must Be Satisfied)
#'
#' Creates a hard constraint - must be 100% satisfied, otherwise the list will 
#' be marked as invalid and discarded.
#'
#' @param col_name Column name to constrain (e.g., "Condition", "Tar_Onset")
#' @param type "max_rep" (maximum consecutive repetitions) or "min_dist" (minimum distance)
#' @param val Threshold value (e.g., 1, 2, 10)
#' @param description Optional. Human-readable description for the report.
#'
#' @return A constraint object used by constrain_mix()
#' @export
#'
#' @examples
#' hard_constraint("Condition", "max_rep", 2)
#' hard_constraint("Picture", "min_dist", 10)
hard_constraint <- function(col_name, type = c("max_rep", "min_dist"), val,
                            description = NULL) {
  type <- match.arg(type)

  if (is.null(description)) {
    description <- if (type == "max_rep") {
      sprintf("%s cannot repeat consecutively more than %d times", col_name, val)
    } else {
      sprintf("Same %s must be at least %d trials apart", col_name, val)
    }
  }

  structure(
    list(
      col = col_name,
      type = type,
      val = val,
      level = "hard",
      weight = 10000,
      description = description
    ),
    class = "constraint"
  )
}


#' Create a Soft Constraint (Prefer But Not Required)
#'
#' Creates a soft constraint - algorithm will try to satisfy it, but will not 
#' throw an error if unable to. Suitable for conditions where compromises are 
#' necessary due to item limitations (e.g., certain semantic categories are too few).
#'
#' @param col_name Column name to constrain
#' @param type "max_rep" or "min_dist"
#' @param val Threshold value
#' @param description Optional. Human-readable description
#'
#' @return A constraint object
#' @export
#'
#' @examples
#' soft_constraint("SemanticCat", "max_rep", 1)
soft_constraint <- function(col_name, type = c("max_rep", "min_dist"), val,
                            description = NULL) {
  type <- match.arg(type)

  if (is.null(description)) {
    description <- if (type == "max_rep") {
      sprintf("%s should preferably not repeat consecutively more than %d times", col_name, val)
    } else {
      sprintf("Same %s should preferably be %d trials apart", col_name, val)
    }
  }

  structure(
    list(
      col = col_name,
      type = type,
      val = val,
      level = "soft",
      weight = 10,
      description = description
    ),
    class = "constraint"
  )
}


#' Batch Create Constraints
#'
#' Batch creation of multiple constraints of the same type (e.g., simultaneously 
#' limiting four dimensions of Target and Distractor)
#'
#' @param col_names Vector of column names (e.g. c("Tar_Onset", "Dis_Onset"))
#' @param type "max_rep" or "min_dist"
#' @param val Threshold value
#' @param level "hard" (default) or "soft"
#'
#' @return List of constraints
#' @export
#'
#' @examples
#' constraints_batch(c("Tar_Onset", "Tar_Tone"), type = "max_rep", val = 1)
constraints_batch <- function(col_names, type = "max_rep", val = 1,
                              level = c("hard", "soft")) {
  level <- match.arg(level)

  lapply(col_names, function(col) {
    if (level == "hard") {
      hard_constraint(col, type = type, val = val)
    } else {
      soft_constraint(col, type = type, val = val)
    }
  })
}


#' Print Constraint Summary
#'
#' Display constraint configuration summary
#'
#' @param constraints List of constraints
#' @export
print_constraint_summary <- function(constraints) {
  constraints <- flatten_constraints(constraints)

  cat("\n", rep("=", 70), "\n", sep = "")
  cat("Constraint Configuration Summary\n")
  cat(rep("=", 70), "\n", sep = "")

  n_hard <- sum(vapply(constraints, function(x) x$level == "hard", logical(1)))
  n_soft <- sum(vapply(constraints, function(x) x$level == "soft", logical(1)))

  cat(sprintf("Total: %d constraints (Hard: %d, Soft: %d)\n\n",
              length(constraints), n_hard, n_soft))

  for (i in seq_along(constraints)) {
    c <- constraints[[i]]
    icon <- if (c$level == "hard") "\U0001F534" else "\U0001F7E1"
    type_str <- if (c$type == "max_rep") {
      sprintf("MaxRep <= %d", c$val)
    } else {
      sprintf("MinDist >= %d", c$val)
    }
    cat(sprintf("%s %2d. %-15s | %-14s | %s\n",
                icon, i, c$col, type_str, c$level))
  }

  cat(rep("-", 70), "\n", sep = "")
  cat("\U0001F534 Hard: Must be fully satisfied, otherwise list is invalid\n")
  cat("\U0001F7E1 Soft: Preferably satisfied, minor violations allowed\n")
  cat(rep("=", 70), "\n\n", sep = "")
}


#' Validate Constraints Configuration
#'
#' Check if constraint configuration is reasonable
#'
#' @param data Data frame
#' @param constraints List of constraints
#'
#' @return Logical (invisible), TRUE if all checks passed
#' @export
validate_constraints <- function(data, constraints) {
  constraints <- flatten_constraints(constraints)
  issues <- list()

  for (c in constraints) {
    if (!c$col %in% names(data)) {
      issues[[length(issues) + 1]] <- sprintf(
        "\U274C Column '%s' does not exist in data", c$col
      )
      next
    }

    vec <- data[[c$col]]
    unique_vals <- unique(vec)
    n_unique <- length(unique_vals)

    if (c$type == "min_dist" && n_unique > 0) {
      max_possible <- nrow(data) / n_unique
      if (c$val > max_possible) {
        issues[[length(issues) + 1]] <- sprintf(
          "\U26A0\UFE0F '%s' MinDist=%d may be too large (theoretical limit ~%.1f)",
          c$col, c$val, max_possible
        )
      }
    }

    if (c$type == "max_rep" && c$val == 1 && n_unique < 3) {
      issues[[length(issues) + 1]] <- sprintf(
        "\U26A0\UFE0F '%s' has only %d unique values, MaxRep=1 is very difficult to satisfy",
        c$col, n_unique
      )
    }
  }

  if (length(issues) == 0) {
    cat("\U2705 Constraint configuration logical check passed\n")
    return(invisible(TRUE))
  } else {
    cat("\U26A0\UFE0F Constraint configuration may have potential issues (for reference):\n")
    for (issue in issues) {
      cat(paste0("  ", issue, "\n"))
    }
    return(invisible(FALSE))
  }
}


#' Flatten Nested Constraint Lists
#'
#' Internal function: Flatten nested lists produced by constraints_batch
#'
#' @param constraints Possibly nested list of constraints
#' @return Flat list of constraints
#' @keywords internal
flatten_constraints <- function(constraints) {
  flat <- list()
  for (item in constraints) {
    if (inherits(item, "constraint")) {
      flat[[length(flat) + 1]] <- item
    } else if (is.list(item)) {
      flat <- c(flat, flatten_constraints(item))
    }
  }
  flat
}
