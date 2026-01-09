# =============================================================================
# Solver - Simulated Annealing Optimization
# =============================================================================

#' Solve Single Block
#'
#' Internal function: Perform pseudo-random optimization on a single block.
#' Optimization: Only call lightweight calculate_score during iteration, 
#' calculate_score_detailed only at the end.
#'
#' @param block_data Data frame for a single block
#' @param constraints List of constraints
#' @param max_steps Maximum optimization steps
#' @param verbose Print detailed progress
#'
#' @return List with data, score, hard_violations, and violations
#' @keywords internal
solve_block_internal <- function(block_data, constraints, max_steps = 5000, verbose = FALSE) {
  # Initial random shuffle
  curr_data <- block_data[sample(nrow(block_data)), ]
  curr_score <- calculate_score(curr_data, constraints)

  # Perfect solution, return early
  if (curr_score == 0) {
    result <- calculate_score_detailed(curr_data, constraints)
    return(list(
      data = curr_data,
      score = 0,
      hard_violations = 0,
      violations = result$violations
    ))
  }

  best_data <- curr_data
  best_score <- curr_score

  # Simulated annealing optimization
  for (i in seq_len(max_steps)) {
    # Randomly swap two rows
    idx <- sample(nrow(curr_data), 2)
    tmp_data <- curr_data
    tmp_data[c(idx[1], idx[2]), ] <- tmp_data[c(idx[2], idx[1]), ]

    # Lightweight scoring (only returns score)
    new_score <- calculate_score(tmp_data, constraints)

    # Accept better or equal solutions
    if (new_score <= curr_score) {
      curr_data <- tmp_data
      curr_score <- new_score

      if (new_score < best_score) {
        best_data <- tmp_data
        best_score <- new_score
      }

      # Perfect solution, exit early
      if (new_score == 0) {
        result <- calculate_score_detailed(curr_data, constraints)
        return(list(
          data = curr_data,
          score = 0,
          hard_violations = 0,
          violations = result$violations
        ))
      }
    }
  }

  # Only call detailed scoring once at the end
  final_result <- calculate_score_detailed(best_data, constraints)

  list(
    data = best_data,
    score = final_result$score,
    hard_violations = final_result$hard_violations,
    violations = final_result$violations
  )
}


#' Generate Pseudo-Random Lists with Constraints
#'
#' Core low-level function: Generate pseudo-random lists that satisfy constraints.
#' Recommend using the higher-level generate_balanced_lists() function.
#'
#' @param data Data frame of items
#' @param constraints List of constraint objects
#' @param n_lists Number of lists to generate
#' @param max_steps Max optimization steps per block
#' @param block_col Optional block column name (if data already has blocks)
#' @param verbose Print detailed progress
#' @param show_feedback Show constraint feedback
#' @param save_to Directory to save CSVs (NULL = don't save)
#' @param strict_mode If TRUE, reject lists that violate ANY hard constraint (default TRUE)
#' @param track_occurrences If TRUE, add occurrence count column (default FALSE)
#' @param track_by Column to track occurrences (auto-detect if NULL)
#' @param occurrence_count_name Name for the occurrence count column
#'
#' @return List of results, each containing data, score, violations, etc.
#' @export
constrain_mix <- function(data, constraints, n_lists = 1, max_steps = 5000,
                          block_col = NULL, verbose = FALSE,
                          show_feedback = TRUE, save_to = NULL,
                          strict_mode = TRUE,
                          track_occurrences = FALSE,
                          track_by = NULL,
                          occurrence_count_name = "Occurrence") {

  results <- list()

  # --- Auto-detect track_by column (if tracking is needed) ---
  if (track_occurrences && is.null(track_by)) {
    common_names <- c("Item", "Picture", "Word", "Stimulus", "Sentence",
                      "Target", "Prime", "item", "picture", "word", "stimulus")
    found <- intersect(common_names, names(data))

    if (length(found) == 1) {
      track_by <- found
      if (show_feedback) cat(sprintf("\U2139\UFE0F Auto-detected tracking column: '%s'\n", found))
    } else if (length(found) > 1) {
      stop(sprintf(
        "Multiple possible tracking columns detected: %s\nPlease specify explicitly using track_by parameter.",
        paste(found, collapse = ", ")
      ))
    } else {
      stop("Unable to auto-detect tracking column. Please specify using track_by parameter.")
    }
  }

  # Flatten constraint list
  constraints <- flatten_constraints(constraints)

  # Validate constraint configuration
  if (show_feedback) {
    print_constraint_summary(constraints)
    validate_constraints(data, constraints)
    cat("\n")
  }

  # Create output directory
  if (!is.null(save_to) && !dir.exists(save_to)) {
    dir.create(save_to, recursive = TRUE)
    if (show_feedback) cat(sprintf("\U2713 Created output directory: %s\n\n", save_to))
  }

  # Generate lists
  success_count <- 0
  attempt_count <- 0

  while (success_count < n_lists && attempt_count < n_lists * 5) {
    attempt_count <- attempt_count + 1

    final_data <- NULL
    total_score <- 0
    total_hard_viol <- 0
    all_violations <- list()

    if (!is.null(block_col)) {
      # Process by Block
      if (!block_col %in% names(data)) {
        stop(sprintf("Block column '%s' not found!", block_col))
      }
      blocks <- split(data, data[[block_col]])
      solved_blocks <- list()

      for (b_name in names(blocks)) {
        res <- solve_block_internal(blocks[[b_name]], constraints, max_steps, verbose = verbose)
        temp_data <- res$data
        original_val <- blocks[[b_name]][[block_col]][1]
        temp_data[[block_col]] <- original_val
        solved_blocks[[b_name]] <- temp_data
        total_score <- total_score + res$score
        total_hard_viol <- total_hard_viol + res$hard_violations
        all_violations[[b_name]] <- res$violations
      }

      final_data <- do.call(rbind, solved_blocks)
      rownames(final_data) <- NULL

      # Merge violation reports from all blocks
      violation_report <- merge_violations(all_violations)

    } else {
      # Process without Blocks
      res <- solve_block_internal(data, constraints, max_steps, verbose = verbose)
      final_data <- res$data
      total_score <- res$score
      total_hard_viol <- res$hard_violations
      violation_report <- res$violations
    }

    # Determine whether to accept this list
    is_valid <- (total_hard_viol == 0) || !strict_mode

    if (is_valid) {
      success_count <- success_count + 1

      # --- Add Occurrence count (if needed) ---
      if (track_occurrences) {
        final_data <- add_occurrence_count(
          final_data,
          track_by = track_by,
          count_col_name = occurrence_count_name,
          reset_by = NULL
        )
      }

      # Package result
      results[[success_count]] <- list(
        list_id = success_count,
        data = final_data,
        score = total_score,
        is_perfect = (total_score == 0),
        hard_violations = total_hard_viol,
        violations = violation_report
      )

      # Save file
      if (!is.null(save_to)) {
        fname <- file.path(save_to, sprintf("List_Version_%d.csv", success_count))
        tryCatch({
          readr::write_excel_csv(final_data, fname)
        }, error = function(e) {
          warning(sprintf("Save failed (List %d): %s", success_count, e$message))
        })
      }

      # Display concise feedback
      if (show_feedback) {
        print_feedback_concise(results[[success_count]])
      }

    } else {
      if (verbose) cat(".")
    }
  }

  if (success_count < n_lists) {
    warning(sprintf(
      "\nWarning: Only generated %d/%d lists. Please check if constraints are too strict.\n",
      success_count, n_lists
    ))
  } else {
    if (show_feedback) cat("\nAll completed!\n")
  }

  return(results)
}


#' Print Feedback (Concise Version)
#'
#' Internal function: Print concise list generation feedback
#'
#' @param result The result object for a single list
#' @keywords internal
print_feedback_concise <- function(result) {
  if (result$is_perfect) {
    cat(sprintf("List %d: Perfect!\n", result$list_id))
  } else {
    v <- result$violations
    issues <- v[v > 0]

    if (length(issues) > 0) {
      issue_str <- paste(names(issues), issues, sep = ": ", collapse = ", ")
      cat(sprintf("List %d: Score %d | Violations: { %s }\n",
                  result$list_id, result$score, issue_str))
    } else {
      cat(sprintf("List %d: Score %d (No specific violations recorded)\n",
                  result$list_id, result$score))
    }
  }
}
