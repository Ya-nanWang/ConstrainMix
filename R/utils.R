# =============================================================================
# Utils - Pseudo-Random Experimental Material Generation Utility Functions
# =============================================================================
# Dependencies: dplyr, rlang, readr

# =============================================================================
# Core Function: Generate Balanced Lists
# =============================================================================

#' Generate Lists with Block-Internal Constraints and Cross-Version Balance
#'
#' Integrated function: Implements block-internal constraints + cross-version 
#' block assignment balance. This is the most complete solution, suitable for 
#' experimental designs requiring breaks between blocks.
#'
#' @param data Data frame
#' @param constraints List of constraints
#' @param n_lists Number of lists to generate
#' @param item_col Item identifier column (e.g., "Picture", "Word"),
#'   used for Latin square grouping to determine which rows belong to the same item
#' @param balance_cols Factor columns for block balancing. Supports single or multiple columns:
#'   - Single column: balance_cols = "Condition"
#'   - Multiple columns: balance_cols = c("Factor1", "Factor2")
#'   When multiple columns are specified, a combination column is automatically created for balancing
#' @param n_blocks Number of blocks (default 2). If n_blocks = 1, skip Latin square.
#' @param block_col_name Name for Block column
#' @param max_steps Max optimization steps per block
#' @param show_feedback Show progress
#' @param strict_mode If TRUE, reject lists with hard constraint violations
#' @param track_occurrences Add occurrence count
#' @param track_by Column for tracking occurrences (defaults to item_col,
#'   but can specify another column separately)
#' @param occurrence_count_name Name for occurrence column
#' @param save_to Directory to save CSV files (NULL = don't save)
#'
#' @details
#' This function implements triple balancing (when n_blocks >= 2):
#' 1. **Block-internal constraints**: Each block independently satisfies all constraint rules
#' 2. **Block-internal condition balance**: Uses Latin square to ensure equal numbers of each condition within each block
#' 3. **Cross-version balance**: Different versions use different block assignment patterns, ensuring between-subject balance
#'
#' When n_blocks = 1, Latin square logic is skipped and global constraint optimization is used directly.
#'
#' @return Results list
#' @export
generate_balanced_lists <- function(data,
                                    constraints,
                                    n_lists = 1,
                                    item_col,
                                    balance_cols,
                                    n_blocks = 2,
                                    block_col_name = "Block",
                                    max_steps = 5000,
                                    show_feedback = TRUE,
                                    strict_mode = TRUE,
                                    track_occurrences = TRUE,
                                    track_by = NULL,
                                    occurrence_count_name = "Occurrence",
                                    save_to = NULL) {

  # ═══════════════════════════════════════════════════════════════════════
  # 0. Parameter checking and preprocessing
  # ═══════════════════════════════════════════════════════════════════════
  if (missing(item_col)) {
    stop("Please specify item_col parameter (Item identifier column, e.g. 'Picture' or 'Word')")
  }
  if (missing(balance_cols)) {
    stop("Please specify balance_cols parameter (Factor columns to balance, e.g. 'Condition' or c('Factor1', 'Factor2'))")
  }
  if (!item_col %in% names(data)) {
    stop(sprintf("Column '%s' does not exist in data", item_col))
  }

  for (col in balance_cols) {
    if (!col %in% names(data)) {
      stop(sprintf("Column '%s' does not exist in data", col))
    }
  }

  if (track_occurrences && is.null(track_by)) {
    track_by <- item_col
  }

  constraints <- flatten_constraints(constraints)

  # ═══════════════════════════════════════════════════════════════════════
  # 0b. Handle multi-factor balance_cols: create combination column
  # ═══════════════════════════════════════════════════════════════════════
  working_data <- data
  balance_col_internal <- NULL
  created_combo_col <- FALSE

  if (length(balance_cols) == 1) {
    balance_col_internal <- balance_cols
  } else {
    combo_col_name <- paste0(".balance_combo_", paste(balance_cols, collapse = "_"))
    working_data[[combo_col_name]] <- apply(
      working_data[, balance_cols, drop = FALSE],
      1,
      function(x) paste(x, collapse = "_")
    )
    balance_col_internal <- combo_col_name
    created_combo_col <- TRUE

    if (show_feedback) {
      cat(sprintf("\U2139\UFE0F Multi-factor balancing: Created combination column (%s)\n",
                  paste(balance_cols, collapse = " × ")))
      cat(sprintf("   Combination levels: %s\n\n",
                  paste(unique(working_data[[combo_col_name]]), collapse = ", ")))
    }
  }

  # ═══════════════════════════════════════════════════════════════════════
  # Special case: n_blocks = 1, skip Latin square, use global optimization
  # ═══════════════════════════════════════════════════════════════════════
  if (n_blocks == 1) {
    if (show_feedback) {
      cat("\n", rep("=", 70), "\n", sep = "")
      cat("Generate Lists (Global optimization mode, no block splitting)\n")
      cat(rep("=", 70), "\n", sep = "")
      cat(sprintf("  Items: %d (by '%s')\n", length(unique(working_data[[item_col]])), item_col))
      cat(sprintf("  Target: %d lists\n", n_lists))
      cat("  Mode: Global constraint optimization (n_blocks = 1, skip Latin square)\n\n")
    }

    results <- constrain_mix(
      data = working_data,
      constraints = constraints,
      n_lists = n_lists,
      max_steps = max_steps,
      block_col = NULL,
      verbose = FALSE,
      show_feedback = show_feedback,
      save_to = save_to,
      strict_mode = strict_mode,
      track_occurrences = track_occurrences,
      track_by = track_by,
      occurrence_count_name = occurrence_count_name
    )

    if (created_combo_col) {
      for (i in seq_along(results)) {
        results[[i]]$data[[combo_col_name]] <- NULL
      }
    }

    if (show_feedback) {
      cat("\n", rep("=", 70), "\n", sep = "")
      cat(sprintf("\U2713 Complete! Successfully generated %d lists", length(results)))
      if (!is.null(save_to)) {
        cat(sprintf(", saved to: %s/", save_to))
      }
      cat("\n\n")
    }

    return(results)
  }

  # ═══════════════════════════════════════════════════════════════════════
  # 1. Prepare Latin square pattern for block assignment (n_blocks >= 2)
  # ═══════════════════════════════════════════════════════════════════════
  unique_items <- unique(working_data[[item_col]])
  n_items <- length(unique_items)
  unique_levels <- unique(working_data[[balance_col_internal]])
  n_levels <- length(unique_levels)

  if (n_levels %% n_blocks != 0) {
    stop(sprintf(
      "Balance factor has %d levels, cannot be evenly divided into %d blocks.\nPlease ensure the number of levels is divisible by the number of blocks.\nLevels: %s",
      n_levels, n_blocks, paste(unique_levels, collapse = ", ")
    ))
  }

  levels_per_block <- n_levels / n_blocks
  combos <- combn(unique_levels, levels_per_block)
  n_patterns <- ncol(combos)

  if (show_feedback) {
    cat("\n", rep("=", 70), "\n", sep = "")
    cat("Generate Balanced Lists (Latin Square Balancing Mode)\n")
    cat(rep("=", 70), "\n", sep = "")
    cat(sprintf("  Items: %d (by '%s')\n", n_items, item_col))

    if (length(balance_cols) == 1) {
      cat(sprintf("  Balance factor: '%s' (%d levels)\n", balance_cols, n_levels))
    } else {
      cat(sprintf("  Balance factor: Combination of %s (%d levels)\n",
                  paste(balance_cols, collapse = " × "), n_levels))
    }

    cat(sprintf("  Blocks: %d (each block %d levels)\n", n_blocks, levels_per_block))
    cat(sprintf("  Block assignment patterns: %d types\n", n_patterns))
    cat(sprintf("  Target: %d lists\n", n_lists))

    if (track_occurrences) {
      cat(sprintf("  Tracking: '%s' -> '%s'\n", track_by, occurrence_count_name))
    }

    cat("\n  Available block assignment patterns:\n")
    for (p in 1:n_patterns) {
      b1 <- paste(combos[, p], collapse = ", ")
      remaining_levels <- setdiff(unique_levels, combos[, p])
      b2 <- paste(remaining_levels, collapse = ", ")
      cat(sprintf("    P%d: Block1=(%s) | Block2=(%s)\n", p, b1, b2))
    }
    cat(rep("-", 70), "\n", sep = "")

    print_constraint_summary(constraints)
  }

  if (!is.null(save_to) && !dir.exists(save_to)) {
    dir.create(save_to, recursive = TRUE)
    if (show_feedback) cat(sprintf("\U2713 Created output directory: %s\n\n", save_to))
  }

  # ═══════════════════════════════════════════════════════════════════════
  # 2. Generate list for each version
  # ═══════════════════════════════════════════════════════════════════════
  results <- list()
  success_count <- 0
  attempt_count <- 0

  while (success_count < n_lists && attempt_count < n_lists * 5) {
    attempt_count <- attempt_count + 1
    v <- success_count + 1

    # --- 2a. Assign blocks for current version ---
    offset <- (v - 1) %% n_patterns
    pattern_indices <- ((seq_along(unique_items) - 1 + offset) %% n_patterns) + 1

    data_v <- working_data
    data_v[[block_col_name]] <- NA

    for (i in seq_along(unique_items)) {
      itm <- unique_items[i]
      pat_idx <- pattern_indices[i]
      levels_for_block1 <- combos[, pat_idx]

      rows_idx <- which(data_v[[item_col]] == itm)
      data_v[[block_col_name]][rows_idx] <- ifelse(
        data_v[[balance_col_internal]][rows_idx] %in% levels_for_block1, 1, 2
      )
    }

    # --- 2b. Optimize each block ---
    total_score <- 0
    total_hard_viol <- 0
    all_violations <- list()
    solved_blocks <- list()

    blocks <- split(data_v, data_v[[block_col_name]])

    for (b_name in names(blocks)) {
      res <- solve_block_internal(blocks[[b_name]], constraints, max_steps, verbose = FALSE)
      temp_data <- res$data
      original_val <- blocks[[b_name]][[block_col_name]][1]
      temp_data[[block_col_name]] <- original_val

      solved_blocks[[b_name]] <- temp_data
      total_score <- total_score + res$score
      total_hard_viol <- total_hard_viol + res$hard_violations
      all_violations[[b_name]] <- res$violations
    }

    final_data <- do.call(rbind, solved_blocks)
    rownames(final_data) <- NULL
    violation_report <- merge_violations(all_violations)

    # --- 2c. Determine if accept this list ---
    is_valid <- (total_hard_viol == 0) || !strict_mode

    if (is_valid) {
      success_count <- success_count + 1

      # --- Add occurrence count ---
      if (track_occurrences) {
        final_data <- add_occurrence_count(
          final_data,
          track_by = track_by,
          count_col_name = occurrence_count_name,
          reset_by = NULL
        )
      }

      # --- Remove combination column if created ---
      if (created_combo_col) {
        final_data[[combo_col_name]] <- NULL
      }

      # --- Package result ---
      results[[success_count]] <- list(
        list_id = success_count,
        data = final_data,
        score = total_score,
        is_perfect = (total_score == 0),
        hard_violations = total_hard_viol,
        violations = violation_report
      )

      # --- Save file ---
      if (!is.null(save_to)) {
        fname <- file.path(save_to, sprintf("List_Version_%d.csv", success_count))
        tryCatch({
          readr::write_excel_csv(final_data, fname)
        }, error = function(e) {
          warning(sprintf("Save failed (List %d): %s", success_count, e$message))
        })
      }

      # --- Display concise feedback ---
      if (show_feedback) {
        print_feedback_concise(results[[success_count]])
      }
    }
  }

  # --- Final summary ---
  if (success_count < n_lists) {
    warning(sprintf(
      "\nWarning: Only generated %d/%d lists. Please check if constraints are too strict.\n",
      success_count, n_lists
    ))
  } else {
    if (show_feedback) {
      cat("\n", rep("=", 70), "\n", sep = "")
      cat(sprintf("\U2713 Complete! Successfully generated %d lists", success_count))
      if (!is.null(save_to)) {
        cat(sprintf(", saved to: %s/", save_to))
      }
      cat("\n\n")
    }
  }

  return(results)
}


# =============================================================================
# Block Balance Diagnostics
# =============================================================================

#' Check Block-Internal Condition Balance
#'
#' Verify that each block contains the expected number of each condition level.
#'
#' @param data Data frame with block assignments
#' @param balance_cols Column(s) to check for balance
#' @param block_col Block column name
#'
#' @return Invisible summary table
#' @export
check_block_internal_balance <- function(data,
                                         balance_cols,
                                         block_col = "Block") {

  if (!block_col %in% names(data)) {
    stop(sprintf("Block column '%s' not found in data", block_col))
  }

  for (col in balance_cols) {
    if (!col %in% names(data)) {
      stop(sprintf("Column '%s' not found in data", col))
    }
  }

  cat("\n", rep("=", 70), "\n", sep = "")
  cat("Block-Internal Condition Balance Report\n")
  cat(rep("=", 70), "\n\n")

  blocks <- split(data, data[[block_col]])

  for (b_name in names(blocks)) {
    b_data <- blocks[[b_name]]

    cat(sprintf("Block %s (N = %d):\n", b_name, nrow(b_data)))

    for (col in balance_cols) {
      freq <- table(b_data[[col]])
      cat(sprintf("  %s: %s\n", col,
                  paste(names(freq), freq, sep = "=", collapse = ", ")))
    }
    cat("\n")
  }

  cat(rep("=", 70), "\n\n")

  invisible(blocks)
}


#' Check Cross-Version Balance
#'
#' Verify that each item appears in each block assignment pattern approximately 
#' equally across versions (Latin square balance).
#'
#' @param results List of results from generate_balanced_lists()
#' @param item_col Item identifier column
#' @param balance_cols Factor column(s) for balancing
#' @param n_blocks Number of blocks
#' @param block_col_name Block column name
#'
#' @return Invisible matrix showing pattern distribution
#' @export
check_cross_version_balance <- function(results,
                                        item_col,
                                        balance_cols,
                                        n_blocks = 2,
                                        block_col_name = "Block") {

  n_versions <- length(results)
  sample_data <- results[[1]]$data

  if (length(balance_cols) == 1) {
    get_balance_vals <- function(df) df[[balance_cols]]
  } else {
    get_balance_vals <- function(df) {
      apply(df[, balance_cols, drop = FALSE], 1, function(x) paste(x, collapse = "_"))
    }
  }

  unique_items <- unique(sample_data[[item_col]])
  unique_levels <- unique(get_balance_vals(sample_data))
  n_levels <- length(unique_levels)
  levels_per_block <- n_levels / n_blocks

  combos <- combn(unique_levels, levels_per_block)
  n_patterns <- ncol(combos)

  cat("\n", rep("=", 70), "\n", sep = "")
  cat("Cross-Version Block Balance Detailed Report\n")
  cat(rep("=", 70), "\n\n", sep = "")
  cat(sprintf("Number of versions: %d\n", n_versions))
  cat(sprintf("Blocks: %d\n", n_blocks))
  cat(sprintf("Number of patterns: %d\n", n_patterns))
  cat(sprintf("Ideal frequency per pattern: %.2f\n\n", n_versions / n_patterns))

  item_pattern_matrix <- matrix(0, nrow = length(unique_items), ncol = n_patterns)
  rownames(item_pattern_matrix) <- unique_items
  colnames(item_pattern_matrix) <- paste0("P", 1:n_patterns)

  for (v in seq_along(results)) {
    data <- results[[v]]$data

    for (i in seq_along(unique_items)) {
      itm <- unique_items[i]
      item_data <- data[data[[item_col]] == itm, ]
      balance_vals <- get_balance_vals(item_data)
      b1_levels <- sort(unique(balance_vals[item_data[[block_col_name]] == 1]))

      for (p in 1:n_patterns) {
        if (identical(as.character(b1_levels), as.character(sort(combos[, p])))) {
          item_pattern_matrix[i, p] <- item_pattern_matrix[i, p] + 1
          break
        }
      }
    }
  }

  cat("Pattern distribution for each item (first 10):\n\n")
  n_show <- min(10, length(unique_items))
  print(item_pattern_matrix[1:n_show, , drop = FALSE])

  cat("\n\nStatistical summary:\n")
  cat(sprintf("  Minimum: %d\n", min(item_pattern_matrix)))
  cat(sprintf("  Maximum: %d\n", max(item_pattern_matrix)))
  cat(sprintf("  Standard deviation: %.2f\n", sd(as.vector(item_pattern_matrix))))

  ideal <- n_versions / n_patterns
  max_deviation <- max(abs(item_pattern_matrix - ideal))

  if (max_deviation <= 1) {
    cat("\n\U2713 Cross-version balance: Excellent (max deviation <= 1)\n")
  } else if (max_deviation <= 2) {
    cat("\n\U26A0\UFE0F Cross-version balance: Good (max deviation <= 2)\n")
  } else {
    cat("\n\U274C Cross-version balance: Needs improvement (max deviation > 2)\n")
  }

  cat(rep("=", 70), "\n\n", sep = "")

  invisible(item_pattern_matrix)
}


# =============================================================================
# Occurrence Tracking
# =============================================================================

#' Add Occurrence Count to Generated List
#'
#' Add item occurrence count to generated list
#'
#' @param data Data frame with generated list.
#' @param track_by Column name for items to track.
#' @param count_col_name Name for the new occurrence count column.
#' @param reset_by Optional. Column name to reset counting (e.g., "Block").
#'
#' @return Data frame with added occurrence count column
#' @export
add_occurrence_count <- function(data, track_by,
                                 count_col_name = "Occurrence",
                                 reset_by = NULL) {

  if (!track_by %in% names(data)) stop(sprintf("Column '%s' does not exist", track_by))
  if (!is.null(reset_by) && !reset_by %in% names(data)) {
    stop(sprintf("Reset column '%s' does not exist", reset_by))
  }

  data_proc <- data
  data_proc$.temp_row_id <- seq_len(nrow(data_proc))

  if (!is.null(reset_by)) {
    data_proc <- data_proc %>%
      dplyr::group_by(!!rlang::sym(reset_by), !!rlang::sym(track_by)) %>%
      dplyr::mutate(!!count_col_name := dplyr::row_number()) %>%
      dplyr::ungroup()
  } else {
    data_proc <- data_proc %>%
      dplyr::group_by(!!rlang::sym(track_by)) %>%
      dplyr::mutate(!!count_col_name := dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  data_final <- data_proc %>%
    dplyr::arrange(.temp_row_id) %>%
    dplyr::select(-.temp_row_id) %>%
    as.data.frame()

  return(data_final)
}


#' Batch Add Occurrence Count to Multiple Lists
#'
#' @param results List of results.
#' @param track_by Column name for items to track.
#' @param count_col_name Name for the occurrence count column.
#' @param reset_by Optional. Column to reset counting.
#'
#' @return Modified results list.
#' @export
batch_add_occurrence_count <- function(results, track_by,
                                       count_col_name = "Occurrence",
                                       reset_by = NULL) {

  cat(sprintf("Adding occurrence count to %d lists (Track: %s)...\n", length(results), track_by))

  for (i in seq_along(results)) {
    if (!is.null(results[[i]]$data)) {
      tryCatch({
        results[[i]]$data <- add_occurrence_count(
          results[[i]]$data,
          track_by = track_by,
          count_col_name = count_col_name,
          reset_by = reset_by
        )
      }, error = function(e) {
        warning(sprintf("Error processing list %d: %s", i, e$message))
      })
    }
  }

  cat("\U2713 Batch processing complete!\n")
  return(results)
}


#' Analyze Repetition Effects from Occurrence Data
#'
#' @param data Data frame with occurrence count.
#' @param occurrence_col Name of occurrence count column.
#' @export
analyze_repetition_distribution <- function(data, occurrence_col = "Occurrence") {
  if (!occurrence_col %in% names(data)) stop(sprintf("Column '%s' does not exist", occurrence_col))

  cat("\n", rep("=", 70), "\n", sep = "")
  cat("REPETITION ANALYSIS\n")
  cat(rep("=", 70), "\n\n")

  occurrence_table <- table(data[[occurrence_col]])
  for (i in seq_along(occurrence_table)) {
    cat(sprintf("  Repetition %s: %d trials\n", names(occurrence_table)[i], occurrence_table[i]))
  }
  cat("\n", rep("=", 70), "\n")
}


#' Export Block Summary
#'
#' @param data Data with Block assignments
#' @param output_file Output CSV filename
#' @param block_col Block column name
#' @param balance_cols Column(s) used for balancing
#' @param item_col Item column name
#' @return Invisible data frame with summary
#' @export
export_block_summary <- function(data, output_file = "block_summary.csv",
                                 block_col = "Block",
                                 balance_cols,
                                 item_col) {

  if (missing(balance_cols)) stop("Please specify balance_cols parameter")
  if (missing(item_col)) stop("Please specify item_col parameter")

  if (length(balance_cols) == 1) {
    get_balance_vals <- function(df) df[[balance_cols]]
  } else {
    get_balance_vals <- function(df) {
      apply(df[, balance_cols, drop = FALSE], 1, function(x) paste(x, collapse = "_"))
    }
  }

  summary_list <- list()

  for (itm in unique(data[[item_col]])) {
    item_data <- data[data[[item_col]] == itm, ]

    for (b in unique(data[[block_col]])) {
      block_item_data <- item_data[item_data[[block_col]] == b, ]

      if (nrow(block_item_data) > 0) {
        balance_vals <- get_balance_vals(block_item_data)
        levels_in_block <- paste(sort(unique(balance_vals)), collapse = ", ")

        summary_list[[length(summary_list) + 1]] <- data.frame(
          Item = itm,
          Block = b,
          Levels = levels_in_block,
          N_Trials = nrow(block_item_data)
        )
      }
    }
  }

  summary_df <- do.call(rbind, summary_list)
  readr::write_csv(summary_df, output_file)

  cat(sprintf("\U2713 Block summary saved to: %s\n", output_file))

  return(invisible(summary_df))
}
