#' Create a heatmap of ABC connections
#'
#' This function creates a heatmap visualization of ABC connections using base R graphics.
#'
#' @param abc_results A data frame containing ABC results from apply_abc_model().
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param show_labels Logical. If TRUE, shows labels on the tiles.
#' @param title Plot title.
#'
#' @return NULL invisibly. The function creates a plot as a side effect.
#' @export
#' @importFrom graphics par image axis mtext text rect layout
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' \dontrun{
#' vis_abc_heatmap(abc_results, top_n = 20)
#' }
vis_abc_heatmap <- function(abc_results, top_n = 25, min_score = 0.1,
                            show_labels = TRUE, title = "ABC Connections Heatmap") {

  # Check if results are empty
  if (nrow(abc_results) == 0) {
    stop("ABC results are empty")
  }

  # Filter and sort results
  results <- abc_results[abc_results$abc_score >= min_score, ]
  results <- results[order(-results$abc_score), ]
  if (nrow(results) > top_n) {
    results <- results[1:top_n, ]
  }

  # If still no results after filtering, stop
  if (nrow(results) == 0) {
    stop("No results remain after filtering")
  }

  # Get unique A terms, B terms, and C terms
  a_terms <- unique(results$a_term)
  b_terms <- unique(results$b_term)
  c_terms <- unique(results$c_term)

  # Limit the number of terms to display to avoid margin errors
  max_terms <- 15  # Maximum number of terms to display
  if (length(b_terms) > max_terms) {
    b_terms <- b_terms[1:max_terms]
    results <- results[results$b_term %in% b_terms, ]
  }
  if (length(c_terms) > max_terms) {
    c_terms <- c_terms[1:max_terms]
    results <- results[results$c_term %in% c_terms, ]
  }

  # If there are multiple A terms, we need to create subplots
  n_a_terms <- length(a_terms)

  # Set up layout for subplots (one per A term)
  if (n_a_terms > 1) {
    # Determine layout dimensions
    n_cols <- min(3, n_a_terms)  # At most 3 columns
    n_rows <- ceiling(n_a_terms / n_cols)

    # Create layout
    layout_matrix <- matrix(1:(n_rows * n_cols), nrow = n_rows, ncol = n_cols, byrow = TRUE)
    layout(layout_matrix)
  }

  # Function to create a single heatmap for one A term
  create_single_heatmap <- function(a_term) {
    # Filter results for this A term
    a_results <- results[results$a_term == a_term, ]

    # Get B and C terms for this A term
    a_b_terms <- unique(a_results$b_term)
    a_c_terms <- unique(a_results$c_term)

    # Skip if there are no results after filtering
    if (length(a_b_terms) == 0 || length(a_c_terms) == 0) {
      message("No valid B-C pairs for A term: ", a_term)
      return(invisible(NULL))
    }

    # Create matrix for heatmap
    heat_matrix <- matrix(NA, nrow = length(a_b_terms), ncol = length(a_c_terms))
    rownames(heat_matrix) <- a_b_terms
    colnames(heat_matrix) <- a_c_terms

    # Fill the matrix with ABC scores
    for (i in 1:nrow(a_results)) {
      b_idx <- which(a_b_terms == a_results$b_term[i])
      c_idx <- which(a_c_terms == a_results$c_term[i])
      if (length(b_idx) > 0 && length(c_idx) > 0) {
        heat_matrix[b_idx, c_idx] <- a_results$abc_score[i]
      }
    }

    # Color palette - blue gradient
    color_palette <- colorRampPalette(c("lightblue", "darkblue"))(100)

    # Adjust margins based on term length
    max_b_length <- max(nchar(a_b_terms))
    max_c_length <- max(nchar(a_c_terms))

    # Calculate proper margins - scale with term length but with min/max bounds
    left_margin <- min(max(5, max_c_length * 0.5), 10)
    bottom_margin <- min(max(5, max_b_length * 0.5), 10)

    # Set up margins
    par(mar = c(bottom_margin, left_margin, 3, 2))

    # Create proper coordinates for x and y
    # x represents C terms (columns), y represents B terms (rows)
    x_coords <- 1:length(a_c_terms)  # C terms (will be on x-axis)
    y_coords <- 1:length(a_b_terms)  # B terms (will be on y-axis)

    # Transpose the matrix so dimensions match: C x B
    z_matrix <- t(heat_matrix)

    # Draw the heatmap
    image(x = x_coords,
          y = y_coords,
          z = z_matrix,
          col = color_palette,
          axes = FALSE,
          xlab = "", ylab = "",
          main = if (n_a_terms > 1) paste("A Term:", a_term) else title)

    # Add column and row labels with appropriate cex based on number of items
    c_cex <- min(0.8, 5 / length(a_c_terms))
    b_cex <- min(0.8, 5 / length(a_b_terms))

    axis(1, at = x_coords, labels = a_c_terms, las = 2, cex.axis = c_cex)
    axis(2, at = y_coords, labels = a_b_terms, las = 2, cex.axis = b_cex)

    # Add titles for axes
    mtext("C Terms", side = 1, line = bottom_margin - 2, cex = 0.8)
    mtext("B Terms", side = 2, line = left_margin - 2, cex = 0.8)

    # Add score labels if requested
    if (show_labels) {
      label_cex <- min(0.7, 3 / max(length(a_b_terms), length(a_c_terms)))
      # After transpose, z_matrix[i,j] corresponds to C term i and B term j
      # In image coordinates: x=i (C term), y=j (B term)
      # Original heat_matrix[j,i] has B term j and C term i
      for (i in 1:length(a_c_terms)) {
        for (j in 1:length(a_b_terms)) {
          # Get value from original matrix: heat_matrix[B, C]
          value <- heat_matrix[j, i]
          if (!is.na(value)) {
            text(i, j, round(value, 2), cex = label_cex)
          }
        }
      }
    }

    # Add a simple color legend instead of the complex one
    if (n_a_terms == 1) {  # Only add legend for single plot
      z_range <- range(heat_matrix, na.rm = TRUE)
      usr <- par("usr")
      # Simple legend bar at bottom right
      rect_x <- seq(usr[2] * 0.7, usr[2] * 0.95, length.out = 101)
      rect_width <- rect_x[2] - rect_x[1]
      rect_y <- rep(usr[3] * 1.1, 100)
      rect_height <- abs(usr[3] * 0.05)

      # Draw color rectangles
      for (i in 1:100) {
        rect(rect_x[i], rect_y[i], rect_x[i+1], rect_y[i] + rect_height,
             col = color_palette[i], border = NA)
      }

      # Add min/max labels
      text(rect_x[1], rect_y[1] + rect_height * 1.5,
           round(z_range[1], 2), cex = 0.7, adj = 0)
      text(rect_x[101], rect_y[1] + rect_height * 1.5,
           round(z_range[2], 2), cex = 0.7, adj = 1)
      text(mean(rect_x), rect_y[1] + rect_height * 2.5,
           "ABC Score", cex = 0.8)
    }
  }

  # Create heatmaps for each A term
  if (n_a_terms > 1) {
    old_par <- par(no.readonly = TRUE)  # Save current par settings
    on.exit(par(old_par))               # Restore on exit

    for (a_term in a_terms) {
      create_single_heatmap(a_term)
    }

    # Add an overall title
    mtext(title, side = 3, line = -1.5, outer = TRUE, cex = 1.2)
  } else {
    create_single_heatmap(a_terms)
  }

  # Reset layout
  if (n_a_terms > 1) {
    layout(1)
  }

  # Return invisible NULL
  invisible(NULL)
}
