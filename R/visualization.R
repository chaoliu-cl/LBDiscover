#' Visualize ABC model results as a network
#'
#' @name visualize_abc_network
#' @aliases visualize_abc_network
#'
#' @description
#' Create a network visualization of ABC connections using base R graphics.
#'
#' @param abc_results A data frame containing ABC results from apply_abc_model().
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param node_size_factor Factor for scaling node sizes.
#' @param edge_width_factor Factor for scaling edge widths.
#' @param color_by Column to use for node colors. Default is 'type'.
#' @param title Plot title.
#'
#' @return NULL invisibly. The function creates a plot as a side effect.
#' @export
#' @importFrom graphics par arrows points text legend
#' @importFrom grDevices rainbow
#'
#' @examples
#' \dontrun{
#' # Create a network visualization of ABC model results
#' vis_abc_network(abc_results, top_n = 20)
#' }
vis_abc_network <- function(abc_results, top_n = 25, min_score = 0.1,
                            node_size_factor = 3, edge_width_factor = 1,
                            color_by = "type", title = "ABC Model Network") {

  # Check if igraph is available for layout calculation
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The igraph package is required for network layout. Install it with: install.packages('igraph')")
  }

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

  # Create edge list
  edges_a_b <- data.frame(
    from = results$a_term,
    to = results$b_term,
    weight = results$a_b_score,
    stringsAsFactors = FALSE
  )
  edges_a_b <- unique(edges_a_b)

  edges_b_c <- data.frame(
    from = results$b_term,
    to = results$c_term,
    weight = results$b_c_score,
    stringsAsFactors = FALSE
  )
  edges_b_c <- unique(edges_b_c)

  # Combine edges
  edges <- rbind(edges_a_b, edges_b_c)
  edges <- unique(edges)

  # Get unique nodes
  all_terms <- unique(c(results$a_term, results$b_term, results$c_term))

  # Create node attributes
  nodes <- data.frame(
    name = all_terms,
    stringsAsFactors = FALSE
  )

  # Add node types if available
  if (all(c("a_type", "b_type", "c_type") %in% colnames(results))) {
    # Create a data frame mapping terms to types
    node_types <- data.frame(
      term = c(results$a_term, results$b_term, results$c_term),
      type = c(results$a_type, results$b_type, results$c_type),
      stringsAsFactors = FALSE
    )
    node_types <- unique(node_types)

    # Match types to nodes
    nodes$type <- sapply(nodes$name, function(n) {
      idx <- which(node_types$term == n)[1]
      if (length(idx) > 0) node_types$type[idx] else NA
    })
  } else {
    # If types are not available, create role-based types
    nodes$type <- sapply(nodes$name, function(n) {
      if (n %in% results$a_term) "A"
      else if (n %in% results$c_term) "C"
      else "B"
    })
  }

  # Create graph for layout calculation
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  # Calculate node degrees for sizing
  nodes$degree <- sapply(nodes$name, function(n) {
    sum(edges$from == n | edges$to == n)
  })

  # Calculate layout using igraph's Fruchterman-Reingold algorithm
  layout <- igraph::layout_with_fr(graph, dim = 2, niter = 1000)

  # Set node coordinates
  nodes$x <- layout[, 1]
  nodes$y <- layout[, 2]

  # Map node types to colors
  if (color_by %in% colnames(nodes)) {
    node_types <- unique(nodes[[color_by]])
  } else {
    color_by <- "type"
    message("Color attribute '", color_by, "' not found, using 'type' instead")
    node_types <- unique(nodes$type)
  }

  # Define color palette
  color_palette <- rainbow(length(node_types))
  names(color_palette) <- node_types

  # Map node colors
  nodes$color <- sapply(nodes[[color_by]], function(t) color_palette[as.character(t)])

  # Calculate node sizes based on degree and factor
  max_degree <- max(nodes$degree)
  min_size <- 5
  max_size <- 15 * node_size_factor
  nodes$size <- min_size + (nodes$degree / max_degree) * (max_size - min_size)

  # Set up plot area
  plot_margin <- 0.1  # Margin as a fraction of the plot range
  x_range <- range(nodes$x)
  y_range <- range(nodes$y)
  x_margin <- diff(x_range) * plot_margin
  y_margin <- diff(y_range) * plot_margin

  # Create plot
  par(mar = c(2, 2, 3, 6))  # Adjust margins for legend
  plot(NULL,
       xlim = c(min(x_range) - x_margin, max(x_range) + x_margin),
       ylim = c(min(y_range) - y_margin, max(y_range) + y_margin),
       xlab = "", ylab = "",
       main = title,
       type = "n", axes = FALSE)

  # Draw edges
  for (i in 1:nrow(edges)) {
    from_idx <- which(nodes$name == edges$from[i])
    to_idx <- which(nodes$name == edges$to[i])

    if (length(from_idx) > 0 && length(to_idx) > 0) {
      # Calculate arrow position
      x1 <- nodes$x[from_idx]
      y1 <- nodes$y[from_idx]
      x2 <- nodes$x[to_idx]
      y2 <- nodes$y[to_idx]

      # Normalize edge width
      edge_width <- 1 + (edges$weight[i] / max(edges$weight)) * 2 * edge_width_factor

      # Draw edge
      arrows(x1, y1, x2, y2,
             lwd = edge_width,
             length = 0.1,
             col = "gray50",
             angle = 15)
    }
  }

  # Draw nodes
  for (i in 1:nrow(nodes)) {
    points(nodes$x[i], nodes$y[i],
           pch = 19,  # Filled circle
           col = nodes$color[i],
           cex = nodes$size[i] / 5)  # Scale size for better display
  }

  # Add node labels
  for (i in 1:nrow(nodes)) {
    text(nodes$x[i], nodes$y[i],
         labels = nodes$name[i],
         pos = NULL,  # Center text on point
         offset = 1.5,
         cex = 0.8)
  }

  # Add legend
  legend("topright",
         legend = legend_items,
         col = legend_colors,
         pch = 19,
         title = legend_title,
         cex = 0.8,
         pt.cex = 1.5,
         inset = c(-0.25, 0),  # Increase space from plot edge
         xpd = TRUE,  # Allow legend outside plot area
         bg = "white",  # Add background to legend for better visibility
         box.col = "darkgray")  # Add border to legend

  # Return invisible NULL
  invisible(NULL)
}

#' Export ABC results to simple HTML network
#'
#' This function exports ABC results to a simple HTML file with a visualization.
#' If the visNetwork package is available, it will use it for a more interactive visualization.
#'
#' @param abc_results A data frame containing ABC results from apply_abc_model().
#' @param output_file File path for the output HTML file.
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param open Logical. If TRUE, opens the HTML file after creation.
#'
#' @return The file path of the created HTML file (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' export_network(abc_results, output_file = "abc_network.html")
#' }
export_network <- function(abc_results, output_file = "abc_network.html",
                           top_n = 50, min_score = 0.1, open = TRUE) {

  # Check if igraph is available for basic network structure
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The igraph package is required. Install it with: install.packages('igraph')")
  }

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

  # Create edge list
  edges_a_b <- data.frame(
    from = results$a_term,
    to = results$b_term,
    value = results$a_b_score,
    title = paste("Score:", round(results$a_b_score, 3)),
    stringsAsFactors = FALSE
  )
  edges_a_b <- unique(edges_a_b)

  edges_b_c <- data.frame(
    from = results$b_term,
    to = results$c_term,
    value = results$b_c_score,
    title = paste("Score:", round(results$b_c_score, 3)),
    stringsAsFactors = FALSE
  )
  edges_b_c <- unique(edges_b_c)

  # Combine edges
  edges <- rbind(edges_a_b, edges_b_c)
  edges <- unique(edges)

  # Get unique nodes
  all_terms <- unique(c(results$a_term, results$b_term, results$c_term))

  # Create node data frame
  nodes <- data.frame(
    id = all_terms,
    label = all_terms,
    title = all_terms,
    stringsAsFactors = FALSE
  )

  # Add node types/groups
  if (all(c("a_type", "b_type", "c_type") %in% colnames(results))) {
    # Create mapping of term to type
    term_types <- c()
    for (i in 1:nrow(results)) {
      term_types[results$a_term[i]] <- results$a_type[i]
      term_types[results$b_term[i]] <- results$b_type[i]
      term_types[results$c_term[i]] <- results$c_type[i]
    }

    # Add type to nodes
    nodes$group <- term_types[nodes$id]
  } else {
    # Use role-based grouping (A, B, C)
    node_groups <- rep("B", nrow(nodes))
    names(node_groups) <- nodes$id

    # A terms
    a_terms <- unique(results$a_term)
    node_groups[a_terms] <- "A"

    # C terms
    c_terms <- unique(results$c_term)
    node_groups[c_terms] <- "C"

    # Add groups to nodes
    nodes$group <- node_groups[nodes$id]
  }

  # If visNetwork is available, use it for better visualization
  if (requireNamespace("visNetwork", quietly = TRUE)) {
    # Create visNetwork visualization
    network <- visNetwork::visNetwork(nodes, edges, width = "100%") |>
      visNetwork::visEdges(arrows = NULL, smooth = TRUE) |>
      visNetwork::visGroups(groupname = "A", color = "red") |>
      visNetwork::visGroups(groupname = "B", color = "green") |>
      visNetwork::visGroups(groupname = "C", color = "blue") |>
      visNetwork::visLayout(randomSeed = 123) |>
      visNetwork::visOptions(
        highlightNearest = TRUE,
        nodesIdSelection = TRUE,
        selectedBy = "group"
      ) |>
      visNetwork::visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -50)
      )

    # Save to HTML file
    visNetwork::visSave(network, file = output_file)
  } else {
    # Create a simple HTML network visualization with D3.js

    # Create graph for layout calculation
    graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

    # Get layout
    set.seed(123)  # For reproducibility
    layout <- igraph::layout_with_fr(graph)

    # Scale layout to fit in the svg
    layout <- layout * 200 / max(abs(layout)) + 300

    # Add coordinates to nodes
    nodes$x <- layout[, 1]
    nodes$y <- layout[, 2]

    # Define colors for groups
    group_colors <- c(A = "red", B = "green", C = "blue")

    # Create HTML content
    html_content <- c(
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      "  <title>ABC Model Network</title>",
      "  <style>",
      "    body { font-family: Arial, sans-serif; }",
      "    .node circle { stroke: #fff; stroke-width: 1.5px; }",
      "    .link { stroke: #999; stroke-opacity: 0.6; }",
      "    .tooltip { position: absolute; background: white; border: 1px solid gray; padding: 5px; border-radius: 5px; }",
      "  </style>",
      "</head>",
      "<body>",
      "  <h1>ABC Model Network</h1>",
      "  <svg width=\"800\" height=\"600\">",
      "    <g>"
    )

    # Add edges
    html_content <- c(html_content, "    <!-- Edges -->")
    for (i in 1:nrow(edges)) {
      source_idx <- which(nodes$id == edges$from[i])
      target_idx <- which(nodes$id == edges$to[i])

      if (length(source_idx) > 0 && length(target_idx) > 0) {
        # Create SVG path
        x1 <- nodes$x[source_idx]
        y1 <- nodes$y[source_idx]
        x2 <- nodes$x[target_idx]
        y2 <- nodes$y[target_idx]

        edge_html <- sprintf(
          '    <line class="link" x1="%f" y1="%f" x2="%f" y2="%f" stroke-width="%f" title="%s"></line>',
          x1, y1, x2, y2, 1 + edges$value[i] * 2, edges$title[i]
        )
      }
    }

    # Add nodes
    html_content <- c(html_content, "    <!-- Nodes -->")
    for (i in 1:nrow(nodes)) {
      node_group <- nodes$group[i]
      node_color <- if (!is.null(group_colors[node_group])) group_colors[node_group] else "gray"

      node_html <- sprintf(
        '    <circle class="node" cx="%f" cy="%f" r="8" fill="%s" title="%s"></circle>',
        nodes$x[i], nodes$y[i], node_color, nodes$title[i]
      )
      html_content <- c(html_content, node_html)

      # Add node labels
      label_html <- sprintf(
        '    <text x="%f" y="%f" text-anchor="middle" dy=".3em" font-size="10px">%s</text>',
        nodes$x[i], nodes$y[i] + 15, nodes$label[i]
      )
      html_content <- c(html_content, label_html)
    }

    # Close SVG and HTML tags
    html_content <- c(html_content,
                      "    </g>",
                      "  </svg>",
                      "  <div>",
                      "    <h3>Legend</h3>",
                      "    <ul>"
    )

    # Add legend items
    for (group in unique(nodes$group)) {
      color <- if (!is.null(group_colors[group])) group_colors[group] else "gray"
      legend_html <- sprintf(
        '      <li><span style="color: %s;">\u25CF</span> %s</li>',
        color, group
      )
      html_content <- c(html_content, legend_html)
    }

    # Close HTML tags
    html_content <- c(html_content,
                      "    </ul>",
                      "  </div>",
                      "</body>",
                      "</html>"
    )

    # Write to file
    writeLines(html_content, output_file)
  }

  # Open in browser if requested
  if (open) {
    utils::browseURL(output_file)
  }

  # Return file path invisibly
  invisible(output_file)
}

#' Create an enhanced heatmap of ABC connections
#'
#' This function creates an improved heatmap visualization of ABC connections
#' that can display entity type information when available, without enforcing
#' type constraints.
#'
#' @param abc_results A data frame containing ABC results.
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param show_significance Logical. If TRUE, marks significant connections.
#' @param color_palette Character. Color palette to use for the heatmap.
#' @param title Plot title.
#' @param show_entity_types Logical. If TRUE, includes entity types in axis labels.
#'
#' @return NULL invisibly. The function creates a plot as a side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' vis_heatmap(abc_results, top_n = 20, show_significance = TRUE)
#' }
vis_heatmap <- function(abc_results, top_n = 25, min_score = 0.1,
                        show_significance = TRUE,
                        color_palette = "blues",
                        title = "ABC Connections Heatmap",
                        show_entity_types = TRUE) {

  # Check if results are empty
  if (nrow(abc_results) == 0) {
    stop("ABC results are empty")
  }

  # Check if entity types are available
  has_entity_types <- all(c("a_type", "b_type", "c_type") %in% colnames(abc_results))
  if (show_entity_types && !has_entity_types) {
    warning("Entity types not found in results. Setting show_entity_types = FALSE")
    show_entity_types <- FALSE
  }

  # Check if significance values are available
  has_significance <- "significant" %in% colnames(abc_results) && "p_value" %in% colnames(abc_results)
  if (show_significance && !has_significance) {
    warning("Significance information not found in results. Setting show_significance = FALSE")
    show_significance <- FALSE
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

  # Add explicit significant column with strict logical values if it doesn't exist
  if (show_significance && has_significance) {
    # Create a new column for strict significance checking
    results$strict_significant <- FALSE
    # Only mark as TRUE those with p-value < 0.05
    results$strict_significant[results$p_value < 0.05] <- TRUE

    # Check if any connections are actually significant
    any_significant <- any(results$p_value < 0.05, na.rm = TRUE)
    if (!any_significant) {
      message("No connections are statistically significant (p < 0.05)")
    } else {
      sig_count <- sum(results$p_value < 0.05, na.rm = TRUE)
      message(sig_count, " connection(s) are statistically significant (p < 0.05)")
    }
  }

  # Get unique A terms, B terms, and C terms
  a_terms <- unique(results$a_term)
  b_terms <- unique(results$b_term)
  c_terms <- unique(results$c_term)

  # Limit the number of terms to display to avoid crowding
  max_terms <- 15
  if (length(b_terms) > max_terms) {
    b_terms <- b_terms[1:max_terms]
    message("Limiting visualization to top ", max_terms, " B terms")
    results <- results[results$b_term %in% b_terms, ]
  }
  if (length(c_terms) > max_terms) {
    c_terms <- c_terms[1:max_terms]
    message("Limiting visualization to top ", max_terms, " C terms")
    results <- results[results$c_term %in% c_terms, ]
  }

  # Color palette function
  get_color_palette <- function(palette_name, n) {
    if (palette_name == "blues") {
      return(colorRampPalette(c("lightblue", "steelblue", "darkblue"))(n))
    } else if (palette_name == "reds") {
      return(colorRampPalette(c("mistyrose", "salmon", "firebrick"))(n))
    } else if (palette_name == "greens") {
      return(colorRampPalette(c("palegreen", "limegreen", "darkgreen"))(n))
    } else if (palette_name == "purples") {
      return(colorRampPalette(c("lavender", "mediumpurple", "darkviolet"))(n))
    } else if (palette_name == "rainbow") {
      return(rainbow(n))
    } else {
      return(colorRampPalette(c("lightblue", "steelblue", "darkblue"))(n))
    }
  }

  # Function to create a single heatmap for one A term
  create_vis_heatmap <- function(a_term) {
    # Filter results for this A term
    a_results <- results[results$a_term == a_term, ]

    # Get B and C terms for this A term
    a_b_terms <- unique(a_results$b_term)
    a_c_terms <- unique(a_results$c_term)

    # Skip if there are no valid B-C pairs
    if (length(a_b_terms) == 0 || length(a_c_terms) == 0) {
      message("No valid B-C pairs for A term: ", a_term)
      return(invisible(NULL))
    }

    # Create matrix for heatmap
    heat_matrix <- matrix(0, nrow = length(a_b_terms), ncol = length(a_c_terms))
    rownames(heat_matrix) <- a_b_terms
    colnames(heat_matrix) <- a_c_terms

    # Create significance matrix if needed
    sig_matrix <- NULL
    if (show_significance && has_significance) {
      sig_matrix <- matrix(FALSE, nrow = length(a_b_terms), ncol = length(a_c_terms))
      rownames(sig_matrix) <- a_b_terms
      colnames(sig_matrix) <- a_c_terms
    }

    # Get entity types for B and C terms if available
    if (has_entity_types && show_entity_types) {
      b_types <- sapply(a_b_terms, function(b) {
        idx <- which(a_results$b_term == b)[1]
        if (length(idx) > 0) a_results$b_type[idx] else "unknown"
      })

      c_types <- sapply(a_c_terms, function(c) {
        idx <- which(a_results$c_term == c)[1]
        if (length(idx) > 0) a_results$c_type[idx] else "unknown"
      })

      # Use just the terms themselves without entity types
      b_labels <- a_b_terms
      c_labels <- a_c_terms
    } else {
      b_labels <- a_b_terms
      c_labels <- a_c_terms
    }

    # Fill the matrices
    for (i in 1:nrow(a_results)) {
      row <- a_results[i, ]
      b_term <- row$b_term
      c_term <- row$c_term

      b_idx <- match(b_term, a_b_terms)
      c_idx <- match(c_term, a_c_terms)

      if (!is.na(b_idx) && !is.na(c_idx)) {
        heat_matrix[b_idx, c_idx] <- row$abc_score

        if (show_significance && has_significance) {
          # Set significance based on our strict significance column
          sig_matrix[b_idx, c_idx] <- row$strict_significant
        }
      }
    }

    # Create color palette for heatmap
    color_palette_values <- get_color_palette(color_palette, 100)

    # Calculate base margins
    base_left_margin <- 7
    base_bottom_margin <- 7
    base_top_margin <- 4
    base_right_margin <- 2

    # Additional margin based on label length
    max_b_length <- max(nchar(b_labels))
    max_c_length <- max(nchar(c_labels))

    # Calculate margins with conservative limits
    left_margin <- min(max(base_left_margin, max_c_length * 0.3), 10)
    bottom_margin <- min(max(base_bottom_margin, max_b_length * 0.3), 10)

    # Set up margins
    par(mar = c(bottom_margin, left_margin, base_top_margin, base_right_margin))

    # Create proper coordinates for x and y
    x_coords <- seq_len(length(a_b_terms))
    y_coords <- seq_len(length(a_c_terms))

    # Create plot title
    if (length(a_terms) > 1) {
      plot_title <- paste0("A Term: ", a_term)
    } else {
      # If user provided a specific title, use that
      if (title != "ABC Connections Heatmap") {
        plot_title <- title
      } else {
        # Otherwise use default format "A Term: [a_term]"
        plot_title <- paste0("A Term: ", a_term)
      }
    }

    # Check if any connections are significant
    has_any_significant <- FALSE
    if (show_significance && has_significance && !is.null(sig_matrix)) {
      has_any_significant <- any(sig_matrix, na.rm = TRUE)
    }

    # No longer adding the significance information to the title
    # Keeping the plot_title as is

    # Try to create the plot with error handling
    tryCatch({
      # Create empty plot with correct dimensions
      plot(NA, xlim = range(x_coords) + c(-0.5, 0.5),
           ylim = range(y_coords) + c(-0.5, 0.5),
           xlab = "", ylab = "", axes = FALSE,
           main = plot_title)

      # Create a subtitle with the significance information, but only if there are significant connections
      if (show_significance && has_significance && has_any_significant) {
        mtext("* p < 0.05", line = 0.3, cex = 0.7, col = "red")
      }

      # Draw colored rectangles for each cell
      for (i in seq_along(x_coords)) {
        for (j in seq_along(y_coords)) {
          value <- heat_matrix[i, j]

          # Skip empty cells
          if (value == 0) next

          # Map value to color index (1-100)
          min_val <- min(heat_matrix[heat_matrix > 0])
          max_val <- max(heat_matrix)
          range_val <- max_val - min_val

          if (range_val > 0) {
            color_idx <- max(1, min(100, ceiling((value - min_val) / range_val * 99) + 1))
          } else {
            color_idx <- 50  # Default middle color if all values are the same
          }

          # Draw rectangle
          rect(x_coords[i] - 0.5, y_coords[j] - 0.5,
               x_coords[i] + 0.5, y_coords[j] + 0.5,
               col = color_palette_values[color_idx], border = "white")

          # Add score text
          text_color <- ifelse(color_idx > 70, "white", "black")
          text(x_coords[i], y_coords[j],
               format(round(value, 2), nsmall = 2),
               col = text_color,
               cex = 0.7)

          # Add significance marker if available and significant
          if (show_significance && has_significance && !is.null(sig_matrix)) {
            if (sig_matrix[i, j]) {
              # Add a small star in the corner for significant connections
              text(x_coords[i] + 0.3, y_coords[j] + 0.3, "*", col = "red", cex = 1.2, font = 2)
            }
          }
        }
      }

      # Calculate font size for axis labels
      y_cex <- min(0.9, 6 / length(a_c_terms))
      x_cex <- y_cex

      # Add axes with appropriate spacing for labels
      axis(1, at = x_coords, labels = b_labels,
           las = 2, cex.axis = x_cex,
           mgp = c(0, 0.7, 0))

      axis(2, at = y_coords, labels = c_labels,
           las = 2, cex.axis = y_cex,
           mgp = c(0, 0.7, 0))

      # Add axis titles
      if (has_entity_types && show_entity_types) {
        title_b <- paste0("B Terms")
        title_c <- paste0("C Terms")

        mtext(title_b, side = 1, line = 4.5, cex = 0.9)
        mtext(title_c, side = 2, line = 4.5, cex = 0.9)
      } else {
        mtext("B Terms", side = 1, line = 4.5, cex = 0.9)
        mtext("C Terms", side = 2, line = 4.5, cex = 0.9)
      }

      # Add color legend for single plot
      if (length(a_terms) == 1) {
        z_range <- range(heat_matrix[heat_matrix > 0], na.rm = TRUE)

        # Try to create the legend with error handling
        tryCatch({
          # Create a new plotting region for the legend
          par(fig = c(0.78, 0.98, 0.01, 0.15), new = TRUE, mar = c(1, 1, 1, 1))

          # Draw an empty plot for the legend area
          plot(NA, xlim = c(0, 1), ylim = c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")

          # Position the legend
          rect_y <- rep(0.2, 100)

          # Placement of the color bar
          legend_rects <- 10
          legend_x <- seq(0, 1, length.out = legend_rects + 1)
          legend_colors <- color_palette_values[seq(1, 100, length.out = legend_rects)]
          rect_height <- 0.15

          # Draw the "ABC Score" label
          text(0.5, rect_y[1] + rect_height + 0.25, "ABC Score", cex = 0.7)

          # Draw color rectangles for the legend bar
          for (i in 1:legend_rects) {
            rect(legend_x[i], rect_y[i], legend_x[i+1], rect_y[i] + rect_height,
                 col = legend_colors[i], border = NA)
          }

          # Add min/max labels
          text(0, rect_y[1] - 0.15, format(round(z_range[1], 2), nsmall = 2), pos = 4, cex = 0.6)
          text(1, rect_y[1] - 0.15, format(round(z_range[2], 2), nsmall = 2), pos = 2, cex = 0.6)

          # Add significance legend, but only if there are significant connections
          if (show_significance && has_significance && has_any_significant) {
            # Position at bottom right of main plot area
            par(fig = c(0, 1, 0, 1), new = TRUE)
            text(par("usr")[2] * 0.95, par("usr")[3] * 1.1,
                 "* p < 0.05", cex = 0.6, adj = 1)
            par(fig = c(0.78, 0.98, 0.01, 0.15), new = TRUE)
          }
        }, error = function(e) {
          message("Note: Couldn't add color legend due to space constraints: ", e$message)
        })

        # Reset to main plot area
        par(fig = c(0, 1, 0, 1), new = FALSE)
      }
    }, error = function(e) {
      if (grepl("figure margins too large", e$message)) {
        # Try with minimum margins if the initial margins are too large
        par(mar = c(5, 5, 3, 2))
        plot(NA, xlim = range(x_coords) + c(-0.5, 0.5),
             ylim = range(y_coords) + c(-0.5, 0.5),
             xlab = "", ylab = "", axes = FALSE,
             main = plot_title)

        # Display a message indicating reduced functionality
        text(mean(range(x_coords)), mean(range(y_coords)),
             "Simplified heatmap (margins too large for full display)",
             cex = 0.8)

        # Add a basic explanation
        text(mean(range(x_coords)), mean(range(y_coords)) - 1,
             paste0("See full output in console for details on ", length(a_b_terms),
                    " B terms and ", length(a_c_terms), " C terms"),
             cex = 0.7)
      } else {
        # Re-throw other errors
        stop(e)
      }
    })
  }

  # Set up layout for subplots (one per A term)
  if (length(a_terms) > 1) {
    # Determine layout dimensions
    n_cols <- min(2, length(a_terms))
    n_rows <- ceiling(length(a_terms) / n_cols)

    # Create layout
    layout_matrix <- matrix(1:(n_rows * n_cols), nrow = n_rows, ncol = n_cols, byrow = TRUE)
    layout(layout_matrix)

    # Save original par settings
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))

    # Create heatmaps for each A term
    for (a_term in a_terms) {
      create_vis_heatmap(a_term)
    }

    # Add an overall title
    mtext(title, side = 3, line = -1, outer = TRUE, cex = 1.0)

    # Reset layout
    layout(1)
  } else {
    create_vis_heatmap(a_terms)
  }

  # Return invisible NULL
  invisible(NULL)
}

#' Create an enhanced network visualization of ABC connections
#'
#' This function creates an improved network visualization of ABC connections
#' that displays entity types when available, without enforcing type constraints.
#'
#' @param abc_results A data frame containing ABC results.
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param show_significance Logical. If TRUE, highlights significant connections.
#' @param node_size_factor Factor for scaling node sizes.
#' @param color_by Column to use for node colors. Default is 'type'.
#' @param title Plot title.
#' @param show_entity_types Logical. If TRUE, includes entity types in node labels.
#' @param label_size Relative size for labels. Default is 1.
#'
#' @return NULL invisibly. The function creates a plot as a side effect.
#' @importFrom graphics segments
#' @export
#'
#' @examples
#' \dontrun{
#' vis_network(abc_results, top_n = 20, show_significance = TRUE,
#'                      show_entity_types = TRUE)
#' }
vis_network <- function(abc_results, top_n = 25, min_score = 0.1,
                        show_significance = TRUE,
                        node_size_factor = 5,
                        color_by = "type",
                        title = "ABC Model Network",
                        show_entity_types = TRUE,
                        label_size = 1) {

  # Check if igraph is available for layout calculation
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The igraph package is required for network layout. Install it with: install.packages('igraph')")
  }

  # Check if results are empty
  if (nrow(abc_results) == 0) {
    stop("ABC results are empty")
  }

  # Check if entity types are available
  has_entity_types <- all(c("a_type", "b_type", "c_type") %in% colnames(abc_results))
  if (show_entity_types && !has_entity_types) {
    warning("Entity types not found in results. Setting show_entity_types = FALSE")
    show_entity_types <- FALSE
  }

  # Check if significance is available
  has_significance <- "significant" %in% colnames(abc_results)
  if (show_significance && !has_significance) {
    warning("Significance information not found in results. Setting show_significance = FALSE")
    show_significance <- FALSE
  }

  # Filter and sort results
  # Use abc_score if available, otherwise use a default score column or create one
  if ("abc_score" %in% colnames(abc_results)) {
    score_col <- "abc_score"
  } else if ("score" %in% colnames(abc_results)) {
    score_col <- "score"
  } else {
    # Create a default score if none exists
    abc_results$default_score <- 1
    score_col <- "default_score"
    min_score <- 0  # Reset min_score since we're using default scores
  }

  results <- abc_results[abc_results[[score_col]] >= min_score, ]
  results <- results[order(-results[[score_col]]), ]
  if (nrow(results) > top_n) {
    results <- results[1:top_n, ]
  }

  # If still no results after filtering, stop
  if (nrow(results) == 0) {
    stop("No results remain after filtering")
  }

  # Create edge list with appropriate weight columns
  # Check if a_b_score and b_c_score exist, otherwise use the main score or default weights
  if (all(c("a_b_score", "b_c_score") %in% colnames(results))) {
    edges_a_b <- data.frame(
      from = results$a_term,
      to = results$b_term,
      weight = results$a_b_score,
      stringsAsFactors = FALSE
    )
    edges_b_c <- data.frame(
      from = results$b_term,
      to = results$c_term,
      weight = results$b_c_score,
      stringsAsFactors = FALSE
    )
  } else {
    # Use the main score for both connections or default weight
    default_weight <- if (score_col != "default_score") results[[score_col]] else rep(0.5, nrow(results))

    edges_a_b <- data.frame(
      from = results$a_term,
      to = results$b_term,
      weight = default_weight,
      stringsAsFactors = FALSE
    )
    edges_b_c <- data.frame(
      from = results$b_term,
      to = results$c_term,
      weight = default_weight,
      stringsAsFactors = FALSE
    )
  }

  edges_a_b <- unique(edges_a_b)
  edges_b_c <- unique(edges_b_c)

  # Combine edges
  edges <- rbind(edges_a_b, edges_b_c)
  edges <- unique(edges)

  # Get unique nodes
  all_terms <- unique(c(results$a_term, results$b_term, results$c_term))

  # Create node attributes
  nodes <- data.frame(
    name = all_terms,
    stringsAsFactors = FALSE
  )

  # Add node types if available
  if (has_entity_types) {
    # Create a mapping of term to type
    term_types <- c()
    for (i in 1:nrow(results)) {
      term_types[results$a_term[i]] <- results$a_type[i]
      term_types[results$b_term[i]] <- results$b_type[i]
      term_types[results$c_term[i]] <- results$c_type[i]
    }

    # Add type to nodes
    nodes$type <- term_types[nodes$name]
  } else {
    # Use role-based types (A, B, C)
    nodes$type <- sapply(nodes$name, function(n) {
      if (n %in% results$a_term) "A"
      else if (n %in% results$c_term) "C"
      else "B"
    })
  }

  # Add node role (A, B, C) regardless of type
  nodes$role <- sapply(nodes$name, function(n) {
    if (n %in% results$a_term) "A"
    else if (n %in% results$c_term) "C"
    else "B"
  })

  # Assign node labels with entity types if requested
  if (show_entity_types && has_entity_types) {
    nodes$label <- paste0(nodes$name, "\n(", nodes$type, ")")
  } else {
    nodes$label <- nodes$name
  }

  # Create graph for layout calculation
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  # Calculate node degree and betweenness centrality for sizing
  nodes$degree <- igraph::degree(graph, mode = "all")

  tryCatch({
    nodes$betweenness <- igraph::betweenness(graph, directed = TRUE)
  }, error = function(e) {
    message("Could not calculate betweenness centrality: ", e$message)
    nodes$betweenness <- 0
  })

  # Calculate layout using igraph's Fruchterman-Reingold algorithm
  set.seed(42)  # For consistent layout
  layout <- igraph::layout_with_fr(graph, niter = 1000)

  # Set node coordinates
  nodes$x <- layout[, 1]
  nodes$y <- layout[, 2]

  # Map node types to colors (using static_data)
  if (color_by %in% colnames(nodes)) {
    node_categories <- unique(nodes[[color_by]])
    node_categories <- node_categories[!is.na(node_categories)]
  } else {
    color_by <- "role"
    message("Color attribute '", color_by, "' not found, using 'role' instead")
    node_categories <- unique(nodes$role)
  }

  # Assign colors based on node categories (using static_data)
  node_colors <- c()
  for (category in node_categories) {
    if (category %in% names(static_data$entity_type_colors)) {
      node_colors[category] <- static_data$entity_type_colors[[category]]
    } else {
      # Generate a new color for unknown categories
      node_colors[category] <- grDevices::rainbow(1)
    }
  }

  # Map node colors
  nodes$color <- sapply(nodes[[color_by]], function(t) {
    if (!is.na(t) && t %in% names(node_colors)) {
      return(node_colors[t])
    } else {
      return("#AAAAAA")  # Gray for unknown types
    }
  })

  # Calculate node sizes based on importance
  # Combine degree and betweenness, with extra weight for A and C nodes
  nodes$importance <- nodes$degree + nodes$betweenness/max(nodes$betweenness+0.1) * 10

  # Scale by role (A and C terms are more important)
  role_multiplier <- c("A" = 1.5, "B" = 1.0, "C" = 1.3)
  nodes$importance <- nodes$importance * sapply(nodes$role, function(r) role_multiplier[r])

  # Calculate final node size
  max_importance <- max(nodes$importance)
  min_size <- 5
  max_size <- 20 * node_size_factor
  nodes$size <- min_size + (nodes$importance / max(max_importance, 1)) * (max_size - min_size)

  # Add significance information to edges if available
  if (show_significance && has_significance) {
    edge_significance <- data.frame(
      from = character(),
      to = character(),
      significant = logical(),
      stringsAsFactors = FALSE
    )

    # Map significance to A-B and B-C connections
    for (i in 1:nrow(results)) {
      a <- results$a_term[i]
      b <- results$b_term[i]
      c <- results$c_term[i]
      significant <- results$significant[i]

      edge_significance <- rbind(edge_significance,
                                 data.frame(
                                   from = a,
                                   to = b,
                                   significant = significant,
                                   stringsAsFactors = FALSE
                                 ),
                                 data.frame(
                                   from = b,
                                   to = c,
                                   significant = significant,
                                   stringsAsFactors = FALSE
                                 ))
    }

    edge_significance <- unique(edge_significance)

    # Add to edges
    edges$significant <- sapply(1:nrow(edges), function(i) {
      idx <- which(edge_significance$from == edges$from[i] &
                     edge_significance$to == edges$to[i])
      if (length(idx) > 0) edge_significance$significant[idx[1]] else FALSE
    })
  } else {
    edges$significant <- FALSE
  }

  # Set up plot area
  plot_margin <- 0.25
  x_range <- range(nodes$x)
  y_range <- range(nodes$y)
  x_margin <- diff(x_range) * plot_margin
  y_margin <- diff(y_range) * plot_margin

  # Create plot
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mar = c(3, 3, 4, 10))  # Adjust margins for legend
  plot(NULL,
       xlim = c(min(x_range) - x_margin, max(x_range) + x_margin),
       ylim = c(min(y_range) - y_margin, max(y_range) + y_margin),
       xlab = "", ylab = "",
       main = title,
       type = "n", axes = FALSE)

  # Draw edges with layering
  # First draw non-significant edges
  for (i in 1:nrow(edges)) {
    if (!edges$significant[i]) {
      from_idx <- which(nodes$name == edges$from[i])
      to_idx <- which(nodes$name == edges$to[i])

      if (length(from_idx) > 0 && length(to_idx) > 0) {
        x1 <- nodes$x[from_idx]
        y1 <- nodes$y[from_idx]
        x2 <- nodes$x[to_idx]
        y2 <- nodes$y[to_idx]

        # Calculate direction vector
        dx <- x2 - x1
        dy <- y2 - y1
        dist <- sqrt(dx^2 + dy^2)

        # Adjust end points to stop at node boundaries
        from_radius <- nodes$size[from_idx] / 5
        to_radius <- nodes$size[to_idx] / 5

        if (dist > (from_radius + to_radius)) {
          dx_norm <- dx / dist
          dy_norm <- dy / dist

          x1_adj <- x1 + dx_norm * from_radius
          y1_adj <- y1 + dy_norm * from_radius
          x2_adj <- x2 - dx_norm * to_radius
          y2_adj <- y2 - dy_norm * to_radius

          # Normalize edge width based on weight
          edge_width <- 1 + (edges$weight[i] / max(edges$weight)) * 3

          # Draw line
          segments(x1_adj, y1_adj, x2_adj, y2_adj,
                   lwd = edge_width,
                   col = "gray70")
        }
      }
    }
  }

  # Then draw significant edges on top
  for (i in 1:nrow(edges)) {
    if (edges$significant[i]) {
      from_idx <- which(nodes$name == edges$from[i])
      to_idx <- which(nodes$name == edges$to[i])

      if (length(from_idx) > 0 && length(to_idx) > 0) {
        x1 <- nodes$x[from_idx]
        y1 <- nodes$y[from_idx]
        x2 <- nodes$x[to_idx]
        y2 <- nodes$y[to_idx]

        dx <- x2 - x1
        dy <- y2 - y1
        dist <- sqrt(dx^2 + dy^2)

        from_radius <- nodes$size[from_idx] / 5
        to_radius <- nodes$size[to_idx] / 5

        if (dist > (from_radius + to_radius)) {
          dx_norm <- dx / dist
          dy_norm <- dy / dist

          x1_adj <- x1 + dx_norm * from_radius
          y1_adj <- y1 + dy_norm * from_radius
          x2_adj <- x2 - dx_norm * to_radius
          y2_adj <- y2 - dy_norm * to_radius

          edge_width <- 1 + (edges$weight[i] / max(edges$weight)) * 3

          # Draw significant edge with bright red color
          segments(x1_adj, y1_adj, x2_adj, y2_adj,
                   lwd = edge_width,
                   col = "#E41A1C") # Bright red for significance
        }
      }
    }
  }

  # Draw nodes in layers: first B, then C, then A
  node_layers <- c("B", "C", "A")
  for (layer in node_layers) {
    layer_nodes <- which(nodes$role == layer)

    for (i in layer_nodes) {
      # Draw filled circle
      points(nodes$x[i], nodes$y[i],
             pch = 19,  # Filled circle
             col = nodes$color[i],
             cex = nodes$size[i] / 5)

      # Add border
      points(nodes$x[i], nodes$y[i],
             pch = 1,  # Circle border
             col = "black",
             cex = nodes$size[i] / 5)
    }
  }

  # Add node labels
  label_cex <- 0.8 * label_size
  for (i in 1:nrow(nodes)) {
    pos <- NULL
    label_offset <- 1.0

    if (nodes$x[i] < mean(x_range)) {
      pos <- 2  # Left
    } else if (nodes$x[i] > mean(x_range)) {
      pos <- 4  # Right
    } else if (nodes$y[i] < mean(y_range)) {
      pos <- 1  # Below
    } else {
      pos <- 3  # Above
    }

    # Enhance visibility of important nodes (A and C)
    if (nodes$role[i] %in% c("A", "C")) {
      text(nodes$x[i], nodes$y[i],
           labels = nodes$label[i],
           pos = pos,
           offset = label_offset,
           cex = label_cex,
           col = "black")
    } else {
      text(nodes$x[i], nodes$y[i],
           labels = nodes$label[i],
           pos = pos,
           offset = label_offset,
           cex = label_cex * 0.9)
    }
  }

  # Add legend for node colors
  legend_items <- names(node_colors)
  legend_colors <- unname(node_colors)

  if (length(legend_items) > 0) {
    legend_title <- switch(color_by,
                           "type" = "Entity Type",
                           "role" = "Node Role",
                           color_by)

    legend("topright",
           legend = legend_items,
           col = legend_colors,
           pch = 19,
           title = legend_title,
           cex = 0.8,
           pt.cex = 1.5,
           inset = c(-0.2, 0),
           xpd = TRUE)
  }

  # Add significance legend if showing significance
  if (show_significance && has_significance && any(edges$significant)) {
    legend("bottomright",
           legend = "Significant (p < 0.05)",
           col = "#E41A1C",
           lwd = 2,
           cex = 0.8,
           inset = c(0.05, 0.05))
  }

  invisible(NULL)
}

#' Helper function to draw text with a shadow/background
#' @keywords internal
shadowtext <- function(x, y, labels, col = "black", bg = "white",
                       pos = NULL, offset = 0.5, cex = 1, ...) {
  # Draw text with background for better visibility
  if (!is.null(pos)) {
    for (i in c(-0.5, 0, 0.5)) {
      for (j in c(-0.5, 0, 0.5)) {
        if (i != 0 || j != 0) {
          text(x + i, y + j, labels, col = bg, pos = pos, offset = offset, cex = cex, ...)
        }
      }
    }
    text(x, y, labels, col = col, pos = pos, offset = offset, cex = cex, ...)
  } else {
    for (i in c(-0.5, 0, 0.5)) {
      for (j in c(-0.5, 0, 0.5)) {
        if (i != 0 || j != 0) {
          text(x + i, y + j, labels, col = bg, cex = cex, ...)
        }
      }
    }
    text(x, y, labels, col = col, cex = cex, ...)
  }
}

#' Helper function to draw text with a shadow/background
#' @keywords internal
shadowtext <- function(x, y, labels, col = "black", bg = "white",
                       pos = NULL, offset = 0.5, cex = 1, ...) {
  # Draw text with background for better visibility
  if (!is.null(pos)) {
    for (i in c(-0.5, 0, 0.5)) {
      for (j in c(-0.5, 0, 0.5)) {
        if (i != 0 || j != 0) {
          text(x + i, y + j, labels, col = bg, pos = pos, offset = offset, cex = cex, ...)
        }
      }
    }
    text(x, y, labels, col = col, pos = pos, offset = offset, cex = cex, ...)
  } else {
    for (i in c(-0.5, 0, 0.5)) {
      for (j in c(-0.5, 0, 0.5)) {
        if (i != 0 || j != 0) {
          text(x + i, y + j, labels, col = bg, cex = cex, ...)
        }
      }
    }
    text(x, y, labels, col = col, cex = cex, ...)
  }
}

#' Export interactive HTML chord diagram for ABC connections
#'
#' This function creates an HTML chord diagram visualization for ABC connections.
#'
#' @param abc_results A data frame containing ABC results.
#' @param output_file File path for the output HTML file.
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param open Logical. If TRUE, opens the HTML file after creation.
#'
#' @return The file path of the created HTML file (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' export_chord(abc_results, output_file = "abc_chord.html")
#' }
export_chord <- function(abc_results, output_file = "abc_chord.html",
                         top_n = 50, min_score = 0.1, open = TRUE) {
  # This is just a wrapper around export_chord_diagram
  export_chord_diagram(abc_results, output_file, top_n, min_score, open)
}

#' Generate a comprehensive discovery report
#'
#' This function generates an HTML report summarizing discovery results
#' without enforcing entity type constraints. It includes data validation
#' to avoid errors with publication years and other data issues.
#'
#' @param results A list containing discovery results from different approaches.
#' @param visualizations A list containing file paths to visualizations.
#' @param articles A data frame containing the original articles.
#' @param output_file File path for the output HTML report.
#'
#' @return The file path of the created HTML report (invisibly).
#' @export
create_report <- function(results, visualizations = NULL, articles = NULL,
                          output_file = "discovery_report.html") {

  # Create HTML header
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <title>Literature-Based Discovery Report</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.6; }",
    "    h1, h2, h3 { color: #2c3e50; }",
    "    .container { max-width: 1200px; margin: 0 auto; }",
    "    .section { margin-bottom: 30px; border: 1px solid #ddd; padding: 20px; border-radius: 5px; }",
    "    .plot-container { text-align: center; margin: 20px 0; }",
    "    table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
    "    th, td { padding: 8px; text-align: left; border: 1px solid #ddd; }",
    "    th { background-color: #f2f2f2; }",
    "    tr:nth-child(even) { background-color: #f9f9f9; }",
    "    .nav { position: fixed; top: 0; width: 100%; background-color: #2c3e50; color: white; z-index: 1000; }",
    "    .nav ul { list-style-type: none; margin: 0; padding: 0; overflow: hidden; }",
    "    .nav li { float: left; }",
    "    .nav li a { display: block; color: white; text-align: center; padding: 14px 16px; text-decoration: none; }",
    "    .nav li a:hover { background-color: #1a252f; }",
    "    .content { margin-top: 60px; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <div class='nav'>",
    "    <ul>",
    "      <li><a href='#overview'>Overview</a></li>"
  )

  # Add navigation links for each approach
  for (approach in names(results)) {
    if (!is.null(results[[approach]]) && nrow(results[[approach]]) > 0) {
      html_content <- c(html_content,
                        paste0("      <li><a href='#", approach, "'>", toupper(approach), " Results</a></li>")
      )
    }
  }

  # Add navigation links for visualizations and data
  if (!is.null(visualizations)) {
    html_content <- c(html_content, "      <li><a href='#visualizations'>Visualizations</a></li>")
  }

  if (!is.null(articles) && nrow(articles) > 0) {
    html_content <- c(html_content, "      <li><a href='#data'>Data Summary</a></li>")
  }

  # Close navigation and start content
  html_content <- c(html_content,
                    "    </ul>",
                    "  </div>",
                    "  <div class='content'>",
                    "    <div class='container'>",
                    "      <h1>Literature-Based Discovery Report</h1>",
                    "      <div class='section' id='overview'>",
                    "        <h2>Overview</h2>",
                    paste0("        <p>Report generated on: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p>"),
                    "        <p>This report contains the results of literature-based discovery analysis using multiple approaches. The analysis was performed without enforcing entity type constraints, allowing for more flexible discovery of potential connections.</p>"
  )

  # Add summary of results
  html_content <- c(html_content, "        <h3>Results Summary</h3>", "        <table>",
                    "          <tr><th>Approach</th><th>Number of Connections</th></tr>")

  for (approach in names(results)) {
    if (!is.null(results[[approach]])) {
      n_results <- nrow(results[[approach]])
      html_content <- c(html_content,
                        paste0("          <tr><td>", toupper(approach), "</td><td>", n_results, "</td></tr>"))
    }
  }

  html_content <- c(html_content, "        </table>", "      </div>")

  # Add results for each approach
  for (approach in names(results)) {
    if (!is.null(results[[approach]]) && nrow(results[[approach]]) > 0) {
      approach_results <- results[[approach]]
      n_results <- min(50, nrow(approach_results))  # Limit to top 50 for display

      html_content <- c(html_content,
                        paste0("      <div class='section' id='", approach, "'>"),
                        paste0("        <h2>", toupper(approach), " Results (Top ", n_results, ")</h2>"),
                        "        <table>",
                        "          <tr>")

      # Add headers based on available columns
      columns <- colnames(approach_results)
      for (col in columns[1:min(10, length(columns))]) {  # Limit to 10 columns for readability
        html_content <- c(html_content, paste0("            <th>", col, "</th>"))
      }

      html_content <- c(html_content, "          </tr>")

      # Add rows
      for (i in 1:n_results) {
        html_content <- c(html_content, "          <tr>")

        for (col in columns[1:min(10, length(columns))]) {
          value <- approach_results[i, col]

          # Format based on column type
          if (is.numeric(value)) {
            formatted_value <- format(round(value, 4), nsmall = 4)
          } else {
            formatted_value <- as.character(value)
          }

          html_content <- c(html_content, paste0("            <td>", formatted_value, "</td>"))
        }

        html_content <- c(html_content, "          </tr>")
      }

      html_content <- c(html_content, "        </table>", "      </div>")
    }
  }

  # Add visualizations section if provided
  if (!is.null(visualizations)) {
    html_content <- c(html_content,
                      "      <div class='section' id='visualizations'>",
                      "        <h2>Visualizations</h2>")

    # Add heatmap visualization
    if (!is.null(visualizations$heatmap)) {
      html_content <- c(html_content,
                        "        <div class='plot-container'>",
                        "          <h3>Heatmap Visualization</h3>",
                        paste0("          <img src='", visualizations$heatmap, "' alt='Heatmap' style='max-width: 100%;'>"),
                        "        </div>")
    }

    # Add network visualization as a link, not embedded - similar to chord diagram
    if (!is.null(visualizations$network)) {
      # Check if the file is HTML (interactive) or PNG (static)
      is_interactive <- grepl("\\.html$", visualizations$network)
      network_file_to_use <- if (is_interactive) visualizations$network else "migraine_network.html"

      html_content <- c(html_content,
                        "        <div class='plot-container'>",
                        "          <h3>Network Visualization</h3>",
                        paste0("          <p>The network visualization is available as a separate interactive visualization. <a href='", network_file_to_use, "' target='_blank'>Open Network Visualization</a></p>"),
                        "        </div>")
    }

    # Add chord diagram as a link, not embedded
    if (!is.null(visualizations$chord)) {
      html_content <- c(html_content,
                        "        <div class='plot-container'>",
                        "          <h3>Chord Diagram</h3>",
                        paste0("          <p>The chord diagram is available as a separate visualization. <a href='", visualizations$chord, "' target='_blank'>Open Chord Diagram</a></p>"),
                        "        </div>")
    }

    html_content <- c(html_content, "      </div>")
  }

  # Add data summary section if articles provided
  if (!is.null(articles) && nrow(articles) > 0) {
    html_content <- c(html_content,
                      "      <div class='section' id='data'>",
                      "        <h2>Data Summary</h2>",
                      paste0("        <p>Analysis based on ", nrow(articles), " articles.</p>"))

    # Add summary statistics if publication_year is available
    if ("publication_year" %in% colnames(articles)) {
      # Ensure publication_year is numeric and handle missing values
      pub_years <- articles$publication_year
      if (!is.numeric(pub_years)) {
        pub_years <- suppressWarnings(as.numeric(as.character(pub_years)))
      }

      # Remove NA values
      pub_years <- pub_years[!is.na(pub_years)]

      if (length(pub_years) > 0) {
        year_counts <- table(pub_years)
        html_content <- c(html_content,
                          "        <h3>Publication Years Distribution</h3>",
                          "        <table>",
                          "          <tr><th>Year</th><th>Number of Articles</th></tr>")

        # Sort years numerically in descending order
        for (year in sort(as.numeric(names(year_counts)), decreasing = TRUE)) {
          html_content <- c(html_content,
                            paste0("          <tr><td>", year, "</td><td>", year_counts[as.character(year)], "</td></tr>"))
        }

        html_content <- c(html_content, "        </table>")
      } else {
        html_content <- c(html_content, "        <p>No valid publication years found in the data.</p>")
      }
    }

    html_content <- c(html_content, "      </div>")
  }

  # Close HTML content
  html_content <- c(html_content,
                    "    </div>",
                    "  </div>",
                    "</body>",
                    "</html>")

  # Write HTML file
  writeLines(html_content, output_file)

  # Return file path invisibly
  invisible(output_file)
}

#' Export interactive HTML chord diagram for ABC connections
#'
#' This function creates an HTML chord diagram visualization for ABC connections,
#' properly coloring the arcs based on whether each term is an A, B, or C term.
#'
#' @param abc_results A data frame containing ABC results.
#' @param output_file File path for the output HTML file.
#' @param top_n Number of top results to visualize.
#' @param min_score Minimum score threshold for including connections.
#' @param open Logical. If TRUE, opens the HTML file after creation.
#'
#' @return The file path of the created HTML file (invisibly).
#' @importFrom stats complete.cases
#' @export
#'
#' @examples
#' \dontrun{
#' export_chord_diagram(abc_results, output_file = "abc_chord.html")
#' }
export_chord_diagram <- function(abc_results, output_file = "abc_chord.html",
                                 top_n = 50, min_score = 0.1, open = TRUE) {

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

  # Remove any rows with missing required fields
  required_fields <- c("a_term", "b_term", "c_term", "a_b_score", "b_c_score")
  complete_rows <- stats::complete.cases(results[, required_fields])
  if (sum(!complete_rows) > 0) {
    warning("Removing ", sum(!complete_rows), " rows with missing required fields")
    results <- results[complete_rows, ]

    if (nrow(results) == 0) {
      stop("No complete rows remain after filtering")
    }
  }

  # Get unique terms
  all_terms <- unique(c(results$a_term, results$b_term, results$c_term))
  n_terms <- length(all_terms)

  # Print debugging info
  message("Number of unique terms: ", n_terms)
  if (n_terms > 0) {
    message("First few terms: ", paste(head(all_terms, min(5, n_terms)), collapse=", "))
  }

  # Create matrix for chord diagram
  matrix_data <- matrix(0, nrow = n_terms, ncol = n_terms)
  rownames(matrix_data) <- all_terms
  colnames(matrix_data) <- all_terms

  # Fill matrix with connection strengths
  for (i in 1:nrow(results)) {
    a_term <- as.character(results$a_term[i])
    b_term <- as.character(results$b_term[i])
    c_term <- as.character(results$c_term[i])

    # Skip if any term is missing from the all_terms
    if (!(a_term %in% all_terms) || !(b_term %in% all_terms) || !(c_term %in% all_terms)) {
      warning("Terms not found in all_terms: ",
              paste(c(a_term, b_term, c_term)[!c(a_term, b_term, c_term) %in% all_terms], collapse=", "))
      next
    }

    # Get indices directly to avoid string matching errors
    a_idx <- match(a_term, all_terms)
    b_idx <- match(b_term, all_terms)
    c_idx <- match(c_term, all_terms)

    # Directly access matrix by index
    matrix_data[a_idx, b_idx] <- results$a_b_score[i]
    matrix_data[b_idx, a_idx] <- results$a_b_score[i]  # Make symmetric

    matrix_data[b_idx, c_idx] <- results$b_c_score[i]
    matrix_data[c_idx, b_idx] <- results$b_c_score[i]  # Make symmetric
  }

  # Create term role assignments (A, B, C)
  roles <- rep("B", length(all_terms))  # Default all to B
  names(roles) <- all_terms

  # Set A terms
  a_terms <- unique(results$a_term)
  for (term in a_terms) {
    if (term %in% names(roles)) {
      roles[term] <- "A"
    }
  }

  # Set C terms (don't override A terms)
  c_terms <- unique(results$c_term)
  for (term in c_terms) {
    if (term %in% names(roles) && roles[term] != "A") {
      roles[term] <- "C"
    }
  }

  # Verify roles vector is in the exact same order as all_terms
  ordered_roles <- roles[all_terms]

  # Check for NAs in the ordered_roles
  if (any(is.na(ordered_roles))) {
    warning("Some roles are NA. Replacing with 'B'.")
    ordered_roles[is.na(ordered_roles)] <- "B"
  }

  # Replace the roles vector with the ordered version
  roles <- ordered_roles

  # Print term role counts for debugging
  message("Role assignments: A=", sum(roles == "A"),
          ", B=", sum(roles == "B"),
          ", C=", sum(roles == "C"))

  # Create HTML content
  html_content <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "  <meta charset=\"UTF-8\">",
    "  <title>ABC Model Chord Diagram</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }",
    "    #chord { width: 900px; height: 900px; margin: 0 auto; }",
    "    .group-arc { stroke: #fff; stroke-width: 1.5px; }",
    "    .chord { opacity: 0.7; }",
    "    .chord:hover { opacity: 1; }",
    "    .tooltip { position: absolute; background: white; border: 1px solid black; padding: 5px; border-radius: 5px; box-shadow: 2px 2px 4px rgba(0,0,0,0.3); font-size: 12px; }",
    "    h1 { text-align: center; color: #333; }",
    "    .legend { text-align: center; margin-bottom: 20px; }",
    "    .legend-item { display: inline-block; margin: 10px; padding: 5px; }",
    "    .legend-color { display: inline-block; width: 15px; height: 15px; margin-right: 5px; vertical-align: middle; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <h1>ABC Model Chord Diagram</h1>",
    "  <div class='legend'>",
    "    <div class='legend-item'><span class='legend-color' style='background-color: #ff7f0e;'></span> A Terms</div>",
    "    <div class='legend-item'><span class='legend-color' style='background-color: #1f77b4;'></span> B Terms</div>",
    "    <div class='legend-item'><span class='legend-color' style='background-color: #2ca02c;'></span> C Terms</div>",
    "  </div>",
    "  <div id='chord'></div>"
  )

  # Add JavaScript part
  js_content <- c(
    "  <script src='https://d3js.org/d3.v5.min.js'></script>",
    "  <script>"
  )

  # Ensure matrix is valid (no NAs, etc.)
  matrix_data[is.na(matrix_data)] <- 0

  # Convert matrix to JSON as a string
  matrix_json <- paste0("[", paste(apply(matrix_data, 1, function(row) {
    paste0("[", paste(row, collapse = ", "), "]")
  }), collapse = ", "), "]")

  # Convert term names to JSON as a string - properly escape special characters
  terms_json <- paste0("[", paste(sapply(all_terms, function(term) {
    # Handle NA/NULL
    if(is.null(term) || is.na(term)) return("\"\"")
    # Escape quotes and backslashes
    term_escaped <- gsub("\\\\", "\\\\\\\\", term) # double backslashes
    term_escaped <- gsub("\"", "\\\\\"", term_escaped) # escape quotes
    paste0("\"", term_escaped, "\"")
  }), collapse=", "), "]")

  # Convert roles to JSON as a string
  roles_json <- paste0("[", paste(paste0("\"", roles, "\""), collapse = ", "), "]")

  # Add data to JavaScript
  js_data <- c(
    paste0("    const matrix = ", matrix_json, ";"),
    paste0("    const names = ", terms_json, ";"),
    paste0("    const roles = ", roles_json, ";")
  )

  # Add D3.js code
  js_visualization <- c(
    "    // Set up dimensions",
    "    const width = 800;",
    "    const height = 800;",
    "    const innerRadius = Math.min(width, height) * 0.4;",
    "    const outerRadius = innerRadius * 1.1;",
    "",
    "    // Define role colors directly",
    "    const roleColors = {",
    "      'A': '#ff7f0e',  // orange",
    "      'B': '#1f77b4',  // blue",
    "      'C': '#2ca02c'   // green",
    "    };",
    "",
    "    // Create SVG element",
    "    const svg = d3.select('#chord')",
    "      .append('svg')",
    "      .attr('width', width)",
    "      .attr('height', height)",
    "      .append('g')",
    "      .attr('transform', `translate(${width / 2}, ${height / 2})`);",
    "",
    "    // Create chord layout",
    "    const chord = d3.chord()",
    "      .padAngle(0.05)",
    "      .sortSubgroups(d3.descending);",
    "",
    "    // Generate chord diagram data",
    "    const chords = chord(matrix);",
    "",
    "    // Create tooltip",
    "    const tooltip = d3.select('body')",
    "      .append('div')",
    "      .attr('class', 'tooltip')",
    "      .style('opacity', 0);",
    "",
    "    // Draw outer group arcs",
    "    const arcGroups = svg.append('g')",
    "      .selectAll('g')",
    "      .data(chords.groups)",
    "      .enter()",
    "      .append('g');",
    "",
    "    // Add the outer arc paths with colors based on roles",
    "    arcGroups.append('path')",
    "      .attr('d', d3.arc().innerRadius(innerRadius).outerRadius(outerRadius))",
    "      .style('fill', d => {",
    "        if (d.index < 0 || d.index >= roles.length) {",
    "          console.error('Invalid index for role:', d.index);",
    "          return '#999';",
    "        }",
    "        const role = roles[d.index];",
    "        return roleColors[role] || '#999';",
    "      })",
    "      .style('stroke', 'white')",
    "      .style('stroke-width', '1.5px')",
    "      .on('mouseover', function(d) {",
    "        let term = 'undefined';",
    "        let role = 'unknown';",
    "        ",
    "        if (d.index >= 0 && d.index < names.length) {",
    "          term = names[d.index] || 'unnamed';",
    "          role = roles[d.index] || 'unknown';",
    "        }",
    "        ",
    "        tooltip.transition().duration(200).style('opacity', 0.9);",
    "        tooltip.html(`${term} (${role} Term)`)",
    "          .style('left', (d3.event.pageX + 10) + 'px')",
    "          .style('top', (d3.event.pageY - 28) + 'px');",
    "      })",
    "      .on('mouseout', function() {",
    "        tooltip.transition().duration(500).style('opacity', 0);",
    "      });",
    "",
    "    // Add white backdrop for term labels to improve readability",
    "    arcGroups.append('text')",
    "      .each(d => { d.angle = (d.startAngle + d.endAngle) / 2; })",
    "      .attr('dy', '.35em')",
    "      .attr('transform', d => {",
    "        const rotate = (d.angle * 180 / Math.PI - 90);",
    "        const flip = d.angle > Math.PI ? 'rotate(180)' : '';",
    "        return `rotate(${rotate}) translate(${outerRadius + 10},0) ${flip}`;",
    "      })",
    "      .attr('text-anchor', d => d.angle > Math.PI ? 'end' : null)",
    "      .text(d => {",
    "        if (d.index < 0 || d.index >= names.length) {",
    "          console.error('Invalid index for name:', d.index);",
    "          return 'undefined';",
    "        }",
    "        return names[d.index] || 'unnamed';",
    "      })",
    "      .style('font-size', '10px')",
    "      .style('stroke', 'white')",
    "      .style('stroke-width', '3px')",
    "      .style('fill', 'none');",
    "",
    "    // Add actual term labels",
    "    arcGroups.append('text')",
    "      .each(d => { d.angle = (d.startAngle + d.endAngle) / 2; })",
    "      .attr('dy', '.35em')",
    "      .attr('transform', d => {",
    "        const rotate = (d.angle * 180 / Math.PI - 90);",
    "        const flip = d.angle > Math.PI ? 'rotate(180)' : '';",
    "        return `rotate(${rotate}) translate(${outerRadius + 10},0) ${flip}`;",
    "      })",
    "      .attr('text-anchor', d => d.angle > Math.PI ? 'end' : null)",
    "      .text(d => {",
    "        if (d.index < 0 || d.index >= names.length) {",
    "          return 'undefined';",
    "        }",
    "        return names[d.index] || 'unnamed';",
    "      })",
    "      .style('font-size', '10px')",
    "      .style('fill', '#333');",
    "",
    "    // Add ribbons for connections",
    "    svg.append('g')",
    "      .selectAll('path')",
    "      .data(chords)",
    "      .enter()",
    "      .append('path')",
    "      .attr('d', d3.ribbon().radius(innerRadius))",
    "      .style('fill', d => {",
    "        let sourceRole = 'B';  // Default role",
    "        let targetRole = 'B';  // Default role",
    "        ",
    "        if (d.source.index >= 0 && d.source.index < roles.length) {",
    "          sourceRole = roles[d.source.index] || 'B';",
    "        }",
    "        ",
    "        if (d.target.index >= 0 && d.target.index < roles.length) {",
    "          targetRole = roles[d.target.index] || 'B';",
    "        }",
    "        ",
    "        const sourceColor = roleColors[sourceRole] || '#999';",
    "        const targetColor = roleColors[targetRole] || '#999';",
    "        return d3.interpolateRgb(sourceColor, targetColor)(0.3);",
    "      })",
    "      .style('stroke', 'white')",
    "      .style('stroke-width', '0.5px')",
    "      .style('opacity', 0.7)",
    "      .on('mouseover', function(d) {",
    "        d3.select(this)",
    "          .style('opacity', 1)",
    "          .style('stroke-width', '1.5px');",
    "          ",
    "        const sourceTerm = names[d.source.index];",
    "        const targetTerm = names[d.target.index];",
    "        const sourceRole = roles[d.source.index];",
    "        const targetRole = roles[d.target.index];",
    "          ",
    "        tooltip.transition()",
    "          .duration(200)",
    "          .style('opacity', 0.9);",
    "          ",
    "        tooltip.html(`${sourceTerm} (${sourceRole}) <-> ${targetTerm} (${targetRole})<br>Strength: ${d.source.value.toFixed(3)}`)",
    "          .style('left', (d3.event.pageX + 10) + 'px')",
    "          .style('top', (d3.event.pageY - 28) + 'px');",
    "      })",
    "      .on('mouseout', function() {",
    "        d3.select(this)",
    "          .style('opacity', 0.7)",
    "          .style('stroke-width', '0.5px');",
    "          ",
    "        tooltip.transition()",
    "          .duration(500)",
    "          .style('opacity', 0);",
    "      });"
  )

  # Close JavaScript and HTML
  js_end <- c(
    "  </script>",
    "</body>",
    "</html>"
  )

  # Combine all HTML content
  html_content <- c(html_content, js_content, js_data, js_visualization, js_end)

  # Write HTML file
  writeLines(html_content, output_file)

  # Open in browser if requested
  if (open) {
    utils::browseURL(output_file)
  }

  # Return file path invisibly
  invisible(output_file)
}
