#' Perform comprehensive literature-based discovery without type constraints
#'
#' This function performs a comprehensive literature-based discovery analysis
#' using multiple approaches without enforcing entity type constraints.
#'
#' @param search_query Character string, the search query for retrieving initial articles.
#' @param a_term Character string, the source term (A) for discovery.
#' @param max_results Maximum number of results to return for each approach.
#' @param discovery_approaches Character vector, the discovery approaches to use.
#' @param include_visualizations Logical. If TRUE, generates visualizations.
#' @param output_file File path for the output report.
#' @param api_key Character string. API key for PubMed and other services.
#' @param dictionary_sources Character vector. Sources for entity dictionaries: "local", "mesh", "umls".
#' @param entity_categories Character vector. Entity categories to include.
#'
#' @return A list containing discovery results from all approaches.
#' @export
#'
#' @examples
#' \dontrun{
#' discovery_results <- run_lbd(
#'   search_query = "migraine headache",
#'   a_term = "migraine",
#'   discovery_approaches = c("abc", "anc", "lsi"),
#'   dictionary_sources = c("mesh"),
#'   entity_categories = c("disease", "drug", "gene", "protein")
#' )
#' }
run_lbd <- function(search_query, a_term,
                    max_results = 100,
                    discovery_approaches = c("abc", "anc", "lsi", "bitola"),
                    include_visualizations = TRUE,
                    output_file = "discovery_report.html",
                    api_key = NULL,
                    dictionary_sources = c("local", "mesh", "umls"),
                    entity_categories = c("disease", "drug", "gene")) {

  # Check if discovery_approaches contains valid approaches
  valid_approaches <- c("abc", "anc", "lsi", "bitola")
  invalid_approaches <- setdiff(discovery_approaches, valid_approaches)

  if (length(invalid_approaches) > 0) {
    stop("Invalid discovery approaches: ", paste(invalid_approaches, collapse = ", "),
         ". Valid approaches are: ", paste(valid_approaches, collapse = ", "))
  }

  # Validate dictionary sources
  valid_sources <- c("local", "mesh", "umls")
  invalid_sources <- setdiff(dictionary_sources, valid_sources)

  if (length(invalid_sources) > 0) {
    stop("Invalid dictionary sources: ", paste(invalid_sources, collapse = ", "),
         ". Valid sources are: ", paste(valid_sources, collapse = ", "))
  }

  # Step 1: Retrieve articles from PubMed
  message("Step 1: Retrieving articles from PubMed...")
  articles <- pubmed_search(search_query, max_results = 1000, api_key = api_key)

  if (nrow(articles) == 0) {
    stop("No articles found for the search query: ", search_query)
  }

  message("Retrieved ", nrow(articles), " articles")

  # Step 2: Preprocess text
  message("Step 2: Preprocessing article text...")
  preprocessed_data <- vec_preprocess(
    articles,
    text_column = "abstract",
    remove_stopwords = TRUE
  )

  # Step 3: Create combined dictionary from specified sources and categories
  message("Step 3: Creating dictionaries for entity extraction...")
  combined_dict <- NULL

  for (source in dictionary_sources) {
    for (category in entity_categories) {
      message("Loading ", category, " dictionary from ", source, " source...")

      if (source == "umls" && is.null(api_key)) {
        message("Skipping UMLS source as API key is required")
        next
      }

      # Try to load dictionary
      dict <- tryCatch({
        load_dictionary(
          dictionary_type = category,
          source = source,
          api_key = api_key
        )
      }, error = function(e) {
        message("Error loading ", category, " dictionary from ", source, ": ", e$message)
        return(NULL)
      })

      # Combine dictionaries
      if (!is.null(dict) && nrow(dict) > 0) {
        message("Added ", nrow(dict), " terms from ", category, " (", source, ")")
        if (is.null(combined_dict)) {
          combined_dict <- dict
        } else {
          combined_dict <- rbind(combined_dict, dict)
        }
      }
    }
  }

  if (is.null(combined_dict) || nrow(combined_dict) == 0) {
    stop("No valid dictionary terms found. Check dictionary sources and categories.")
  }

  message("Created combined dictionary with ", nrow(combined_dict), " terms")

  # Step 4: Extract entities using the combined dictionary
  message("Step 4: Extracting biomedical entities...")
  entities <- extract_entities(
    preprocessed_data,
    text_column = "abstract",
    dictionary = combined_dict
  )

  # Step 5: Create co-occurrence matrix
  message("Step 5: Creating co-occurrence matrix...")
  co_matrix <- create_comat(
    entities,
    doc_id_col = "doc_id",
    entity_col = "entity",
    type_col = "entity_type",
    normalize = TRUE
  )

  # Step 6: Apply discovery approaches
  message("Step 6: Applying discovery approaches...")
  results <- list()

  # Standard ABC model (without entity type constraints)
  if ("abc" %in% discovery_approaches) {
    message("Applying ABC model...")
    results$abc <- abc_model(
      co_matrix,
      a_term = a_term,
      n_results = max_results,
      scoring_method = "combined"
    )
  }

  # AnC model (if included in discovery approaches)
  if ("anc" %in% discovery_approaches) {
    message("Applying AnC model...")
    results$anc <- anc_model(
      co_matrix,
      a_term = a_term,
      n_b_terms = 5,
      n_results = max_results
    )
  }

  # LSI model (if included in discovery approaches)
  if ("lsi" %in% discovery_approaches) {
    message("Applying LSI model...")
    # Create term-document matrix for LSI
    tdm <- create_term_document_matrix(preprocessed_data)

    results$lsi <- lsi_model(
      tdm,
      a_term = a_term,
      n_factors = 100,
      n_results = max_results
    )
  }

  # BITOLA model (if included in discovery approaches)
  if ("bitola" %in% discovery_approaches) {
    message("Applying BITOLA model...")
    # Get entity type of A term (if available)
    entity_types <- attr(co_matrix, "entity_types")

    if (!is.null(entity_types) && a_term %in% names(entity_types)) {
      a_type <- entity_types[a_term]
      message("A term '", a_term, "' has entity type: ", a_type)

      # Get all unique entity types
      all_types <- unique(entity_types)

      # Instead of enforcing a specific target type, use all types other than the A term's type
      target_types <- setdiff(all_types, a_type)

      if (length(target_types) > 0) {
        message("Using all available entity types for potential C terms")
        # Use a modified version of BITOLA that doesn't enforce strict type constraints
        results$bitola <- apply_bitola_flexible(
          co_matrix,
          a_term = a_term,
          n_results = max_results
        )
      } else {
        message("No entity types found for C terms, using standard ABC model for BITOLA approach")
        results$bitola <- abc_model(
          co_matrix,
          a_term = a_term,
          n_results = max_results
        )
      }
    } else {
      message("A term '", a_term, "' has no entity type information, using standard ABC model for BITOLA approach")
      results$bitola <- abc_model(
        co_matrix,
        a_term = a_term,
        n_results = max_results
      )
    }
  }

  # Step 7: Create visualizations and report
  if (include_visualizations) {
    message("Step 7: Creating visualizations and report...")
    visualizations <- list()

    # Create visualizations for ABC results
    if ("abc" %in% discovery_approaches && !is.null(results$abc) && nrow(results$abc) > 0) {
      # Create heatmap
      heatmap_file <- tempfile(fileext = ".png")
      png(heatmap_file, width = 1000, height = 800)
      vis_heatmap(
        results$abc,
        top_n = min(25, nrow(results$abc)),
        show_significance = FALSE,
        title = "ABC Model Heatmap"
      )
      dev.off()
      visualizations$heatmap <- heatmap_file

      # Create network visualization
      network_file <- tempfile(fileext = ".html")
      export_network(
        results$abc,
        output_file = network_file,
        top_n = min(50, nrow(results$abc)),
        open = FALSE
      )
      visualizations$network <- network_file

      # Create chord diagram
      chord_file <- tempfile(fileext = ".html")
      export_chord_diagram(
        results$abc,
        output_file = chord_file,
        top_n = min(50, nrow(results$abc)),
        open = FALSE
      )
      visualizations$chord <- chord_file
    }

    # Generate HTML report
    create_report(
      results = results,
      visualizations = visualizations,
      articles = articles,
      output_file = output_file
    )
  }

  # Return results
  return(results)
}

#' Apply a flexible BITOLA-style discovery model without strict type constraints
#'
#' This function implements a modified BITOLA-style discovery model that preserves
#' entity type information but doesn't enforce strict type constraints.
#'
#' @param co_matrix A co-occurrence matrix with entity types as an attribute.
#' @param a_term Character string, the source term (A).
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#'
#' @return A data frame with ranked discovery results.
#' @keywords internal
apply_bitola_flexible <- function(co_matrix, a_term, min_score = 0.1, n_results = 100) {
  # Check if the matrix has entity types
  has_entity_types <- !is.null(attr(co_matrix, "entity_types"))

  if (!has_entity_types) {
    message("No entity type information available. Using standard ABC model.")
    return(abc_model(co_matrix, a_term, min_score = min_score, n_results = n_results))
  }

  # Check if A term exists in the matrix
  if (!a_term %in% rownames(co_matrix)) {
    stop("A-term '", a_term, "' not found in the co-occurrence matrix")
  }

  # Get entity types
  entity_types <- attr(co_matrix, "entity_types")

  # Check if A term has a type
  a_type <- NA
  if (a_term %in% names(entity_types)) {
    a_type <- entity_types[a_term]
  } else {
    message("A-term '", a_term, "' has no entity type information.")
  }

  # Extract A-B associations
  a_associations <- co_matrix[a_term, ]

  # Filter B terms by removing terms with low association to A
  b_terms <- names(a_associations[a_associations > min_score])

  # Remove A term from B terms if present
  b_terms <- b_terms[b_terms != a_term]

  # If no B terms found, return empty result
  if (length(b_terms) == 0) {
    message("No B terms found with association score > ", min_score)
    return(data.frame(
      a_term = character(),
      a_type = character(),
      b_term = character(),
      b_type = character(),
      c_term = character(),
      c_type = character(),
      a_b_score = numeric(),
      b_c_score = numeric(),
      bitola_score = numeric(),
      support = integer(),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize results
  results <- data.frame(
    a_term = character(),
    a_type = character(),
    b_term = character(),
    b_type = character(),
    c_term = character(),
    c_type = character(),
    a_b_score = numeric(),
    b_c_score = numeric(),
    bitola_score = numeric(),
    support = integer(),
    stringsAsFactors = FALSE
  )

  # For each B term
  message("Analyzing ", length(b_terms), " B terms...")
  pb <- utils::txtProgressBar(min = 0, max = length(b_terms), style = 3)

  for (b_idx in seq_along(b_terms)) {
    utils::setTxtProgressBar(pb, b_idx)

    b_term <- b_terms[b_idx]
    b_type <- if (b_term %in% names(entity_types)) entity_types[b_term] else NA

    # Get B-C associations
    b_associations <- co_matrix[b_term, ]

    # Filter C terms with sufficient connection
    potential_c_terms <- names(b_associations[b_associations > min_score])

    # Remove A and B terms from potential C terms
    potential_c_terms <- setdiff(potential_c_terms, c(a_term, b_term))

    if (length(potential_c_terms) > 0) {
      for (c_term in potential_c_terms) {
        c_type <- if (c_term %in% names(entity_types)) entity_types[c_term] else NA

        # Get scores
        a_b_score <- a_associations[b_term]
        b_c_score <- b_associations[c_term]

        # Calculate BITOLA score
        # Modified formula based on BITOLA paper
        bitola_score <- (a_b_score * b_c_score)^2

        # Create result entry
        results <- rbind(results, data.frame(
          a_term = a_term,
          a_type = if (is.na(a_type)) "unknown" else a_type,
          b_term = b_term,
          b_type = if (is.na(b_type)) "unknown" else b_type,
          c_term = c_term,
          c_type = if (is.na(c_type)) "unknown" else c_type,
          a_b_score = a_b_score,
          b_c_score = b_c_score,
          bitola_score = bitola_score,
          support = 1,  # Will be summed later
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  close(pb)

  # If no results found, return empty data frame
  if (nrow(results) == 0) {
    message("No BITOLA connections found")
    return(results)
  }

  # Aggregate results by C term (support count)
  aggregated <- aggregate(
    cbind(support, bitola_score) ~ a_term + a_type + c_term + c_type,
    data = results,
    FUN = function(x) c(sum = sum(x), max = max(x))
  )

  # Restructure the aggregated results
  final_results <- data.frame(
    a_term = aggregated$a_term,
    a_type = aggregated$a_type,
    c_term = aggregated$c_term,
    c_type = aggregated$c_type,
    support = aggregated$support[, "sum"],
    bitola_score = aggregated$bitola_score[, "max"],
    stringsAsFactors = FALSE
  )

  # Add intermediate B terms for each A-C pair
  final_results$b_terms <- sapply(1:nrow(final_results), function(i) {
    ac_results <- results[results$a_term == final_results$a_term[i] &
                            results$c_term == final_results$c_term[i], ]

    # Sort B terms by score
    sorted_indices <- order(-ac_results$bitola_score)
    sorted_b_terms <- ac_results$b_term[sorted_indices]

    # Return comma-separated list of B terms
    paste(sorted_b_terms, collapse = ", ")
  })

  # Calculate new ranking score based on both support and BITOLA score
  final_results$ranking_score <- final_results$support * final_results$bitola_score

  # Sort by ranking score and limit to n_results
  final_results <- final_results[order(-final_results$ranking_score), ]
  if (nrow(final_results) > n_results) {
    final_results <- final_results[1:n_results, ]
  }

  return(final_results)
}
