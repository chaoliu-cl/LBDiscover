# Function implementations for the text_preprocessing module

#' Environment to store dictionary cache data
#' @keywords internal
.dict_cache_env <- new.env(parent = emptyenv())

#' Get dictionary cache environment
#' @return The environment containing cached dictionary data
#' @export
get_dict_cache <- function() {
  .dict_cache_env
}

#' Preprocess article text
#'
#' This function preprocesses article text for further analysis.
#'
#' @param text_data A data frame containing article text data (title, abstract, etc.).
#' @param text_column Name of the column containing text to process.
#' @param remove_stopwords Logical. If TRUE, removes stopwords.
#' @param custom_stopwords Character vector of additional stopwords to remove.
#' @param stem_words Logical. If TRUE, applies stemming to words.
#' @param min_word_length Minimum word length to keep.
#' @param max_word_length Maximum word length to keep.
#'
#' @return A data frame with processed text and extracted terms.
#' @export
#'
#' @examples
#' \dontrun{
#' processed_data <- preprocess_text(article_data, text_column = "abstract")
#' }
preprocess_text <- function(text_data, text_column = "abstract",
                            remove_stopwords = TRUE,
                            custom_stopwords = NULL,
                            stem_words = FALSE,
                            min_word_length = 3,
                            max_word_length = 50) {

  # Add ID column if not present
  if (!"doc_id" %in% colnames(text_data)) {
    text_data$doc_id <- seq_len(nrow(text_data))
  }

  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Create a copy of the data
  processed_data <- text_data

  # Ensure text is character
  processed_data[[text_column]] <- as.character(processed_data[[text_column]])

  # Remove missing values
  processed_data <- processed_data[!is.na(processed_data[[text_column]]), ]

  # Load standard English stopwords if needed
  stopword_list <- character(0)
  if (remove_stopwords) {
    # Define a basic set of English stopwords if tidytext not available
    stopword_list <- c(
      "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "from", "had",
      "has", "have", "he", "her", "his", "i", "in", "is", "it", "its", "of", "on",
      "or", "that", "the", "this", "to", "was", "were", "which", "with", "you"
    )

    # Add custom stopwords if provided
    if (!is.null(custom_stopwords)) {
      stopword_list <- c(stopword_list, custom_stopwords)
    }
  }

  # Function to tokenize text
  tokenize_text <- function(text) {
    # Convert to lowercase
    text <- tolower(text)

    # Replace non-alphanumeric characters with spaces
    text <- gsub("[^a-zA-Z0-9]", " ", text)

    # Split by whitespace
    words <- unlist(strsplit(text, "\\s+"))

    # Remove empty strings
    words <- words[words != ""]

    # Apply length filtering
    words <- words[nchar(words) >= min_word_length & nchar(words) <= max_word_length]

    # Remove stopwords if requested
    if (remove_stopwords) {
      words <- words[!words %in% stopword_list]
    }

    # Apply stemming if requested
    if (stem_words) {
      if (!requireNamespace("SnowballC", quietly = TRUE)) {
        stop("The SnowballC package is required for stemming. Install it with: install.packages('SnowballC')")
      }
      words <- SnowballC::wordStem(words, language = "english")
    }

    # Return the tokenized words
    return(words)
  }

  message("Tokenizing text...")

  # Process each document
  terms_list <- list()
  for (i in seq_len(nrow(processed_data))) {
    text <- processed_data[[text_column]][i]
    if (!is.na(text) && text != "") {
      # Tokenize the text
      words <- tokenize_text(text)

      # Count word frequencies
      word_counts <- table(words)

      # Convert to data frame
      if (length(word_counts) > 0) {
        terms_df <- data.frame(
          word = names(word_counts),
          count = as.numeric(word_counts),
          stringsAsFactors = FALSE
        )
        terms_list[[i]] <- terms_df
      } else {
        terms_list[[i]] <- data.frame(word = character(0), count = numeric(0), stringsAsFactors = FALSE)
      }
    } else {
      terms_list[[i]] <- data.frame(word = character(0), count = numeric(0), stringsAsFactors = FALSE)
    }
  }

  # Add terms to the processed data
  processed_data$terms <- terms_list

  return(processed_data)
}

#' Extract and classify entities from text with multi-domain types
#'
#' This function extracts entities from text and optionally assigns them to specific
#' semantic categories based on dictionaries.
#'
#' @param text_data A data frame containing article text data.
#' @param text_column Name of the column containing text to process.
#' @param dictionary Combined dictionary or list of dictionaries for entity extraction.
#' @param case_sensitive Logical. If TRUE, matching is case-sensitive.
#' @param overlap_strategy How to handle terms that match multiple dictionaries: "priority", "all", or "longest".
#' @param sanitize_dict Logical. If TRUE, sanitizes the dictionary before extraction.
#'
#' @return A data frame with extracted entities, their types, and positions.
#' @export
extract_entities <- function(text_data, text_column = "abstract",
                             dictionary = NULL,
                             case_sensitive = FALSE,
                             overlap_strategy = c("priority", "all", "longest"),
                             sanitize_dict = TRUE) {

  # Match overlap_strategy argument
  overlap_strategy <- match.arg(overlap_strategy)

  # Check inputs
  if (is.null(dictionary)) {
    stop("Dictionary must be provided")
  }

  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Sanitize dictionary if requested
  if (sanitize_dict) {
    original_count <- nrow(dictionary)
    dictionary <- sanitize_dictionary(dictionary, term_column = "term")
    if (nrow(dictionary) < original_count) {
      message("Dictionary sanitized: ", nrow(dictionary), " of ", original_count, " terms retained")
    }

    # If dictionary is empty after sanitization, stop
    if (nrow(dictionary) == 0) {
      stop("No terms remain in the dictionary after sanitization")
    }
  }

  # Add ID column if not present
  if (!"doc_id" %in% colnames(text_data)) {
    text_data$doc_id <- seq_len(nrow(text_data))
  }

  # Initialize results data frame
  results <- data.frame(
    doc_id = integer(),
    entity = character(),
    entity_type = character(),
    start_pos = integer(),
    end_pos = integer(),
    sentence = character(),
    stringsAsFactors = FALSE
  )

  # Function to split text into sentences for better context
  split_into_sentences <- function(text) {
    # Simple sentence splitting - can be enhanced with more rules
    sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))

    # Remove empty sentences
    sentences <- sentences[sentences != ""]

    # Calculate start and end positions for each sentence
    positions <- list()
    start_pos <- 1

    for (i in seq_along(sentences)) {
      end_pos <- start_pos + nchar(sentences[i]) - 1
      positions[[i]] <- c(start_pos, end_pos)
      start_pos <- end_pos + 2  # +2 to account for delimiter and space
    }

    return(list(sentences = sentences, positions = positions))
  }

  # Process each document
  message("Extracting entities from ", nrow(text_data), " documents...")

  for (i in seq_len(nrow(text_data))) {
    if (i %% 100 == 0) {
      message("Processing document ", i, " of ", nrow(text_data))
    }

    doc_id <- text_data$doc_id[i]
    text <- text_data[[text_column]][i]

    # Skip empty or NA text
    if (is.na(text) || text == "") {
      next
    }

    # Split text into sentences for better context
    text_sentences <- split_into_sentences(text)
    sentences <- text_sentences$sentences
    sentence_positions <- text_sentences$positions

    # Search for entities in each sentence
    for (s in seq_along(sentences)) {
      sentence <- sentences[s]
      sentence_start <- sentence_positions[[s]][1]

      # Text to search (handle case sensitivity)
      text_to_search <- if (case_sensitive) sentence else tolower(sentence)

      # Track all matches to handle overlaps
      all_matches <- list()

      # Process each term in the dictionary
      for (j in seq_len(nrow(dictionary))) {
        term <- dictionary$term[j]
        term_type <- dictionary$type[j]

        # Skip empty terms
        if (is.na(term) || term == "") {
          next
        }

        # For case-insensitive matching, convert term to lowercase
        search_term <- if (case_sensitive) term else tolower(term)

        # Create pattern with word boundaries
        pattern <- paste0("\\b", search_term, "\\b")

        # Find all matches
        matches <- gregexpr(pattern, text_to_search, fixed = FALSE, perl = TRUE)

        # If matches found
        if (matches[[1]][1] != -1) {
          match_starts <- matches[[1]]
          match_ends <- match_starts + attr(matches[[1]], "match.length") - 1

          for (m in seq_along(match_starts)) {
            # Calculate positions in the original text
            start_pos <- sentence_start + match_starts[m] - 1
            end_pos <- sentence_start + match_ends[m] - 1

            # Add to all matches
            all_matches[[length(all_matches) + 1]] <- list(
              term = term,
              type = term_type,
              priority = j,  # Lower index = higher priority
              start_pos = start_pos,
              end_pos = end_pos,
              length = end_pos - start_pos + 1,
              sentence = sentence
            )
          }
        }
      }

      # Handle overlapping matches based on strategy
      final_matches <- list()

      if (length(all_matches) > 0) {
        # Sort matches by start position
        all_matches <- all_matches[order(sapply(all_matches, function(x) x$start_pos))]

        if (overlap_strategy == "all") {
          # Keep all matches, including overlapping ones
          final_matches <- all_matches
        } else {
          # Check for overlaps
          current_end <- 0

          for (m in seq_along(all_matches)) {
            match <- all_matches[[m]]

            if (match$start_pos > current_end) {
              # No overlap with previous matches
              final_matches[[length(final_matches) + 1]] <- match
              current_end <- match$end_pos
            } else {
              # Overlap - apply strategy
              if (overlap_strategy == "longest") {
                # Check if current match is longer than the last accepted match
                if (match$length > (final_matches[[length(final_matches)]]$length)) {
                  # Replace last match with this longer one
                  final_matches[[length(final_matches)]] <- match
                  current_end <- match$end_pos
                }
              } else if (overlap_strategy == "priority") {
                # Check if current match has higher priority (lower priority number)
                if (match$priority < final_matches[[length(final_matches)]]$priority) {
                  # Replace last match with higher priority one
                  final_matches[[length(final_matches)]] <- match
                  current_end <- match$end_pos
                }
              }
            }
          }
        }
      }

      # Add final matches to results
      for (match in final_matches) {
        results <- rbind(results, data.frame(
          doc_id = doc_id,
          entity = match$term,
          entity_type = match$type,
          start_pos = match$start_pos,
          end_pos = match$end_pos,
          sentence = match$sentence,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # If no entities found, return empty data frame with correct structure
  if (nrow(results) == 0) {
    message("No entities found")
    return(data.frame(
      doc_id = integer(),
      entity = character(),
      entity_type = character(),
      start_pos = integer(),
      end_pos = integer(),
      sentence = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Count entity occurrences by type
  entity_counts <- table(results$entity_type)
  message("Extracted ", nrow(results), " entity mentions:")
  for (type in names(entity_counts)) {
    message("  ", type, ": ", entity_counts[type])
  }

  # Add frequency count as a column
  entity_frequency <- as.data.frame(table(results$entity, results$entity_type))
  colnames(entity_frequency) <- c("entity", "entity_type", "frequency")

  # Merge frequency back into results
  results <- merge(results, entity_frequency, by = c("entity", "entity_type"))

  # Sort by frequency and doc_id
  results <- results[order(-results$frequency, results$doc_id), ]

  return(results)
}

#' Load biomedical dictionaries with improved error handling
#'
#' This function loads pre-defined biomedical dictionaries or fetches terms from MeSH/UMLS.
#'
#' @param dictionary_type Type of dictionary to load. For local dictionaries, limited to "disease", "drug", "gene".
#'   For MeSH and UMLS, expanded to include more semantic categories.
#' @param custom_path Optional path to a custom dictionary file.
#' @param source The source to fetch terms from: "local", "mesh", or "umls".
#' @param api_key UMLS API key for authentication (required if source = "umls").
#' @param n_terms Number of terms to fetch.
#' @param mesh_query Additional query to filter MeSH terms (only if source = "mesh").
#' @param semantic_type_filter Filter by semantic type (used mainly with UMLS).
#' @param sanitize Logical. If TRUE, sanitizes the dictionary terms.
#' @param extended_mesh Logical. If TRUE and source is "mesh", uses PubMed search for additional terms.
#' @param mesh_queries Named list of MeSH queries for different categories (only if extended_mesh = TRUE).
#'
#' @return A data frame containing the dictionary.
#' @export
load_dictionary <- function(dictionary_type = NULL,
                            custom_path = NULL,
                            source = c("local", "mesh", "umls"),
                            api_key = NULL,
                            n_terms = 200,
                            mesh_query = NULL,
                            semantic_type_filter = NULL,
                            sanitize = TRUE,
                            extended_mesh = FALSE,
                            mesh_queries = NULL) {

  source <- match.arg(source)

  # Define allowed dictionary types for local source (limited to original three)
  local_dict_types <- c("disease", "drug", "gene")

  # Define expanded dictionary types for MeSH and UMLS
  mesh_dict_types <- c(
    "disease", "drug", "gene", "protein", "chemical", "pathway",
    "anatomy", "organism", "biological_process", "cell", "tissue",
    "symptom", "diagnostic_procedure", "therapeutic_procedure",
    "phenotype", "molecular_function"
  )

  umls_dict_types <- c(
    "disease", "drug", "gene", "protein", "chemical", "pathway",
    "anatomy", "organism", "biological_process", "cellular_component",
    "molecular_function", "diagnostic_procedure", "therapeutic_procedure",
    "phenotype", "symptom", "cell", "tissue", "mental_process",
    "physiologic_function", "laboratory_procedure"
  )

  # Validate dictionary_type based on source
  if (!is.null(dictionary_type)) {
    if (source == "local" && !(dictionary_type %in% local_dict_types)) {
      # Instead of stopping, switch to mesh source for expanded types
      message("Type '", dictionary_type, "' not supported for local source. Switching to MeSH source.")
      source <- "mesh"
    } else if (source == "mesh" && !(dictionary_type %in% mesh_dict_types)) {
      stop("For MeSH dictionaries, dictionary_type must be one of: ", paste(mesh_dict_types, collapse = ", "))
    } else if (source == "umls" && !(dictionary_type %in% umls_dict_types)) {
      stop("For UMLS dictionaries, dictionary_type must be one of: ", paste(umls_dict_types, collapse = ", "))
    }
  }

  # If custom path is provided, use it
  if (!is.null(custom_path)) {
    if (!file.exists(custom_path)) {
      stop("Custom dictionary file not found: ", custom_path)
    }

    ext <- tools::file_ext(custom_path)

    if (ext == "csv") {
      dict <- utils::read.csv(custom_path, stringsAsFactors = FALSE)
    } else if (ext == "rds") {
      dict <- readRDS(custom_path)
    } else {
      stop("Unsupported file format: ", ext, ". Supported formats: csv, rds")
    }

    # Check if the dictionary has the required columns
    required_cols <- c("term", "type")
    if (!all(required_cols %in% colnames(dict))) {
      stop("Dictionary must have columns: ", paste(required_cols, collapse = ", "))
    }

    # Apply sanitization if requested
    if (sanitize) {
      dict <- sanitize_dictionary(dict)
    }

    return(dict)
  }

  # Define semantic types based on dictionary_type if not provided
  if (is.null(semantic_type_filter)) {
    if (source == "umls") {
      semantic_type_filter <- get_umls_semantic_types(dictionary_type)
    }
  }

  # Fetch terms from different sources
  if (source == "mesh") {
    # Regular MeSH loading
    base_dict <- load_from_mesh(dictionary_type, n_terms, mesh_query)

    # If extended_mesh is requested, add terms from PubMed/MeSH searches
    if (extended_mesh && !is.null(mesh_queries)) {
      pubmed_dict <- load_mesh_terms_from_pubmed(
        mesh_queries = mesh_queries,
        max_results = n_terms / 2,  # Split the max results between direct and extended
        sanitize = FALSE  # We'll sanitize the combined dictionary later
      )

      if (nrow(pubmed_dict) > 0) {
        # Combine dictionaries
        base_dict <- rbind(base_dict, pubmed_dict)
        # Remove duplicates based on term
        base_dict <- base_dict[!duplicated(base_dict$term), ]
      }
    }

    # Apply sanitization if requested
    if (sanitize) {
      base_dict <- sanitize_dictionary(base_dict)
    }

    return(base_dict)

  } else if (source == "umls") {
    if (is.null(api_key)) {
      message("UMLS API key is required for fetching terms from UMLS. Switching to MeSH.")
      # Recursively call with mesh source
      return(load_dictionary(dictionary_type, custom_path = custom_path,
                             source = "mesh", n_terms = n_terms,
                             mesh_query = mesh_query, sanitize = sanitize))
    }

    umls_dict <- load_from_umls(dictionary_type, api_key, n_terms, semantic_type_filter)

    # Apply sanitization if requested
    if (sanitize) {
      umls_dict <- sanitize_dictionary(umls_dict)
    }

    return(umls_dict)
  } else {
    # Local source
    # Check if dictionary type is supported by local source
    if (!is.null(dictionary_type) && !(dictionary_type %in% local_dict_types)) {
      message("Dictionary type '", dictionary_type, "' not supported by local source. Trying MeSH source.")
      # Recursively call with mesh source
      return(load_dictionary(dictionary_type, custom_path = custom_path,
                             source = "mesh", n_terms = n_terms,
                             mesh_query = mesh_query, sanitize = sanitize))
    }

    # Try to load from package data
    pkg_path <- system.file("extdata", "dictionaries", package = "LBDiscover")

    if (pkg_path == "") {
      # If package is not installed, use example dictionaries for development
      message("Package not installed or dictionary not found. Using example dictionaries.")
      local_dict <- create_dummy_dictionary(dictionary_type)
    } else {
      # Load dictionary from package
      dict_path <- file.path(pkg_path, paste0(dictionary_type, "_dictionary.rds"))

      if (!file.exists(dict_path)) {
        message("Dictionary not found: ", dict_path, ". Using example dictionary.")
        local_dict <- create_dummy_dictionary(dictionary_type)
      } else {
        local_dict <- readRDS(dict_path)
      }
    }

    # Apply sanitization if requested
    if (sanitize) {
      local_dict <- sanitize_dictionary(local_dict)
    }

    return(local_dict)
  }
}

#' Load terms from MeSH using rentrez with improved error handling
#'
#' This function uses the rentrez package to retrieve terms from MeSH database.
#'
#' @param dictionary_type Type of dictionary to load (e.g., "disease", "drug", "gene").
#' @param n_terms Maximum number of terms to fetch.
#' @param query Additional query to filter MeSH terms.
#'
#' @return A data frame containing the MeSH terms.
#' @keywords internal
load_from_mesh <- function(dictionary_type, n_terms = 200, query = NULL) {
  # Check if rentrez is available
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    stop("The rentrez package is required for MeSH queries. Install it with: install.packages('rentrez')")
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("The xml2 package is required for parsing MeSH data. Install it with: install.packages('xml2')")
  }

  # Build query based on dictionary type - expanded for more categories
  mesh_query_map <- list(
    "disease" = "disease[MeSH]",
    "drug" = "pharmaceutical preparations[MeSH]",
    "gene" = "genes[MeSH]",
    "protein" = "proteins[MeSH]",
    "chemical" = "chemicals[MeSH]",
    "pathway" = "metabolic networks and pathways[MeSH]",
    "anatomy" = "anatomy[MeSH]",
    "organism" = "organisms[MeSH]",
    "biological_process" = "biological phenomena[MeSH]",
    "cell" = "cells[MeSH]",
    "tissue" = "tissues[MeSH]",
    "symptom" = "signs and symptoms[MeSH]",
    "diagnostic_procedure" = "diagnostic techniques and procedures[MeSH]",
    "therapeutic_procedure" = "therapeutics[MeSH]",
    "phenotype" = "phenotype[MeSH]",
    "molecular_function" = "molecular biology[MeSH]"
  )

  if (is.null(dictionary_type) || !dictionary_type %in% names(mesh_query_map)) {
    # If dictionary_type is not specified or not in our mapping, use a general search
    base_query <- "mesh[sb]"
  } else {
    base_query <- mesh_query_map[[dictionary_type]]
  }

  # Add user-provided query if available
  if (!is.null(query)) {
    # If the query already has MeSH qualifier, use it directly
    if (grepl("\\[MeSH\\]", query)) {
      search_query <- query
    } else {
      # Otherwise, treat it as a keyword to search within MeSH terms
      search_query <- paste0("(", query, "[MeSH Terms]) AND ", base_query)
    }
  } else {
    search_query <- base_query
  }

  message("Searching MeSH database for: ", search_query)

  # Try to fetch MeSH IDs with more robust error handling
  mesh_ids <- NULL

  # Try searching PubMed with MeSH terms first
  pubmed_search <- tryCatch({
    rentrez::entrez_search(
      db = "pubmed",
      term = search_query,
      use_history = TRUE,
      retmax = 0  # We only need the count
    )
  }, error = function(e) {
    message("PubMed search error: ", e$message)
    return(NULL)
  })

  if (!is.null(pubmed_search) && pubmed_search$count > 0) {
    message("Found ", pubmed_search$count, " PubMed records with matching MeSH terms")

    # Get most common MeSH terms from these results
    mesh_count <- tryCatch({
      rentrez::entrez_link(
        dbfrom = "pubmed",
        db = "mesh",
        cmd = "neighbor_score",
        query_key = pubmed_search$QueryKey,
        WebEnv = pubmed_search$WebEnv
      )
    }, error = function(e) {
      message("Error getting MeSH links: ", e$message)
      return(NULL)
    })

    if (!is.null(mesh_count) && !is.null(mesh_count$links) &&
        !is.null(mesh_count$links$pubmed_mesh) &&
        length(mesh_count$links$pubmed_mesh) > 0) {

      # Limit the number of MeSH IDs to retrieve
      max_ids <- min(n_terms, length(mesh_count$links$pubmed_mesh))
      mesh_ids <- mesh_count$links$pubmed_mesh[1:max_ids]
    }
  }

  # If we didn't get MeSH IDs from PubMed, try direct MeSH search
  if (is.null(mesh_ids)) {
    message("Trying direct MeSH database search...")

    # For direct MeSH search, simplify the query
    if (!is.null(query)) {
      direct_query <- query
    } else if (!is.null(dictionary_type) && dictionary_type %in% names(mesh_query_map)) {
      # Extract keyword from MeSH query (removing the [MeSH] qualifier)
      direct_query <- gsub("\\[MeSH\\]", "", mesh_query_map[[dictionary_type]])
    } else {
      direct_query <- ""  # Empty query will return all MeSH terms
    }

    # Use shorter term limit for direct MeSH search to avoid large results
    limited_n_terms <- min(n_terms, 100)

    mesh_search <- tryCatch({
      rentrez::entrez_search(
        db = "mesh",
        term = direct_query,
        retmax = limited_n_terms
      )
    }, error = function(e) {
      message("MeSH search error: ", e$message)
      return(NULL)
    })

    if (!is.null(mesh_search) && length(mesh_search$ids) > 0) {
      mesh_ids <- mesh_search$ids
    } else {
      warning("No results found in MeSH for query: ", direct_query)
      return(create_dummy_dictionary(dictionary_type))
    }
  }

  # If we still don't have MeSH IDs, return dummy dictionary
  if (is.null(mesh_ids) || length(mesh_ids) == 0) {
    warning("Could not retrieve any MeSH IDs")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Fetch MeSH records in smaller batches to avoid memory issues
  batch_size <- 20
  num_batches <- ceiling(length(mesh_ids) / batch_size)

  # Initialize combined dictionary
  combined_dict <- data.frame(
    term = character(),
    id = character(),
    type = character(),
    source = character(),
    stringsAsFactors = FALSE
  )

  for (batch in 1:num_batches) {
    message("Processing batch ", batch, " of ", num_batches)

    # Get batch of IDs
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(mesh_ids))
    batch_ids <- mesh_ids[start_idx:end_idx]

    # Fetch MeSH record data for this batch
    mesh_records <- tryCatch({
      rentrez::entrez_fetch(
        db = "mesh",
        id = batch_ids,
        rettype = "full",
        retmode = "xml"
      )
    }, error = function(e) {
      message("Error fetching MeSH records for batch ", batch, ": ", e$message)
      return(NULL)
    })

    if (!is.null(mesh_records)) {
      # Process the batch
      batch_dict <- process_mesh_xml(mesh_records, dictionary_type)

      # Add to combined dictionary
      if (nrow(batch_dict) > 0 && !inherits(batch_dict, "try-error")) {
        combined_dict <- rbind(combined_dict, batch_dict)
      }
    }
  }

  # Check if we got any terms
  if (nrow(combined_dict) == 0) {
    warning("No terms extracted from MeSH records")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Remove duplicates
  combined_dict <- combined_dict[!duplicated(combined_dict$term), ]
  message("Retrieved ", nrow(combined_dict), " unique terms from MeSH")

  return(combined_dict)
}
#' Process MeSH XML data with improved error handling
#'
#' Helper function to process MeSH XML data and extract terms
#'
#' @param mesh_records XML data from MeSH database
#' @param dictionary_type Type of dictionary
#'
#' @return A data frame with MeSH terms
#' @keywords internal
process_mesh_xml <- function(mesh_records, dictionary_type) {
  # Check if the input is empty or invalid
  if (is.null(mesh_records) || length(mesh_records) == 0) {
    warning("Empty MeSH records provided")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Check for excessively large input to avoid memory issues
  if (nchar(mesh_records) > 1e7) {  # 10MB limit
    warning("MeSH records too large to process at once, using chunked processing")
    return(process_mesh_chunks(mesh_records, dictionary_type))
  }

  # Try to parse the XML with improved error handling
  tryCatch({
    # First, check if the data looks like valid XML
    if (!grepl("<\\?xml|<DescriptorRecord|<Concept", substr(mesh_records, 1, 1000))) {
      # If not, it might be text data instead of XML - try to extract useful information
      return(extract_mesh_from_text(mesh_records, dictionary_type))
    }

    # Try to parse as XML
    mesh_xml <- xml2::read_xml(mesh_records)

    # Extract terms and IDs - handle both DescriptorRecord and Concept formats
    mesh_nodes <- xml2::xml_find_all(mesh_xml, "//DescriptorRecord|//Concept")

    if (length(mesh_nodes) == 0) {
      warning("No MeSH descriptor records found in XML")
      return(create_dummy_dictionary(dictionary_type))
    }

    # Initialize vectors to store extracted data
    terms <- character()
    ids <- character()

    # Process each node
    for (node in mesh_nodes) {
      node_name <- xml2::xml_name(node)

      if (node_name == "DescriptorRecord") {
        # Get descriptor UI (ID)
        descriptor_ui_node <- xml2::xml_find_first(node, ".//DescriptorUI")
        if (!is.na(descriptor_ui_node)) {
          descriptor_ui <- xml2::xml_text(descriptor_ui_node)

          # Get descriptor name (term)
          descriptor_name_node <- xml2::xml_find_first(node, ".//DescriptorName/String")
          if (!is.na(descriptor_name_node)) {
            descriptor_name <- xml2::xml_text(descriptor_name_node)

            # Add to vectors
            terms <- c(terms, descriptor_name)
            ids <- c(ids, descriptor_ui)

            # Also get entry terms (synonyms)
            entry_terms <- xml2::xml_find_all(node, ".//ConceptList//Term/String")

            if (length(entry_terms) > 0) {
              entry_terms_text <- xml2::xml_text(entry_terms)
              terms <- c(terms, entry_terms_text)
              ids <- c(ids, rep(descriptor_ui, length(entry_terms_text)))
            }
          }
        }
      } else if (node_name == "Concept") {
        # For Concept records
        concept_ui_node <- xml2::xml_find_first(node, ".//ConceptUI")
        if (!is.na(concept_ui_node)) {
          concept_ui <- xml2::xml_text(concept_ui_node)

          concept_name_node <- xml2::xml_find_first(node, ".//ConceptName/String")
          if (!is.na(concept_name_node)) {
            concept_name <- xml2::xml_text(concept_name_node)

            # Add to vectors
            terms <- c(terms, concept_name)
            ids <- c(ids, concept_ui)

            # Also get terms
            term_nodes <- xml2::xml_find_all(node, ".//TermList/Term/String")

            if (length(term_nodes) > 0) {
              term_texts <- xml2::xml_text(term_nodes)
              terms <- c(terms, term_texts)
              ids <- c(ids, rep(concept_ui, length(term_texts)))
            }
          }
        }
      }
    }

    if (length(terms) == 0) {
      warning("No terms extracted from MeSH XML")
      return(create_dummy_dictionary(dictionary_type))
    }

    # Create data frame
    dict <- data.frame(
      term = terms,
      id = ids,
      type = rep(dictionary_type, length(terms)),
      source = rep("mesh", length(terms)),
      stringsAsFactors = FALSE
    )

    # Remove duplicates
    dict <- dict[!duplicated(dict$term), ]

    message("Retrieved ", nrow(dict), " unique terms from MeSH")

    return(dict)

  }, error = function(e) {
    warning("Error parsing MeSH XML: ", e$message)
    return(create_dummy_dictionary(dictionary_type))
  })
}

#' Process MeSH data in chunks to avoid memory issues
#'
#' @param mesh_records Large MeSH records data
#' @param dictionary_type Type of dictionary
#'
#' @return A data frame with MeSH terms
#' @keywords internal
process_mesh_chunks <- function(mesh_records, dictionary_type) {
  # Extract records by splitting on record boundaries
  chunk_size <- 1e6  # 1MB chunks
  num_chunks <- ceiling(nchar(mesh_records) / chunk_size)

  # Initialize data frame for results
  combined_dict <- data.frame(
    term = character(),
    id = character(),
    type = character(),
    source = character(),
    stringsAsFactors = FALSE
  )

  # Process in chunks
  for (i in 1:num_chunks) {
    start_pos <- (i-1) * chunk_size + 1
    end_pos <- min(i * chunk_size, nchar(mesh_records))

    # Extract chunk
    chunk <- substr(mesh_records, start_pos, end_pos)

    # Find complete records in this chunk
    # Look for record start/end patterns
    start_tags <- gregexpr("<DescriptorRecord>|<Concept>", chunk)[[1]]
    end_tags <- gregexpr("</DescriptorRecord>|</Concept>", chunk)[[1]]

    # If no complete records in this chunk, combine with next chunk
    if (length(start_tags) == 0 || length(end_tags) == 0 ||
        start_tags[1] == -1 || end_tags[1] == -1) {
      next
    }

    # Process each complete record
    for (j in 1:length(start_tags)) {
      # Find matching end tag
      matched_end_idx <- which(end_tags > start_tags[j])[1]

      if (!is.na(matched_end_idx)) {
        record <- substr(chunk, start_tags[j], end_tags[matched_end_idx] +
                           attr(end_tags, "match.length")[matched_end_idx] - 1)

        # Try to parse this record
        tryCatch({
          # Wrap in root element for XML parsing
          record_xml <- paste0("<root>", record, "</root>")
          dict_chunk <- process_mesh_xml(record_xml, dictionary_type)

          # Add to combined dictionary
          if (nrow(dict_chunk) > 0 && !inherits(dict_chunk, "try-error")) {
            combined_dict <- rbind(combined_dict, dict_chunk)
          }
        }, error = function(e) {
          # Skip this record on error
        })
      }
    }
  }

  # Remove duplicates from combined dictionary
  if (nrow(combined_dict) > 0) {
    combined_dict <- combined_dict[!duplicated(combined_dict$term), ]
    message("Retrieved ", nrow(combined_dict), " unique terms from chunked MeSH processing")
    return(combined_dict)
  } else {
    return(create_dummy_dictionary(dictionary_type))
  }
}

#' Extract MeSH terms from text format instead of XML
#'
#' @param mesh_text Text containing MeSH data in non-XML format
#' @param dictionary_type Type of dictionary
#'
#' @return A data frame with MeSH terms
#' @keywords internal
extract_mesh_from_text <- function(mesh_text, dictionary_type) {
  # Initialize vectors to store terms and IDs
  terms <- character()
  ids <- character()

  # Try to identify terms and their IDs from the text
  # Look for patterns like "1: Term Name" or "Tree Number(s): D25.651"

  # Extract terms - look for patterns like "1: Term Name" or just capitalized phrases
  term_matches <- gregexpr("\\d+:\\s*([A-Z][^,\\n\\r]+)|(^[A-Z][^,\\n\\r]+)", mesh_text, perl = TRUE)
  if (term_matches[[1]][1] != -1) {
    term_starts <- term_matches[[1]]
    term_lengths <- attr(term_matches[[1]], "match.length")

    for (i in 1:length(term_starts)) {
      term_text <- substr(mesh_text, term_starts[i], term_starts[i] + term_lengths[i] - 1)

      # Clean up the term
      term_text <- gsub("^\\d+:\\s*", "", term_text)
      term_text <- trimws(term_text)

      # Add to our vectors
      if (nchar(term_text) > 0) {
        terms <- c(terms, term_text)
        # Generate a placeholder ID if we can't extract it
        ids <- c(ids, paste0("MESH_", i))
      }
    }
  }

  # Look for Entry Terms section
  entry_term_section <- regexpr("Entry Terms?:([^\\n]+)\\n", mesh_text)
  if (entry_term_section[1] != -1) {
    entry_text <- substr(mesh_text,
                         entry_term_section[1],
                         entry_term_section[1] + attr(entry_term_section, "match.length")[1] - 1)

    # Extract entry terms
    entry_terms <- strsplit(gsub("Entry Terms?:", "", entry_text), ",")[[1]]
    entry_terms <- trimws(entry_terms)

    # Add to our vectors
    if (length(entry_terms) > 0) {
      terms <- c(terms, entry_terms)
      # Use the same ID for all entry terms from this section
      ids <- c(ids, rep(paste0("MESH_ENTRY_", length(ids) + 1), length(entry_terms)))
    }
  }

  # Extract Tree Numbers - these might help us generate better IDs
  tree_matches <- regexpr("Tree Number\\(s\\):\\s*([^\\n]+)", mesh_text)
  tree_numbers <- character()

  if (tree_matches[1] != -1) {
    tree_text <- substr(mesh_text,
                        tree_matches[1],
                        tree_matches[1] + attr(tree_matches, "match.length")[1] - 1)

    # Extract tree numbers
    tree_numbers <- strsplit(gsub("Tree Number\\(s\\):\\s*", "", tree_text), ",")[[1]]
    tree_numbers <- trimws(tree_numbers)

    # If we have tree numbers, use them to improve our IDs
    if (length(tree_numbers) > 0 && length(ids) > 0) {
      # Update the first ID with a tree number
      ids[1] <- gsub("\\.", "", tree_numbers[1])
    }
  }

  # If we failed to extract terms, return dummy dictionary
  if (length(terms) == 0) {
    warning("No terms extracted from MeSH text")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Create data frame
  dict <- data.frame(
    term = terms,
    id = ids,
    type = rep(dictionary_type, length(terms)),
    source = rep("mesh_text", length(terms)),
    stringsAsFactors = FALSE
  )

  # Remove duplicates
  dict <- dict[!duplicated(dict$term), ]

  message("Extracted ", nrow(dict), " unique terms from MeSH text format")

  return(dict)
}

#' Load terms from UMLS API
#'
#' This function retrieves terms from UMLS using the REST API.
#'
#' @param dictionary_type Type of dictionary to load (e.g., "disease", "drug", "gene").
#' @param api_key UMLS API key for authentication.
#' @param n_terms Maximum number of terms to fetch.
#' @param semantic_types Vector of semantic type identifiers to filter by.
#'
#' @return A data frame containing the UMLS terms.
#' @keywords internal
load_from_umls <- function(dictionary_type, api_key, n_terms = 200, semantic_types = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The httr package is required for UMLS API calls. Install it with: install.packages('httr')")
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("The jsonlite package is required for parsing UMLS API responses. Install it with: install.packages('jsonlite')")
  }

  if (is.null(semantic_types)) {
    warning("No semantic types specified for UMLS query. Using default types for: ", dictionary_type)

    semantic_types <- switch(dictionary_type,
                             "disease" = c("T047", "T048", "T019", "T046"), # Disease and Syndrome, Mental Disorder, Congenital Abnormality, Pathologic Function
                             "drug" = c("T116", "T121", "T195", "T200"), # Amino Acid, Lipid, Antibiotic, Clinical Drug
                             "gene" = c("T028", "T087", "T123"), # Gene, Amino Acid Sequence, Nucleotide Sequence
                             stop("Unsupported dictionary type for UMLS: ", dictionary_type))
  }

  message("Authenticating with UMLS...")

  # Authenticate with UMLS to get TGT
  tgt_url <- authenticate_umls(api_key)

  if (is.null(tgt_url)) {
    warning("Failed to authenticate with UMLS. Using dummy dictionary instead.")
    return(create_dummy_dictionary(dictionary_type))
  }

  message("Fetching terms from UMLS for semantic types: ", paste(semantic_types, collapse = ", "))

  # Instead of searching by semantic type directly (which seems to be causing issues),
  # we'll search using a keyword based on the dictionary type and then filter by semantic type
  search_string <- switch(dictionary_type,
                          "disease" = "disease",
                          "drug" = "drug",
                          "gene" = "gene",
                          "")

  # Get service ticket for this request
  service_ticket <- get_service_ticket(tgt_url)

  if (is.null(service_ticket)) {
    warning("Failed to get service ticket")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Perform search with keyword
  search_url <- "https://uts-ws.nlm.nih.gov/rest/search/current"

  # Search query parameters
  query_params <- list(
    ticket = service_ticket,
    string = search_string,
    sabs = "SNOMEDCT_US,ICD10CM,MSH", # Limit to these vocabularies
    searchType = "words",
    returnIdType = "concept",
    pageSize = n_terms
  )

  response <- httr::GET(url = search_url, query = query_params)

  if (httr::status_code(response) != 200) {
    warning("Failed to search UMLS: ", httr::http_status(response)$message)
    return(create_dummy_dictionary(dictionary_type))
  }

  # Parse response
  content <- httr::content(response, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(content)

  # Check if results exist
  if (length(result$result$results) == 0) {
    warning("No results found for search: ", search_string)
    return(create_dummy_dictionary(dictionary_type))
  }

  # Get CUIs from the search results
  cuis <- result$result$results$ui

  # Now for each CUI, get its semantic types and filter
  all_terms <- list()

  for (cui in cuis) {
    # Get a fresh service ticket for each request to avoid expiration
    service_ticket <- get_service_ticket(tgt_url)

    if (is.null(service_ticket)) {
      next
    }

    # Get concept info to check its semantic type
    concept_url <- paste0("https://uts-ws.nlm.nih.gov/rest/content/current/CUI/", cui)

    response <- httr::GET(
      url = concept_url,
      query = list(ticket = service_ticket)
    )

    if (httr::status_code(response) != 200) {
      next
    }

    content <- httr::content(response, "text", encoding = "UTF-8")
    concept_info <- jsonlite::fromJSON(content)

    # Get another service ticket for semantic types
    service_ticket <- get_service_ticket(tgt_url)

    if (is.null(service_ticket)) {
      next
    }

    # Get semantic types for this concept
    sem_types_url <- concept_info$result$semanticTypes

    # Check if semanticTypes is a URL or just a nested JSON element
    if (is.null(sem_types_url) || !is.character(sem_types_url) || length(sem_types_url) == 0) {
      # Try to extract semantic types directly from concept_info
      if (!is.null(concept_info$result$semanticTypes) && is.list(concept_info$result$semanticTypes)) {
        sem_types_info <- list(result = concept_info$result$semanticTypes)
      } else {
        next  # Skip if we can't find semantic types
      }
    } else {
      # Ensure we have a single valid URL
      if (length(sem_types_url) > 1) {
        sem_types_url <- sem_types_url[1]  # Take just the first URL if multiple are returned
      }

      # Make sure it's a valid URL
      if (!grepl("^https?://", sem_types_url)) {
        next  # Skip if not a valid URL
      }

      # Get semantic types data
      response <- httr::GET(
        url = sem_types_url,
        query = list(ticket = service_ticket, pageSize = 100)
      )

      if (httr::status_code(response) != 200) {
        next
      }

      content <- httr::content(response, "text", encoding = "UTF-8")
      sem_types_info <- jsonlite::fromJSON(content)
    }

    # This block has been moved into the conditional above

    # Check if this concept has one of our target semantic types
    if (length(sem_types_info$result) == 0) {
      next
    }

    # Extract semantic type IDs, handling different response formats
    concept_sem_types <- tryCatch({
      if (is.data.frame(sem_types_info$result)) {
        # If result is a data frame
        if ("uri" %in% names(sem_types_info$result)) {
          # Extract TUIs from URIs
          uris <- sem_types_info$result$uri
          sapply(uris, function(uri) {
            tui_match <- regmatches(uri, regexpr("T[0-9]+", uri))
            if (length(tui_match) > 0) return(tui_match[1])
            else return(NA)
          })
        } else if ("semanticType" %in% names(sem_types_info$result)) {
          sem_types_info$result$semanticType
        } else {
          NA
        }
      } else if (is.list(sem_types_info$result)) {
        # If result is a list of semantic types
        sem_types <- vector("character")

        # Process each element in the result list
        for (i in seq_along(sem_types_info$result)) {
          st <- sem_types_info$result[[i]]

          if (is.list(st)) {
            # Handle list element
            if (!is.null(st$semanticType)) {
              sem_types <- c(sem_types, st$semanticType)
            } else if (!is.null(st$uri)) {
              # Extract TUI from URI if available
              tui_match <- regmatches(st$uri, regexpr("T[0-9]+", st$uri))
              if (length(tui_match) > 0) {
                sem_types <- c(sem_types, tui_match[1])
              }
            }
          } else if (is.character(st)) {
            # Handle character element (might be a direct TUI)
            if (grepl("^T[0-9]+$", st)) {
              sem_types <- c(sem_types, st)
            }
          }
        }

        if (length(sem_types) > 0) {
          sem_types
        } else {
          NA
        }
      } else {
        NA
      }
    }, error = function(e) {
      message("Error extracting semantic types: ", e$message)
      return(NA)
    })

    concept_sem_types <- concept_sem_types[!is.na(concept_sem_types)]

    # Check if any of the concept's semantic types match our filter
    if (any(concept_sem_types %in% semantic_types)) {
      matching_sem_types <- concept_sem_types[concept_sem_types %in% semantic_types]

      for (sem_type in matching_sem_types) {
        # Store concept with its matching semantic type
        if (is.null(all_terms[[sem_type]])) {
          all_terms[[sem_type]] <- list()
        }

        all_terms[[sem_type]][[length(all_terms[[sem_type]]) + 1]] <- list(
          ui = concept_info$result$ui,
          name = concept_info$result$name,
          semantic_type = sem_type
        )
      }
    }
  }

  # Process all collected terms
  if (length(all_terms) == 0) {
    warning("No terms found with matching semantic types. Using dummy dictionary instead.")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Create data frame with all terms
  terms_list <- list()

  for (sem_type in names(all_terms)) {
    sem_type_terms <- all_terms[[sem_type]]

    if (length(sem_type_terms) > 0) {
      df <- data.frame(
        term = sapply(sem_type_terms, function(x) x$name),
        id = sapply(sem_type_terms, function(x) x$ui),
        type = rep(dictionary_type, length(sem_type_terms)),
        source = rep("umls", length(sem_type_terms)),
        semantic_type = rep(sem_type, length(sem_type_terms)),
        stringsAsFactors = FALSE
      )

      terms_list[[sem_type]] <- df
    }
  }

  if (length(terms_list) == 0) {
    warning("Failed to process UMLS terms. Using dummy dictionary instead.")
    return(create_dummy_dictionary(dictionary_type))
  }

  # Combine all data frames
  combined_dict <- do.call(rbind, terms_list)

  # Remove duplicates based on term
  deduped_dict <- combined_dict[!duplicated(combined_dict$term), ]

  message("Retrieved ", nrow(deduped_dict), " unique terms from UMLS")

  return(deduped_dict)
}

#' Authenticate with UMLS
#'
#' This function authenticates with UMLS and returns a TGT URL.
#'
#' @param api_key UMLS API key
#'
#' @return Character string with TGT URL or NULL if authentication fails
#' @keywords internal
authenticate_umls <- function(api_key) {
  # Base URL for UTS REST API authentication
  auth_url <- "https://utslogin.nlm.nih.gov/cas/v1/api-key"

  tryCatch({
    # Get Ticket Granting Ticket (TGT)
    response <- httr::POST(
      url = auth_url,
      body = list(apikey = api_key),
      encode = "form"
    )

    if (httr::status_code(response) != 201) {
      warning("Failed to authenticate with UMLS: ", httr::content(response, "text"))
      return(NULL)
    }

    # Extract TGT URL from response
    tgt_url <- httr::headers(response)$location

    if (is.null(tgt_url)) {
      warning("No TGT URL found in response headers")
      return(NULL)
    }

    return(tgt_url)

  }, error = function(e) {
    warning("Error authenticating with UMLS: ", e$message)
    return(NULL)
  })
}

#' Get a service ticket from a TGT URL
#'
#' This function gets a service ticket for a specific service using the TGT URL.
#'
#' @param tgt_url Ticket Granting Ticket URL
#'
#' @return Character string with service ticket or NULL if it fails
#' @keywords internal
get_service_ticket <- function(tgt_url) {
  tryCatch({
    # Get Service Ticket
    service_ticket_url <- tgt_url
    response <- httr::POST(
      url = service_ticket_url,
      body = list(service = "http://umlsks.nlm.nih.gov"),
      encode = "form"
    )

    if (httr::status_code(response) != 200) {
      warning("Failed to get service ticket: ", httr::content(response, "text"))
      return(NULL)
    }

    service_ticket <- httr::content(response, "text")
    return(service_ticket)

  }, error = function(e) {
    warning("Error getting service ticket: ", e$message)
    return(NULL)
  })
}

#' Validate a UMLS API key
#'
#' This function validates a UMLS API key using the validation endpoint.
#'
#' @param api_key UMLS API key to validate
#' @param validator_api_key Your application's UMLS API key (for third-party validation)
#'
#' @return Logical indicating if the API key is valid
#' @export
#'
#' @examples
#' \dontrun{
#' is_valid <- validate_umls_key("user_api_key")
#' }
validate_umls_key <- function(api_key, validator_api_key = NULL) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The httr package is required for UMLS API calls. Install it with: install.packages('httr')")
  }

  if (is.null(validator_api_key)) {
    # Use the API key itself for validation by trying to authenticate
    result <- !is.null(authenticate_umls(api_key))
    return(result)
  } else {
    # Use third-party validation endpoint
    validate_url <- "https://utslogin.nlm.nih.gov/validateUser"

    response <- httr::GET(
      url = validate_url,
      query = list(
        validatorApiKey = validator_api_key,
        apiKey = api_key
      )
    )

    # Check the response
    if (httr::status_code(response) != 200) {
      warning("Failed to validate UMLS API key: ", httr::content(response, "text"))
      return(FALSE)
    }

    # Parse the response content
    content <- httr::content(response, "text")

    # The endpoint returns "true" or "false"
    return(tolower(content) == "true")
  }
}

#' Helper function to create dummy dictionaries
#' @keywords internal
create_dummy_dictionary <- function(dictionary_type) {
  message("Creating dummy dictionary for ", dictionary_type)

  if (dictionary_type == "disease") {
    dict <- data.frame(
      term = c("covid-19", "sars-cov-2", "coronavirus", "pneumonia", "fever",
               "cough", "dyspnea", "migraine", "headache", "hypertension",
               "diabetes", "asthma", "cancer", "obesity", "myocarditis",
               "thrombosis", "stroke", "anosmia", "fatigue", "hypoxemia"),
      id = paste0("D", sprintf("%06d", 1:20)),
      type = rep("disease", 20),
      source = rep("dummy", 20),
      stringsAsFactors = FALSE
    )
  } else if (dictionary_type == "drug") {
    dict <- data.frame(
      term = c("remdesivir", "dexamethasone", "hydroxychloroquine", "azithromycin",
               "favipiravir", "tocilizumab", "heparin", "aspirin", "ibuprofen",
               "paracetamol", "acetaminophen", "statins", "metformin", "insulin",
               "warfarin", "enoxaparin", "ivermectin", "lopinavir", "ritonavir", "oseltamivir"),
      id = paste0("D", sprintf("%06d", 21:40)),
      type = rep("drug", 20),
      source = rep("dummy", 20),
      stringsAsFactors = FALSE
    )
  } else if (dictionary_type == "gene") {
    dict <- data.frame(
      term = c("ACE2", "TMPRSS2", "IL6", "IL1B", "TNF", "IFNG", "CXCL10",
               "CCL2", "TNFRSF1A", "NFKB1", "TLR3", "TLR7", "MYD88", "IRF3",
               "IRF7", "STAT1", "STAT3", "JAK1", "JAK2", "MAPK1"),
      id = paste0("G", sprintf("%06d", 1:20)),
      type = rep("gene", 20),
      source = rep("dummy", 20),
      stringsAsFactors = FALSE
    )
  } else {
    dict <- data.frame(
      term = character(0),
      id = character(0),
      type = character(0),
      source = character(0),
      stringsAsFactors = FALSE
    )
  }

  return(dict)
}

# Helper function to search UMLS by semantic type
search_umls_by_semantic_type <- function(tgt_url, semantic_type, max_results = 100) {
  # Get Service Ticket
  service_ticket_url <- paste0(tgt_url, "?service=http://umlsks.nlm.nih.gov")
  response <- httr::POST(service_ticket_url)
  service_ticket <- httr::content(response, "text")

  # Base URL for UMLS REST API
  base_url <- "https://uts-ws.nlm.nih.gov/rest"

  # Search by semantic type
  search_url <- paste0(base_url, "/search/current")
  response <- httr::GET(
    url = search_url,
    query = list(
      string = "",
      sabs = "SNOMEDCT_US,ICD10CM,MSH",
      searchType = "exact",
      inputType = "sourceUi",
      rootSource = "SEMANTIC_TYPE",
      pageSize = max_results,
      ticket = service_ticket
    )
  )

  if (httr::status_code(response) != 200) {
    warning("Failed to search UMLS: ", httr::content(response, "text"))
    return(list())
  }

  # Extract results
  search_results <- httr::content(response)

  # If no results, return empty list
  if (length(search_results$result$results) == 0) {
    return(list())
  }

  # Process results
  terms <- list()
  for (result in search_results$result$results) {
    terms[[length(terms) + 1]] <- list(
      ui = result$ui,
      name = result$name,
      semantic_type = semantic_type
    )
  }

  return(terms)
}

#' Detect language of text
#'
#' This function attempts to detect the language of a text string.
#' It implements a simple n-gram based approach that doesn't require
#' additional packages.
#'
#' @param text Text string to analyze
#' @param sample_size Maximum number of characters to sample for language detection
#'
#' @return Character string containing the ISO 639-1 language code
#' @export
#'
#' @examples
#' \dontrun{
#' lang <- detect_lang("This is English text")
#' # Returns "en"
#' }
detect_lang <- function(text, sample_size = 1000) {
  # Simple language detection based on common words in different languages
  # This is a basic implementation - more sophisticated methods would use proper language models

  # Sample the text to avoid processing very large texts
  if (nchar(text) > sample_size) {
    text <- substr(text, 1, sample_size)
  }

  # Convert to lowercase
  text <- tolower(text)

  # Define common words for major languages
  language_profiles <- list(
    en = c("the", "and", "in", "of", "to", "a", "is", "that", "for", "it", "with", "as", "was", "on"),
    es = c("el", "la", "de", "que", "y", "en", "un", "por", "con", "no", "una", "su", "para", "es"),
    fr = c("le", "la", "de", "et", "les", "des", "en", "un", "du", "une", "que", "est", "dans", "qui"),
    de = c("der", "die", "und", "in", "den", "von", "zu", "das", "mit", "sich", "des", "auf", "f\u00fcr", "ist"),
    it = c("il", "di", "che", "e", "la", "in", "un", "a", "per", "\u00e8", "una", "con", "sono", "su"),
    pt = c("de", "a", "o", "que", "e", "do", "da", "em", "um", "para", "com", "n\u00e3o", "uma", "os"),
    nl = c("de", "en", "van", "het", "een", "in", "is", "dat", "op", "te", "zijn", "met", "voor", "niet"),
    ru = c("\u0438", "\u0432", "\u043d\u0435", "\u043d\u0430", "\u044f", "\u0447\u0442\u043e", "\u0441", "\u043f\u043e", "\u044d\u0442\u043e", "\u043a", "\u0430", "\u043d\u043e", "\u043a\u0430\u043a", "\u0438\u0437")
  )

  # Count occurrences of words in each language profile
  scores <- sapply(language_profiles, function(profile) {
    # Extract words from text
    words <- unlist(strsplit(gsub("[^a-z\u0430-\u044f ]", " ", text), "\\s+"))
    words <- words[words != ""]

    # Count matches
    matches <- sum(words %in% profile)
    return(matches / length(profile))
  })

  # Return the language with the highest score
  if (max(scores) > 0.1) {  # Threshold to avoid misidentification of very short texts
    return(names(which.max(scores)))
  } else {
    return("unknown")
  }
}

#' Extract n-grams from text
#'
#' This function extracts n-grams (sequences of n words) from text.
#'
#' @param text Character vector of texts to process
#' @param n Integer specifying the n-gram size (1 for unigrams, 2 for bigrams, etc.)
#' @param min_freq Minimum frequency to include an n-gram
#'
#' @return A data frame containing n-grams and their frequencies
#' @export
#'
#' @examples
#' \dontrun{
#' bigrams <- extract_ngrams(abstracts, n = 2)
#' }
extract_ngrams <- function(text, n = 1, min_freq = 2) {
  # Ensure text is character
  text <- as.character(text)

  # Remove missing values
  text <- text[!is.na(text)]

  # Function to extract n-grams from a single text
  extract_text_ngrams <- function(single_text, n) {
    # Remove non-alphanumeric characters and convert to lowercase
    clean_text <- tolower(gsub("[^a-zA-Z0-9 ]", " ", single_text))

    # Split into words
    words <- unlist(strsplit(clean_text, "\\s+"))
    words <- words[words != ""]

    # If fewer words than n, return empty result
    if (length(words) < n) {
      return(character(0))
    }

    # Create n-grams
    ngrams <- character(length(words) - n + 1)
    for (i in 1:(length(words) - n + 1)) {
      ngrams[i] <- paste(words[i:(i + n - 1)], collapse = " ")
    }

    return(ngrams)
  }

  # Apply to all texts and combine results
  all_ngrams <- unlist(lapply(text, extract_text_ngrams, n = n))

  # Count frequencies
  ngram_freq <- table(all_ngrams)

  # Convert to data frame and filter by minimum frequency
  result <- data.frame(
    ngram = names(ngram_freq),
    frequency = as.numeric(ngram_freq),
    stringsAsFactors = FALSE
  )

  # Filter by minimum frequency
  result <- result[result$frequency >= min_freq, ]

  # Sort by frequency
  result <- result[order(result$frequency, decreasing = TRUE), ]

  return(result)
}

#' Perform named entity recognition on text
#'
#' This function performs a simple dictionary-based named entity recognition.
#' For more advanced NER, consider using external tools via reticulate.
#'
#' @param text Character vector of texts to process
#' @param entity_types Character vector of entity types to recognize
#' @param custom_dictionaries List of custom dictionaries (named by entity type)
#'
#' @return A data frame containing found entities, their types, and positions
#' @export
#'
#' @examples
#' \dontrun{
#' entities <- extract_ner(abstracts,
#'                                   entity_types = c("disease", "drug", "gene"))
#' }
extract_ner <- function(text, entity_types = c("disease", "drug", "gene"),
                        custom_dictionaries = NULL) {
  # Load dictionaries for requested entity types
  dictionaries <- list()

  for (type in entity_types) {
    if (!is.null(custom_dictionaries) && type %in% names(custom_dictionaries)) {
      dictionaries[[type]] <- custom_dictionaries[[type]]
    } else {
      # Try to load built-in dictionary
      dict <- try(load_dictionary(type), silent = TRUE)
      if (!inherits(dict, "try-error")) {
        dictionaries[[type]] <- dict
      } else {
        warning("Dictionary for entity type '", type, "' not found")
      }
    }
  }

  # Check if any dictionaries were loaded
  if (length(dictionaries) == 0) {
    stop("No valid dictionaries found for the requested entity types")
  }

  # Initialize results
  results <- data.frame(
    text_id = integer(),
    entity = character(),
    entity_type = character(),
    start_pos = integer(),
    end_pos = integer(),
    stringsAsFactors = FALSE
  )

  # Process each text
  for (i in seq_along(text)) {
    if (i %% 10 == 0) {
      message("Processing text ", i, " of ", length(text))
    }

    current_text <- text[i]

    # Skip missing values
    if (is.na(current_text) || current_text == "") {
      next
    }

    # Scan for entities from each dictionary
    for (type in names(dictionaries)) {
      dict <- dictionaries[[type]]

      for (j in 1:nrow(dict)) {
        term <- dict$term[j]

        # Create a regex pattern with word boundaries
        pattern <- paste0("\\b", term, "\\b")

        # Find all matches
        matches <- gregexpr(pattern, current_text, ignore.case = TRUE)

        # Extract match positions
        if (matches[[1]][1] != -1) {  # -1 indicates no match
          start_positions <- matches[[1]]
          end_positions <- start_positions + attr(matches[[1]], "match.length") - 1

          # Add to results
          for (k in seq_along(start_positions)) {
            results <- rbind(results, data.frame(
              text_id = i,
              entity = term,
              entity_type = type,
              start_pos = start_positions[k],
              end_pos = end_positions[k],
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }

  # Return empty data frame if no entities found
  if (nrow(results) == 0) {
    message("No entities found")
    return(data.frame(
      text_id = integer(),
      entity = character(),
      entity_type = character(),
      start_pos = integer(),
      end_pos = integer(),
      stringsAsFactors = FALSE
    ))
  }

  return(results)
}

#' Perform sentence segmentation on text
#'
#' This function splits text into sentences.
#'
#' @param text Character vector of texts to process
#'
#' @return A list where each element contains a character vector of sentences
#' @export
#'
#' @examples
#' \dontrun{
#' sentences <- segment_sentences(abstracts)
#' }
segment_sentences <- function(text) {
  # Ensure text is character
  text <- as.character(text)

  # Remove missing values
  text <- text[!is.na(text)]

  # Function to split a single text into sentences
  split_text <- function(single_text) {
    # Handle common abbreviations to avoid splitting at them
    abbrevs <- c("Dr.", "Mr.", "Mrs.", "Ms.", "Prof.", "Inc.", "Ltd.", "Co.", "Fig.", "e.g.", "i.e.", "etc.")

    # Temporarily replace periods in abbreviations
    for (abbr in abbrevs) {
      single_text <- gsub(abbr, gsub("\\.", "<<DOT>>", abbr), single_text)
    }

    # Split on sentence boundaries
    sentences <- unlist(strsplit(single_text, "(?<=[.!?])\\s+", perl = TRUE))

    # Restore periods in abbreviations
    sentences <- gsub("<<DOT>>", ".", sentences)

    # Remove empty sentences
    sentences <- sentences[sentences != ""]

    return(sentences)
  }

  # Apply to all texts
  result <- lapply(text, split_text)

  return(result)
}

#' Map terms to biomedical ontologies
#'
#' This function maps terms to standard biomedical ontologies like MeSH or UMLS.
#'
#' @param terms Character vector of terms to map
#' @param ontology Character string. The ontology to use: "mesh" or "umls"
#' @param api_key UMLS API key (required if ontology = "umls")
#' @param fuzzy_match Logical. If TRUE, allows fuzzy matching of terms
#' @param similarity_threshold Numeric between 0 and 1. Minimum similarity for fuzzy matching
#' @param mesh_query Additional query to filter MeSH terms (only if ontology = "mesh")
#' @param semantic_types Vector of semantic types to filter UMLS results
#' @param dictionary_type Type of dictionary to use ("disease", "drug", "gene", etc.)
#'
#' @return A data frame with mapped terms and ontology identifiers
#' @export
#'
#' @examples
#' \dontrun{
#' # Map terms to MeSH
#' mesh_mappings <- map_ontology(c("headache", "migraine"), ontology = "mesh")
#'
#' # Map terms to UMLS with API key
#' umls_mappings <- map_ontology(c("headache", "migraine"), ontology = "umls",
#'                               api_key = "your_api_key")
#' }
map_ontology <- function(terms, ontology = c("mesh", "umls"),
                         api_key = NULL,
                         fuzzy_match = FALSE,
                         similarity_threshold = 0.8,
                         mesh_query = NULL,
                         semantic_types = NULL,
                         dictionary_type = "disease") {

  # Match ontology argument
  ontology <- match.arg(ontology)

  # Check if terms is empty
  if (length(terms) == 0) {
    return(data.frame(
      term = character(),
      ontology_id = character(),
      ontology_term = character(),
      match_type = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Check if API key is provided when using UMLS
  if (ontology == "umls" && is.null(api_key)) {
    stop("API key is required for UMLS mapping")
  }

  # Load ontology dictionary using the improved functions
  if (ontology == "mesh") {
    ontology_dict <- load_dictionary(dictionary_type,
                                     source = "mesh",
                                     mesh_query = mesh_query)
  } else {
    ontology_dict <- load_dictionary(dictionary_type,
                                     source = "umls",
                                     api_key = api_key,
                                     semantic_type_filter = semantic_types)
  }

  # Check if we have a valid dictionary
  if (nrow(ontology_dict) == 0) {
    warning("No terms found in the ", ontology, " dictionary")
    return(data.frame(
      term = character(),
      ontology_id = character(),
      ontology_term = character(),
      match_type = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize results
  results <- data.frame(
    term = character(),
    ontology_id = character(),
    ontology_term = character(),
    match_type = character(),
    stringsAsFactors = FALSE
  )

  # Function to calculate string similarity for fuzzy matching
  string_similarity <- function(a, b) {
    # Simple implementation of Levenshtein distance-based similarity
    a <- tolower(a)
    b <- tolower(b)
    distance <- adist(a, b)[1]
    max_length <- max(nchar(a), nchar(b))
    similarity <- 1 - (distance / max_length)
    return(similarity)
  }

  # Process each term
  for (term in terms) {
    # Exact match - case insensitive
    exact_matches <- ontology_dict[tolower(ontology_dict$term) == tolower(term), ]

    if (nrow(exact_matches) > 0) {
      for (i in 1:nrow(exact_matches)) {
        results <- rbind(results, data.frame(
          term = term,
          ontology_id = exact_matches$id[i],
          ontology_term = exact_matches$term[i],
          match_type = "exact",
          stringsAsFactors = FALSE
        ))
      }
    } else if (fuzzy_match) {
      # Fuzzy matching
      # Calculate similarity with all terms in the dictionary
      all_terms <- ontology_dict$term
      similarities <- numeric(length(all_terms))

      for (i in seq_along(all_terms)) {
        similarities[i] <- string_similarity(term, all_terms[i])
      }

      # Get matches above threshold
      matches <- which(similarities >= similarity_threshold)

      if (length(matches) > 0) {
        for (i in matches) {
          results <- rbind(results, data.frame(
            term = term,
            ontology_id = ontology_dict$id[i],
            ontology_term = ontology_dict$term[i],
            match_type = "fuzzy",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  # If no matches found
  if (nrow(results) == 0) {
    message("No matches found for the input terms in the ", ontology, " ontology")
  }

  return(results)
}

#' Apply topic modeling to a corpus
#'
#' This function implements a simple non-negative matrix factorization (NMF)
#' approach to topic modeling, without requiring additional packages.
#'
#' @param text_data A data frame containing the text data
#' @param text_column Name of the column containing the text
#' @param n_topics Number of topics to extract
#' @param max_terms Maximum number of terms per topic to return
#' @param n_iterations Number of iterations for the NMF algorithm
#'
#' @return A list containing topic-term and document-topic matrices
#' @export
#'
#' @examples
#' \dontrun{
#' topics <- extract_topics(article_data, text_column = "abstract", n_topics = 5)
#' }
extract_topics <- function(text_data, text_column = "abstract", n_topics = 5,
                           max_terms = 10, n_iterations = 50) {
  # Check if text column exists
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Preprocess text data
  preprocessed_data <- preprocess_text(text_data, text_column = text_column,
                                       remove_stopwords = TRUE)

  # Extract terms and create document-term matrix
  all_terms <- list()
  for (i in 1:nrow(preprocessed_data)) {
    terms_df <- preprocessed_data$terms[[i]]
    if (nrow(terms_df) > 0) {
      all_terms[[i]] <- terms_df
    }
  }

  # Get unique terms
  unique_terms <- unique(unlist(lapply(all_terms, function(df) df$word)))

  # Create document-term matrix
  dtm <- matrix(0, nrow = nrow(preprocessed_data), ncol = length(unique_terms))
  colnames(dtm) <- unique_terms

  for (i in 1:nrow(preprocessed_data)) {
    if (i %in% seq_along(all_terms)) {
      terms_df <- all_terms[[i]]
      if (nrow(terms_df) > 0) {
        term_indices <- match(terms_df$word, unique_terms)
        dtm[i, term_indices] <- terms_df$count
      }
    }
  }

  # Simple implementation of NMF (Non-negative Matrix Factorization)
  # Initialize random matrices W and H
  set.seed(42)  # For reproducibility
  W <- matrix(runif(nrow(dtm) * n_topics), nrow = nrow(dtm))
  H <- matrix(runif(n_topics * ncol(dtm)), nrow = n_topics)

  # Iterative update of W and H
  for (iter in 1:n_iterations) {
    # Update H
    H <- H * (t(W) %*% dtm) / (t(W) %*% W %*% H + 1e-10)

    # Update W
    W <- W * (dtm %*% t(H)) / (W %*% H %*% t(H) + 1e-10)

    # Normalize columns of W
    W <- W / apply(W, 2, sum)
  }

  # Extract top terms for each topic
  top_terms <- list()
  for (i in 1:n_topics) {
    term_weights <- H[i, ]
    top_indices <- order(term_weights, decreasing = TRUE)[1:min(max_terms, length(term_weights))]
    top_terms[[i]] <- data.frame(
      term = unique_terms[top_indices],
      weight = term_weights[top_indices],
      stringsAsFactors = FALSE
    )
  }

  # Assign topic labels
  topic_labels <- paste("Topic", 1:n_topics)
  names(top_terms) <- topic_labels

  # Document-topic assignments
  doc_topics <- W
  colnames(doc_topics) <- topic_labels

  # Return results
  return(list(
    topics = top_terms,
    document_topics = doc_topics,
    dtm = dtm
  ))
}

#' Get UMLS semantic types for a given dictionary type
#'
#' Helper function to map dictionary types to UMLS semantic type identifiers
#'
#' @param dictionary_type The type of dictionary to get semantic types for
#' @return Vector of UMLS semantic type identifiers
#' @keywords internal
get_umls_semantic_types <- function(dictionary_type) {
  # Create a comprehensive mapping of dictionary types to UMLS semantic types
  semantic_types <- list(
    # Original mappings
    "disease" = c("T047", "T048", "T019", "T046"), # Disease and Syndrome, Mental Disorder, Congenital Abnormality, Pathologic Function
    "drug" = c("T116", "T121", "T195", "T200"), # Amino Acid, Lipid, Antibiotic, Clinical Drug
    "gene" = c("T028", "T087", "T123"), # Gene, Amino Acid Sequence, Nucleotide Sequence

    # New expanded mappings
    "protein" = c("T116", "T126", "T125"), # Amino Acid, Peptide/Protein, Enzyme
    "chemical" = c("T103", "T104", "T109", "T196"), # Chemical, Chemical Viewed Structurally, Organic Chemical, Element, Ion, or Isotope
    "pathway" = c("T044", "T042", "T045"), # Molecular Function, Organ or Tissue Function, Genetic Function
    "anatomy" = c("T017", "T018", "T023", "T024"), # Anatomical Structure, Embryonic Structure, Body Part, Organ, or Organ Component, Tissue
    "organism" = c("T001", "T002", "T004", "T005", "T007"), # Organism, Plant, Fungus, Virus, Bacterium
    "biological_process" = c("T038", "T039", "T040", "T041"), # Biologic Function, Physiologic Function, Organism Function, Mental Process
    "cellular_component" = c("T026", "T025", "T029"), # Cell Component, Cell, Body Location or Region
    "molecular_function" = c("T044", "T045", "T118"), # Molecular Function, Genetic Function, Carbohydrate
    "diagnostic_procedure" = c("T059", "T060"), # Laboratory Procedure, Diagnostic Procedure
    "therapeutic_procedure" = c("T061", "T058"), # Therapeutic or Preventive Procedure, Health Care Activity
    "phenotype" = c("T046", "T047", "T048", "T020"), # Pathologic Function, Disease or Syndrome, Mental or Behavioral Dysfunction, Acquired Abnormality
    "symptom" = c("T184", "T046", "T048"), # Sign or Symptom, Pathologic Function, Mental or Behavioral Dysfunction
    "cell" = c("T025"), # Cell
    "tissue" = c("T024"), # Tissue
    "mental_process" = c("T041", "T048"), # Mental Process, Mental or Behavioral Dysfunction
    "physiologic_function" = c("T039", "T040"), # Physiologic Function, Organism Function
    "laboratory_procedure" = c("T059") # Laboratory Procedure
  )

  if (!is.null(dictionary_type) && dictionary_type %in% names(semantic_types)) {
    return(semantic_types[[dictionary_type]])
  } else {
    return(NULL)
  }

  if (verbose && nrow(safe_dict) < original_count) {
    message("  Removed ", original_count - nrow(safe_dict),
            " terms with non-alphanumeric characters (final cleanup)")
  }

  if (verbose) {
    message("Sanitization complete. ", nrow(safe_dict), " terms remaining (",
            round(nrow(safe_dict) / nrow(dictionary) * 100, 1), "% of original)")
  }

  return(safe_dict)
}

#' Enhanced sanitize dictionary function
#'
#' This function sanitizes dictionary terms to ensure they're valid for entity extraction.
#'
#' @param dictionary A data frame containing dictionary terms.
#' @param term_column The name of the column containing the terms to sanitize.
#' @param type_column The name of the column containing entity types.
#' @param validate_types Logical. If TRUE, validates terms against their claimed type.
#' @param verbose Logical. If TRUE, prints information about the filtering process.
#'
#' @return A data frame with sanitized terms.
#' @export
sanitize_dictionary <- function(dictionary, term_column = "term", type_column = "type",
                                validate_types = TRUE, verbose = TRUE) {
  # Check input
  if (is.null(dictionary) || nrow(dictionary) == 0) {
    warning("Empty dictionary provided for sanitization")
    return(dictionary)
  }

  if (!term_column %in% colnames(dictionary)) {
    stop("Term column '", term_column, "' not found in the dictionary")
  }

  original_count <- nrow(dictionary)

  if (verbose) {
    message("Sanitizing dictionary with ", original_count, " terms...")
  }

  # Check for NA terms
  na_terms <- is.na(dictionary[[term_column]])
  if (any(na_terms)) {
    if (verbose) {
      message("  Removing ", sum(na_terms), " NA terms")
    }
    dictionary <- dictionary[!na_terms, ]
  }

  # Check for empty terms
  empty_terms <- dictionary[[term_column]] == ""
  if (any(empty_terms)) {
    if (verbose) {
      message("  Removing ", sum(empty_terms), " empty terms")
    }
    dictionary <- dictionary[!empty_terms, ]
  }

  # Step 1: Remove terms with regex special characters that could cause issues
  safe_dict <- dictionary[!grepl('[\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\/]', dictionary[[term_column]]), ]

  if (verbose && nrow(safe_dict) < nrow(dictionary)) {
    message("  Removed ", nrow(dictionary) - nrow(safe_dict), " terms with regex special characters")
  }

  # If the dictionary is now empty, early return to avoid further processing
  if (nrow(safe_dict) == 0) {
    warning("No terms remain after removing terms with regex special characters")
    return(safe_dict)
  }

  # Step 2: Remove terms with numbers followed by special characters
  original_count <- nrow(safe_dict)
  safe_dict <- safe_dict[!grepl('\\d\\s*[^a-zA-Z0-9\\s]', safe_dict[[term_column]]), ]

  if (verbose && nrow(safe_dict) < original_count) {
    message("  Removed ", original_count - nrow(safe_dict), " terms with numbers followed by special characters")
  }

  # If the dictionary is now empty, early return
  if (nrow(safe_dict) == 0) {
    warning("No terms remain after removing terms with numbers followed by special characters")
    return(safe_dict)
  }

  # Step 3: Check for specific problematic patterns and remove them
  problem_terms <- grep('\\d\\s*\\[', safe_dict[[term_column]], value = TRUE)
  if (length(problem_terms) > 0) {
    if (verbose) {
      message("  Warning: Found potentially problematic terms like: ",
              paste(head(problem_terms, 5), collapse = ", "))
    }

    original_count <- nrow(safe_dict)
    safe_dict <- safe_dict[!grepl('\\d\\s*\\[', safe_dict[[term_column]]), ]

    if (verbose && nrow(safe_dict) < original_count) {
      message("  Removed ", original_count - nrow(safe_dict), " problematic terms with patterns like '68 [1'")
    }
  }

  # If the dictionary is now empty, early return
  if (nrow(safe_dict) == 0) {
    warning("No terms remain after removing problematic terms")
    return(safe_dict)
  }

  # Step 4: Remove common conjunctive adverbs and non-medical terms
  original_count <- nrow(safe_dict)

  # Expanded list of common non-medical terms and general terminology
  common_adverbs <- c(
    "however", "therefore", "furthermore", "moreover", "consequently",
    "nevertheless", "accordingly", "thus", "hence", "meanwhile", "subsequently",
    "conversely", "indeed", "instead", "likewise", "namely", "regardless",
    "similarly", "specifically", "undoubtedly", "whereas", "annotation", "annotated",
    "nonetheless", "meanwhile", "absolutely", "accordingly", "additionally",
    "afterwards", "albeit", "although", "altogether", "anyway", "certainly",
    "clearly", "considering", "despite", "earlier", "especially", "essentially",
    "eventually", "exactly", "evidently", "finally", "further", "generally",
    "granted", "fortunately", "hopefully", "importantly", "incidentally",
    "initially", "ironically", "literally", "merely", "naturally", "obviously",
    "occasionally", "otherwise", "particularly", "perhaps", "possibly",
    "presumably", "previously", "primarily", "probably", "rather",
    "regardless", "relatively", "reportedly", "seemingly", "seriously",
    "significantly", "simultaneously", "specifically", "strangely", "surprisingly",
    "ultimately", "unfortunately", "whereby"
  )

  # List of common abbreviations and academic terms
  common_abbrevs <- c(
    "et al", "fig", "etc", "ie", "eg", "viz", "vs", "ref", "refs",
    "approx", "ca", "cf", "cp", "ed", "eds", "eq", "eqn", "eqns", "intro",
    "min", "max", "nb", "no", "non", "vol", "vols", "yr", "yrs",
    "abstract", "table", "figure", "chart", "graph", "analysis", "summary",
    "introduction", "conclusion", "discussion", "result", "results", "method",
    "methods", "protocol", "review", "sample", "case", "cases", "control",
    "controls", "subject", "subjects", "participant", "participants",
    "pubmed", "embase", "medline", "cochrane", "meta", "database", "databases"
  )

  # Common non-specific terms that are incorrectly appearing in our results
  common_nonspecific <- c(
    "effect", "effects", "affect", "affects", "influence", "influences",
    "impact", "impacts", "association", "associated", "associations",
    "correlation", "correlations", "relationship", "relationships",
    "connection", "connections", "link", "links", "cause", "causes",
    "causing", "caused", "increase", "increases", "increasing",
    "decrease", "decreases", "decreasing", "demonstrate", "demonstrates",
    "demonstrated", "show", "shows", "shown", "find", "finds", "found",
    "observe", "observes", "observed", "report", "reports", "reported",
    "reveal", "reveals", "revealed", "suggest", "suggests", "suggested",
    "indicates", "indicated", "document", "documented", "value", "values",
    "level", "levels", "measure", "measures", "measured", "significant",
    "significance", "compare", "compared", "comparing", "determine",
    "determined", "determining", "identify", "identified", "identifying",
    "estimate", "estimated", "estimating", "total", "high", "higher",
    "highest", "low", "lower", "lowest", "new", "novel", "recent", "previous",
    "older", "younger", "early", "earlier", "late", "later", "frequently",
    "frequently", "rarely", "common", "commonly", "rare", "rarely", "severe",
    "severity", "mild", "moderate", "normal", "abnormal", "improvement",
    "improved", "improving", "worsening", "worsened", "detection", "detected",
    "detecting", "prevention", "prevented", "preventing", "assessment",
    "assessed", "assessing", "evaluation", "evaluated", "evaluating",
    "investigation", "investigated", "investigating", "calculation",
    "calculated", "calculating", "classification", "classified", "classifying",
    "stat", "stats", "clinical", "treatment", "patient", "patients", "outcome",
    "outcomes", "intervention", "interventions", "agent", "agents", "order",
    "orders", "ample", "amples", "population", "play", "negative", "accuracy",
    "leading", "baseline", "evidence", "efficacy", "effectiveness", "placebo",
    "follow", "following", "design", "trial", "trials", "quality", "recommendation",
    "recommendations", "response", "responses", "safe", "safety", "adverse",
    "profile", "profiles", "interaction", "interactions", "combination",
    "combinations", "standard", "standards", "guideline", "guidelines",
    "first", "second", "third", "fourth", "fifth", "primary", "secondary",
    "tertiary", "initial", "final", "potential", "beneficial", "beneficial",
    "strategy", "strategies", "approach", "approaches", "method", "methods",
    "technique", "techniques", "procedure", "procedures", "process", "processes",
    "long", "short", "term", "single", "double", "multiple", "simple", "complex",
    "positive", "subsequent", "beneficial", "harmful", "randomized", "randomised",
    "controlled", "substantial", "significant", "consistent", "considerable",
    "relevant", "important", "interesting", "promising", "similar", "different",
    "distinct", "specific", "particular", "major", "minor", "key", "main",
    "essential", "necessary", "sufficient", "adequate", "proper", "appropriate",
    "suitable", "consecutive", "simultaneous", "various", "variable", "concurrent",
    "concomitant", "overall", "entire", "whole", "optimum", "optimal", "ideal",
    "better", "best", "worse", "worst", "efficacious", "limited", "extensive",
    "intensive", "widespread", "reliable", "reproducible", "repeatable",
    "comparable", "varied", "useful", "valuable", "successful", "unsuccessful",
    "effective", "ineffective", "extensive", "intensive", "minimum", "maximum"
  )

  # New: Medical jargon that's too general to be useful biomedical entities
  medical_jargon <- c(
    "acute", "chronic", "condition", "conditions", "diagnosis", "diagnoses",
    "diagnostic", "diagnostics", "disorder", "disorders", "dose", "doses",
    "dosage", "dosages", "progressive", "regressive", "regimen", "regimens",
    "symptom", "symptoms", "syndrome", "syndromes", "therapeutic", "therapy",
    "therapies", "onset", "remission", "recurrent", "relapse", "relapses",
    "treatment", "treatments", "manageable", "unmanageable", "intensive",
    "supportive", "palliative", "preventive", "preventative", "prophylactic",
    "prophylaxis", "maintenance", "induction", "consolidation", "adjuvant",
    "neoadjuvant", "first-line", "second-line", "rescue", "salvage",
    "empiric", "empirical", "targeted", "systemic", "topical", "local",
    "regional", "general", "medical", "surgical", "pharmacological",
    "behavioral", "behavioural", "psychological", "physical", "occupational",
    "alternative", "complementary", "conventional", "standard", "novel",
    "experimental", "investigational", "established", "emerging", "conventional"
  )

  # Combine all terms to filter out
  terms_to_filter <- unique(c(common_adverbs, common_abbrevs, common_nonspecific, medical_jargon))

  # Make term filtering case-insensitive
  safe_dict <- safe_dict[!tolower(safe_dict[[term_column]]) %in% terms_to_filter, ]

  if (verbose && nrow(safe_dict) < original_count) {
    message("  Removed ", original_count - nrow(safe_dict),
            " common non-medical terms, conjunctive adverbs, and general terms")
  }

  # If the dictionary is now empty, early return
  if (nrow(safe_dict) == 0) {
    warning("No terms remain after removing common non-medical terms")
    return(safe_dict)
  }

  # Step 5: Apply term-type corrections for commonly misclassified terms
  if (validate_types && type_column %in% colnames(safe_dict)) {
    # Additional term-type corrections for common misclassified terms
    term_type_corrections <- list(
      # Analytical techniques/methods that are often misclassified as chemicals
      "faers" = "method",              # FDA Adverse Event Reporting System
      "bcpnn" = "method",              # Bayesian Confidence Propagation Neural Network
      "uplc" = "method",               # Ultra Performance Liquid Chromatography
      "frap" = "method",               # Fluorescence Recovery After Photobleaching
      "hplc" = "method",               # High Performance Liquid Chromatography
      "lc-ms" = "method",              # Liquid Chromatography-Mass Spectrometry
      "gc-ms" = "method",              # Gas Chromatography-Mass Spectrometry
      "maldi" = "method",              # Matrix-Assisted Laser Desorption/Ionization
      "elisa" = "method",              # Enzyme-Linked Immunosorbent Assay
      "ft-ir" = "method",              # Fourier Transform Infrared Spectroscopy
      "nmr" = "method",                # Nuclear Magnetic Resonance
      "pcr" = "method",                # Polymerase Chain Reaction
      "sem" = "method",                # Scanning Electron Microscopy
      "tem" = "method",                # Transmission Electron Microscopy
      "xrd" = "method",                # X-Ray Diffraction
      "saxs" = "method",               # Small-Angle X-ray Scattering
      "uv-vis" = "method",             # Ultraviolet-Visible Spectroscopy
      "ms" = "method",                 # Mass Spectrometry
      "ms/ms" = "method",              # Tandem Mass Spectrometry
      "lc" = "method",                 # Liquid Chromatography
      "gc" = "method",                 # Gas Chromatography
      "tga" = "method",                # Thermogravimetric Analysis
      "dsc" = "method",                # Differential Scanning Calorimetry
      "uv" = "method",                 # Ultraviolet
      "ir" = "method",                 # Infrared
      "rna-seq" = "method",            # RNA Sequencing
      "qtof" = "method",               # Quadrupole Time-of-Flight

      # Common biostatistical methods incorrectly classified as chemicals
      "anova" = "method",              # Analysis of Variance
      "ancova" = "method",             # Analysis of Covariance
      "manova" = "method",             # Multivariate Analysis of Variance
      "pca" = "method",                # Principal Component Analysis
      "sem" = "method",                # Structural Equation Modeling
      "glm" = "method",                # Generalized Linear Model
      "lda" = "method",                # Linear Discriminant Analysis
      "svm" = "method",                # Support Vector Machine
      "ann" = "method",                # Artificial Neural Network
      "kmeans" = "method",             # K-means clustering
      "roc" = "method",                # Receiver Operating Characteristic
      "auc" = "method",                # Area Under the Curve

      # Symptoms that are often misclassified
      "pain" = "symptom",
      "headache" = "symptom",
      "migraine" = "disease",
      "nausea" = "symptom",
      "vomiting" = "symptom",
      "dizziness" = "symptom",
      "fatigue" = "symptom",
      "weakness" = "symptom",
      "aura" = "symptom",
      "photophobia" = "symptom",
      "phonophobia" = "symptom",

      # Proteins and receptors
      "receptor" = "protein",
      "receptors" = "protein",
      "channel" = "protein",
      "channels" = "protein",
      "transporter" = "protein",
      "transporters" = "protein",

      # Biological processes
      "inflammation" = "biological_process",
      "signaling" = "biological_process",
      "activation" = "biological_process",
      "inhibition" = "biological_process",
      "regulation" = "biological_process",
      "phosphorylation" = "biological_process",

      # Diseases
      "migraine" = "disease",
      "malformation" = "disease",
      "disorder" = "disease",
      "syndrome" = "disease"
    )

    # Apply corrections to dictionary
    corrections_made <- 0
    for (i in 1:nrow(safe_dict)) {
      term_lower <- tolower(safe_dict[[term_column]][i])
      if (term_lower %in% names(term_type_corrections)) {
        # Get the correct type
        correct_type <- term_type_corrections[[term_lower]]

        # If current type is different from the correct type
        if (safe_dict[[type_column]][i] != correct_type) {
          # Update the type to the correct one
          if (verbose) {
            message("  Correcting type for '", safe_dict[[term_column]][i],
                    "' from '", safe_dict[[type_column]][i],
                    "' to '", correct_type, "'")
          }
          safe_dict[[type_column]][i] <- correct_type
          corrections_made <- corrections_made + 1
        }
      }
    }

    if (verbose && corrections_made > 0) {
      message("  Applied ", corrections_made, " type corrections for commonly misclassified terms")
    }
  }

  # Step 6: Type validation - ensure terms match their claimed entity types
  if (validate_types && type_column %in% colnames(safe_dict)) {
    original_count <- nrow(safe_dict)

    # Rules for validating types - define characteristics for each entity type
    is_valid_type <- function(term, claimed_type) {
      term <- tolower(term)
      claimed_type <- tolower(claimed_type)

      # Single character or very short terms are likely not valid entities
      if (nchar(term) < 3) {
        return(FALSE)
      }

      # Check for capitalized acronyms (common for genes)
      is_acronym <- grepl("^[A-Z0-9]{2,}$", term)

      # Term validation rules by type
      if (claimed_type == "gene") {
        # Genes are often acronyms or have numbers
        known_gene_patterns <- c("gene", "receptor", "factor", "kinase", "protein", "enzyme",
                                 "domain", "binding", "transcription", "transporter", "channel")
        pattern_match <- any(sapply(known_gene_patterns, function(p) grepl(p, term)))
        return(is_acronym || pattern_match)
      }
      else if (claimed_type == "protein") {
        # Proteins often end with -in or contain protein-related terms
        known_protein_patterns <- c("protein", "enzyme", "antibody", "receptor", "factor",
                                    "kinase", "ase$", "in$", "globulin", "albumin", "peptide")
        pattern_match <- any(sapply(known_protein_patterns, function(p) grepl(p, term)))

        # Special case for receptor/receptors
        if (term == "receptor" || term == "receptors") {
          return(TRUE)
        }

        return(pattern_match)
      }
      else if (claimed_type == "drug") {
        # Drugs often end with specific suffixes
        known_drug_patterns <- c("caine$", "mycin$", "oxacin$", "dronate$", "olol$", "pril$",
                                 "sartan$", "mab$", "nib$", "gliptin$", "prazole$", "vastatin$",
                                 "dine$", "zosin$", "parin$", "ide$", "ane$", "ene$")
        pattern_match <- any(sapply(known_drug_patterns, function(p) grepl(p, term)))

        # Known drug classes or categories
        drug_classes <- c("antibiotic", "inhibitor", "antagonist", "agonist", "blocker",
                          "vaccine", "antidepressant", "antipsychotic", "antiepileptic",
                          "sedative", "stimulant", "antihistamine", "analgesic", "hormone")
        class_match <- any(sapply(drug_classes, function(c) grepl(c, term)))

        return(pattern_match || class_match)
      }
      else if (claimed_type == "disease") {
        # Diseases often contain specific terms
        known_disease_patterns <- c("disease$", "disorder$", "syndrome$", "itis$", "emia$",
                                    "pathy$", "oma$", "osis$", "iasis$", "itis$", "algia$",
                                    "cancer", "tumor", "infection", "deficiency", "failure")
        pattern_match <- any(sapply(known_disease_patterns, function(p) grepl(p, term)))

        # Special case for "migraine"
        if (term == "migraine") {
          return(TRUE)
        }

        return(pattern_match)
      }
      else if (claimed_type == "pathway") {
        # Pathways often contain these terms
        known_pathway_patterns <- c("pathway", "signaling", "cascade", "metabolism",
                                    "biosynthesis", "cycle", "regulation", "transport")
        pattern_match <- any(sapply(known_pathway_patterns, function(p) grepl(p, term)))
        return(pattern_match)
      }
      else if (claimed_type == "symptom") {
        # Symptoms often contain these terms
        known_symptom_patterns <- c("pain", "ache", "fatigue", "nausea", "vomiting",
                                    "headache", "fever", "cough", "dizziness", "vertigo",
                                    "weakness", "numbness", "tingling", "photophobia",
                                    "phonophobia", "aura")

        # Direct check for common symptoms
        if (term %in% known_symptom_patterns) {
          return(TRUE)
        }

        pattern_match <- any(sapply(known_symptom_patterns, function(p) grepl(p, term)))
        return(pattern_match)
      }
      else if (claimed_type == "biological_process") {
        # Biological processes often contain these terms
        known_bioprocess_patterns <- c("process", "signaling", "activation", "inhibition",
                                       "regulation", "expression", "secretion", "transcription",
                                       "phosphorylation", "metabolism", "inflammation")

        # Direct check for common biological processes
        if (term %in% c("inflammation", "signaling", "activation", "inhibition", "regulation")) {
          return(TRUE)
        }

        pattern_match <- any(sapply(known_bioprocess_patterns, function(p) grepl(p, term)))
        return(pattern_match)
      }
      else if (claimed_type == "method") {
        # Check if term is a known analytical method
        analytical_methods <- c(
          "faers", "bcpnn", "uplc", "frap", "hplc", "lc-ms", "gc-ms", "maldi",
          "elisa", "ft-ir", "nmr", "pcr", "sem", "tem", "xrd", "saxs", "uv-vis",
          "ms", "ms/ms", "lc", "gc", "tga", "dsc", "uv", "ir", "rna-seq", "qtof",
          "mri", "ct", "pet", "spect", "ecg", "eeg", "emg", "fmri", "qsar", "qspr",
          "anova", "ancova", "manova", "pca", "sem", "glm", "lda", "svm", "ann",
          "roc", "auc"
        )

        if (term %in% analytical_methods) {
          return(TRUE)
        }

        # Check for method terms
        method_patterns <- c("method", "technique", "assay", "analysis", "procedure",
                             "protocol", "algorithm", "approach")
        pattern_match <- any(sapply(method_patterns, function(p) grepl(p, term)))
        return(pattern_match)
      }

      # If no specific rules for this type, allow it to pass
      return(TRUE)
    }

    # Apply type validation
    valid_rows <- rep(TRUE, nrow(safe_dict))
    for (i in seq_len(nrow(safe_dict))) {
      term <- safe_dict[[term_column]][i]
      claimed_type <- safe_dict[[type_column]][i]

      # Check if term matches its claimed type
      if (!is.na(claimed_type) && !is_valid_type(term, claimed_type)) {
        valid_rows[i] <- FALSE
      }
    }

    # Keep only rows with valid type assignments
    safe_dict <- safe_dict[valid_rows, ]

    if (verbose && nrow(safe_dict) < original_count) {
      message("  Removed ", original_count - nrow(safe_dict),
              " terms that did not match their claimed entity types")
    }
  }

  # Final step: Remove terms consisting solely of numbers
  original_count <- nrow(safe_dict)
  safe_dict <- safe_dict[!grepl("^[0-9]+$", safe_dict[[term_column]]), ]

  if (verbose && nrow(safe_dict) < original_count) {
    message("  Removed ", original_count - nrow(safe_dict),
            " terms consisting solely of numbers")
  }

  # Final cleanup - only include terms with alphanumeric characters and spaces
  original_count <- nrow(safe_dict)
  safe_dict <- safe_dict[grepl('^[a-zA-Z0-9\\s]+$', safe_dict[[term_column]]), ]

  if (verbose && nrow(safe_dict) < original_count) {
    message("  Removed ", original_count - nrow(safe_dict),
            " terms with non-alphanumeric characters (final cleanup)")
  }

  if (verbose) {
    message("Sanitization complete. ", nrow(safe_dict), " terms remaining (",
            round(nrow(safe_dict) / nrow(dictionary) * 100, 1), "% of original)")
  }

  return(safe_dict)
}


#' Load terms from MeSH using PubMed search
#'
#' This function enhances the MeSH dictionary by extracting additional terms
#' from PubMed search results using MeSH queries.
#'
#' @param mesh_queries A named list of MeSH queries for different categories.
#' @param max_results Maximum number of results to retrieve per query.
#' @param min_term_length Minimum length of terms to include.
#' @param sanitize Logical. If TRUE, sanitizes the extracted terms.
#'
#' @return A data frame containing the combined dictionary with extracted terms.
#' @keywords internal
load_mesh_terms_from_pubmed <- function(mesh_queries, max_results = 50, min_term_length = 3, sanitize = TRUE) {
  # Check if pubmed_search function is available
  if (!exists("pubmed_search", mode = "function")) {
    stop("pubmed_search function not available. Make sure required dependencies are loaded.")
  }

  # Initialize combined dictionary
  combined_dict <- data.frame(
    term = character(),
    type = character(),
    id = character(),
    source = character(),
    stringsAsFactors = FALSE
  )

  # Process each query
  for (category in names(mesh_queries)) {
    message("Getting ", category, " terms from PubMed/MeSH...")

    # Use PubMed search to get relevant MeSH terms
    search_result <- tryCatch({
      pubmed_search(
        query = mesh_queries[[category]],
        max_results = max_results,
        use_mesh = TRUE
      )
    }, error = function(e) {
      message("Error searching for ", category, " terms: ", e$message)
      return(NULL)
    })

    if (!is.null(search_result) && nrow(search_result) > 0) {
      # Extract potential terms from titles and abstracts
      terms_text <- paste(search_result$title, search_result$abstract, sep = " ")

      # Simple term extraction - extract words and phrases
      term_candidates <- unique(unlist(strsplit(terms_text, "[\\s,.;:()/]+|and\\s|or\\s")))
      term_candidates <- term_candidates[nchar(term_candidates) > min_term_length] # Filter out short terms

      # Filter out terms with ANY regex special characters completely
      clean_terms <- character(0)
      for (term in term_candidates) {
        if (!grepl("[\\[\\]\\(\\)\\{\\}\\^\\$\\*\\+\\?\\|\\\\/]", term)) {
          clean_terms <- c(clean_terms, term)
        }
      }

      # If we have clean terms, create a dictionary
      if (length(clean_terms) > 0) {
        mini_dict <- data.frame(
          term = clean_terms,
          type = category,
          id = paste0(category, "_", 1:length(clean_terms)),
          source = "pubmed_mesh_search",
          stringsAsFactors = FALSE
        )

        # Add to combined dictionary
        message("Added ", nrow(mini_dict), " potential ", category, " terms")
        combined_dict <- rbind(combined_dict, mini_dict)
      } else {
        message("No valid terms found for category: ", category)
      }
    } else {
      message("No search results found for category: ", category)
    }
  }

  message("Created combined dictionary with ", nrow(combined_dict), " terms")

  # Sanitize the dictionary if requested
  if (sanitize && nrow(combined_dict) > 0) {
    combined_dict <- sanitize_dictionary(combined_dict, term_column = "term")
  }

  return(combined_dict)
}

#' Extract entities from text with improved efficiency using only base R
#'
#' This function provides a complete workflow for extracting entities from text
#' using dictionaries from multiple sources, with improved performance and robust error handling.
#'
#' @param text_data A data frame containing article text data.
#' @param text_column Name of the column containing text to process.
#' @param entity_types Character vector of entity types to include.
#' @param dictionary_sources Character vector of sources for entity dictionaries.
#' @param additional_mesh_queries Named list of additional MeSH queries.
#' @param sanitize Logical. If TRUE, sanitizes dictionaries before extraction.
#' @param api_key API key for UMLS access (if "umls" is in dictionary_sources).
#' @param custom_dictionary A data frame containing custom dictionary entries to
#'        incorporate into the entity extraction process.
#' @param max_terms_per_type Maximum number of terms to fetch per entity type. Default is 200.
#' @param verbose Logical. If TRUE, prints detailed progress information.
#' @param batch_size Number of documents to process in a single batch. Default is 500.
#' @param parallel Logical. If TRUE, uses parallel processing when available. Default is FALSE.
#' @param num_cores Number of cores to use for parallel processing. Default is 2.
#' @param cache_dictionaries Logical. If TRUE, caches dictionaries for faster reuse. Default is TRUE.
#'
#' @return A data frame with extracted entities, their types, and positions.
#' @export
extract_entities_workflow <- function(text_data, text_column = "abstract",
                                      entity_types = c("disease", "drug", "gene"),
                                      dictionary_sources = c("local", "mesh", "umls"),
                                      additional_mesh_queries = NULL,
                                      sanitize = TRUE,
                                      api_key = NULL,
                                      custom_dictionary = NULL,
                                      max_terms_per_type = 200,
                                      verbose = TRUE,
                                      batch_size = 500,
                                      parallel = FALSE,
                                      num_cores = 2,
                                      cache_dictionaries = TRUE) {
  # Check if running in R CMD check environment
  is_check <- !interactive() &&
    (!is.null(Sys.getenv("R_CHECK_RUNNING")) &&
       Sys.getenv("R_CHECK_RUNNING") == "true")

  # More robust check for testing environment
  if (!is_check && !is.null(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) {
    is_check <- TRUE
  }

  # Adjust parameters if in check environment
  if (is_check) {
    parallel <- FALSE
    num_cores <- 1
    if (verbose) message("Running in R CMD check environment. Disabling parallel processing.")
  }

  # Ensure num_cores doesn't exceed system capabilities or reasonable limits
  if (parallel) {
    # Get available cores
    available_cores <- parallel::detectCores()

    # Limit to reasonable maximum (e.g., available - 1, not exceeding 8)
    max_cores <- min(available_cores - 1, 8)

    # Ensure at least 1 core
    num_cores <- max(1, min(num_cores, max_cores))
  } else {
    num_cores <- 1
  }

  # Start timing the function for performance reporting
  start_time <- Sys.time()

  # Initialize dictionary cache environment if caching is enabled
  if (cache_dictionaries) {
    # Use the package's dict_cache_env instead of GlobalEnv
    dict_cache <- get_dict_cache()
  }

  # Function to create a unique cache key
  create_cache_key <- function(entity_type, source) {
    paste(entity_type, source, sep = "_")
  }

  # Validate inputs
  if (!text_column %in% colnames(text_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Setup parallel processing if requested
  use_parallel <- FALSE
  cl <- NULL

  if (parallel) {
    tryCatch({
      if (!requireNamespace("parallel", quietly = TRUE)) {
        warning("The 'parallel' package is required for parallel processing. Falling back to sequential processing.")
      } else {
        # Detect available cores if not specified
        if (is.null(num_cores) || num_cores < 1) {
          num_cores <- parallel::detectCores() - 1
          num_cores <- max(1, num_cores)  # Ensure at least 1 core
        }

        if (num_cores > 1) {
          use_parallel <- TRUE
          if (verbose) message("Setting up parallel processing with ", num_cores, " cores...")
        } else {
          if (verbose) message("Only 1 core available, using sequential processing.")
        }
      }
    }, error = function(e) {
      warning("Error setting up parallel processing: ", e$message, ". Falling back to sequential processing.")
    })
  }

  # Validate dictionary sources
  valid_sources <- c("local", "mesh", "umls", "pubmed")
  invalid_sources <- setdiff(dictionary_sources, valid_sources)
  if (length(invalid_sources) > 0) {
    warning("Invalid dictionary sources: ", paste(invalid_sources, collapse = ", "),
            ". Valid sources are: ", paste(valid_sources, collapse = ", "))
    dictionary_sources <- intersect(dictionary_sources, valid_sources)
  }

  # Check for UMLS API key if needed
  if ("umls" %in% dictionary_sources && is.null(api_key)) {
    warning("UMLS source requested but no API key provided. Skipping UMLS.")
    dictionary_sources <- setdiff(dictionary_sources, "umls")
  }

  # Define which entity types are supported by which sources
  local_dict_types <- c("disease", "drug", "gene")
  mesh_dict_types <- c(
    "disease", "drug", "gene", "protein", "chemical", "pathway",
    "anatomy", "organism", "biological_process", "cell", "tissue",
    "symptom", "diagnostic_procedure", "therapeutic_procedure",
    "phenotype", "molecular_function"
  )

  # For each entity type, determine which sources can provide it
  source_map <- list()
  for (etype in entity_types) {
    if (etype %in% local_dict_types) {
      source_map[[etype]] <- dictionary_sources
    } else {
      # For expanded types, remove 'local' from sources
      source_map[[etype]] <- setdiff(dictionary_sources, "local")
      if (length(source_map[[etype]]) == 0 && "local" %in% dictionary_sources) {
        # If only local was specified but not supported for this type, try mesh
        source_map[[etype]] <- c("mesh")
        if (verbose) {
          message("Entity type '", etype, "' not supported by local source. Using MeSH instead.")
        }
      }
    }
  }

  # Preprocess the text data if needed
  if (!"doc_id" %in% colnames(text_data)) {
    if (verbose) message("Adding document IDs to text data...")
    text_data$doc_id <- seq_len(nrow(text_data))
  }

  # Create combined dictionary
  if (verbose) message("Creating dictionaries for entity extraction...")
  combined_dict <- NULL

  # Add custom dictionary first if provided
  if (!is.null(custom_dictionary) && nrow(custom_dictionary) > 0) {
    if (verbose) message("Adding ", nrow(custom_dictionary), " terms from custom dictionary")
    # Validate custom dictionary format
    required_cols <- c("term", "type")
    if (!all(required_cols %in% colnames(custom_dictionary))) {
      stop("Custom dictionary must have at least columns: ", paste(required_cols, collapse = ", "))
    }

    # Ensure 'source' column exists
    if (!"source" %in% colnames(custom_dictionary)) {
      custom_dictionary$source <- "custom"
    }

    # Ensure 'id' column exists
    if (!"id" %in% colnames(custom_dictionary)) {
      custom_dictionary$id <- paste0("custom_", seq_len(nrow(custom_dictionary)))
    }

    combined_dict <- custom_dictionary
  }

  # Function to load dictionary for a single entity type
  load_dict_single <- function(entity_type) {
    tryCatch({
      # Get the sources that support this entity type
      valid_sources <- source_map[[entity_type]]

      if (length(valid_sources) == 0) {
        return(NULL)
      }

      # Try each source in order until successful
      dict <- NULL
      for (source in valid_sources) {
        # Check cache first if enabled
        if (cache_dictionaries) {
          cache_key <- create_cache_key(entity_type, source)
          if (exists(cache_key, envir = dict_cache)) {
            dict <- get(cache_key, envir = dict_cache)
            if (!is.null(dict) && nrow(dict) > 0) {
              if (verbose) message("  Using cached dictionary for ", entity_type, " (", source, ")")
              return(dict)
            }
          }
        }

        # Skip local source for expanded types
        if (source == "local" && !(entity_type %in% local_dict_types)) {
          next
        }

        # Check if we should use extended MeSH
        extended_mesh <- (source == "mesh" && !is.null(additional_mesh_queries))

        # Extract appropriate MeSH queries for this type
        mesh_queries_for_type <- NULL
        if (extended_mesh && entity_type %in% names(additional_mesh_queries)) {
          mesh_queries_for_type <- list()
          mesh_queries_for_type[[entity_type]] <- additional_mesh_queries[[entity_type]]
        }

        # Try to load the dictionary with error handling
        tryCatch({
          dict <- load_dictionary(
            dictionary_type = entity_type,
            source = source,
            api_key = api_key,
            n_terms = max_terms_per_type,
            extended_mesh = (source == "mesh" && !is.null(mesh_queries_for_type)),
            mesh_queries = mesh_queries_for_type,
            sanitize = FALSE  # We'll sanitize the combined dictionary later
          )

          # Store in cache if successful
          if (cache_dictionaries && !is.null(dict) && nrow(dict) > 0) {
            assign(cache_key, dict, envir = dict_cache)
          }

          # If successful, break the loop
          if (!is.null(dict) && nrow(dict) > 0) {
            if (verbose) message("  Added ", nrow(dict), " terms from ", entity_type, " (", source, ")")
            return(dict)
          }
        }, error = function(e) {
          if (verbose) message("  Error loading ", entity_type, " dictionary from ", source, ": ", e$message)
          return(NULL)
        })
      }

      # Return NULL if no dictionary was loaded
      return(dict)
    }, error = function(e) {
      if (verbose) message("  Error processing ", entity_type, ": ", e$message)
      return(NULL)
    })
  }

  # Load dictionaries (parallel or sequential)
  dict_list <- list()

  if (use_parallel) {
    if (verbose) message("Loading dictionaries in parallel...")
    # Create a parallel cluster
    cl <- parallel::makeCluster(num_cores)

    # Export necessary functions and data to the cluster
    parallel::clusterExport(cl, varlist = c("source_map", "local_dict_types",
                                            "additional_mesh_queries", "api_key",
                                            "max_terms_per_type", "verbose",
                                            "cache_dictionaries", "dict_cache"),
                            envir = environment())

    # Make sure load_dictionary is available in the cluster
    tryCatch({
      parallel::clusterEvalQ(cl, {
        # Load any required functions
        if (exists("load_dictionary", mode = "function")) {
          # Function is already available in global environment
        } else if (requireNamespace("LBDiscover", quietly = TRUE)) {
          # Try loading from the package
          load_dictionary <- LBDiscover::load_dictionary
        } else {
          # Function might be in the current environment
          # The calling environment's functions should be exported already
        }
      })

      # Use parLapply for cross-platform parallel execution
      dict_list <- parallel::parLapply(cl, entity_types, load_dict_single)
    }, error = function(e) {
      warning("Error in parallel dictionary loading: ", e$message,
              ". Falling back to sequential processing.")
      dict_list <- lapply(entity_types, load_dict_single)
    }, finally = {
      # Always stop the cluster
      parallel::stopCluster(cl)
      cl <- NULL
    })
  } else {
    # Sequential dictionary loading
    if (verbose) message("Loading dictionaries sequentially...")
    dict_list <- lapply(entity_types, load_dict_single)
  }

  # Combine dictionaries using base R
  for (dict in dict_list) {
    if (!is.null(dict) && nrow(dict) > 0) {
      if (is.null(combined_dict)) {
        combined_dict <- dict
      } else {
        # Use base R rbind for combining
        combined_dict <- rbind(combined_dict, dict)
      }
    }
  }

  # Check if combined dictionary was created
  if (is.null(combined_dict) || nrow(combined_dict) == 0) {
    warning("No valid dictionary terms found. Creating fallback dictionary.")

    # Create a minimal fallback dictionary
    combined_dict <- data.frame(
      term = c("disease", "drug", "gene", "protein", "chemical", "pathway"),
      id = paste0("FALLBACK_", 1:6),
      type = c("disease", "drug", "gene", "protein", "chemical", "pathway"),
      source = rep("fallback", 6),
      stringsAsFactors = FALSE
    )
  }

  # Remove duplicates with prioritization for custom entries
  if (!is.null(custom_dictionary) && nrow(custom_dictionary) > 0) {
    # Add is_custom column using base R
    combined_dict$is_custom <- combined_dict$source == "custom"

    # Sort the data frame to prioritize custom entries (TRUE first)
    combined_dict <- combined_dict[order(combined_dict$is_custom, decreasing = TRUE), ]

    # Keep first occurrence of each term (prioritizing custom entries)
    combined_dict <- combined_dict[!duplicated(combined_dict$term), ]

    # Remove the temporary is_custom column
    combined_dict$is_custom <- NULL
  } else {
    # Simple deduplication with base R
    combined_dict <- combined_dict[!duplicated(combined_dict$term), ]
  }

  if (verbose) message("Created combined dictionary with ", nrow(combined_dict), " unique terms")

  # Sanitize the dictionary if requested, but preserve custom dictionary entries
  if (sanitize) {
    # OPTIMIZATION: Only sanitize if more than 10,000 terms to avoid overhead for small dictionaries
    if (nrow(combined_dict) > 10000) {
      if (verbose) message("Large dictionary detected, applying optimized sanitization...")

      if (!is.null(custom_dictionary) && nrow(custom_dictionary) > 0) {
        # Separate custom entries from other entries
        custom_entries <- combined_dict[combined_dict$source == "custom", ]
        other_entries <- combined_dict[combined_dict$source != "custom", ]

        # Apply sanitization in chunks for large dictionaries
        if (nrow(other_entries) > 10000) {
          chunk_size <- 5000
          num_chunks <- ceiling(nrow(other_entries) / chunk_size)

          if (verbose) message("Processing in ", num_chunks, " chunks for efficient sanitization")

          sanitized_chunks <- list()
          for (i in 1:num_chunks) {
            start_idx <- (i - 1) * chunk_size + 1
            end_idx <- min(i * chunk_size, nrow(other_entries))
            if (verbose && i %% 5 == 0) message("  Sanitizing chunk ", i, "/", num_chunks)

            chunk <- other_entries[start_idx:end_idx, ]
            sanitized_chunks[[i]] <- sanitize_dictionary(chunk, verbose = FALSE)
          }

          # Combine sanitized chunks
          other_entries <- do.call(rbind, sanitized_chunks)
        } else {
          # Sanitize in one go for smaller dictionaries
          other_entries <- sanitize_dictionary(other_entries, verbose = verbose)
        }

        # Recombine
        combined_dict <- rbind(custom_entries, other_entries)
      } else {
        # No custom entries, sanitize everything
        combined_dict <- sanitize_dictionary(combined_dict, verbose = verbose)
      }
    } else {
      # Standard sanitization for reasonable-sized dictionaries
      if (!is.null(custom_dictionary) && nrow(custom_dictionary) > 0) {
        # Separate custom entries from other entries
        custom_entries <- combined_dict[combined_dict$source == "custom", ]
        other_entries <- combined_dict[combined_dict$source != "custom", ]

        # Sanitize only non-custom entries
        if (nrow(other_entries) > 0) {
          other_entries <- sanitize_dictionary(other_entries, verbose = verbose)
        }

        # Recombine
        combined_dict <- rbind(custom_entries, other_entries)
      } else {
        # No custom entries, sanitize everything
        combined_dict <- sanitize_dictionary(combined_dict, verbose = verbose)
      }
    }
  }

  # Set up indexing for large dictionaries using base R
  dict_size <- nrow(combined_dict)
  if (dict_size > 10000) {
    # Create a named vector for fast lookups
    if (verbose) message("Creating index for large dictionary with ", dict_size, " terms...")
    term_index <- seq_len(nrow(combined_dict))
    names(term_index) <- combined_dict$term
  }

  # Extract entities with proper error handling
  if (verbose) message("Extracting entities from ", nrow(text_data), " documents...")

  # Create smaller batches to avoid memory issues
  # Use dynamic batch sizing based on the number of documents and dictionary size
  adjusted_batch_size <- min(batch_size, ceiling(10000000 / (nrow(combined_dict) + 100)))
  final_batch_size <- max(100, min(adjusted_batch_size, nrow(text_data)))

  num_batches <- ceiling(nrow(text_data) / final_batch_size)

  if (verbose && num_batches > 1) {
    message("Processing in ", num_batches, " batches (", final_batch_size, " documents per batch) to optimize memory usage")
  }

  # Define process_batch function for both sequential and parallel processing
  process_batch <- function(batch_idx) {
    tryCatch({
      start_idx <- (batch_idx - 1) * final_batch_size + 1
      end_idx <- min(batch_idx * final_batch_size, nrow(text_data))

      # Extract entities for this batch
      batch_data <- text_data[start_idx:end_idx, ]

      # Extract entities
      batch_entities <- extract_entities(
        text_data = batch_data,
        text_column = text_column,
        dictionary = combined_dict,
        case_sensitive = FALSE,
        overlap_strategy = "priority",
        sanitize_dict = FALSE  # Already sanitized above
      )

      return(batch_entities)
    }, error = function(e) {
      warning("Error in entity extraction for batch ", batch_idx, ": ", e$message)
      # Return empty data frame with correct structure
      return(data.frame(
        doc_id = integer(),
        entity = character(),
        entity_type = character(),
        start_pos = integer(),
        end_pos = integer(),
        sentence = character(),
        frequency = integer(),
        stringsAsFactors = FALSE
      ))
    })
  }

  # Process batches (parallel or sequential)
  all_entities <- data.frame()

  if (use_parallel && num_batches > 1) {
    # Create a parallel cluster for batch processing
    if (verbose) message("Processing document batches in parallel with ", num_cores, " cores...")
    cl <- parallel::makeCluster(num_cores)

    # Export necessary functions and data to the cluster
    parallel::clusterExport(cl, varlist = c("text_data", "text_column", "combined_dict",
                                            "final_batch_size", "extract_entities"),
                            envir = environment())

    # Process batches in parallel
    batch_results <- tryCatch({
      parallel::parLapply(cl, 1:num_batches, process_batch)
    }, error = function(e) {
      warning("Error in parallel batch processing: ", e$message,
              ". Falling back to sequential processing.")
      lapply(1:num_batches, process_batch)
    }, finally = {
      # Always stop the cluster
      parallel::stopCluster(cl)
      cl <- NULL
    })

    # Combine results
    # Use do.call with rbind for base R approach
    non_empty_batches <- which(sapply(batch_results, nrow) > 0)
    if (length(non_empty_batches) > 0) {
      all_entities <- do.call(rbind, batch_results[non_empty_batches])
    } else {
      all_entities <- data.frame()
    }
  } else {
    # Process batches sequentially
    for (batch_idx in 1:num_batches) {
      if (verbose && (batch_idx %% 5 == 0 || batch_idx == 1 || batch_idx == num_batches)) {
        message("Processing batch ", batch_idx, "/", num_batches)
      }

      # Process this batch
      batch_entities <- process_batch(batch_idx)

      # Combine with previous results
      if (nrow(batch_entities) > 0) {
        if (nrow(all_entities) == 0) {
          all_entities <- batch_entities
        } else {
          # Use rbind for combining
          all_entities <- rbind(all_entities, batch_entities)
        }
      }

      # Clean up memory
      rm(batch_entities)
      gc(verbose = FALSE)
    }
  }

  # Clean up any lingering cluster
  if (!is.null(cl)) {
    tryCatch({
      parallel::stopCluster(cl)
    }, error = function(e) {
      # Ignore errors when stopping cluster
    })
  }

  # Return the results
  if (nrow(all_entities) == 0) {
    warning("No entities found in the text")
  } else {
    if (verbose) {
      end_time <- Sys.time()
      processing_time <- difftime(end_time, start_time, units = "mins")

      message("Extracted ", nrow(all_entities), " entity mentions in ",
              round(processing_time, 2), " minutes")

      # Count by type
      entity_types_count <- table(all_entities$entity_type)
      for (type in names(entity_types_count)) {
        message("  ", type, ": ", entity_types_count[type])
      }
    }
  }

  return(all_entities)
}

#' Create a term-document matrix from preprocessed text
#'
#' This function creates a term-document matrix from preprocessed text data.
#' It's a simplified version of create_tdm() for direct use in models.
#'
#' @param preprocessed_data A data frame with preprocessed text data.
#' @param min_df Minimum document frequency for a term to be included.
#' @param max_df Maximum document frequency (as a proportion) for a term to be included.
#'
#' @return A term-document matrix.
#' @export
#'
#' @examples
#' \dontrun{
#' preprocessed <- preprocess_text(article_data, text_column = "abstract")
#' tdm <- create_term_document_matrix(preprocessed)
#' }
create_term_document_matrix <- function(preprocessed_data, min_df = 2, max_df = 0.9) {
  # Check if terms column exists
  if (!"terms" %in% colnames(preprocessed_data)) {
    stop("Terms column not found in preprocessed data")
  }

  # Extract all unique terms
  all_terms <- unique(unlist(lapply(preprocessed_data$terms, function(df) {
    if (is.data.frame(df) && nrow(df) > 0) {
      return(df$word)
    } else {
      return(character(0))
    }
  })))

  # Create term-document matrix
  tdm <- matrix(0, nrow = length(all_terms), ncol = nrow(preprocessed_data))
  rownames(tdm) <- all_terms

  # Fill the term-document matrix
  for (i in 1:nrow(preprocessed_data)) {
    terms_df <- preprocessed_data$terms[[i]]
    if (is.data.frame(terms_df) && nrow(terms_df) > 0) {
      for (j in 1:nrow(terms_df)) {
        term <- terms_df$word[j]
        count <- terms_df$count[j]
        tdm[term, i] <- count
      }
    }
  }

  # Calculate document frequency
  doc_freq <- rowSums(tdm > 0)

  # Filter terms by document frequency
  min_doc_count <- min_df
  max_doc_count <- max_df * ncol(tdm)

  valid_terms <- which(doc_freq >= min_doc_count & doc_freq <= max_doc_count)

  if (length(valid_terms) == 0) {
    stop("No terms remain after filtering by document frequency")
  }

  # Subset matrix to include only valid terms
  tdm <- tdm[valid_terms, , drop = FALSE]

  return(tdm)
}
