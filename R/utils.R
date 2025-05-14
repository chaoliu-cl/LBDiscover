#' Convert a list of articles to a data frame
#'
#' This function converts a list of articles to a data frame.
#'
#' @param articles A list of articles, each containing metadata.
#'
#' @return A data frame containing article metadata.
#' @keywords internal
list_to_df <- function(articles) {
  # Check if the input is a list
  if (!is.list(articles)) {
    stop("Input must be a list")
  }

  # Check if the list is empty
  if (length(articles) == 0) {
    return(data.frame())
  }

  # Initialize an empty data frame
  result_df <- data.frame(
    pmid = character(),
    title = character(),
    abstract = character(),
    authors = character(),
    publication_year = character(),
    journal = character(),
    stringsAsFactors = FALSE
  )

  # Convert each article to a row in the data frame
  for (article in articles) {
    # Create a new row
    new_row <- data.frame(
      pmid = ifelse(is.null(article$pmid), NA_character_, article$pmid),
      title = ifelse(is.null(article$title), NA_character_, article$title),
      abstract = ifelse(is.null(article$abstract), NA_character_, article$abstract),
      authors = ifelse(is.null(article$authors), NA_character_, paste(article$authors, collapse = ", ")),
      publication_year = ifelse(is.null(article$publication_year), NA_character_, article$publication_year),
      journal = ifelse(is.null(article$journal), NA_character_, article$journal),
      stringsAsFactors = FALSE
    )

    # Append to the result
    result_df <- rbind(result_df, new_row)
  }

  return(result_df)
}

#' Save search results to a file
#'
#' This function saves search results to a file.
#'
#' @param results A data frame containing search results.
#' @param file_path File path to save the results.
#' @param format File format to use. One of "csv", "rds", or "xlsx".
#'
#' @return The file path (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' save_results(search_results, file_path = "search_results.csv")
#' }
save_results <- function(results, file_path, format = c("csv", "rds", "xlsx")) {
  # Match format argument
  format <- match.arg(format)

  # Get file extension from file_path
  ext <- tools::file_ext(file_path)

  # If extension doesn't match format, warn and adjust file_path
  if (ext != format) {
    warning("File extension does not match format argument. Using format: ", format)
    file_path <- paste0(tools::file_path_sans_ext(file_path), ".", format)
  }

  # Save the file in the appropriate format
  message("Saving results to: ", file_path)

  if (format == "csv") {
    # Ensure character columns stay as character
    for (col in names(results)) {
      if (is.character(results[[col]])) {
        results[[col]] <- as.character(results[[col]])
      }
    }
    utils::write.csv(results, file = file_path, row.names = FALSE)
  } else if (format == "rds") {
    saveRDS(results, file = file_path)
  } else if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("The openxlsx package is required for Excel format. Install it with: install.packages('openxlsx')")
    }
    openxlsx::write.xlsx(results, file = file_path)
  }

  # Return the file path invisibly
  invisible(file_path)
}

#' Load saved results from a file
#'
#' This function loads previously saved results from a file.
#'
#' @param file_path File path to load the results from.
#'
#' @return A data frame containing the loaded results.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- load_results("search_results.csv")
#' }
load_results <- function(file_path) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Get file extension
  ext <- tools::file_ext(file_path)

  # Load the file based on its extension
  if (ext == "csv") {
    results <- utils::read.csv(file_path, stringsAsFactors = FALSE)

    # Convert numeric IDs to character if they look like strings
    if ("pmid" %in% colnames(results)) {
      results$pmid <- as.character(results$pmid)
    }
  } else if (ext == "rds") {
    results <- readRDS(file_path)
  } else if (ext == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("The openxlsx package is required for Excel format. Install it with: install.packages('openxlsx')")
    }
    results <- openxlsx::read.xlsx(file_path)
  } else {
    stop("Unsupported file format: ", ext, ". Supported formats: csv, rds, xlsx")
  }

  return(results)
}

#' Merge multiple search results
#'
#' This function merges multiple search results into a single data frame.
#'
#' @param ... Data frames containing search results.
#' @param remove_duplicates Logical. If TRUE, removes duplicate articles.
#'
#' @return A merged data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' merged_results <- merge_results(results1, results2, results3)
#' }
merge_results <- function(..., remove_duplicates = TRUE) {
  # Get all data frames
  data_frames <- list(...)

  # Check if all inputs are data frames
  if (!all(sapply(data_frames, is.data.frame))) {
    stop("All inputs must be data frames")
  }

  # Check if any data frames are empty
  if (any(sapply(data_frames, nrow) == 0)) {
    warning("Some data frames are empty")
  }

  # Merge all data frames using rbind
  merged_df <- do.call(rbind, data_frames)

  # Remove duplicates if requested
  if (remove_duplicates && nrow(merged_df) > 0) {
    # Check if PMID column exists
    if ("pmid" %in% colnames(merged_df)) {
      # Remove duplicates based on PMID
      merged_df <- merged_df[!duplicated(merged_df$pmid), ]
    } else {
      # If no PMID, use title for deduplication
      if ("title" %in% colnames(merged_df)) {
        merged_df <- merged_df[!duplicated(merged_df$title), ]
      }
    }
  }

  return(merged_df)
}

#' Create a citation network from article data
#'
#' This function creates a citation network from article data.
#' Note: Currently a placeholder as it requires citation data not available through basic PubMed queries.
#'
#' @param article_data A data frame containing article data.
#' @param citation_data A data frame containing citation data (optional).
#'
#' @return An igraph object representing the citation network.
#' @export
#'
#' @examples
#' \dontrun{
#' network <- create_citation_net(article_data)
#' }
create_citation_net <- function(article_data, citation_data = NULL) {
  # Check for required packages
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("The igraph package is required. Install it with: install.packages('igraph')")
  }

  message("Note: Full citation network creation requires citation data not available through basic PubMed API.")
  message("This function currently creates a placeholder network based on available data.")

  # Check if article_data has required columns
  if (!all(c("pmid", "title") %in% colnames(article_data))) {
    stop("article_data must contain at least 'pmid' and 'title' columns")
  }

  # Create nodes from article data
  nodes <- article_data[, c("pmid", "title")]
  names(nodes) <- c("id", "label")
  nodes <- unique(nodes)

  # If citation_data is provided, use it to create edges
  if (!is.null(citation_data)) {
    # Check if citation_data has required columns
    if (!all(c("citing_pmid", "cited_pmid") %in% colnames(citation_data))) {
      stop("citation_data must contain 'citing_pmid' and 'cited_pmid' columns")
    }

    # Create edges from citation data
    edges <- citation_data[, c("citing_pmid", "cited_pmid")]
    names(edges) <- c("from", "to")

    # Filter for edges where both from and to are in the nodes
    edges <- edges[edges$from %in% nodes$id & edges$to %in% nodes$id, ]
    edges <- unique(edges)
  } else {
    # Create a placeholder network based on publication year if available
    if ("publication_year" %in% colnames(article_data)) {
      # Sort articles by publication year
      sorted_articles <- article_data[, c("pmid", "publication_year")]
      sorted_articles <- sorted_articles[order(sorted_articles$publication_year), ]

      # Create simple edges based on publication year proximity
      # This is just a placeholder approach
      edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)

      # Get unique years and create connections between articles in consecutive years
      years <- sort(unique(as.numeric(sorted_articles$publication_year)))

      if (length(years) > 1) {
        for (i in 1:(length(years)-1)) {
          current_year <- years[i]
          next_year <- years[i+1]

          current_articles <- sorted_articles$pmid[sorted_articles$publication_year == current_year]
          next_articles <- sorted_articles$pmid[sorted_articles$publication_year == next_year]

          # Connect some articles (just for demonstration)
          if (length(current_articles) > 0 && length(next_articles) > 0) {
            n_edges <- min(5, length(current_articles), length(next_articles))

            for (j in 1:n_edges) {
              edges <- rbind(edges, data.frame(
                from = next_articles[j],
                to = current_articles[j],
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      } else {
        message("Only one publication year found. Creating empty edge list.")
      }
    } else {
      # If no publication year, create an empty edge list
      edges <- data.frame(
        from = character(),
        to = character(),
        stringsAsFactors = FALSE
      )

      message("No citation data or publication year available. Creating empty network.")
    }
  }

  # Create igraph object
  network <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  return(network)
}

#' Calculate basic bibliometric statistics
#'
#' This function calculates basic bibliometric statistics from article data.
#'
#' @param article_data A data frame containing article data.
#' @param by_year Logical. If TRUE, calculates statistics by year.
#'
#' @return A list containing bibliometric statistics.
#' @export
#' @importFrom utils head
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' stats <- calc_bibliometrics(article_data)
#' }
calc_bibliometrics <- function(article_data, by_year = TRUE) {
  # Check if article_data is empty
  if (nrow(article_data) == 0) {
    stop("article_data is empty")
  }

  # Initialize results list
  stats <- list()

  # Basic statistics
  stats$total_articles <- nrow(article_data)

  # Check if journal column exists
  if ("journal" %in% colnames(article_data)) {
    # Top journals
    journal_table <- table(article_data$journal)
    journal_table <- sort(journal_table, decreasing = TRUE)
    journal_counts <- data.frame(
      journal = names(journal_table),
      n = as.numeric(journal_table),
      stringsAsFactors = FALSE
    )
    stats$top_journals <- head(journal_counts, 10)
  }

  # Check if authors column exists
  if ("authors" %in% colnames(article_data)) {
    # Extract individual authors
    authors <- unlist(strsplit(article_data$authors, ", "))

    # Count author occurrences
    author_counts <- table(authors)
    author_counts <- sort(author_counts, decreasing = TRUE)

    # Top authors - limit to 10 if there are more
    max_authors <- min(10, length(author_counts))
    if (max_authors > 0) {
      stats$top_authors <- head(author_counts, max_authors)
    } else {
      stats$top_authors <- author_counts # If there are less than 10, use all
    }
  }

  # Check if publication_year column exists
  if ("publication_year" %in% colnames(article_data) && by_year) {
    # Articles by year
    year_table <- table(article_data$publication_year)
    year_table <- sort(year_table, decreasing = TRUE)
    year_counts <- data.frame(
      publication_year = names(year_table),
      n = as.numeric(year_table),
      stringsAsFactors = FALSE
    )
    stats$articles_by_year <- year_counts
  }

  # Calculate additional metrics if possible

  # Average number of authors per paper
  if ("authors" %in% colnames(article_data)) {
    author_counts <- sapply(strsplit(article_data$authors, ", "), length)
    stats$avg_authors_per_paper <- mean(author_counts, na.rm = TRUE)
    stats$median_authors_per_paper <- median(author_counts, na.rm = TRUE)
  }

  # Word count statistics for abstracts
  if ("abstract" %in% colnames(article_data)) {
    # Remove NA abstracts
    abstracts <- article_data$abstract[!is.na(article_data$abstract)]

    if (length(abstracts) > 0) {
      # Count words in each abstract
      word_counts <- sapply(abstracts, function(x) {
        length(unlist(strsplit(x, "\\s+")))
      })

      stats$avg_abstract_length <- mean(word_counts, na.rm = TRUE)
      stats$median_abstract_length <- median(word_counts, na.rm = TRUE)
      stats$max_abstract_length <- max(word_counts, na.rm = TRUE)
      stats$min_abstract_length <- min(word_counts, na.rm = TRUE)
    }
  }

  return(stats)
}

#' Extract common terms from a corpus
#'
#' This function extracts and counts the most common terms in a corpus.
#'
#' @param article_data A data frame containing article data.
#' @param text_column Name of the column containing the text to analyze.
#' @param n Number of top terms to return.
#' @param remove_stopwords Logical. If TRUE, removes stopwords.
#' @param min_word_length Minimum word length to include.
#'
#' @return A data frame containing term counts.
#' @export
#'
#' @examples
#' \dontrun{
#' common_terms <- extract_terms(article_data, text_column = "abstract")
#' }
extract_terms <- function(article_data, text_column = "abstract",
                          n = 100, remove_stopwords = TRUE,
                          min_word_length = 3) {

  # Check if text column exists
  if (!text_column %in% colnames(article_data)) {
    stop("Text column '", text_column, "' not found in the data")
  }

  # Add ID column if not present
  if (!"doc_id" %in% colnames(article_data)) {
    article_data$doc_id <- seq_len(nrow(article_data))
  }

  # Define a list of common English stopwords
  stopword_list <- c(
    "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "from", "had",
    "has", "have", "he", "her", "his", "i", "in", "is", "it", "its", "of", "on",
    "or", "that", "the", "this", "to", "was", "were", "which", "with", "you"
  )

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
    words <- words[nchar(words) >= min_word_length]

    # Remove stopwords if requested
    if (remove_stopwords) {
      words <- words[!words %in% stopword_list]
    }

    return(words)
  }

  # Initialize a vector to store all words
  all_words <- character()

  # Process each document
  for (i in seq_len(nrow(article_data))) {
    text <- article_data[[text_column]][i]
    if (!is.na(text) && text != "") {
      # Tokenize the text
      words <- tokenize_text(text)

      # Add to all words
      all_words <- c(all_words, words)
    }
  }

  # Count term frequencies
  term_counts <- table(all_words)

  # Sort by frequency and convert to data frame
  term_counts_sorted <- sort(term_counts, decreasing = TRUE)

  # Limit to top n terms
  if (length(term_counts_sorted) > n) {
    term_counts_sorted <- term_counts_sorted[1:n]
  }

  # Convert to data frame
  result <- data.frame(
    word = names(term_counts_sorted),
    n = as.numeric(term_counts_sorted),
    stringsAsFactors = FALSE
  )

  return(result)
}

#' Compare term frequencies between two corpora
#'
#' This function compares term frequencies between two sets of articles.
#'
#' @param corpus1 First corpus (data frame).
#' @param corpus2 Second corpus (data frame).
#' @param text_column Name of the column containing the text to analyze.
#' @param corpus1_name Name for the first corpus in the output.
#' @param corpus2_name Name for the second corpus in the output.
#' @param n Number of top terms to return.
#' @param remove_stopwords Logical. If TRUE, removes stopwords.
#'
#' @return A data frame containing term frequency comparisons.
#' @export
#'
#' @examples
#' \dontrun{
#' comparison <- compare_terms(corpus1, corpus2,
#'                                       corpus1_name = "Migraine",
#'                                       corpus2_name = "Magnesium")
#' }
compare_terms <- function(corpus1, corpus2, text_column = "abstract",
                          corpus1_name = "Corpus1",
                          corpus2_name = "Corpus2",
                          n = 100, remove_stopwords = TRUE) {

  # Check if text column exists in both corpora
  if (!text_column %in% colnames(corpus1) || !text_column %in% colnames(corpus2)) {
    stop("Text column '", text_column, "' not found in one or both corpora")
  }

  # Define a list of common English stopwords
  stopword_list <- c(
    "a", "an", "and", "are", "as", "at", "be", "but", "by", "for", "from", "had",
    "has", "have", "he", "her", "his", "i", "in", "is", "it", "its", "of", "on",
    "or", "that", "the", "this", "to", "was", "were", "which", "with", "you"
  )

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

    # Remove stopwords if requested
    if (remove_stopwords) {
      words <- words[!words %in% stopword_list]
    }

    return(words)
  }

  # Function to count terms in a corpus
  count_corpus_terms <- function(corpus) {
    # Initialize a vector to store all words
    all_words <- character()

    # Process each document
    for (i in seq_len(nrow(corpus))) {
      text <- corpus[[text_column]][i]
      if (!is.na(text) && text != "") {
        # Tokenize the text
        words <- tokenize_text(text)

        # Add to all words
        all_words <- c(all_words, words)
      }
    }

    # Count term frequencies
    term_counts <- table(all_words)

    return(term_counts)
  }

  # Count terms in each corpus
  counts1 <- count_corpus_terms(corpus1)
  counts2 <- count_corpus_terms(corpus2)

  # Get all unique words from both corpora
  all_words <- unique(c(names(counts1), names(counts2)))

  # Create a data frame with all words and their counts in each corpus
  result <- data.frame(
    word = all_words,
    stringsAsFactors = FALSE
  )

  # Add counts for corpus1
  result[[corpus1_name]] <- sapply(result$word, function(w) {
    if (w %in% names(counts1)) counts1[w] else 0
  })

  # Add counts for corpus2
  result[[corpus2_name]] <- sapply(result$word, function(w) {
    if (w %in% names(counts2)) counts2[w] else 0
  })

  # Calculate total and ratio
  result$total <- result[[corpus1_name]] + result[[corpus2_name]]
  result$ratio <- (result[[corpus1_name]] + 0.5) / (result[[corpus2_name]] + 0.5)

  # Sort by total and limit to top n terms
  result <- result[order(-result$total), ]
  if (nrow(result) > n) {
    result <- result[1:n, ]
  }

  return(result)
}

#' Extract term variations from text corpus
#'
#' This function identifies variations of a primary term within a corpus of articles.
#'
#' @param articles A data frame containing article data with text columns
#' @param primary_term The primary term to find variations of
#' @param text_col Name of the column containing the text to search
#'
#' @return A character vector of unique term variations, sorted by length
#' @export
get_term_vars <- function(articles, primary_term, text_col = "abstract") {
  # Extract all occurrences of primary term with context
  variations <- character(0)

  for (i in 1:nrow(articles)) {
    abstract <- articles[[text_col]][i]
    if (!is.na(abstract) && grepl(primary_term, abstract, ignore.case = TRUE)) {
      # Find all occurrences with some surrounding context
      matches <- gregexpr(paste0("\\b\\w*", primary_term, "\\w*\\b"),
                          abstract, ignore.case = TRUE)

      if (matches[[1]][1] != -1) {
        terms <- regmatches(abstract, matches)[[1]]
        variations <- c(variations, terms)
      }
    }
  }

  # Remove duplicates and sort by length (shortest first)
  unique_variations <- unique(variations)
  return(unique_variations[order(nchar(unique_variations))])
}

#' Combine and deduplicate entity datasets
#'
#' This function combines custom and standard entity datasets, handling the case
#' where one or both might be empty, and removes duplicates.
#'
#' @param custom_entities Data frame of custom entities (can be NULL)
#' @param standard_entities Data frame of standard entities (can be NULL)
#' @param primary_term The primary term of interest
#' @param primary_type The entity type of the primary term (default: "disease")
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return A data frame of combined entities
#' @export
merge_entities <- function(custom_entities, standard_entities,
                           primary_term, primary_type = "disease",
                           verbose = TRUE) {
  # Check if both entity sets exist and have content
  if (!is.null(custom_entities) && nrow(custom_entities) > 0 &&
      !is.null(standard_entities) && nrow(standard_entities) > 0) {
    # Use rbind to combine both dataframes
    entities <- rbind(custom_entities, standard_entities)
    # Remove duplicates if needed
    entities <- entities[!duplicated(paste(entities$doc_id, entities$entity, entities$start_pos)), ]

    if (verbose) {
      cat("Combined", nrow(custom_entities), "custom entities with",
          nrow(standard_entities), "standard entities.\n")
    }
  } else if (!is.null(standard_entities) && nrow(standard_entities) > 0) {
    entities <- standard_entities
    if (verbose) cat("Using only standard entities (", nrow(entities), ").\n")
  } else if (!is.null(custom_entities) && nrow(custom_entities) > 0) {
    entities <- custom_entities
    if (verbose) cat("Using only custom entities (", nrow(entities), ").\n")
  } else {
    if (verbose) cat("WARNING: No entities extracted from either method!\n")
    # Create a minimal entity dataframe with just our primary term
    entities <- data.frame(
      doc_id = 1,
      entity = primary_term,
      entity_type = primary_type,
      start_pos = 1,
      end_pos = nchar(primary_term),
      sentence = primary_term,
      frequency = 1,
      stringsAsFactors = FALSE
    )
  }

  return(entities)
}

#' Filter entities to include only valid biomedical terms
#'
#' This function applies validation to ensure only legitimate biomedical entities
#' are included, while preserving trusted terms.
#'
#' @param entities Data frame of entities to filter
#' @param primary_term The primary term to trust
#' @param primary_term_variations Vector of variations of the primary term to trust
#' @param validation_function Function to validate entities (default: is_valid_biomedical_entity)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#' @param entity_col Name of the column containing entity names (default: "entity")
#' @param type_col Name of the column containing entity types (default: "entity_type")
#'
#' @return A data frame of filtered entities
#' @export
valid_entities <- function(entities, primary_term, primary_term_variations = NULL,
                           validation_function = NULL,
                           verbose = TRUE,
                           entity_col = "entity",
                           type_col = "entity_type") {
  if (nrow(entities) == 0) {
    return(entities)
  }

  # Verify that the required columns exist
  if (!entity_col %in% colnames(entities)) {
    stop("Entity column '", entity_col, "' not found in entities data frame")
  }
  if (!type_col %in% colnames(entities)) {
    stop("Type column '", type_col, "' not found in entities data frame")
  }

  # If validation_function is NULL, get the function from the package environment
  if (is.null(validation_function)) {
    # First try to get the function from the namespace
    if (exists("is_valid_biomedical_entity", envir = asNamespace("LBDiscover"))) {
      validation_function <- get("is_valid_biomedical_entity", envir = asNamespace("LBDiscover"))
    } else {
      # Fallback to a very simple validation (trust everything)
      validation_function <- function(term, type) TRUE
      warning("Validation function 'is_valid_biomedical_entity' not found, using simple validation.")
    }
  }

  # Store original count for reporting
  original_count <- nrow(entities)

  # Get unique entity-type pairs
  entity_type_pairs <- unique(entities[, c(entity_col, type_col)])

  # Apply validation function to each pair
  valid_rows <- sapply(1:nrow(entity_type_pairs), function(i) {
    term <- entity_type_pairs[[entity_col]][i]
    claimed_type <- entity_type_pairs[[type_col]][i]

    # Skip our primary term and its variations (they're trusted)
    if (term == primary_term || term %in% primary_term_variations) {
      return(TRUE)
    }

    # Apply validation function
    validation_function(term, claimed_type)
  })

  # Get valid entity-type pairs
  valid_pairs <- entity_type_pairs[valid_rows, ]

  # Filter the original entities dataframe
  filtered_entities <- merge(entities, valid_pairs, by = c(entity_col, type_col))

  if (verbose) {
    cat("Filtered from", original_count, "to", nrow(filtered_entities), "validated entities\n")
  }

  return(filtered_entities)
}

#' Find primary term in co-occurrence matrix
#'
#' This function verifies that the primary term exists in the co-occurrence matrix,
#' and if not, attempts to find a suitable variation.
#'
#' @param co_matrix The co-occurrence matrix
#' @param primary_term The primary term to find
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return The found term (either exact match or variation)
#' @export
find_term <- function(co_matrix, primary_term, verbose = TRUE) {
  matrix_terms <- rownames(co_matrix)

  # First try for exact match
  primary_term_matches <- matrix_terms[grep(paste0("^", primary_term, "$"), matrix_terms)]

  if (length(primary_term_matches) > 0) {
    if (verbose) cat("Found primary term in co-occurrence matrix\n")
    return(primary_term_matches[1])  # Use the exact primary term
  } else {
    # If our exact term is missing, look for variations
    primary_term_var_matches <- matrix_terms[grep(primary_term, matrix_terms, ignore.case = TRUE)]

    if (length(primary_term_var_matches) > 0) {
      if (verbose) {
        cat("Primary term not found exactly, but found variations:\n")
        print(primary_term_var_matches)
      }
      return(primary_term_var_matches[1])  # Use the first matching variation
    } else {
      stop("Primary term and variations missing from co-occurrence matrix!")
    }
  }
}

#' Diversify ABC results with error handling
#'
#' This function diversifies ABC results to avoid redundancy, with error handling
#' to ensure results are always returned.
#'
#' @param top_results The top ABC results to diversify
#' @param diversity_method Method for diversification (default: "both")
#' @param max_per_group Maximum results per group (default: 5)
#' @param min_score Minimum score threshold (default: 0.0001)
#' @param min_results Minimum number of desired results (default: 5)
#' @param fallback_count Number of top results to use if diversification fails (default: 15)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return A data frame of diversified results
#' @export
safe_diversify <- function(top_results, diversity_method = "both",
                           max_per_group = 5, min_score = 0.0001,
                           min_results = 5, fallback_count = 15,
                           verbose = TRUE) {
  # Safely diversify results with error handling
  diverse_results <- tryCatch({
    result <- diversify_abc(
      top_results,
      diversity_method = diversity_method,
      max_per_group = max_per_group,
      min_score = min_score
    )

    # Check if we have enough results
    if (nrow(result) < min_results && nrow(top_results) > 0) {
      if (verbose) cat("Not enough diverse results, using top results directly\n")
      return(head(top_results, fallback_count))
    }
    return(result)
  }, error = function(e) {
    if (verbose) {
      cat("Error in diversification:", e$message, "\n")
      cat("Using top results directly...\n")
    }
    return(head(top_results, fallback_count))
  })

  return(diverse_results)
}

#' Ensure minimum results for visualization
#'
#' This function ensures there are sufficient results for visualization,
#' creating placeholder data if necessary.
#'
#' @param diverse_results Current diversified results
#' @param top_results Original top results
#' @param a_term The primary term for the analysis
#' @param min_results Minimum number of desired results (default: 3)
#' @param fallback_count Number of top results to use as fallback (default: 15)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return A data frame with sufficient results for visualization
#' @export
min_results <- function(diverse_results, top_results, a_term,
                        min_results = 3, fallback_count = 15,
                        verbose = TRUE) {
  # Check if we have enough results
  if (nrow(diverse_results) < min_results && nrow(top_results) > min_results) {
    if (verbose) cat("Too few diverse results, using top results directly\n")
    return(head(top_results, fallback_count))
  } else if (nrow(diverse_results) == 0) {
    if (verbose) cat("No results found. Creating a placeholder result for demonstration.\n")

    # Create a placeholder result
    placeholder <- data.frame(
      a_term = a_term,
      b_term = c("serotonin", "CGRP", "cortical spreading depression"),
      c_term = c("sumatriptan", "topiramate", "propranolol"),
      a_b_score = c(0.05, 0.04, 0.03),
      b_c_score = c(0.08, 0.07, 0.06),
      abc_score = c(0.04, 0.03, 0.02),
      p_value = c(0.1, 0.2, 0.3),
      significant = c(FALSE, FALSE, FALSE),
      stringsAsFactors = FALSE
    )

    # Add type information if available in original results
    if (!is.null(top_results) && nrow(top_results) > 0 && "a_type" %in% colnames(top_results)) {
      placeholder$a_type <- rep("disease", nrow(placeholder))
      placeholder$b_type <- c("chemical", "protein", "biological_process")
      placeholder$c_type <- c("drug", "drug", "drug")
    }

    return(placeholder)
  }

  # Return the original results if they're adequate
  return(diverse_results)
}

#' Create heatmap visualization from results
#'
#' This function creates a heatmap visualization from ABC results.
#'
#' @param results The results to visualize
#' @param output_file Filename for the output PNG (default: "heatmap.png")
#' @param width Width of the output image (default: 1200)
#' @param height Height of the output image (default: 900)
#' @param resolution Resolution of the output image (default: 120)
#' @param top_n Maximum number of results to include (default: 15)
#' @param min_score Minimum score threshold (default: 0.0001)
#' @param color_palette Color palette for the heatmap (default: "blues")
#' @param show_entity_types Logical; if TRUE, show entity types (default: TRUE)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return Invisible NULL (creates a file as a side effect)
#' @export
plot_heatmap <- function(results, output_file = "heatmap.png",
                         width = 1200, height = 900, resolution = 120,
                         top_n = 15, min_score = 0.0001,
                         color_palette = "blues",
                         show_entity_types = TRUE,
                         verbose = TRUE) {
  if (!requireNamespace("graphics", quietly = TRUE)) {
    warning("graphics package required for heatmap visualization.")
    return(invisible(NULL))
  }

  # Create the PNG file
  png(output_file, width = width, height = height, res = resolution)

  # Generate the heatmap
  vis_heatmap(
    results,
    top_n = min(top_n, nrow(results)),
    min_score = min_score,
    show_significance = "significant" %in% colnames(results),
    color_palette = color_palette,
    show_entity_types = show_entity_types
  )

  # Close the PNG device
  dev.off()

  if (verbose) cat("Created heatmap visualization:", output_file, "\n")

  return(invisible(NULL))
}

#' Create network visualization from results
#'
#' This function creates a network visualization from ABC results.
#'
#' @param results The results to visualize
#' @param output_file Filename for the output PNG (default: "network.png")
#' @param width Width of the output image (default: 1200)
#' @param height Height of the output image (default: 900)
#' @param resolution Resolution of the output image (default: 120)
#' @param top_n Maximum number of results to include (default: 15)
#' @param min_score Minimum score threshold (default: 0.0001)
#' @param node_size_factor Factor for scaling node sizes (default: 5)
#' @param color_by Column to use for node colors (default: "type")
#' @param title Plot title (default: "Network Visualization")
#' @param show_entity_types Logical; if TRUE, show entity types (default: TRUE)
#' @param label_size Relative size for labels (default: 1.0)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return Invisible NULL (creates a file as a side effect)
#' @export
plot_network <- function(results, output_file = "network.png",
                         width = 1200, height = 900, resolution = 120,
                         top_n = 15, min_score = 0.0001,
                         node_size_factor = 5, color_by = "type",
                         title = "Network Visualization",
                         show_entity_types = TRUE, label_size = 1.0,
                         verbose = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    warning("igraph package required for network visualization.")
    return(invisible(NULL))
  }

  # Create the PNG file
  png(output_file, width = width, height = height, res = resolution)

  # Generate the network visualization
  vis_network(
    results,
    top_n = min(top_n, nrow(results)),
    min_score = min_score,
    show_significance = "significant" %in% colnames(results),
    node_size_factor = node_size_factor,
    color_by = color_by,
    title = title,
    show_entity_types = show_entity_types,
    label_size = label_size
  )

  # Close the PNG device
  dev.off()

  if (verbose) cat("Created network visualization:", output_file, "\n")

  return(invisible(NULL))
}

#' Evaluate literature support for discovery results
#'
#' This function evaluates the top results by searching for supporting evidence
#' in the literature for the connections.
#'
#' @param results The results to evaluate
#' @param max_results Maximum number of results to evaluate (default: 5)
#' @param base_term The base term for direct connection queries (e.g., "migraine")
#' @param max_articles Maximum number of articles to retrieve per search (default: 5)
#' @param verbose Logical; if TRUE, print evaluation results (default: TRUE)
#'
#' @return A list containing evaluation results
#' @export
eval_evidence <- function(results, max_results = 5, base_term = NULL,
                          max_articles = 5, verbose = TRUE) {
  # If base_term is NULL, try to use the a_term from the first row
  if (is.null(base_term) && nrow(results) > 0 && "a_term" %in% colnames(results)) {
    base_term <- results$a_term[1]
  }

  # Limit to max_results
  eval_results <- head(results, min(max_results, nrow(results)))

  # Initialize the evaluation results list
  evaluation <- list()

  if (verbose) cat("\n=== Evaluation of Top Results ===\n")

  # Loop through the results to evaluate
  for (i in 1:nrow(eval_results)) {
    c_term <- eval_results$c_term[i]
    b_term <- eval_results$b_term[i]

    # Get entity type info if available
    c_type_info <- if ("c_type" %in% colnames(eval_results))
      paste0(" (", eval_results$c_type[i], ")") else ""

    if (verbose) {
      cat("\nEvaluating potential treatment:", c_term, c_type_info, "\n")
      cat("ABC score:", round(eval_results$abc_score[i], 4), "\n")

      # Add significance info if available
      if ("significant" %in% colnames(eval_results)) {
        sig_status <- if (eval_results$significant[i]) "Statistically significant" else "Not statistically significant"
        if ("p_value" %in% colnames(eval_results)) {
          cat("P-value:", round(eval_results$p_value[i], 4), "-", sig_status, "\n")
        } else {
          cat(sig_status, "\n")
        }
      }

      cat("Connection through intermediary:", b_term, "\n")
    }

    # Search for direct connections
    direct_query <- paste0(base_term, " AND ", c_term)
    direct_results <- pubmed_search(direct_query, max_results = max_articles)

    # Store results for this evaluation
    eval_item <- list(
      c_term = c_term,
      c_type = if ("c_type" %in% colnames(eval_results)) eval_results$c_type[i] else NA,
      b_term = b_term,
      b_type = if ("b_type" %in% colnames(eval_results)) eval_results$b_type[i] else NA,
      abc_score = eval_results$abc_score[i],
      p_value = if ("p_value" %in% colnames(eval_results)) eval_results$p_value[i] else NA,
      significant = if ("significant" %in% colnames(eval_results)) eval_results$significant[i] else NA,
      direct_evidence = list(
        found = nrow(direct_results) > 0,
        count = nrow(direct_results),
        articles = direct_results
      )
    )

    if (verbose) {
      if (nrow(direct_results) > 0) {
        cat("Found", nrow(direct_results), "articles directly linking", base_term, "and", c_term, "\n")
        cat("Most recent article:", direct_results$title[1], "\n")
      } else {
        cat("No direct evidence found - this may be a novel connection!\n")
      }
    }

    # If no direct evidence, check for B term connection
    if (nrow(direct_results) == 0) {
      b_query <- paste0(b_term, " AND ", c_term)
      b_results <- pubmed_search(b_query, max_results = max_articles)

      # Add B term connection evidence
      eval_item$b_term_evidence <- list(
        found = nrow(b_results) > 0,
        count = nrow(b_results),
        articles = b_results
      )

      if (verbose) {
        cat("Checking for mechanism through:", b_term, "\n")
        if (nrow(b_results) > 0) {
          cat("Found supporting evidence for", b_term, "and", c_term, "connection\n")
        } else {
          cat("No supporting evidence found for the proposed mechanism\n")
        }
      }
    }

    # Add this evaluation to the list
    evaluation[[i]] <- eval_item
  }

  # Add names to the evaluation list
  names(evaluation) <- paste0("result_", seq_along(evaluation))

  return(evaluation)
}

#' Prepare articles for report generation
#'
#' This function ensures article data is valid for report generation,
#' particularly handling publication years.
#'
#' @param articles The article data frame (can be NULL)
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return A data frame of articles with validated publication years
#' @export
prep_articles <- function(articles, verbose = TRUE) {
  # Return NULL if input is NULL
  if (is.null(articles)) {
    return(NULL)
  }

  # Check if publication_year exists
  if ("publication_year" %in% colnames(articles)) {
    # Convert to numeric
    articles$publication_year <- suppressWarnings(as.numeric(articles$publication_year))

    # Count valid years
    valid_years <- sum(!is.na(articles$publication_year))

    if (verbose) {
      cat("Found", valid_years, "articles with valid publication years\n")
    }

    # Use only articles with valid years
    articles_with_years <- articles[!is.na(articles$publication_year), ]
    return(articles_with_years)
  } else {
    # Return original articles if no publication_year column
    return(articles)
  }
}

#' Generate comprehensive discovery report
#'
#' This function creates a comprehensive HTML report from discovery results
#' and visualizations.
#'
#' @param results_list A list of result data frames from different approaches
#' @param visualizations A list with paths to visualization files
#' @param articles Prepared article data
#' @param output_file Filename for the output HTML report
#' @param verbose Logical; if TRUE, print status messages (default: TRUE)
#'
#' @return Invisible output_file path
#' @export
gen_report <- function(results_list, visualizations = NULL,
                       articles = NULL, output_file = "discoveries.html",
                       verbose = TRUE) {
  # Create the report
  create_report(
    results = results_list,
    visualizations = visualizations,
    articles = articles,
    output_file = output_file
  )

  if (verbose) {
    cat("Generated comprehensive report:", output_file, "\n")
  }

  return(invisible(output_file))
}
