# Example usage of performance optimization functions

# Load required libraries
library(Matrix)  # For sparse matrices

#' This script demonstrates the usage of optimized functions
#' for literature-based discovery with large datasets

#------------------------------------------------------------------------------
# Section 1: Creating a Sparse Co-occurrence Matrix
#------------------------------------------------------------------------------

# Example: Let's create a sparse co-occurrence matrix from entity data

# First, simulate some entity data
create_sample_entity_data <- function(n_docs = 1000, n_entities = 500, sparsity = 0.01) {
  # Generate random entity occurrences
  n_occurrences <- ceiling(n_docs * n_entities * sparsity)

  # Random document IDs
  doc_ids <- sample(paste0("doc_", 1:n_docs), n_occurrences, replace = TRUE)

  # Random entity names
  entity_types <- c("disease", "drug", "gene", "protein", "chemical")
  entities <- sample(paste0("entity_", 1:n_entities), n_occurrences, replace = TRUE)

  # Random entity types
  types <- sample(entity_types, n_occurrences, replace = TRUE)

  # Create data frame
  entity_data <- data.frame(
    doc_id = doc_ids,
    entity = entities,
    entity_type = types,
    count = rpois(n_occurrences, lambda = 1) + 1,  # Random counts
    stringsAsFactors = FALSE
  )

  return(entity_data)
}

# Generate sample data
set.seed(42)  # For reproducibility
entity_data <- create_sample_entity_data()

cat("Created sample entity data with", nrow(entity_data), "occurrences\n")

# Now use the optimized sparse co-occurrence matrix function
cat("Creating sparse co-occurrence matrix...\n")
start_time <- Sys.time()

co_matrix <- create_sparse_comat(
  entity_data = entity_data,
  doc_id_col = "doc_id",
  entity_col = "entity",
  count_col = "count",
  type_col = "entity_type",
  normalize = TRUE
)

end_time <- Sys.time()
cat("Matrix creation completed in", difftime(end_time, start_time, units = "secs"), "seconds\n")
cat("Matrix dimensions:", nrow(co_matrix), "x", ncol(co_matrix), "\n")
cat("Matrix density:", 100 * Matrix::nnzero(co_matrix) / (nrow(co_matrix) * ncol(co_matrix)), "%\n")

#------------------------------------------------------------------------------
# Section 2: Vectorized Text Preprocessing
#------------------------------------------------------------------------------

# Example: Preprocess text data using vectorized operations

# Create sample text data
create_sample_text_data <- function(n_docs = 100) {
  # Sample abstracts (simulated biomedical text)
  abstract_templates <- c(
    "The study investigated the role of %s in patients with %s. Results showed significant effects on %s levels.",
    "A randomized trial of %s therapy for %s. The treatment showed efficacy in reducing %s symptoms.",
    "This paper examines the relationship between %s and %s. We found evidence suggesting %s may be a key factor.",
    "Clinical outcomes of %s administration in %s patients. The treatment resulted in improved %s markers.",
    "Molecular analysis of %s expression in %s tissue samples revealed correlation with %s activity."
  )

  # Sample terms to insert
  drugs <- c("aspirin", "ibuprofen", "metformin", "simvastatin", "atorvastatin", "lisinopril")
  diseases <- c("hypertension", "diabetes", "migraine", "arthritis", "asthma", "depression")
  biomarkers <- c("IL-6", "TNF-alpha", "CRP", "glucose", "cholesterol", "cortisol")

  # Generate abstracts
  abstracts <- character(n_docs)
  for (i in 1:n_docs) {
    template <- sample(abstract_templates, 1)
    drug <- sample(drugs, 1)
    disease <- sample(diseases, 1)
    biomarker <- sample(biomarkers, 1)

    abstracts[i] <- sprintf(template,
                            sample(c(drug, disease, biomarker), 1),
                            sample(c(drug, disease, biomarker), 1),
                            sample(c(drug, disease, biomarker), 1))
  }

  # Create data frame
  text_data <- data.frame(
    doc_id = 1:n_docs,
    title = paste("Sample Study", 1:n_docs),
    abstract = abstracts,
    stringsAsFactors = FALSE
  )

  return(text_data)
}

# Generate sample text data
text_data <- create_sample_text_data(n_docs = 200)

cat("Created sample text data with", nrow(text_data), "documents\n")

# Process text using optimized vectorized function
cat("Running vectorized text preprocessing...\n")
start_time <- Sys.time()

processed_data <- vec_preprocess(
  text_data = text_data,
  text_column = "abstract",
  remove_stopwords = TRUE,
  min_word_length = 3,
  chunk_size = 50  # Process in chunks of 50 documents
)

end_time <- Sys.time()
cat("Preprocessing completed in", difftime(end_time, start_time, units = "secs"), "seconds\n")

# Check unique terms extracted
all_terms <- unlist(lapply(processed_data$terms, function(df) df$word))
unique_terms <- unique(all_terms)

cat("Extracted", length(all_terms), "total terms,", length(unique_terms), "unique terms\n")

#------------------------------------------------------------------------------
# Section 3: Parallel Document Analysis
#------------------------------------------------------------------------------

# Example: Use parallel processing for document analysis

# Define a simple analysis function
count_entities <- function(text) {
  # Count mentions of diseases, drugs, and genes
  disease_patterns <- c("hypertension", "diabetes", "migraine", "arthritis", "asthma", "depression")
  drug_patterns <- c("aspirin", "ibuprofen", "metformin", "simvastatin", "atorvastatin", "lisinopril")
  gene_patterns <- c("IL-6", "TNF-alpha", "CRP", "glucose", "cholesterol", "cortisol")

  # Count occurrences
  disease_count <- sum(sapply(disease_patterns, function(p) grepl(p, text, ignore.case = TRUE)))
  drug_count <- sum(sapply(drug_patterns, function(p) grepl(p, text, ignore.case = TRUE)))
  gene_count <- sum(sapply(gene_patterns, function(p) grepl(p, text, ignore.case = TRUE)))

  return(list(
    disease_count = disease_count,
    drug_count = drug_count,
    gene_count = gene_count,
    total_count = disease_count + drug_count + gene_count
  ))
}

# Check if parallel package is available
has_parallel <- requireNamespace("parallel", quietly = TRUE)

if (has_parallel) {
  cat("Running parallel document analysis...\n")

  # Count number of cores available
  n_cores <- parallel::detectCores() - 1
  n_cores <- max(1, n_cores)  # Ensure at least 1 core

  cat("Using", n_cores, "cores for parallel processing\n")

  # Run the analysis in parallel
  start_time <- Sys.time()

  results <- parallel_analysis(
    text_data = text_data,
    analysis_function = count_entities,
    text_column = "abstract",
    n_cores = n_cores
  )

  end_time <- Sys.time()
  cat("Parallel analysis completed in", difftime(end_time, start_time, units = "secs"), "seconds\n")

  # Extract results
  entity_counts <- do.call(rbind, results$analysis_result)
  total_entities <- sum(sapply(entity_counts, sum))

  cat("Found a total of", total_entities, "entity mentions in the documents\n")
} else {
  cat("Parallel package not available. Skipping parallel analysis.\n")
}

#------------------------------------------------------------------------------
# Section 4: Optimized ABC Model for Large Matrices
#------------------------------------------------------------------------------

# Example: Apply the ABC model with optimization for large matrices

# First, select a term to use as "A" term
all_entities <- unique(entity_data$entity)
a_term <- all_entities[1]

cat("Applying optimized ABC model with A term:", a_term, "\n")

start_time <- Sys.time()

abc_results <- abc_model_opt(
  co_matrix = co_matrix,
  a_term = a_term,
  min_score = 0.1,
  n_results = 50,
  chunk_size = 100  # Process in chunks of 100 B terms
)

end_time <- Sys.time()
cat("ABC model completed in", difftime(end_time, start_time, units = "secs"), "seconds\n")

# Display top results
if (nrow(abc_results) > 0) {
  cat("Top ABC connections:\n")
  top_n <- min(5, nrow(abc_results))

  for (i in 1:top_n) {
    cat(sprintf("%d. %s -> %s -> %s (Score: %.4f)\n",
                i, abc_results$a_term[i], abc_results$b_term[i],
                abc_results$c_term[i], abc_results$abc_score[i]))
  }
} else {
  cat("No ABC connections found\n")
}

#------------------------------------------------------------------------------
# Section 5: Entity Extraction Workflow
#------------------------------------------------------------------------------

# Example: Extract entities from text using the optimized workflow with base R

# Create a small custom dictionary for demonstration
custom_dictionary <- data.frame(
  term = c("hypertension", "diabetes", "migraine", "aspirin", "ibuprofen", "IL-6"),
  type = c("disease", "disease", "disease", "drug", "drug", "gene"),
  id = paste0("custom_", 1:6),
  source = rep("custom", 6),
  stringsAsFactors = FALSE
)

# Use a subset of the text data
text_subset <- text_data[1:20, ]

cat("Extracting entities using optimized workflow\n")
start_time <- Sys.time()

# First we'll create a simplified version for demonstration that doesn't depend on extract_entities_workflow
simplified_entity_extraction <- function(text_data, text_column, custom_dictionary) {
  # Initialize results
  all_entities <- data.frame(
    doc_id = integer(),
    entity = character(),
    entity_type = character(),
    start_pos = integer(),
    end_pos = integer(),
    sentence = character(),
    frequency = integer(),
    stringsAsFactors = FALSE
  )

  # Process each document
  for (i in 1:nrow(text_data)) {
    doc_id <- text_data$doc_id[i]
    text <- text_data[[text_column]][i]

    # Skip empty text
    if (is.na(text) || text == "") next

    # Extract entities by checking for each dictionary term
    for (j in 1:nrow(custom_dictionary)) {
      term <- custom_dictionary$term[j]
      term_type <- custom_dictionary$type[j]

      # Find all matches with word boundaries
      matches <- gregexpr(paste0("\\b", term, "\\b"), text, ignore.case = TRUE)

      if (matches[[1]][1] != -1) {  # If matches found
        match_positions <- matches[[1]]
        match_lengths <- attr(matches[[1]], "match.length")

        for (k in 1:length(match_positions)) {
          start_pos <- match_positions[k]
          end_pos <- start_pos + match_lengths[k] - 1

          # Extract sentence context (simplified)
          sentence_start <- max(1, start_pos - 50)
          sentence_end <- min(nchar(text), end_pos + 50)
          sentence <- substr(text, sentence_start, sentence_end)

          # Add to results
          entity_row <- data.frame(
            doc_id = doc_id,
            entity = term,
            entity_type = term_type,
            start_pos = start_pos,
            end_pos = end_pos,
            sentence = sentence,
            frequency = 1,
            stringsAsFactors = FALSE
          )

          all_entities <- rbind(all_entities, entity_row)
        }
      }
    }
  }

  # Count entity frequencies
  if (nrow(all_entities) > 0) {
    # Create frequency table
    freq_table <- table(all_entities$entity, all_entities$entity_type)

    # Update frequency in results
    for (i in 1:nrow(all_entities)) {
      entity <- all_entities$entity[i]
      entity_type <- all_entities$entity_type[i]
      all_entities$frequency[i] <- freq_table[entity, entity_type]
    }
  }

  return(all_entities)
}

# Use the simplified function (in a real case, you would use the full extract_entities_workflow)
entities <- simplified_entity_extraction(
  text_data = text_subset,
  text_column = "abstract",
  custom_dictionary = custom_dictionary
)

end_time <- Sys.time()
cat("Entity extraction completed in", difftime(end_time, start_time, units = "secs"), "seconds\n")

# Summarize results
if (!is.null(entities) && nrow(entities) > 0) {
  # Count by entity type
  type_counts <- table(entities$entity_type)

  cat("Extracted entities by type:\n")
  for (type in names(type_counts)) {
    cat("  ", type, ":", type_counts[type], "\n")
  }

  # Most frequent entities
  entity_freq <- sort(table(entities$entity), decreasing = TRUE)

  cat("\nTop 5 most frequent entities:\n")
  for (i in 1:min(5, length(entity_freq))) {
    cat("  ", names(entity_freq)[i], ":", entity_freq[i], "\n")
  }
} else {
  cat("No entities were extracted\n")
}

#------------------------------------------------------------------------------
# Section 6: Complete LBD Workflow Example
#------------------------------------------------------------------------------

# This is a simplified workflow example that uses only base R
# for data manipulation - for demonstration purposes only

cat("\n== Simplified LBD Workflow Example ==\n")

# Step 1: Get the co-occurrence matrix (already created above)
cat("Step 1: Using co-occurrence matrix created earlier\n")

# Step 2: Validate a term for analysis
validate_term <- function(term) {
  # Hard-coded terms for demonstration
  valid_terms <- c("hypertension", "diabetes", "migraine", "aspirin", "ibuprofen")

  if (term %in% valid_terms) {
    return(TRUE)
  } else {
    similar_terms <- grep(term, valid_terms, value = TRUE)
    if (length(similar_terms) > 0) {
      cat("Term not found. Did you mean: ", paste(similar_terms, collapse = ", "), "?\n")
    }
    return(FALSE)
  }
}

# Step 3: Generate ABC connections
generate_connections <- function(term, max_results = 5) {
  # For demonstration, generate some dummy connections
  b_terms <- c("serotonin", "CGRP", "inflammation", "blood vessels", "cortical spreading depression")
  c_terms <- c("sumatriptan", "topiramate", "propranolol", "magnesium", "riboflavin")

  results <- data.frame(
    a_term = rep(term, length(b_terms)),
    b_term = b_terms,
    c_term = sample(c_terms, length(b_terms)),
    a_b_score = runif(length(b_terms), 0.3, 0.8),
    b_c_score = runif(length(b_terms), 0.3, 0.8),
    stringsAsFactors = FALSE
  )

  # Calculate ABC score as product
  results$abc_score <- results$a_b_score * results$b_c_score

  # Sort by score
  results <- results[order(-results$abc_score), ]

  # Limit to max_results
  if (nrow(results) > max_results) {
    results <- results[1:max_results, ]
  }

  return(results)
}

# Run a simple demo
demo_term <- "migraine"
cat("Running LBD demo for term:", demo_term, "\n")

if (validate_term(demo_term)) {
  cat("Term validated. Generating connections...\n")
  connections <- generate_connections(demo_term)

  cat("\nTop ABC connections for", demo_term, ":\n")
  for (i in 1:nrow(connections)) {
    cat(sprintf("%d. %s -> %s -> %s (Score: %.4f)\n",
                i, connections$a_term[i], connections$b_term[i],
                connections$c_term[i], connections$abc_score[i]))
  }
} else {
  cat("Invalid term. Please try again with a valid term.\n")
}

cat("\nPerformance optimization examples completed\n")

