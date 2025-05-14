# Example script for literature-based discovery applied to Alzheimer's research
# This script demonstrates alternative models from alternative_models.R

# 1. Define the primary term of interest for our analysis
primary_term <- "alzheimer"

# 2. Retrieve articles related to Alzheimer's research
alzheimer_articles <- pubmed_search(
  query = paste0(primary_term, " pathophysiology"),
  max_results = 1000
)

# 3. Retrieve articles about drugs and treatments
drug_articles <- pubmed_search(
  query = "neurodegenerative disease treatment OR cognitive impairment therapy OR dementia medication",
  max_results = 1000
)

# 4. Combine articles and remove duplicates
all_articles <- merge_results(alzheimer_articles, drug_articles)
cat("Retrieved", nrow(all_articles), "unique articles\n")

# 5. Extract variations of our primary term using the utility function
primary_term_variations <- get_term_vars(all_articles, primary_term)
cat("Found", length(primary_term_variations), "variations of", primary_term, "in the corpus:\n")
print(head(primary_term_variations, 10))

# 6. Preprocess text
preprocessed_articles <- preprocess_text(
  all_articles,
  text_column = "abstract",
  remove_stopwords = TRUE,
  min_word_length = 2  # Set min_word_length to capture short terms
)

# 7. Create a custom dictionary with all variations of our primary term
custom_dictionary <- data.frame(
  term = c(primary_term, primary_term_variations),
  type = rep("disease", length(primary_term_variations) + 1),
  id = paste0("CUSTOM_", 1:(length(primary_term_variations) + 1)),
  source = rep("custom", length(primary_term_variations) + 1),
  stringsAsFactors = FALSE
)

# 8. Define additional MeSH queries for extended dictionaries
mesh_queries <- list(
  "disease" = paste0(primary_term, " disease[MeSH] OR dementia[MeSH] OR cognitive disorders[MeSH]"),
  "protein" = "amyloid[MeSH] OR tau proteins[MeSH] OR receptors[MeSH]",
  "chemical" = "neurotransmitters[MeSH] OR neuroprotective agents[MeSH]",
  "pathway" = "signal transduction[MeSH] OR synaptic transmission[MeSH]",
  "drug" = "cholinesterase inhibitors[MeSH] OR memantine[MeSH] OR neuroprotective agents[MeSH]",
  "gene" = "genes[MeSH] OR apoe[MeSH] OR presenilin[MeSH]"
)

# 9. Sanitize the custom dictionary
custom_dictionary <- sanitize_dictionary(
  custom_dictionary,
  term_column = "term",
  type_column = "type",
  validate_types = FALSE
)

# 10. Extract entities using our custom dictionary
custom_entities <- extract_entities(
  preprocessed_articles,
  text_column = "abstract",
  dictionary = custom_dictionary,
  case_sensitive = FALSE,
  overlap_strategy = "priority",
  sanitize_dict = FALSE
)

# 11. Extract entities using the standard workflow with improved entity validation
standard_entities <- extract_entities_workflow(
  preprocessed_articles,
  text_column = "abstract",
  entity_types = c("disease", "drug", "gene", "protein", "pathway", "chemical"),
  dictionary_sources = c("local", "mesh"),
  additional_mesh_queries = mesh_queries,
  sanitize = TRUE,
  parallel = TRUE,
  num_cores = 4,
  batch_size = 500
)

# 12. Combine entity datasets using our utility function
entities <- merge_entities(
  custom_entities,
  standard_entities,
  primary_term
)

# 13. Filter entities to ensure only relevant biomedical terms are included
filtered_entities <- valid_entities(
  entities,
  primary_term,
  primary_term_variations,
  validation_function = is_valid_biomedical_entity
)

# 14. Create co-occurrence matrix with validated entities
co_matrix <- create_comat(
  filtered_entities,
  doc_id_col = "doc_id",
  entity_col = "entity",
  type_col = "entity_type",
  normalize = TRUE,
  normalization_method = "cosine"
)

# 15. Find our primary term in the co-occurrence matrix
a_term <- find_term(co_matrix, primary_term)

# 16. Create a term-document matrix for LSI model
tdm <- create_tdm(preprocessed_articles)

############################################
# 17. Apply the AnC model
############################################
cat("\n\nApplying AnC Model...\n")
anc_results <- anc_model(
  co_matrix,
  a_term = a_term,
  n_b_terms = 5,  # Consider top 5 B terms
  c_type = "drug",  # Focus on drugs as potential C terms
  min_score = 0.01,
  n_results = 100
)

# Sort by AnC score and take top results
if (nrow(anc_results) > 0) {
  anc_results <- anc_results[order(-anc_results$anc_score), ]
  cat("Found", nrow(anc_results), "connections using AnC model\n")
  cat("Top 5 AnC results:\n")
  print(head(anc_results[, c("a_term", "b_terms", "c_term", "anc_score")], 5))
} else {
  cat("No results found using AnC model. Trying with less stringent parameters...\n")
  anc_results <- anc_model(
    co_matrix,
    a_term = a_term,
    n_b_terms = 10,  # Increase number of B terms
    c_type = NULL,   # Remove type constraint
    min_score = 0.001,
    n_results = 100
  )

  if (nrow(anc_results) > 0) {
    anc_results <- anc_results[order(-anc_results$anc_score), ]
    cat("Found", nrow(anc_results), "connections using AnC model with relaxed parameters\n")
    cat("Top 5 AnC results:\n")
    print(head(anc_results[, c("a_term", "b_terms", "c_term", "anc_score")], 5))
  }
}

############################################
# 18. Apply the BITOLA model
############################################
cat("\n\nApplying BITOLA Model...\n")

# First, check if entity types are available in the co-matrix
has_entity_types <- !is.null(attr(co_matrix, "entity_types"))

if (has_entity_types) {
  bitola_results <- bitola_model(
    co_matrix,
    a_term = a_term,
    a_semantic_type = "disease",    # Source term is a disease
    c_semantic_type = "drug",       # Target terms are drugs
    min_score = 0.01,
    n_results = 100
  )

  if (nrow(bitola_results) > 0) {
    bitola_results <- bitola_results[order(-bitola_results$ranking_score), ]
    cat("Found", nrow(bitola_results), "connections using BITOLA model\n")
    cat("Top 5 BITOLA results:\n")
    print(head(bitola_results[, c("a_term", "c_term", "support", "bitola_score", "ranking_score")], 5))
  } else {
    cat("No results found using BITOLA model. Trying with less stringent parameters...\n")
    bitola_results <- bitola_model(
      co_matrix,
      a_term = a_term,
      a_semantic_type = "disease",
      c_semantic_type = NULL,  # Remove target type constraint
      min_score = 0.001,
      n_results = 100
    )

    if (nrow(bitola_results) > 0) {
      bitola_results <- bitola_results[order(-bitola_results$ranking_score), ]
      cat("Found", nrow(bitola_results), "connections using BITOLA model with relaxed parameters\n")
      cat("Top 5 BITOLA results:\n")
      print(head(bitola_results[, c("a_term", "c_term", "support", "bitola_score", "ranking_score")], 5))
    }
  }
} else {
  cat("Entity types not available in co-occurrence matrix. Cannot apply BITOLA model.\n")
  cat("Applying flexible ABC model instead...\n")

  # Apply regular ABC model as fallback
  bitola_results <- abc_model(
    co_matrix,
    a_term = a_term,
    min_score = 0.001,
    n_results = 100
  )

  if (nrow(bitola_results) > 0) {
    cat("Found", nrow(bitola_results), "connections using ABC model fallback\n")
    cat("Top 5 ABC results:\n")
    print(head(bitola_results[, c("a_term", "b_term", "c_term", "abc_score")], 5))
  }
}

############################################
# 19. Apply the LSI model
############################################
cat("\n\nApplying LSI Model...\n")

# First check if the TDM has enough terms and documents
if (ncol(tdm) > 0 && nrow(tdm) > 0) {
  lsi_results <- lsi_model(
    tdm,
    a_term = a_term,
    n_factors = 100,  # Number of latent factors
    n_results = 100
  )

  if (nrow(lsi_results) > 0) {
    lsi_results <- lsi_results[order(-lsi_results$lsi_similarity), ]
    cat("Found", nrow(lsi_results), "connections using LSI model\n")
    cat("Top 5 LSI results:\n")
    print(head(lsi_results[, c("a_term", "c_term", "lsi_similarity")], 5))
  } else {
    cat("No results found using LSI model. Trying with more factors...\n")
    lsi_results <- lsi_model(
      tdm,
      a_term = a_term,
      n_factors = 200,  # Increase number of factors
      n_results = 100
    )

    if (nrow(lsi_results) > 0) {
      lsi_results <- lsi_results[order(-lsi_results$lsi_similarity), ]
      cat("Found", nrow(lsi_results), "connections using LSI model with more factors\n")
      cat("Top 5 LSI results:\n")
      print(head(lsi_results[, c("a_term", "c_term", "lsi_similarity")], 5))
    }
  }
} else {
  cat("Term-document matrix is empty or insufficient. Cannot apply LSI model.\n")
}

############################################
# 20. Compare results from different models
############################################
cat("\n\nComparing results from different models...\n")

# Create a function to extract c_terms from different result formats
extract_c_terms <- function(results, model_type) {
  if (nrow(results) == 0) {
    return(character(0))
  }

  if (model_type == "anc") {
    return(results$c_term)
  } else if (model_type == "bitola") {
    if ("c_term" %in% colnames(results)) {
      return(results$c_term)
    } else if ("ranking_score" %in% colnames(results)) {
      # For aggregated BITOLA results
      return(results$c_term)
    } else {
      # Fallback to ABC format
      return(results$c_term)
    }
  } else if (model_type == "lsi") {
    return(results$c_term)
  } else {
    return(character(0))
  }
}

# Get C terms from each model
anc_c_terms <- extract_c_terms(anc_results, "anc")
bitola_c_terms <- extract_c_terms(bitola_results, "bitola")
lsi_c_terms <- extract_c_terms(lsi_results, "lsi")

# Find common C terms across models
common_anc_bitola <- intersect(anc_c_terms, bitola_c_terms)
common_anc_lsi <- intersect(anc_c_terms, lsi_c_terms)
common_bitola_lsi <- intersect(bitola_c_terms, lsi_c_terms)
common_all <- intersect(intersect(anc_c_terms, bitola_c_terms), lsi_c_terms)

cat("Number of unique C terms found by each model:\n")
cat("  AnC model:", length(unique(anc_c_terms)), "\n")
cat("  BITOLA model:", length(unique(bitola_c_terms)), "\n")
cat("  LSI model:", length(unique(lsi_c_terms)), "\n")

cat("\nNumber of C terms found by multiple models:\n")
cat("  Common between AnC and BITOLA:", length(common_anc_bitola), "\n")
cat("  Common between AnC and LSI:", length(common_anc_lsi), "\n")
cat("  Common between BITOLA and LSI:", length(common_bitola_lsi), "\n")
cat("  Common across all three models:", length(common_all), "\n")

if (length(common_all) > 0) {
  cat("\nC terms found by all three models:\n")
  print(common_all)
}

############################################
# 21. Evaluate top common results
############################################
if (length(common_all) > 0) {
  cat("\n\nEvaluating literature support for top common connections...\n")

  # Create a combined results data frame for common results
  common_results <- data.frame(
    a_term = rep(a_term, length(common_all)),
    c_term = common_all,
    stringsAsFactors = FALSE
  )

  # Evaluate literature support
  evaluation <- eval_evidence(
    common_results,
    max_results = min(5, length(common_all)),
    base_term = primary_term,
    max_articles = 5
  )
}

############################################
# 22. Create visualizations for AnC model results
############################################
if (nrow(anc_results) > 0) {
  cat("\n\nCreating visualizations for AnC model results...\n")

  # Convert AnC results to ABC-like format for visualization
  visualization_results <- data.frame(
    a_term = anc_results$a_term,
    b_term = sapply(strsplit(anc_results$b_terms, ", "), function(x) x[1]),  # Use first B term
    c_term = anc_results$c_term,
    a_b_score = NA,  # We don't have individual scores in AnC model
    b_c_score = NA,
    abc_score = anc_results$anc_score,
    stringsAsFactors = FALSE
  )

  # Add type information if available
  if ("a_type" %in% colnames(anc_results)) {
    visualization_results$a_type <- anc_results$a_type
    visualization_results$c_type <- anc_results$c_type

    # Add placeholder B type
    visualization_results$b_type <- "intermediate"
  }

  # Sort by score and take top results for visualization
  visualization_results <- visualization_results[order(-visualization_results$abc_score), ]
  top_for_viz <- min(15, nrow(visualization_results))
  viz_data <- head(visualization_results, top_for_viz)

  # Create heatmap visualization
  plot_heatmap(
    viz_data,
    output_file = "alzheimer_anc_heatmap.png",
    width = 1200,
    height = 900,
    top_n = top_for_viz,
    min_score = 0,  # No minimum score for visualization
    color_palette = "blues",
    show_entity_types = "a_type" %in% colnames(viz_data)
  )

  # Create network visualization
  plot_network(
    viz_data,
    output_file = "alzheimer_anc_network.png",
    width = 1200,
    height = 900,
    top_n = top_for_viz,
    min_score = 0,
    node_size_factor = 5,
    color_by = if ("a_type" %in% colnames(viz_data)) "type" else "role",
    title = "Alzheimer's Disease AnC Model Network",
    show_entity_types = "a_type" %in% colnames(viz_data),
    label_size = 1.0
  )

  # Create interactive HTML network visualization
  export_network(
    viz_data,
    output_file = "alzheimer_anc_network.html",
    top_n = top_for_viz,
    min_score = 0,
    open = FALSE
  )
}

############################################
# 23. Generate comprehensive report
############################################
cat("\n\nGenerating comprehensive report...\n")

# Prepare articles for report generation
articles_with_years <- prep_articles(all_articles)

# Store results from all models
results_list <- list()
if (nrow(anc_results) > 0) {
  results_list$anc <- anc_results
}
if (nrow(bitola_results) > 0) {
  results_list$bitola <- bitola_results
}
if (nrow(lsi_results) > 0) {
  results_list$lsi <- lsi_results
}

# Store visualization paths
visualizations <- list()
if (nrow(anc_results) > 0) {
  visualizations$heatmap <- "alzheimer_anc_heatmap.png"
  visualizations$network <- "alzheimer_anc_network.html"
}

# Create comprehensive report
gen_report(
  results_list = results_list,
  visualizations = visualizations,
  articles = articles_with_years,
  output_file = "alzheimer_alternatives_report.html"
)

cat("\nAlternative models discovery analysis complete!\n")

# Print summary of findings
cat("\n===========================================================\n")
cat("SUMMARY OF LITERATURE-BASED DISCOVERY USING ALTERNATIVE MODELS\n")
cat("===========================================================\n")
cat("Primary term:", primary_term, "\n")
cat("Total articles analyzed:", nrow(all_articles), "\n\n")

cat("FINDINGS BY MODEL:\n")
cat("------------------\n")
if (nrow(anc_results) > 0) {
  cat("AnC Model: Found", nrow(anc_results), "potential connections\n")
  cat("  Top result:", anc_results$c_term[1], "via B terms:", anc_results$b_terms[1], "\n")
}
if (nrow(bitola_results) > 0) {
  cat("BITOLA Model: Found", nrow(bitola_results), "potential connections\n")
  if ("c_term" %in% colnames(bitola_results)) {
    cat("  Top result:", bitola_results$c_term[1])
    if ("b_terms" %in% colnames(bitola_results)) {
      cat(" via B terms:", bitola_results$b_terms[1])
    }
    cat("\n")
  }
}
if (nrow(lsi_results) > 0) {
  cat("LSI Model: Found", nrow(lsi_results), "potential connections\n")
  cat("  Top result:", lsi_results$c_term[1], "with similarity score:", round(lsi_results$lsi_similarity[1], 4), "\n")
}
if (length(common_all) > 0) {
  cat("\nMost promising findings (found by all models):", paste(common_all, collapse=", "), "\n")
}

cat("\nAll results are available in the generated HTML report: alzheimer_alternatives_report.html\n")
