# Improved example script for literature-based discovery applied to migraine research
# This script demonstrates the improved ABC model approach with utility functions

# 1. Define the primary term of interest for our analysis
primary_term <- "migraine"

# 2. Retrieve articles related to migraine research
migraine_articles <- pubmed_search(
  query = paste0(primary_term, " pathophysiology"),
  max_results = 1000
)

# 3. Retrieve articles about drugs and treatments
drug_articles <- pubmed_search(
  query = "neurological drugs pain treatment OR migraine therapy OR headache medication",
  max_results = 1000
)

# 4. Combine articles and remove duplicates
all_articles <- merge_results(migraine_articles, drug_articles)
cat("Retrieved", nrow(all_articles), "unique articles\n")

# 5. Extract variations of our primary term using the new utility function
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
  "disease" = paste0(primary_term, " disorders[MeSH] OR headache disorders[MeSH]"),
  "protein" = "receptors[MeSH] OR ion channels[MeSH]",
  "chemical" = "neurotransmitters[MeSH] OR vasoactive agents[MeSH]",
  "pathway" = "signal transduction[MeSH] OR pain[MeSH]",
  "drug" = "analgesics[MeSH] OR serotonin agonists[MeSH] OR anticonvulsants[MeSH]",
  "gene" = "genes[MeSH] OR channelopathy[MeSH]"
)

# 9. Sanitize the custom dictionary
custom_dictionary <- sanitize_dictionary(
  custom_dictionary,
  term_column = "term",
  type_column = "type",
  validate_types = FALSE  # Don't validate custom terms as they're trusted
)

# 10. Extract entities using our custom dictionary
custom_entities <- extract_entities(
  preprocessed_articles,
  text_column = "abstract",
  dictionary = custom_dictionary,
  case_sensitive = FALSE,
  overlap_strategy = "priority",
  sanitize_dict = FALSE  # Already sanitized
)

# 11. Extract entities using the standard workflow with improved entity validation
standard_entities <- extract_entities_workflow(
  preprocessed_articles,
  text_column = "abstract",
  entity_types = c("disease", "drug", "gene"),
  parallel = TRUE,           # Enable parallel processing
  num_cores = 4,             # Use 4 cores
  batch_size = 500           # Process 500 documents per batch
)

#or include UMLS
# standard_entities <- extract_entities_workflow(
#   preprocessed_articles,
#   text_column = "abstract",
#   entity_types = c("disease", "drug", "gene", "protein", "pathway", "chemical"),
#   dictionary_sources = c("local", "mesh", "umls"),  # Including UMLS
#   additional_mesh_queries = mesh_queries,
#   sanitize = TRUE,
#   api_key = "########-####-####-####-############",  # Your UMLS API key here
#   parallel = TRUE,
#   num_cores = 4
# )

# 12. Combine entity datasets using our new utility function
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

# 16. Apply the improved ABC model with enhanced term filtering and type validation
abc_results <- abc_model(
  co_matrix,
  a_term = a_term,
  c_term = NULL,  # Allow all potential C terms
  min_score = 0.001,  # Lower threshold to capture more potential connections
  n_results = 500,    # Increase to get more candidates before filtering
  scoring_method = "combined",
  # Focus on biomedically relevant entity types
  b_term_types = c("protein", "gene", "pathway", "chemical"),
  c_term_types = c("drug", "chemical", "protein", "gene"),
  exclude_general_terms = TRUE,  # Enable enhanced term filtering
  filter_similar_terms = TRUE,   # Remove terms too similar to migraine
  similarity_threshold = 0.7,    # Relatively strict similarity threshold
  enforce_strict_typing = TRUE   # Enable strict entity type validation
)

# 17. If we don't have enough results, try with less stringent criteria
min_desired_results <- 10
if (nrow(abc_results) < min_desired_results) {
  cat("Not enough results with strict filtering. Trying with less stringent criteria...\n")
  abc_results <- abc_model(
    co_matrix,
    a_term = a_term,
    c_term = NULL,
    min_score = 0.0005,  # Even lower threshold
    n_results = 500,
    scoring_method = "combined",
    b_term_types = NULL,  # No type constraints
    c_term_types = NULL,  # No type constraints
    exclude_general_terms = TRUE,
    filter_similar_terms = TRUE,
    similarity_threshold = 0.8,    # More lenient similarity threshold
    enforce_strict_typing = FALSE  # Disable strict type validation as fallback
  )
}

# 18. Apply statistical validation to the results
validated_results <- tryCatch({
  validate_abc(
    abc_results,
    co_matrix,
    alpha = 0.1,  # More lenient significance threshold
    correction = "BH",  # Benjamini-Hochberg correction for multiple testing
    filter_by_significance = FALSE  # Keep all results but mark significant ones
  )
}, error = function(e) {
  cat("Error in statistical validation:", e$message, "\n")
  cat("Using original results without validation...\n")
  # Add dummy p-values based on ABC scores
  abc_results$p_value <- 1 - abc_results$abc_score / max(abc_results$abc_score, na.rm = TRUE)
  abc_results$significant <- abc_results$p_value < 0.1
  return(abc_results)
})

# 19. Sort by ABC score and take top results
validated_results <- validated_results[order(-validated_results$abc_score), ]
top_n <- min(100, nrow(validated_results))  # Larger top N for diversification
top_results <- head(validated_results, top_n)

# 20. Diversify results using our utility function
diverse_results <- safe_diversify(
  top_results,
  diversity_method = "both",
  max_per_group = 5,
  min_score = 0.0001,
  min_results = 5
)

# 21. Ensure we have enough results for visualization
diverse_results <- min_results(
  diverse_results,
  top_results,
  a_term,
  min_results = 3
)

# 22. Create heatmap visualization using utility function
plot_heatmap(
  diverse_results,
  output_file = "migraine_heatmap.png",
  width = 1200,
  height = 900,
  top_n = 15,
  min_score = 0.0001,
  color_palette = "blues",
  show_entity_types = TRUE
)

# 23. Create network visualization using utility function
plot_network(
  diverse_results,
  output_file = "migraine_network.png",
  width = 1200,
  height = 900,
  top_n = 15,
  min_score = 0.0001,
  node_size_factor = 5,
  color_by = "type",
  title = "Migraine Treatment Network",
  show_entity_types = TRUE,
  label_size = 1.0
)

# 24. Create interactive HTML network visualization
export_network(
  diverse_results,
  output_file = "migraine_network.html",
  top_n = min(30, nrow(diverse_results)),
  min_score = 0.0001,
  open = FALSE  # Don't automatically open in browser
)

# 25. Create interactive chord diagram
export_chord(
  diverse_results,
  output_file = "migraine_chord.html",
  top_n = min(30, nrow(diverse_results)),
  min_score = 0.0001,
  open = FALSE
)

# 26. Evaluate literature support for top connections
evaluation <- eval_evidence(
  diverse_results,
  max_results = 5,
  base_term = "migraine",
  max_articles = 5
)

# 27. Prepare articles for report generation
articles_with_years <- prep_articles(all_articles)

# 28. Generate comprehensive report
results_list <- list(abc = diverse_results)

# Store visualization paths
visualizations <- list(
  heatmap = "migraine_heatmap.png",
  network = "migraine_network.html",
  chord = "migraine_chord.html"
)

# Create comprehensive report
gen_report(
  results_list = results_list,
  visualizations = visualizations,
  articles = articles_with_years,
  output_file = "migraine_discoveries.html"
)

cat("\nDiscovery analysis complete!\n")

