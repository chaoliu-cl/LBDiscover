# Create sample test data
create_test_data <- function(n = 10) {
  # Create sample abstracts with known patterns for testing
  topics <- list(
    computer_science = c("algorithm", "computer", "data", "programming", "software"),
    biology = c("cell", "dna", "genome", "protein", "biology"),
    physics = c("energy", "quantum", "particle", "relativity", "physics")
  )

  set.seed(123) # For reproducibility

  # Function to generate a random abstract based on a topic
  generate_abstract <- function(topic) {
    topic_words <- topics[[topic]]
    other_words <- c("the", "and", "in", "of", "with", "to", "for", "a", "is", "on")

    # Generate a random number of sentences (3-5)
    num_sentences <- sample(3:5, 1)

    sentences <- vector("character", num_sentences)
    for (i in 1:num_sentences) {
      # For each sentence, include 1-3 topic words and 3-6 other words
      num_topic_words <- sample(1:3, 1)
      num_other_words <- sample(3:6, 1)

      topic_selection <- sample(topic_words, num_topic_words, replace = TRUE)
      other_selection <- sample(other_words, num_other_words, replace = TRUE)

      all_words <- c(topic_selection, other_selection)
      # Shuffle words
      all_words <- sample(all_words)

      # Create a sentence
      sentences[i] <- paste0(paste(all_words, collapse = " "), ".")
    }

    # Combine sentences into an abstract
    abstract <- paste(sentences, collapse = " ")
    return(abstract)
  }

  # Assign topics to articles
  topics_vec <- sample(names(topics), n, replace = TRUE)

  # Generate abstracts
  abstracts <- sapply(topics_vec, generate_abstract)

  # Create a data frame
  data.frame(
    doc_id = 1:n,
    title = paste("Article", 1:n),
    publication_year = sample(2010:2023, n, replace = TRUE),
    abstract = abstracts,
    topic = topics_vec,
    stringsAsFactors = FALSE
  )
}

# Test for vec_preprocess function
test_that("vec_preprocess handles text data correctly", {
  # Create test data
  test_data <- create_test_data(5)

  # Test preprocessing
  result <- vec_preprocess(test_data, text_column = "abstract")

  # Check that the function returns the expected structure
  expect_s3_class(result, "data.frame")
  expect_true("terms" %in% colnames(result))
  expect_equal(nrow(result), nrow(test_data))

  # Check that terms are properly extracted
  expect_true(all(sapply(result$terms, function(df) is.data.frame(df))))

  # Check handling of missing values
  test_data_na <- test_data
  test_data_na$abstract[1] <- NA
  result_na <- vec_preprocess(test_data_na, text_column = "abstract")
  expect_equal(nrow(result_na), nrow(test_data) - 1)

  # Check stopword removal
  result_with_stopwords <- vec_preprocess(test_data, remove_stopwords = FALSE)
  result_without_stopwords <- vec_preprocess(test_data, remove_stopwords = TRUE)

  # We would expect more unique terms with stopwords included
  total_unique_terms_with <- length(unique(unlist(lapply(result_with_stopwords$terms, function(df) df$word))))
  total_unique_terms_without <- length(unique(unlist(lapply(result_without_stopwords$terms, function(df) df$word))))

  expect_true(total_unique_terms_with >= total_unique_terms_without)

  # Test custom stopwords
  custom_stopword_list <- c("algorithm", "computer", "data")
  result_custom_stopwords <- vec_preprocess(test_data, custom_stopwords = custom_stopword_list)

  # None of the custom stopwords should appear in the processed terms
  all_words <- unique(unlist(lapply(result_custom_stopwords$terms, function(df) df$word)))
  expect_true(!any(custom_stopword_list %in% all_words))

  # Test word length filtering
  result_min_length <- vec_preprocess(test_data, min_word_length = 5)
  all_words_min_length <- unique(unlist(lapply(result_min_length$terms, function(df) df$word)))
  expect_true(all(nchar(all_words_min_length) >= 5))
})

# Test for calc_doc_sim function
test_that("calc_doc_sim calculates document similarity correctly", {
  # Create test data
  test_data <- create_test_data(8)

  # Calculate similarity matrix
  sim_matrix <- calc_doc_sim(test_data, text_column = "abstract")

  # Check that the function returns a matrix
  expect_type(sim_matrix, "double")
  expect_true(is.matrix(sim_matrix))

  # Check matrix dimensions
  expect_equal(nrow(sim_matrix), nrow(test_data))
  expect_equal(ncol(sim_matrix), nrow(test_data))

  # Check that diagonal values are 1 (document is identical to itself)
  diag_values <- diag(sim_matrix)
  expect_true(all(abs(diag_values - 1) < 1e-10))

  # Check that all values are in [0, 1]
  expect_true(all(sim_matrix >= 0 & sim_matrix <= 1))

  # Check that matrix is symmetric
  expect_true(isSymmetric(sim_matrix))

  # Check that documents with the same topic have higher similarity
  topic_groups <- split(1:nrow(test_data), test_data$topic)

  # Only check if there are at least two documents with the same topic
  for (group in topic_groups) {
    if (length(group) >= 2) {
      # Calculate average similarity within the group
      within_sim <- mean(sim_matrix[group, group][lower.tri(sim_matrix[group, group])])

      # Calculate average similarity between this group and other documents
      other_docs <- setdiff(1:nrow(test_data), group)
      if (length(other_docs) > 0) {
        between_sim <- mean(sim_matrix[group, other_docs])

        # Documents in the same topic should be more similar on average
        expect_true(within_sim >= between_sim)
      }
    }
  }
})

# Test for cluster_docs function
test_that("cluster_docs clusters documents correctly", {
  # Create test data with clear topic separation
  test_data <- create_test_data(15)

  # Set number of clusters to match number of topics
  n_clusters <- length(unique(test_data$topic))

  # Cluster documents
  clustered_data <- cluster_docs(test_data, text_column = "abstract",
                                 n_clusters = n_clusters,
                                 random_seed = 42)

  # Check that the function returns a data frame
  expect_s3_class(clustered_data, "data.frame")

  # Check that cluster assignments are added
  expect_true("cluster" %in% colnames(clustered_data))

  # Check that all documents are assigned to a cluster
  expect_true(all(!is.na(clustered_data$cluster)))

  # Check that cluster numbers are in the expected range
  expect_true(all(clustered_data$cluster >= 1 & clustered_data$cluster <= n_clusters))

  # Check that cluster_terms attribute is present
  expect_true(!is.null(attr(clustered_data, "cluster_terms")))

  # Check that cluster_summaries attribute is present
  expect_true(!is.null(attr(clustered_data, "cluster_summaries")))

  # Check that all clusters have assigned documents
  for (i in 1:n_clusters) {
    expect_true(any(clustered_data$cluster == i))
  }

  # Optional: Evaluate cluster quality using topic labels as ground truth
  # This assumes that the algorithm should ideally group documents by topic

  # For each cluster, find the dominant topic
  cluster_topic_counts <- lapply(1:n_clusters, function(cluster_id) {
    table(clustered_data$topic[clustered_data$cluster == cluster_id])
  })

  # Calculate purity (percentage of correctly clustered documents)
  correct_assignments <- sum(sapply(cluster_topic_counts, max))
  purity <- correct_assignments / nrow(clustered_data)

  # We expect purity to be better than random assignment
  expect_true(purity > 1/n_clusters)
})

# Test for find_similar_docs function
test_that("find_similar_docs finds similar documents correctly", {
  # Create test data
  test_data <- create_test_data(10)

  # Find similar documents for the first document
  target_id <- 1
  n_similar <- 3
  similar_docs <- find_similar_docs(test_data, doc_id = target_id,
                                    text_column = "abstract",
                                    n_similar = n_similar)

  # Check that the function returns a data frame
  expect_s3_class(similar_docs, "data.frame")

  # Check that the number of returned documents is correct
  expect_equal(nrow(similar_docs), n_similar)

  # Check that the target document is not in the results
  expect_true(!target_id %in% similar_docs$doc_id)

  # Check that similarity scores are in [0, 1]
  expect_true(all(similar_docs$similarity_score >= 0 &
                    similar_docs$similarity_score <= 1))

  # Check that the results are sorted by similarity score in descending order
  expect_true(all(diff(similar_docs$similarity_score) <= 0))

  # Check that additional metadata is included if available
  expect_true(all(c("title", "publication_year") %in% colnames(similar_docs)))

  # Test behavior with non-existent document ID
  expect_error(find_similar_docs(test_data, doc_id = 999))

  # Test behavior with documents having the same topic
  # Documents with the same topic should be more similar
  topic_of_target <- test_data$topic[test_data$doc_id == target_id]

  # Get topics of similar documents using base R instead of dplyr
  similar_doc_indices <- match(similar_docs$doc_id, test_data$doc_id)
  similar_topics <- test_data$topic[similar_doc_indices]

  # Count how many similar documents have the same topic
  same_topic_count <- sum(similar_topics == topic_of_target)

  # We would expect at least one of the similar documents to have the same topic
  # Note: This is a probabilistic test and might fail occasionally
  expect_true(same_topic_count > 0)
})
