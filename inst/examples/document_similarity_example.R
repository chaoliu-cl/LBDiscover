# Example script demonstrating document clustering and similarity analysis
# using functions from clustering_similarity.R

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(42)

#############################################################
# STEP 1: Simulate a dataset of medical research abstracts
#############################################################
# This simulates a dataset similar to what would be returned by pubmed_search()

create_simulated_dataset <- function(n_docs = 100) {
  # Create document IDs
  doc_ids <- 1:n_docs

  # Create publication years (between 2010 and 2023)
  years <- sample(2010:2023, n_docs, replace = TRUE)

  # Sample topics for our simulated abstracts
  topics <- c(
    "migraine treatment using magnesium supplements",
    "genetic factors in chronic migraine disorders",
    "neuroimaging studies of migraine patients",
    "cortical spreading depression in migraine",
    "migraine and cardiovascular risk factors",
    "behavioral therapies for chronic pain management",
    "triptans and serotonin receptors in migraine treatment",
    "hormonal influences on migraine frequency",
    "CGRP and migraine pathophysiology",
    "preventive treatments for episodic migraine"
  )

  # Generate simulated abstracts
  abstracts <- character(n_docs)
  for (i in 1:n_docs) {
    # Select 1-2 random topics for this abstract
    n_topics <- sample(1:2, 1)
    selected_topics <- sample(topics, n_topics)

    # Create a title based on the first selected topic
    title <- paste0("Research on ", selected_topics[1])

    # Generate abstract text
    abstract_parts <- c()

    # Introduction
    intro_templates <- c(
      "This study investigates %s in clinical settings.",
      "We examined the relationship between %s and patient outcomes.",
      "Recent advances in understanding %s are discussed in this paper.",
      "The role of %s remains controversial in current literature.",
      "This research explores novel approaches to %s in controlled trials."
    )
    intro <- sprintf(sample(intro_templates, 1), selected_topics[1])
    abstract_parts <- c(abstract_parts, intro)

    # Methods
    methods_templates <- c(
      "A cohort of patients was evaluated using standardized protocols.",
      "Double-blind randomized controlled trials were conducted over 12 months.",
      "We collected data from multiple clinical centers using validated instruments.",
      "Patients were monitored for adverse effects throughout the study period.",
      "Statistical analysis included multivariate regression models to control for confounding variables."
    )
    methods <- sample(methods_templates, 1)
    abstract_parts <- c(abstract_parts, methods)

    # If there's a second topic, add content about it
    if (n_topics > 1) {
      connection_templates <- c(
        "Additionally, we explored %s as a secondary outcome.",
        "The study also examined %s in a subset of participants.",
        "Secondary analysis focused on %s and its clinical implications.",
        "Further investigation into %s revealed interesting patterns.",
        "We also assessed the impact of %s on treatment efficacy."
      )
      connection <- sprintf(sample(connection_templates, 1), selected_topics[2])
      abstract_parts <- c(abstract_parts, connection)
    }

    # Results
    results_templates <- c(
      "Results showed significant improvements in primary endpoints.",
      "Our findings suggest a strong correlation between treatment and outcome measures.",
      "The data revealed unexpected patterns requiring further investigation.",
      "Statistical analysis demonstrated clinically relevant effects in treated groups.",
      "Preliminary results indicate promising therapeutic potential."
    )
    results <- sample(results_templates, 1)
    abstract_parts <- c(abstract_parts, results)

    # Conclusion
    conclusion_templates <- c(
      "This research provides new insights into treatment approaches.",
      "These findings may guide future clinical practice and research directions.",
      "More studies are needed to confirm these preliminary results.",
      "The study highlights the importance of personalized medicine approaches.",
      "Our work contributes to the growing body of evidence in this field."
    )
    conclusion <- sample(conclusion_templates, 1)
    abstract_parts <- c(abstract_parts, conclusion)

    # Combine all parts into a complete abstract
    abstracts[i] <- paste(abstract_parts, collapse = " ")
  }

  # Create titles based on selected topics
  titles <- sapply(1:n_docs, function(i) {
    prefix <- sample(c("Study of", "Research on", "Investigation into",
                       "Analysis of", "Evaluation of"), 1)
    topic_words <- unlist(strsplit(topics[sample(1:length(topics), 1)], " "))
    selected_words <- sample(topic_words, min(4, length(topic_words)))
    paste(prefix, paste(selected_words, collapse = " "))
  })

  # Create data frame with simulated data
  simulated_data <- data.frame(
    doc_id = doc_ids,
    pmid = paste0("PMC", 10000000 + doc_ids),
    title = titles,
    abstract = abstracts,
    publication_year = years,
    stringsAsFactors = FALSE
  )

  return(simulated_data)
}

# Generate simulated data
article_data <- create_simulated_dataset(n_docs = 50)

# Display a sample of the data
cat("Sample of simulated dataset:\n")
print(head(article_data[, c("doc_id", "title", "publication_year")], 3))
cat("\nSample abstract:\n")
cat(substr(article_data$abstract[1], 1, 300), "...\n\n")

#############################################################
# STEP 2: Calculate document similarity matrix
#############################################################
cat("Calculating document similarity matrix...\n")
sim_matrix <- calc_doc_sim(article_data, text_column = "abstract")

# Print dimensions of similarity matrix
cat("Similarity matrix dimensions:", dim(sim_matrix), "\n")

# Show a small subset of the similarity matrix
cat("Sample of similarity matrix (first 5x5):\n")
print(sim_matrix[1:5, 1:5])

#############################################################
# STEP 3: Cluster documents
#############################################################
cat("\nClustering documents using K-means...\n")
n_clusters <- 5
clustered_data <- cluster_docs(
  article_data,
  text_column = "abstract",
  n_clusters = n_clusters,
  min_term_freq = 2,
  max_doc_freq = 0.9
)

# Get cluster summaries
cluster_summaries <- attr(clustered_data, "cluster_summaries")

# Print cluster summaries
cat("Cluster summaries:\n")
for (summary in cluster_summaries) {
  cat(summary, "\n")
}

# Display distribution of documents across clusters
cluster_counts <- table(clustered_data$cluster)
cat("\nDistribution of documents across clusters:\n")
print(cluster_counts)

# Extract terms most characteristic of each cluster
cluster_terms <- attr(clustered_data, "cluster_terms")
cat("\nTop terms for Cluster 1:\n")
print(head(cluster_terms[[1]], 5))

#############################################################
# STEP 4: Find similar documents
#############################################################
# Choose a random document ID
target_doc_id <- sample(article_data$doc_id, 1)
cat("\nFinding documents similar to document ID:", target_doc_id, "\n")
cat("Title:", article_data$title[article_data$doc_id == target_doc_id], "\n")

# Find similar documents
similar_docs <- find_similar_docs(
  article_data,
  doc_id = target_doc_id,
  text_column = "abstract",
  n_similar = 5
)

# Display results
cat("\nMost similar documents:\n")
result_df <- merge(similar_docs, article_data[, c("doc_id", "title")], by = "doc_id")
result_df <- result_df[, c("doc_id", "similarity_score", "title.x", "title.y")]
print(result_df)

#############################################################
# STEP 5: Visualizations
#############################################################
cat("\nVisualizing clustering results...\n")

# Visualize cluster assignments using MDS (Multidimensional Scaling)
# This reduces the high-dimensional TF-IDF space to 2D for visualization
visualize_clusters <- function(tfidf_matrix, cluster_assignments) {
  # Calculate distance matrix from TF-IDF
  dist_matrix <- dist(tfidf_matrix)

  # Apply MDS to reduce to 2D
  mds_result <- cmdscale(dist_matrix, k = 2)

  # Create a data frame for plotting
  plot_data <- data.frame(
    x = mds_result[, 1],
    y = mds_result[, 2],
    cluster = as.factor(cluster_assignments)
  )

  # Plot scatter points, color by cluster
  plot(
    plot_data$x, plot_data$y,
    col = cluster_assignments,
    pch = 19,
    main = "Document Clusters Visualization",
    xlab = "MDS Dimension 1",
    ylab = "MDS Dimension 2"
  )

  # Add a legend
  legend("topright",
         legend = paste("Cluster", 1:length(unique(cluster_assignments))),
         col = 1:length(unique(cluster_assignments)),
         pch = 19)
}

# Get TFIDF matrix and cluster assignments from kmeans result
kmeans_result <- attr(clustered_data, "kmeans_result")
tfidf_matrix <- kmeans_result$centers[kmeans_result$cluster, ]

# For a real visualization, you would uncomment this:
# visualize_clusters(tfidf_matrix, clustered_data$cluster)
cat("Visualization code is included but commented out in this example script.\n")

#############################################################
# STEP 6: Example application - Identifying research trends
#############################################################
cat("\nIdentifying research trends by year...\n")

# Group documents by year and cluster
year_cluster_counts <- table(clustered_data$publication_year, clustered_data$cluster)

# Print the result
cat("Distribution of clusters by publication year:\n")
print(year_cluster_counts)

# Calculate percentage of documents in each cluster by year
year_totals <- rowSums(year_cluster_counts)
year_cluster_pct <- sweep(year_cluster_counts, 1, year_totals, "/") * 100

# Format for readability
year_cluster_pct_formatted <- apply(year_cluster_pct, c(1, 2), function(x) sprintf("%.1f%%", x))

cat("\nPercentage of documents in each cluster by year:\n")
print(year_cluster_pct_formatted)

cat("\nThis trend analysis could help identify how research topics (clusters) have evolved over time.\n")

#############################################################
# SUMMARY
#############################################################
cat("\nSUMMARY OF DEMONSTRATION:\n")
cat("1. Created a simulated dataset of", nrow(article_data), "medical research abstracts\n")
cat("2. Calculated document similarity using TF-IDF and cosine similarity\n")
cat("3. Clustered documents into", n_clusters, "groups using K-means\n")
cat("4. Identified top terms characterizing each cluster\n")
cat("5. Found similar documents for a target document\n")
cat("6. Demonstrated how to analyze research trends over time\n")
cat("\nThis script demonstrates how the clustering_similarity.R functions can be used to analyze\n")
cat("patterns and relationships in a corpus of scientific literature.\n")
