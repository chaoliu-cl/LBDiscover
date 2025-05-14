#' Environment to store PubMed cache data
#' @keywords internal
.pubmed_cache_env <- new.env(parent = emptyenv())

#' Get the pubmed cache environment
#' @return An environment containing cached PubMed data
#' @keywords internal
get_pubmed_cache <- function() {
  .pubmed_cache_env
}

#' Search PubMed for articles with optimized performance
#'
#' This function searches PubMed using the NCBI E-utilities API via the rentrez package.
#' The implementation includes optimizations for speed, memory efficiency, and reliability.
#'
#' @param query Character string containing the search query.
#' @param max_results Maximum number of results to return.
#' @param use_mesh Logical. If TRUE, will attempt to map query terms to MeSH terms.
#' @param date_range Character vector of length 2 with start and end dates in format "YYYY/MM/DD".
#' @param api_key Character string. NCBI API key for higher rate limits (optional).
#' @param batch_size Integer. Number of records to fetch in each batch (default: 200).
#' @param verbose Logical. If TRUE, prints progress information.
#' @param use_cache Logical. If TRUE, cache results to avoid redundant API calls.
#' @param retry_count Integer. Number of times to retry failed API calls.
#' @param retry_delay Integer. Initial delay between retries in seconds.
#'
#' @return A data frame containing the search results with PubMed IDs, titles, and other metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- pubmed_search("migraine headache", max_results = 100)
#' }
pubmed_search <- function(query, max_results = 1000, use_mesh = FALSE,
                          date_range = NULL, api_key = NULL,
                          batch_size = 200, verbose = TRUE,
                          use_cache = TRUE, retry_count = 3,
                          retry_delay = 1) {
  # Check for rentrez package
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    stop("The rentrez package is required for PubMed searches. Install it with: install.packages('rentrez')")
  }

  # Check for xml2 package
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("The xml2 package is required for parsing PubMed data. Install it with: install.packages('xml2')")
  }

  # Set up caching environment if caching is enabled
  if (use_cache) {
    if (verbose) message("Created pubmed_cache environment for result caching")
  }

  # Generate a cache key based on search parameters
  cache_key <- NULL
  if (use_cache) {
    if (!requireNamespace("digest", quietly = TRUE)) {
      warning("The digest package is required for caching. Installing with: install.packages('digest')")
    }

    cache_components <- list(
      query = query,
      max_results = max_results,
      use_mesh = use_mesh,
      date_range = date_range
    )
    cache_key <- digest::digest(cache_components, algo = "md5")

    # Check if results exist in cache
    cache_env <- get_pubmed_cache()
    if (exists(cache_key, envir = cache_env)) {
      if (verbose) message("Using cached results for query: ", query)
      return(get(cache_key, envir = cache_env))
    }
  }

  # Set API key if provided
  if (!is.null(api_key)) {
    rentrez::set_entrez_key(api_key)
    # Increase batch size when using API key (higher rate limits)
    batch_size <- min(500, batch_size)
  }

  # Construct search parameters
  search_params <- list()

  # If using MeSH terms, try to map query
  if (use_mesh) {
    if (verbose) message("Mapping query to MeSH terms...")

    mesh_terms <- tryCatch(
      rentrez::entrez_search(db = "mesh", term = query),
      error = function(e) {
        if (verbose) message("Could not map to MeSH terms: ", e$message)
        return(NULL)
      }
    )

    if (!is.null(mesh_terms) && mesh_terms$count > 0) {
      mesh_data <- tryCatch(
        rentrez::entrez_summary(db = "mesh", id = mesh_terms$ids[1]),
        error = function(e) {
          if (verbose) message("Could not retrieve MeSH term details: ", e$message)
          return(NULL)
        }
      )

      if (!is.null(mesh_data)) {
        query <- paste0(query, "[MeSH Terms]")
        if (verbose) message("Mapped to MeSH term: ", mesh_data$ds_meshterms)
      }
    } else {
      if (verbose) message("Could not map to MeSH terms, using original query")
    }
  }

  # Add date range if specified
  if (!is.null(date_range) && length(date_range) == 2) {
    query <- paste0(query, " AND (\"", date_range[1], "\"[PDAT] : \"",
                    date_range[2], "\"[PDAT])")
  }

  # Perform the search with retry logic
  if (verbose) message("Searching PubMed for: ", query)

  search_result <- retry_api_call(
    rentrez::entrez_search,
    db = "pubmed",
    term = query,
    retmax = 0,  # Just get the count first
    use_history = TRUE,
    verbose = verbose,
    retry_count = retry_count,
    retry_delay = retry_delay
  )

  if (is.null(search_result) || search_result$count == 0) {
    if (verbose) message("No results found for query: ", query)
    return(data.frame())
  }

  if (verbose) {
    message("Found ", search_result$count, " results, retrieving ",
            min(max_results, search_result$count), " records")
  }

  # Calculate number of batches needed
  total_to_fetch <- min(max_results, search_result$count)
  num_batches <- ceiling(total_to_fetch / batch_size)

  # Initialize results data frame
  all_results <- data.frame()

  # Process in batches
  for (batch in 1:num_batches) {
    if (verbose) {
      message("Fetching batch ", batch, " of ", num_batches,
              " (records ", (batch - 1) * batch_size + 1, "-",
              min(batch * batch_size, total_to_fetch), ")")
    }

    # Calculate retstart for this batch
    retstart <- (batch - 1) * batch_size

    # Fetch this batch of records
    fetch_result <- retry_api_call(
      rentrez::entrez_fetch,
      db = "pubmed",
      web_history = search_result$web_history,
      rettype = "xml",
      retmode = "xml",
      retstart = retstart,
      retmax = min(batch_size, total_to_fetch - retstart),
      verbose = verbose,
      retry_count = retry_count,
      retry_delay = retry_delay
    )

    if (is.null(fetch_result)) {
      warning("Failed to fetch batch ", batch, ". Continuing with next batch.")
      next
    }

    # Parse the XML
    batch_results <- tryCatch({
      parse_pubmed_xml(fetch_result, verbose = verbose)
    }, error = function(e) {
      warning("Error parsing XML for batch ", batch, ": ", e$message)
      return(data.frame())
    })

    # Combine results
    if (nrow(batch_results) > 0) {
      if (nrow(all_results) == 0) {
        all_results <- batch_results
      } else {
        all_results <- rbind(all_results, batch_results)
      }
    }

    # Be nice to the NCBI server - add a small delay between batches
    if (batch < num_batches) {
      Sys.sleep(0.5)
    }
  }

  # Check if any results were found
  if (nrow(all_results) == 0) {
    warning("Failed to retrieve any valid results for query: ", query)
    return(data.frame())
  }

  # Add to cache if caching is enabled
  if (use_cache && !is.null(cache_key)) {
    cache_env <- get_pubmed_cache()
    assign(cache_key, all_results, envir = cache_env)
    if (verbose) message("Cached search results for future use")
  }

  return(all_results)
}


#' Parse PubMed XML data with optimized memory usage
#'
#' This function parses PubMed XML data into a data frame using streaming
#' to handle large result sets efficiently.
#'
#' @param xml_data XML data from PubMed.
#' @param verbose Logical. If TRUE, prints progress information.
#'
#' @return A data frame containing article metadata.
#' @keywords internal
parse_pubmed_xml <- function(xml_data, verbose = FALSE) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("The xml2 package is required. Install it with: install.packages('xml2')")
  }

  # Parse the XML
  tryCatch({
    # Check if the data is already an xml_document
    if (!inherits(xml_data, "xml_document")) {
      xml_doc <- xml2::read_xml(xml_data)
    } else {
      xml_doc <- xml_data
    }
  }, error = function(e) {
    stop("Failed to parse XML: ", e$message)
  })

  # Get all articles
  articles <- xml2::xml_find_all(xml_doc, ".//PubmedArticle")

  if (length(articles) == 0) {
    if (verbose) message("No articles found in XML data")
    return(data.frame())
  }

  if (verbose) message("Processing ", length(articles), " articles")

  # Pre-allocate lists for better performance
  pmids <- vector("character", length(articles))
  titles <- vector("character", length(articles))
  abstracts <- vector("character", length(articles))
  author_lists <- vector("list", length(articles))
  publication_years <- vector("character", length(articles))
  journal_names <- vector("character", length(articles))
  keywords <- vector("list", length(articles))
  doi <- vector("character", length(articles))

  # Extract data from each article
  for (i in seq_along(articles)) {
    if (verbose && i %% 100 == 0) {
      message("  Processing article ", i, " of ", length(articles))
    }

    article <- articles[i]

    # PMID - more reliable xpath
    pmid_node <- xml2::xml_find_first(article, ".//MedlineCitation/PMID")
    pmids[i] <- if (!is.na(pmid_node)) xml2::xml_text(pmid_node) else NA_character_

    # Title - more reliable xpath
    title_node <- xml2::xml_find_first(article, ".//MedlineCitation/Article/ArticleTitle")
    titles[i] <- if (!is.na(title_node)) xml2::xml_text(title_node) else NA_character_

    # Abstract - combine all AbstractText nodes
    abstract_nodes <- xml2::xml_find_all(article, ".//MedlineCitation/Article/Abstract/AbstractText")
    if (length(abstract_nodes) > 0) {
      abstract_parts <- character(length(abstract_nodes))

      for (j in seq_along(abstract_nodes)) {
        # Check if the AbstractText has a label attribute
        label <- xml2::xml_attr(abstract_nodes[j], "Label")
        text <- xml2::xml_text(abstract_nodes[j])

        # Format with label if present
        if (!is.na(label) && label != "") {
          abstract_parts[j] <- paste0(label, ": ", text)
        } else {
          abstract_parts[j] <- text
        }
      }

      abstracts[i] <- paste(abstract_parts, collapse = " ")
    } else {
      abstracts[i] <- NA_character_
    }

    # Authors - more efficient processing
    author_nodes <- xml2::xml_find_all(article, ".//MedlineCitation/Article/AuthorList/Author")
    if (length(author_nodes) > 0) {
      author_names <- character(length(author_nodes))

      for (j in seq_along(author_nodes)) {
        last_name <- xml2::xml_text(xml2::xml_find_first(author_nodes[j], "./LastName") %||% NA)
        fore_name <- xml2::xml_text(xml2::xml_find_first(author_nodes[j], "./ForeName") %||% NA)
        initials <- xml2::xml_text(xml2::xml_find_first(author_nodes[j], "./Initials") %||% NA)

        # Construct name based on available parts
        if (!is.na(last_name)) {
          if (!is.na(fore_name)) {
            author_names[j] <- paste(last_name, fore_name)
          } else if (!is.na(initials)) {
            author_names[j] <- paste(last_name, initials)
          } else {
            author_names[j] <- last_name
          }
        } else {
          # Check for collective author
          collective <- xml2::xml_text(xml2::xml_find_first(author_nodes[j], "./CollectiveName") %||% NA)
          if (!is.na(collective)) {
            author_names[j] <- collective
          } else {
            author_names[j] <- NA_character_
          }
        }
      }

      # Remove NA values
      author_names <- author_names[!is.na(author_names)]
      author_lists[[i]] <- author_names
    } else {
      author_lists[[i]] <- character(0)
    }

    # Publication Year - handle multiple date formats
    # First try PubDate
    year_node <- xml2::xml_find_first(article,
                                      ".//MedlineCitation/Article/Journal/JournalIssue/PubDate/Year")

    if (is.na(year_node)) {
      # Next try MedlineDate
      year_node <- xml2::xml_find_first(article,
                                        ".//MedlineCitation/Article/Journal/JournalIssue/PubDate/MedlineDate")
    }

    if (is.na(year_node)) {
      # Try ArticleDate
      year_node <- xml2::xml_find_first(article,
                                        ".//MedlineCitation/Article/ArticleDate/Year")
    }

    publication_years[i] <- if (!is.na(year_node)) {
      # For MedlineDate, extract just the year
      text <- xml2::xml_text(year_node)
      if (grepl("^[0-9]{4}$", text)) {
        text  # Already just a year
      } else {
        # Extract first 4-digit sequence as year
        year_match <- regmatches(text, regexpr("[0-9]{4}", text))
        if (length(year_match) > 0) year_match else text
      }
    } else {
      NA_character_
    }

    # Journal Name
    journal_node <- xml2::xml_find_first(article,
                                         ".//MedlineCitation/Article/Journal/Title")

    if (is.na(journal_node)) {
      # Try abbreviated title
      journal_node <- xml2::xml_find_first(article,
                                           ".//MedlineCitation/Article/Journal/ISOAbbreviation")
    }

    journal_names[i] <- if (!is.na(journal_node)) xml2::xml_text(journal_node) else NA_character_

    # Keywords
    keyword_nodes <- xml2::xml_find_all(article,
                                        ".//MedlineCitation/KeywordList/Keyword")

    if (length(keyword_nodes) > 0) {
      kw <- sapply(keyword_nodes, xml2::xml_text)
      keywords[[i]] <- kw
    } else {
      keywords[[i]] <- character(0)
    }

    # DOI
    doi_node <- xml2::xml_find_first(article,
                                     ".//MedlineCitation/Article/ELocationID[@EIdType='doi']")

    if (is.na(doi_node)) {
      # Try alternate location
      doi_node <- xml2::xml_find_first(article,
                                       ".//PubmedData/ArticleIdList/ArticleId[@IdType='doi']")
    }

    doi[i] <- if (!is.na(doi_node)) xml2::xml_text(doi_node) else NA_character_
  }

  # Create data frame with all parsed information
  result_df <- data.frame(
    pmid = pmids,
    title = titles,
    abstract = abstracts,
    publication_year = publication_years,
    journal = journal_names,
    doi = doi,
    stringsAsFactors = FALSE
  )

  # Add authors as a separate column (as a comma-separated string)
  result_df$authors <- sapply(author_lists, function(x) {
    if (length(x) == 0) {
      return(NA_character_)
    } else {
      return(paste(x, collapse = ", "))
    }
  })

  # Add keywords as a separate column
  result_df$keywords <- sapply(keywords, function(x) {
    if (length(x) == 0) {
      return(NA_character_)
    } else {
      return(paste(x, collapse = ", "))
    }
  })

  return(result_df)
}

#' Null coalescing operator
#'
#' Returns the first argument if it is not NULL or empty, otherwise returns the second argument.
#'
#' @param x An object to test if NULL or empty
#' @param y An object to return if x is NULL or empty
#' @return Returns x if x is not NULL, not empty, or not a missing XML node, otherwise returns y.
#' @name null_coalesce
#' @examples
#' NULL %||% "default"  # returns "default"
#' "value" %||% "default"  # returns "value"
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || inherits(x, "xml_missing")) y else x
}

#' Retry an API call with exponential backoff
#'
#' This function retries a failed API call with exponential backoff.
#'
#' @param fun Function to call
#' @param ... Arguments to pass to the function
#' @param verbose Logical. If TRUE, prints progress information
#' @param retry_count Integer. Number of times to retry
#' @param retry_delay Integer. Initial delay between retries in seconds
#'
#' @return Result of the function call or NULL if all retries fail
#' @keywords internal
retry_api_call <- function(fun, ..., verbose = FALSE, retry_count = 3, retry_delay = 1) {
  for (i in 1:retry_count) {
    result <- tryCatch({
      fun(...)
    }, error = function(e) {
      if (verbose) {
        message("API call failed (attempt ", i, "/", retry_count, "): ", e$message)
      }

      # Check for specific error types that might require different handling
      if (grepl("429|Too Many Requests", e$message)) {
        if (verbose) message("Rate limit exceeded. Waiting longer before retry...")
        Sys.sleep(retry_delay * 2)  # Wait even longer for rate limit errors
      }

      return(NULL)
    })

    if (!is.null(result)) {
      return(result)
    }

    # Exponential backoff for retries
    if (i < retry_count) {
      backoff_time <- retry_delay * (2 ^ (i - 1)) # Exponential backoff
      if (verbose) message("Retrying in ", backoff_time, " seconds...")
      Sys.sleep(backoff_time)
    }
  }

  if (verbose) message("All retry attempts failed")
  return(NULL)
}

#' Clear PubMed cache
#'
#' Removes all cached PubMed search results
#'
#' @return NULL invisibly
#' @export
clear_pubmed_cache <- function() {
  # Get the cache environment
  pubmed_cache <- get_pubmed_cache()

  # Remove all objects from the environment
  rm(list = ls(envir = pubmed_cache), envir = pubmed_cache)

  message("PubMed cache cleared")

  invisible(NULL)
}

#' Retrieve full text from PubMed Central
#'
#' This function retrieves full text articles from PubMed Central.
#'
#' @param pmids Character vector of PubMed IDs.
#' @param api_key Character string. NCBI API key for higher rate limits (optional).
#'
#' @return A data frame containing article metadata and full text.
#' @export
#'
#' @examples
#' \dontrun{
#' full_texts <- get_pmc_fulltext(c("12345678", "23456789"))
#' }
get_pmc_fulltext <- function(pmids, api_key = NULL) {
  # Check for rentrez package
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    stop("The rentrez package is required. Install it with: install.packages('rentrez')")
  }

  # Set API key if provided
  if (!is.null(api_key)) {
    rentrez::set_entrez_key(api_key)
  }

  # Initialize results data frame
  results <- data.frame(
    pmid = character(0),
    pmc_id = character(0),
    fulltext = character(0),
    stringsAsFactors = FALSE
  )

  # Process in batches to avoid API limits
  batch_size <- 50
  n_batches <- ceiling(length(pmids) / batch_size)

  for (i in 1:n_batches) {
    batch_start <- (i - 1) * batch_size + 1
    batch_end <- min(i * batch_size, length(pmids))
    pmid_batch <- pmids[batch_start:batch_end]

    message("Processing batch ", i, " of ", n_batches, " (PMIDs ", batch_start, "-", batch_end, ")")

    # Find PMC IDs corresponding to PMIDs - handle errors with tryCatch
    link_result <- tryCatch({
      rentrez::entrez_link(dbfrom = "pubmed", db = "pmc", id = pmid_batch)
    }, error = function(e) {
      # Just message the error, don't issue a warning
      message("Error in entrez_link: ", e$message)
      return(NULL)
    })

    # Check if link_result is NULL or missing the expected component
    if (is.null(link_result) ||
        is.null(link_result$links) ||
        is.null(link_result$links$pubmed_pmc) ||
        length(link_result$links$pubmed_pmc) == 0) {
      message("No PMC links found for this batch")
      next
    }

    # Get PMC IDs
    pmc_ids <- link_result$links$pubmed_pmc

    # Map PMC IDs back to their PMIDs
    pmid_to_pmc <- data.frame(
      pmid = names(link_result$links$pubmed_pmc),
      pmc_id = pmc_ids,
      stringsAsFactors = FALSE
    )

    # Fetch full text for each PMC ID
    for (j in seq_along(pmc_ids)) {
      message("Retrieving fulltext for PMC ID: ", pmc_ids[j], " (", j, " of ", length(pmc_ids), ")")

      # Try to fetch the fulltext with tryCatch
      fulltext_result <- tryCatch({
        rentrez::entrez_fetch(db = "pmc", id = pmc_ids[j], rettype = "fulltext")
      }, error = function(e) {
        # Just message the error, don't issue a warning
        message("Failed to retrieve fulltext for PMC ID ", pmc_ids[j], ": ", e$message)
        return(NULL)
      })

      if (!is.null(fulltext_result)) {
        # Add to results
        results <- rbind(results, data.frame(
          pmid = pmid_to_pmc$pmid[j],
          pmc_id = pmc_ids[j],
          fulltext = fulltext_result,
          stringsAsFactors = FALSE
        ))
      }

      # Be nice to the API - pause between requests
      Sys.sleep(1)
    }
  }

  return(results)
}

#' Search NCBI databases for articles or data
#'
#' This function searches various NCBI databases using the E-utilities API via the rentrez package.
#'
#' @param query Character string containing the search query.
#' @param database Character string. The NCBI database to search (e.g., "pubmed", "pmc", "gene", "protein").
#' @param max_results Maximum number of results to return.
#' @param use_mesh Logical. If TRUE, will attempt to map query terms to MeSH terms (for PubMed only).
#' @param date_range Character vector of length 2 with start and end dates in format "YYYY/MM/DD".
#' @param api_key Character string. NCBI API key for higher rate limits (optional).
#' @param retry_count Integer. Number of times to retry failed requests.
#' @param retry_delay Integer. Delay between retries in seconds.
#'
#' @return A data frame containing the search results with IDs, titles, and other metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- ncbi_search("migraine headache", database = "pubmed", max_results = 100)
#' gene_results <- ncbi_search("BRCA1", database = "gene", max_results = 10)
#' }
ncbi_search <- function(query, database = "pubmed", max_results = 1000,
                        use_mesh = FALSE, date_range = NULL, api_key = NULL,
                        retry_count = 3, retry_delay = 2) {
  # Check for rentrez package
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    stop("The rentrez package is required for NCBI searches. Install it with: install.packages('rentrez')")
  }

  # Set API key if provided
  if (!is.null(api_key)) {
    rentrez::set_entrez_key(api_key)
  }

  # Rate limiting function
  throttle_api <- function() {
    # If API key is provided, use higher rate limit (10 requests per second)
    # Otherwise use lower rate limit (3 requests per second)
    Sys.sleep(if (!is.null(api_key)) 0.1 else 0.33)
  }

  # Retry function for API calls
  retry_api_call <- function(api_function, ...) {
    for (i in 1:retry_count) {
      result <- try(
        api_function(...),
        silent = TRUE
      )

      if (!inherits(result, "try-error")) {
        return(result)
      }

      if (i < retry_count) {
        message("API call failed, retrying in ", retry_delay, " seconds (", i, "/", retry_count, ")")
        Sys.sleep(retry_delay)
        # Increase delay for subsequent retries
        retry_delay <- retry_delay * 1.5
      }
    }

    # If all retries fail, stop with error
    stop("API call failed after ", retry_count, " retries")
  }

  # Construct search parameters
  search_params <- list()

  # Database-specific handling
  if (database == "pubmed") {
    # Handle PubMed-specific features
    if (use_mesh) {
      message("Mapping query to MeSH terms...")
      throttle_api()
      mesh_terms <- retry_api_call(
        rentrez::entrez_search,
        db = "mesh",
        term = query
      )

      if (mesh_terms$count > 0) {
        throttle_api()
        mesh_data <- retry_api_call(
          rentrez::entrez_summary,
          db = "mesh",
          id = mesh_terms$ids[1]
        )
        query <- paste0(query, "[MeSH Terms]")
        message("Mapped to MeSH term: ", mesh_data$ds_meshterms)
      } else {
        message("Could not map to MeSH terms, using original query")
      }
    }
  }

  # Add date range if specified
  if (!is.null(date_range) && length(date_range) == 2) {
    if (database %in% c("pubmed", "pmc")) {
      query <- paste0(query, " AND (\"", date_range[1], "\"[PDAT] : \"",
                      date_range[2], "\"[PDAT])")
    } else {
      message("Date range filtering is only supported for PubMed and PMC databases")
    }
  }

  # Perform the search with retry logic
  message("Searching ", database, " for: ", query)
  throttle_api()
  search_result <- retry_api_call(
    rentrez::entrez_search,
    db = database,
    term = query,
    retmax = max_results,
    use_history = TRUE
  )

  if (search_result$count == 0) {
    message("No results found for query: ", query)
    return(data.frame())
  }

  message("Found ", search_result$count, " results, retrieving ",
          min(max_results, search_result$count), " records")

  # Handle database-specific retrieval and parsing
  result_df <- switch(
    database,
    "pubmed" = fetch_and_parse_pubmed(search_result, max_results, throttle_api, retry_api_call),
    "gene" = fetch_and_parse_gene(search_result, max_results, throttle_api, retry_api_call),
    "protein" = fetch_and_parse_protein(search_result, max_results, throttle_api, retry_api_call),
    "pmc" = fetch_and_parse_pmc(search_result, max_results, throttle_api, retry_api_call),
    # Default case for other databases - just return IDs
    {
      message("Detailed parsing not implemented for ", database, " database. Returning IDs only.")
      throttle_api()
      ids <- retry_api_call(
        rentrez::entrez_fetch,
        db = database,
        web_history = search_result$web_history,
        rettype = "uilist",
        retmax = min(max_results, search_result$count)
      )
      data.frame(id = strsplit(ids, "\n")[[1]], stringsAsFactors = FALSE)
    }
  )

  return(result_df)
}

# Helper functions for different databases

#' Fetch and parse PubMed data
#' @keywords internal
fetch_and_parse_pubmed <- function(search_result, max_results, throttle_api, retry_api_call) {
  throttle_api()
  fetch_result <- retry_api_call(
    rentrez::entrez_fetch,
    db = "pubmed",
    web_history = search_result$web_history,
    rettype = "xml",
    retmax = min(max_results, search_result$count)
  )

  # Parse the XML
  articles <- xml2::read_xml(fetch_result)

  # Extract article data using the existing parse_pubmed_xml function
  result_df <- parse_pubmed_xml(articles)

  return(result_df)
}

#' Fetch and parse Gene data
#' @keywords internal
fetch_and_parse_gene <- function(search_result, max_results, throttle_api, retry_api_call) {
  # Initialize results structure
  result_df <- data.frame(
    gene_id = character(),
    symbol = character(),
    description = character(),
    organism = character(),
    location = character(),
    stringsAsFactors = FALSE
  )

  # Process in batches to avoid API limits
  batch_size <- 50
  total_to_fetch <- min(max_results, search_result$count)
  n_batches <- ceiling(total_to_fetch / batch_size)

  for (i in 1:n_batches) {
    batch_start <- (i - 1) * batch_size + 1
    batch_end <- min(i * batch_size, total_to_fetch)

    # Adjusted retstart parameter for each batch
    throttle_api()
    batch_ids <- retry_api_call(
      rentrez::entrez_search,
      db = "gene",
      term = search_result$query_key,
      web_history = search_result$web_history,
      retstart = batch_start - 1,
      retmax = batch_end - batch_start + 1
    )$ids

    if (length(batch_ids) == 0) break

    message("Fetching gene batch ", i, " of ", n_batches, " (records ", batch_start, "-", batch_end, ")")

    # Fetch gene summaries
    throttle_api()
    gene_summaries <- retry_api_call(
      rentrez::entrez_summary,
      db = "gene",
      id = batch_ids
    )

    # Extract data from each gene summary
    batch_data <- data.frame(
      gene_id = character(length(batch_ids)),
      symbol = character(length(batch_ids)),
      description = character(length(batch_ids)),
      organism = character(length(batch_ids)),
      location = character(length(batch_ids)),
      stringsAsFactors = FALSE
    )

    for (j in seq_along(batch_ids)) {
      gene_summary <- gene_summaries[[j]]

      batch_data$gene_id[j] <- batch_ids[j]
      batch_data$symbol[j] <- if (!is.null(gene_summary$name)) gene_summary$name else NA_character_
      batch_data$description[j] <- if (!is.null(gene_summary$description)) gene_summary$description else NA_character_
      batch_data$organism[j] <- if (!is.null(gene_summary$organism$scientificname))
        gene_summary$organism$scientificname else NA_character_
      batch_data$location[j] <- if (!is.null(gene_summary$maplocation))
        gene_summary$maplocation else NA_character_
    }

    # Combine with main results
    result_df <- rbind(result_df, batch_data)
  }

  return(result_df)
}

#' Fetch and parse Protein data
#' @keywords internal
fetch_and_parse_protein <- function(search_result, max_results, throttle_api, retry_api_call) {
  # Initialize results structure
  result_df <- data.frame(
    protein_id = character(),
    name = character(),
    organism = character(),
    length = integer(),
    stringsAsFactors = FALSE
  )

  # Process in batches
  batch_size <- 50
  total_to_fetch <- min(max_results, search_result$count)
  n_batches <- ceiling(total_to_fetch / batch_size)

  for (i in 1:n_batches) {
    batch_start <- (i - 1) * batch_size + 1
    batch_end <- min(i * batch_size, total_to_fetch)

    # Get IDs for this batch
    throttle_api()
    batch_ids <- retry_api_call(
      rentrez::entrez_search,
      db = "protein",
      term = search_result$query_key,
      web_history = search_result$web_history,
      retstart = batch_start - 1,
      retmax = batch_end - batch_start + 1
    )$ids

    if (length(batch_ids) == 0) break

    message("Fetching protein batch ", i, " of ", n_batches, " (records ", batch_start, "-", batch_end, ")")

    # Fetch protein summaries
    throttle_api()
    protein_summaries <- retry_api_call(
      rentrez::entrez_summary,
      db = "protein",
      id = batch_ids
    )

    # Extract data from each protein summary
    batch_data <- data.frame(
      protein_id = character(length(batch_ids)),
      name = character(length(batch_ids)),
      organism = character(length(batch_ids)),
      length = integer(length(batch_ids)),
      stringsAsFactors = FALSE
    )

    for (j in seq_along(batch_ids)) {
      protein_summary <- protein_summaries[[j]]

      batch_data$protein_id[j] <- batch_ids[j]
      batch_data$name[j] <- if (!is.null(protein_summary$title)) protein_summary$title else NA_character_
      batch_data$organism[j] <- if (!is.null(protein_summary$organism))
        protein_summary$organism else NA_character_
      batch_data$length[j] <- if (!is.null(protein_summary$length))
        as.integer(protein_summary$length) else NA_integer_
    }

    # Combine with main results
    result_df <- rbind(result_df, batch_data)
  }

  return(result_df)
}

#' Fetch and parse PMC data
#' @keywords internal
fetch_and_parse_pmc <- function(search_result, max_results, throttle_api, retry_api_call) {
  throttle_api()
  fetch_result <- retry_api_call(
    rentrez::entrez_fetch,
    db = "pmc",
    web_history = search_result$web_history,
    rettype = "xml",
    retmax = min(max_results, search_result$count)
  )

  # Parse the XML
  articles <- xml2::read_xml(fetch_result)

  # Initialize lists to store data
  pmcids <- character()
  titles <- character()
  abstracts <- character()
  authors <- list()
  publication_years <- character()
  journal_names <- character()

  # Extract article elements
  article_nodes <- xml2::xml_find_all(articles, ".//article")

  for (article in article_nodes) {
    # PMCID
    pmcid_node <- xml2::xml_find_first(article, ".//article-id[@pub-id-type='pmc']")
    pmcid <- if (!is.na(pmcid_node)) xml2::xml_text(pmcid_node) else NA_character_

    # Title
    title_node <- xml2::xml_find_first(article, ".//article-title")
    title <- if (!is.na(title_node)) xml2::xml_text(title_node) else NA_character_

    # Abstract
    abstract_nodes <- xml2::xml_find_all(article, ".//abstract/p")
    abstract <- NA_character_
    if (length(abstract_nodes) > 0) {
      abstract_text <- sapply(abstract_nodes, xml2::xml_text)
      abstract <- paste(abstract_text, collapse = " ")
    }

    # Authors
    author_nodes <- xml2::xml_find_all(article, ".//contrib[@contrib-type='author']")
    article_authors <- character()
    if (length(author_nodes) > 0) {
      for (author_node in author_nodes) {
        surname_node <- xml2::xml_find_first(author_node, ".//surname")
        given_names_node <- xml2::xml_find_first(author_node, ".//given-names")

        surname <- if (!is.na(surname_node)) xml2::xml_text(surname_node) else ""
        given_names <- if (!is.na(given_names_node)) xml2::xml_text(given_names_node) else ""

        if (surname != "" || given_names != "") {
          article_authors <- c(article_authors, paste(surname, given_names))
        }
      }
    }

    # Publication Year
    year_node <- xml2::xml_find_first(article, ".//pub-date/year")
    year <- if (!is.na(year_node)) xml2::xml_text(year_node) else NA_character_

    # Journal Name
    journal_node <- xml2::xml_find_first(article, ".//journal-title")
    journal <- if (!is.na(journal_node)) xml2::xml_text(journal_node) else NA_character_

    # Append to result lists
    pmcids <- c(pmcids, pmcid)
    titles <- c(titles, title)
    abstracts <- c(abstracts, abstract)
    authors <- c(authors, list(article_authors))
    publication_years <- c(publication_years, year)
    journal_names <- c(journal_names, journal)
  }

  # Create data frame
  result_df <- data.frame(
    pmcid = pmcids,
    title = titles,
    abstract = abstracts,
    publication_year = publication_years,
    journal = journal_names,
    stringsAsFactors = FALSE
  )

  # Add authors as a separate column
  result_df$authors <- sapply(authors, function(x) {
    if (length(x) == 0) {
      return(NA_character_)
    } else {
      return(paste(x, collapse = ", "))
    }
  })

  return(result_df)
}

#' Create a connection pool for NCBI API calls
#'
#' This function sets up a simple connection pool for NCBI API calls
#' to manage rate limits and avoid overloading the API.
#'
#' @param pool_size Number of connections to maintain in the pool.
#' @param api_key Character string. NCBI API key for higher rate limits.
#'
#' @return A list containing connection pool functions.
#' @export
#'
#' @examples
#' \dontrun{
#' connection_pool <- create_conn_pool(api_key = "your_api_key")
#' # Use the connection pool
#' connection_pool$execute(function() {
#'   rentrez::entrez_search(db = "pubmed", term = "migraine")
#' })
#' }
