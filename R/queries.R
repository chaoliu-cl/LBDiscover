#' Query UMLS for term information
#'
#' @param term Character string, the term to query.
#' @param api_key Character string. UMLS API key.
#' @param version Character string. UMLS version to use.
#'
#' @return A data frame with UMLS information for the term.
#' @export
query_umls <- function(term, api_key, version = "current") {
  if (is.null(api_key)) {
    stop("UMLS API key is required. Register at https://uts.nlm.nih.gov/uts/license")
  }

  # Base URL for UTS REST API
  base_url <- "https://uts-ws.nlm.nih.gov/rest"

  # First authenticate to get TGT (Ticket Granting Ticket)
  auth_url <- "https://utslogin.nlm.nih.gov/cas/v1/api-key"
  auth_response <- httr::POST(
    auth_url,
    body = list(
      apikey = api_key
    ),
    encode = "form"
  )

  if (httr::status_code(auth_response) != 201) {
    stop("UMLS authentication failed: ", httr::content(auth_response, "text"))
  }

  # Get service ticket URL from response
  tgt_url <- httr::headers(auth_response)$location

  # Get service ticket
  ticket_response <- httr::POST(
    tgt_url,
    body = list(service = "http://umlsks.nlm.nih.gov"),
    encode = "form"
  )

  service_ticket <- httr::content(ticket_response, "text")

  # Search for the term
  search_url <- paste0(base_url, "/search/", version)
  search_response <- httr::GET(
    search_url,
    query = list(
      string = term,
      ticket = service_ticket
    )
  )

  search_results <- httr::content(search_response)

  # Process results
  if (is.null(search_results$result) ||
      is.null(search_results$result$results) ||
      length(search_results$result$results) == 0) {
    return(data.frame(
      cui = NA_character_,
      term = term,
      semantic_type = "Unknown",
      source = "UMLS",
      definition = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # Get the first result
  best_match <- search_results$result$results[[1]]

  # Check if UI field exists
  if (!is.null(best_match$ui)) {
    cui <- best_match$ui
  } else {
    # Handle missing UI field
    return(data.frame(
      cui = NA_character_,
      term = term,
      semantic_type = "Unknown",
      source = "UMLS",
      definition = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # Get concept details
  concept_url <- paste0(base_url, "/content/", version, "/CUI/", cui)
  concept_response <- httr::GET(
    concept_url,
    query = list(ticket = service_ticket)
  )

  concept_data <- httr::content(concept_response)

  # Check if concept data has the expected structure
  if (is.null(concept_data$result) || is.null(concept_data$result$name)) {
    return(data.frame(
      cui = cui,
      term = term,
      semantic_type = "Unknown",
      source = "UMLS",
      definition = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # Get semantic types
  semantics_url <- paste0(concept_url, "/semanticTypes")
  semantics_response <- httr::GET(
    semantics_url,
    query = list(ticket = service_ticket)
  )

  semantics_data <- httr::content(semantics_response)

  # Check if the result has the expected structure
  if (!is.null(semantics_data$result) && length(semantics_data$result) > 0) {
    semantic_types <- sapply(semantics_data$result, function(x) x$name)
  } else {
    semantic_types <- "Unknown"
  }

  # Get definitions
  definitions_url <- paste0(concept_url, "/definitions")
  definitions_response <- httr::GET(
    definitions_url,
    query = list(ticket = service_ticket)
  )

  definitions_data <- httr::content(definitions_response)
  definition <- NA_character_

  # Check if definitions data has expected structure
  if (!is.null(definitions_data$result) && length(definitions_data$result) > 0) {
    if (!is.null(definitions_data$result[[1]]$value)) {
      definition <- definitions_data$result[[1]]$value
    }
  }

  return(data.frame(
    cui = cui,
    term = concept_data$result$name,
    semantic_type = paste(semantic_types, collapse = ", "),
    source = "UMLS",
    definition = definition,
    stringsAsFactors = FALSE
  ))
}

#' Query for MeSH terms using E-utilities
#'
#' @param term Character string, the term to query.
#' @param api_key Character string. NCBI API key (optional).
#'
#' @return A data frame with MeSH information for the term.
#' @export
query_mesh <- function(term, api_key = NULL) {
  # Try to load the rentrez package
  if (!requireNamespace("rentrez", quietly = TRUE)) {
    message("The rentrez package is required. Install it with: install.packages('rentrez')")
    return(data.frame(
      mesh_id = NA_character_,
      term = term,
      tree_number = NA_character_,
      scope_note = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  # Set API key if provided
  if (!is.null(api_key)) {
    rentrez::set_entrez_key(api_key)
  }

  tryCatch({
    # Search MeSH database for the term
    mesh_search <- rentrez::entrez_search(
      db = "mesh",
      term = term,
      retmax = 1
    )

    if (mesh_search$count == 0) {
      message("No MeSH terms found for: ", term)
      return(data.frame(
        mesh_id = NA_character_,
        term = term,
        tree_number = NA_character_,
        scope_note = paste("No MeSH term found for:", term),
        stringsAsFactors = FALSE
      ))
    }

    # Get MeSH record directly as text
    mesh_record <- rentrez::entrez_fetch(
      db = "mesh",
      id = mesh_search$ids[1],
      rettype = "full",
      retmode = "text"
    )

    # Process the text response directly
    # Extract MeSH ID (Descriptor UI)
    mesh_id <- NA_character_
    mesh_id_match <- regexpr("DescriptorUI: ([A-Z][0-9]+)", mesh_record)
    if (mesh_id_match > 0) {
      mesh_id_str <- regmatches(mesh_record, mesh_id_match)
      mesh_id <- gsub("DescriptorUI: ", "", mesh_id_str)
    }

    # Extract MeSH Term (Descriptor Name)
    mesh_term <- term  # Default to the input term
    term_match <- regexpr("DescriptorName: ([^\n]+)", mesh_record)
    if (term_match > 0) {
      term_str <- regmatches(mesh_record, term_match)
      mesh_term <- gsub("DescriptorName: ", "", term_str)
    }

    # Extract Tree Numbers
    tree_numbers <- character(0)
    tree_pattern <- "Tree Number: ([A-Z][0-9\\.]+)"
    tree_matches <- gregexpr(tree_pattern, mesh_record)
    if (tree_matches[[1]][1] > 0) {
      tree_strings <- regmatches(mesh_record, tree_matches)[[1]]
      tree_numbers <- gsub("Tree Number: ", "", tree_strings)
    }
    tree_number_str <- paste(tree_numbers, collapse = ", ")

    # Extract Scope Note (if available)
    scope_note <- NA_character_
    scope_match <- regexpr("Scope Note: ([^\\n]+)", mesh_record)
    if (scope_match > 0) {
      scope_str <- regmatches(mesh_record, scope_match)
      scope_note <- gsub("Scope Note: ", "", scope_str)
    }

    # Return structured data
    return(data.frame(
      mesh_id = mesh_id,
      term = mesh_term,
      tree_number = tree_number_str,
      scope_note = scope_note,
      stringsAsFactors = FALSE
    ))
  }, error = function(e) {
    # Handle errors gracefully
    message("Error querying MeSH for term '", term, "': ", e$message)
    return(data.frame(
      mesh_id = NA_character_,
      term = term,
      tree_number = NA_character_,
      scope_note = paste("Error:", e$message),
      stringsAsFactors = FALSE
    ))
  })
}

#' Enhance ABC results with external knowledge
#'
#' This function enhances ABC results with information from external knowledge bases.
#'
#' @param abc_results A data frame containing ABC results.
#' @param knowledge_base Character string, the knowledge base to use ("umls" or "mesh").
#' @param api_key Character string. API key for the knowledge base (if needed).
#'
#' @return A data frame with enhanced ABC results.
#' @export
#'
#' @examples
#' \dontrun{
#' enhanced_results <- enhance_abc_kb(abc_results, knowledge_base = "mesh")
#' }
enhance_abc_kb <- function(abc_results, knowledge_base = c("umls", "mesh"),
                           api_key = NULL) {

  # Match knowledge_base argument
  knowledge_base <- match.arg(knowledge_base)

  # Check if results are empty
  if (nrow(abc_results) == 0) {
    message("ABC results are empty")
    return(abc_results)
  }

  # Create a copy of the results
  enhanced_results <- abc_results

  # Get unique terms to query
  unique_terms <- unique(c(abc_results$a_term,
                           unlist(strsplit(abc_results$b_terms, ", ")),
                           abc_results$c_term))

  message("Enhancing ", length(unique_terms), " unique terms with ",
          knowledge_base, " information...")
  pb <- utils::txtProgressBar(min = 0, max = length(unique_terms), style = 3)

  # Query information for each term
  term_info <- list()
  for (i in seq_along(unique_terms)) {
    term <- unique_terms[i]

    # Query the selected knowledge base
    if (knowledge_base == "umls") {
      term_info[[term]] <- query_umls(term, api_key = api_key)
    } else if (knowledge_base == "mesh") {
      term_info[[term]] <- query_mesh(term)
    }

    utils::setTxtProgressBar(pb, i)
  }

  close(pb)

  # Add knowledge base information to results
  if (knowledge_base == "umls") {
    enhanced_results$a_cui <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$cui)
    enhanced_results$a_semantic_type <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$semantic_type)
    enhanced_results$c_cui <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$cui)
    enhanced_results$c_semantic_type <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$semantic_type)
  } else if (knowledge_base == "mesh") {
    enhanced_results$a_mesh_id <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$mesh_id)
    enhanced_results$a_tree_number <- sapply(enhanced_results$a_term, function(term) term_info[[term]]$tree_number)
    enhanced_results$c_mesh_id <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$mesh_id)
    enhanced_results$c_tree_number <- sapply(enhanced_results$c_term, function(term) term_info[[term]]$tree_number)
  }

  return(enhanced_results)
}
