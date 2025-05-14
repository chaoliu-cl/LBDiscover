#' Define global variables to avoid R CMD check NOTEs
#'
#' @noRd
utils::globalVariables(c(
  "abc_score", "entity", "from", "journal", "pmid", "publication_year",
  "citing_pmid", "cited_pmid", "title", "to", "type", "verbose",
  "dictionary", "safe_dict", "original_count"
))

#' @importFrom grDevices colorRampPalette rainbow dev.off png
#' @importFrom graphics arrows axis image layout legend mtext par points rect text
#' @importFrom stats aggregate median runif setNames adist
#' @importFrom utils head txtProgressBar setTxtProgressBar browseURL adist
#' @importFrom httr POST GET headers content status_code
NULL

