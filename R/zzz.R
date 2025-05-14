# zzz.R
.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "doc_id", "entity", "entity_type", "start_pos", "end_pos",
    "sentence", "frequency", "term", "type", "source", "word",
    "count", "a_term", "b_term", "c_term", "a_b_score", "b_c_score",
    "abc_score", "p_value", "significant",
    "legend_items", "legend_colors", "legend_title"
  ))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Loading LBDiscover package")

  # Set a general deprecation handler for all packages
  # This will replace any GitHub URL with the correct one for this package
  setHook(packageEvent("base", "onLoad"), function(...) {
    # Regular expression to match common GitHub issue URL patterns
    github_pattern <- "<https://github\\.com/[^/]+/[^/]+/issues>"

    # Function to update deprecation messages with correct repository
    update_dep_message <- function(msg, call) {
      if(grepl(github_pattern, msg)) {
        # Replace any GitHub URL with the correct one
        corrected_msg <- gsub(
          github_pattern,
          "<https://github.com/chaoliu-cl/LBDiscover/issues>",
          msg
        )

        # Add the package name context if not present
        if(!grepl("LBDiscover package", corrected_msg)) {
          corrected_msg <- paste0(
            corrected_msg,
            "\n\u2139 The deprecated feature was likely used in the LBDiscover package."
          )
        }

        packageStartupMessage(corrected_msg)
      } else {
        # For messages without GitHub URLs, just pass through
        packageStartupMessage(msg)
      }
    }

    # Set handlers for various packages that might emit deprecation warnings
    options(igraph.deprecation.handler = update_dep_message)

    # You can add more package-specific handlers here as needed
    # options(package.deprecation.handler = update_dep_message)
  })
}
