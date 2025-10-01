# zzz.R
# nocov start - Package initialization hooks execute before test coverage tracking begins
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
}
# nocov end
