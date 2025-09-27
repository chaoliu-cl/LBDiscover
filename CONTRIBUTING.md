# Contributing to LBDiscover

Thank you for your interest in contributing to LBDiscover! This document provides guidelines for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Contributing Code](#contributing-code)
- [Development Guidelines](#development-guidelines)
- [Testing](#testing)
- [Documentation](#documentation)
- [Pull Request Process](#pull-request-process)

## Code of Conduct

This project adheres to a code of conduct adapted from the [Contributor Covenant](https://www.contributor-covenant.org/). By participating, you are expected to uphold this code. Please report unacceptable behavior to [chaoliu@cedarville.edu](mailto:chaoliu@cedarville.edu).

## Getting Started

### Prerequisites

- R (>= 4.0.0)
- RStudio (recommended)
- Git
- Required R packages (see DESCRIPTION file)

### Setting up the Development Environment

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/yourusername/LBDiscover.git
   cd LBDiscover
   ```
3. Install development dependencies:
   ```r
   # Install devtools if you haven't already
   install.packages("devtools")
   
   # Install package dependencies
   devtools::install_deps()
   
   # Install additional development packages
   install.packages(c("testthat", "roxygen2", "pkgdown", "covr"))
   ```

## How to Contribute

There are several ways to contribute to LBDiscover:

1. **Report bugs** - Help us identify and fix issues
2. **Suggest enhancements** - Propose new features or improvements
3. **Improve documentation** - Help make the package more accessible
4. **Contribute code** - Fix bugs or implement new features
5. **Write tests** - Help improve code coverage and reliability

## Reporting Bugs

Before creating bug reports, please check the existing [issues](https://github.com/chaoliu-cl/LBDiscover/issues) to avoid duplicates.

### How to Submit a Bug Report

Create an issue on GitHub and include:

- **Clear title** describing the problem
- **Detailed description** of the bug
- **Steps to reproduce** the behavior
- **Expected behavior** vs actual behavior
- **System information**:
  - R version (`R.version.string`)
  - Package version (`packageVersion("LBDiscover")`)
  - Operating system
- **Reproducible example** (if possible)
- **Session info** (`sessionInfo()`)

### Bug Report Template

```markdown
**Description**
A clear description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Load the package with '...'
2. Run function '...'
3. See error

**Expected behavior**
A clear description of what you expected to happen.

**Reproducible example**
```r
# Minimal reproducible example
library(LBDiscover)
# Your code here
```

**System information**
- R version: [e.g., R version 4.3.0]
- LBDiscover version: [e.g., 0.1.0]
- OS: [e.g., macOS 13.0, Windows 11, Ubuntu 20.04]

**Additional context**
Add any other context about the problem here.
```

## Suggesting Enhancements

Enhancement suggestions are welcome! Please:

1. **Check existing issues** to avoid duplicates
2. **Provide a clear title** and detailed description
3. **Explain the motivation** - why would this be useful?
4. **Describe the solution** you'd like to see
5. **Consider alternatives** you've considered

## Contributing Code

### Development Workflow

1. **Create a new branch** for your feature/fix:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** following our coding standards

3. **Test your changes** thoroughly

4. **Update documentation** as needed

5. **Commit your changes** with clear messages:
   ```bash
   git commit -m "Add feature: brief description"
   ```

6. **Push to your fork** and create a pull request

## Development Guidelines

### Coding Standards

#### R Code Style
- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use `snake_case` for function and variable names
- Use `PascalCase` for class names
- Maximum line length: 80 characters
- Use 2 spaces for indentation
- Add spaces around operators (`=`, `+`, `-`, etc.)

#### Function Documentation
- Use `roxygen2` for all exported functions
- Include `@param` for all parameters
- Include `@return` describing what the function returns
- Include `@export` for functions that should be exported
- Include `@examples` with working examples
- Use `@keywords internal` for internal functions

Example:
```r
#' Search PubMed for articles
#'
#' This function searches PubMed using the NCBI E-utilities API.
#'
#' @param query Character string containing the search query.
#' @param max_results Maximum number of results to return (default: 1000).
#' @param verbose Logical. If TRUE, prints progress information (default: TRUE).
#'
#' @return A data frame containing the search results with PubMed IDs, titles, and other metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- pubmed_search("migraine headache", max_results = 100)
#' }
pubmed_search <- function(query, max_results = 1000, verbose = TRUE) {
  # Function implementation
}
```

#### Error Handling
- Use informative error messages
- Validate function arguments
- Use `stop()` for errors, `warning()` for warnings, `message()` for informational messages

### File Organization
- Keep functions in logical groups within files
- Use descriptive file names (e.g., `pubmed_search.R`, `abc_model.R`)
- Place utility functions in `utils.R`
- Keep the main package functions in separate files

### Dependencies
- Minimize external dependencies
- Use `Imports:` in DESCRIPTION for required packages
- Use `Suggests:` for optional dependencies
- Always check if suggested packages are available before using them:
  ```r
  if (!requireNamespace("package", quietly = TRUE)) {
    stop("Package 'package' is required. Install it with: install.packages('package')")
  }
  ```

## Testing

### Test Requirements
- All new functions must have corresponding tests
- Tests should cover normal usage, edge cases, and error conditions
- Aim for high test coverage (>80%)

### Running Tests
```r
# Run all tests
devtools::test()

# Check test coverage
covr::package_coverage()

# Run R CMD check
devtools::check()
```

### Writing Tests
Use `testthat` for testing:

```r
test_that("pubmed_search returns correct structure", {
  # Mock API call if possible
  result <- pubmed_search("test", max_results = 1)
  
  expect_is(result, "data.frame")
  expect_true(nrow(result) >= 0)
  expect_true("pmid" %in% colnames(result))
})
```

## Documentation

### Package Documentation
- Update `README.md` for significant changes
- Update vignettes for new features
- Ensure all exported functions are documented
- Update `NEWS.md` for version changes

### Building Documentation
```r
# Generate documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()

# Build pkgdown site
pkgdown::build_site()
```

## Pull Request Process

### Before Submitting
1. **Ensure tests pass**: Run `devtools::check()` with no errors, warnings, or notes
2. **Update documentation**: Run `devtools::document()` if you've changed function signatures
3. **Check code style**: Ensure your code follows the style guidelines
4. **Update NEWS.md**: Add a brief description of changes
5. **Rebase**: Ensure your branch is up-to-date with main

### Pull Request Template
```markdown
## Description
Brief description of the changes.

## Type of Change
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update

## Testing
- [ ] Tests pass locally with my changes
- [ ] I have added tests that prove my fix is effective or that my feature works
- [ ] New and existing unit tests pass locally with my changes

## Checklist
- [ ] My code follows the style guidelines of this project
- [ ] I have performed a self-review of my own code
- [ ] I have commented my code, particularly in hard-to-understand areas
- [ ] I have made corresponding changes to the documentation
- [ ] My changes generate no new warnings
- [ ] Any dependent changes have been merged and published in downstream modules
```

### Review Process
1. At least one maintainer must approve the PR
2. All automated checks must pass
3. Address any requested changes
4. Once approved, the PR will be merged by a maintainer

## Release Process

For maintainers:

1. Update version number in `DESCRIPTION`
2. Update `NEWS.md` with changes
3. Run `devtools::check()` to ensure everything passes
4. Create a release on GitHub
5. Submit to CRAN (if applicable)

## Getting Help

- **Questions about development**: Open a discussion on GitHub
- **Technical issues**: Create an issue
- **Direct contact**: Email [chaoliu@cedarville.edu](mailto:chaoliu@cedarville.edu)

## Recognition

Contributors will be acknowledged in:
- Package documentation
- DESCRIPTION file (for significant contributions)
- Release notes

Thank you for contributing to LBDiscover!
