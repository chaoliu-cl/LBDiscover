#' Create co-occurrence matrix without explicit entity type constraints
#'
#' This function creates a co-occurrence matrix from entity data
#' while preserving entity type information as an attribute without
#' enforcing type constraints.
#'
#' @param entity_data A data frame with document IDs and entities.
#' @param doc_id_col Name of the column containing document IDs.
#' @param entity_col Name of the column containing entity names.
#' @param count_col Name of the column containing entity counts (optional).
#' @param type_col Name of the column containing entity types (optional).
#' @param normalize Logical. If TRUE, normalizes the co-occurrence matrix.
#' @param normalization_method Method for normalization ("cosine", "jaccard", or "dice").
#'
#' @return A co-occurrence matrix with entity types stored as an attribute.
#' @export
#'
#' @examples
#' \dontrun{
#' co_matrix <- create_comat(entities,
#'                           doc_id_col = "doc_id",
#'                           entity_col = "entity",
#'                           type_col = "entity_type")
#' }
create_comat <- function(entity_data,
                         doc_id_col = "doc_id",
                         entity_col = "entity",
                         count_col = NULL,
                         type_col = "entity_type",
                         normalize = TRUE,
                         normalization_method = c("cosine", "jaccard", "dice")) {

  # Match normalization method
  normalization_method <- match.arg(normalization_method)

  # Check if required columns exist
  required_cols <- c(doc_id_col, entity_col)
  if (!all(required_cols %in% colnames(entity_data))) {
    stop("Required columns not found in the data: ",
         paste(required_cols[!required_cols %in% colnames(entity_data)], collapse = ", "))
  }

  # Check if type column exists and store this information
  has_types <- type_col %in% colnames(entity_data)
  if (!has_types) {
    message("Entity type column '", type_col, "' not found. Co-occurrence matrix will have no type information.")
  }

  # Filter out any rows with NA in required columns
  valid_rows <- !is.na(entity_data[[doc_id_col]]) & !is.na(entity_data[[entity_col]])
  if (has_types) {
    valid_rows <- valid_rows & !is.na(entity_data[[type_col]])
  }

  # If count_col is provided, also filter rows with NA counts
  if (!is.null(count_col) && count_col %in% colnames(entity_data)) {
    valid_rows <- valid_rows & !is.na(entity_data[[count_col]])
  }

  # Apply filtering
  entity_data <- entity_data[valid_rows, ]

  # Check for empty data after filtering
  if (nrow(entity_data) == 0) {
    stop("No valid data after filtering NA values")
  }

  # Extract unique documents and entities
  docs <- unique(entity_data[[doc_id_col]])
  entities <- unique(entity_data[[entity_col]])

  # Ensure docs and entities are character vectors
  docs <- as.character(docs)
  entities <- as.character(entities)

  # Extract entity types if available
  if (has_types) {
    # Create a data frame to handle multiple types per entity
    entity_type_df <- unique(entity_data[, c(entity_col, type_col)])

    # If there are multiple types for the same entity, keep them all
    entity_types <- tapply(entity_type_df[[type_col]], entity_type_df[[entity_col]],
                           function(x) paste(sort(unique(x)), collapse = "|"))
  }

  # Check if Matrix package is available for sparse matrix
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("The Matrix package is required. Install it with: install.packages('Matrix')")
  }

  # Create document-term incidence matrix (binary or weighted)
  message("Building entity-document matrix...")
  dtm <- Matrix::sparseMatrix(
    i = match(entity_data[[doc_id_col]], docs),
    j = match(entity_data[[entity_col]], entities),
    x = if (!is.null(count_col) && count_col %in% colnames(entity_data)) {
      entity_data[[count_col]]
    } else {
      rep(1, nrow(entity_data))
    },
    dims = c(length(docs), length(entities)),
    dimnames = list(docs, entities)
  )

  # Calculate co-occurrence matrix using matrix multiplication
  message("Calculating co-occurrence matrix...")
  co_matrix <- Matrix::t(dtm) %*% dtm

  # Set diagonal to zero (entities don't co-occur with themselves)
  diag(co_matrix) <- 0

  # Normalize if requested
  if (normalize) {
    message("Normalizing co-occurrence matrix using ", normalization_method, " method...")

    # Get the frequency of each entity
    entity_freq <- Matrix::diag(Matrix::t(dtm) %*% dtm)

    if (normalization_method == "cosine") {
      # Cosine normalization: co_ij / sqrt(freq_i * freq_j)
      norm_matrix <- sqrt(outer(entity_freq, entity_freq))
      # Avoid division by zero
      norm_matrix[norm_matrix == 0] <- 1
      # Normalize
      co_matrix <- co_matrix / norm_matrix

    } else if (normalization_method == "jaccard") {
      # Jaccard coefficient: co_ij / (freq_i + freq_j - co_ij)
      for (i in 1:nrow(co_matrix)) {
        for (j in 1:ncol(co_matrix)) {
          if (i != j && co_matrix[i, j] > 0) {
            co_matrix[i, j] <- co_matrix[i, j] /
              (entity_freq[i] + entity_freq[j] - co_matrix[i, j])
          }
        }
      }

    } else if (normalization_method == "dice") {
      # Dice coefficient: 2 * co_ij / (freq_i + freq_j)
      for (i in 1:nrow(co_matrix)) {
        for (j in 1:ncol(co_matrix)) {
          if (i != j && co_matrix[i, j] > 0) {
            co_matrix[i, j] <- (2 * co_matrix[i, j]) /
              (entity_freq[i] + entity_freq[j])
          }
        }
      }
    }
  }

  # Add entity types as attribute if available
  if (has_types) {
    attr(co_matrix, "entity_types") <- entity_types
  }

  # Add entity frequencies as attribute
  attr(co_matrix, "entity_freq") <- entity_freq

  # Add metadata
  attr(co_matrix, "metadata") <- list(
    n_docs = length(docs),
    n_entities = length(entities),
    has_types = has_types,
    normalization = if (normalize) normalization_method else "none"
  )

  return(co_matrix)
}

#' Determine if a term is likely a specific biomedical entity with improved accuracy
#'
#' @param term Character string, the term to check
#' @param claimed_type Character string, the claimed entity type of the term
#' @return Logical, TRUE if the term is likely a valid biomedical entity, FALSE otherwise
#' @export
is_valid_biomedical_entity <- function(term, claimed_type = NULL) {
  # Skip empty terms
  if (is.null(term) || is.na(term) || term == "") {
    return(FALSE)
  }

  # Convert to lowercase for case-insensitive matching
  term_lower <- tolower(term)
  if (!is.null(claimed_type)) {
    claimed_type <- tolower(claimed_type)
  }

  # Dictionary of specific problematic acronyms and their correct types
  acronym_corrections <- list(
    # Analytical techniques/methods that are often misclassified as chemicals
    "faers" = "method",              # FDA Adverse Event Reporting System
    "bcpnn" = "method",              # Bayesian Confidence Propagation Neural Network
    "uplc" = "method",               # Ultra Performance Liquid Chromatography
    "frap" = "method",               # Fluorescence Recovery After Photobleaching
    "hplc" = "method",               # High Performance Liquid Chromatography
    "lc-ms" = "method",              # Liquid Chromatography-Mass Spectrometry
    "gc-ms" = "method",              # Gas Chromatography-Mass Spectrometry
    "maldi" = "method",              # Matrix-Assisted Laser Desorption/Ionization
    "elisa" = "method",              # Enzyme-Linked Immunosorbent Assay
    "ft-ir" = "method",              # Fourier Transform Infrared Spectroscopy
    "nmr" = "method",                # Nuclear Magnetic Resonance
    "pcr" = "method",                # Polymerase Chain Reaction
    "sem" = "method",                # Scanning Electron Microscopy
    "tem" = "method",                # Transmission Electron Microscopy
    "xrd" = "method",                # X-Ray Diffraction
    "saxs" = "method",               # Small-Angle X-ray Scattering
    "uv-vis" = "method",             # Ultraviolet-Visible Spectroscopy
    "ms" = "method",                 # Mass Spectrometry
    "ms/ms" = "method",              # Tandem Mass Spectrometry
    "lc" = "method",                 # Liquid Chromatography
    "gc" = "method",                 # Gas Chromatography
    "tga" = "method",                # Thermogravimetric Analysis
    "dsc" = "method",                # Differential Scanning Calorimetry
    "uv" = "method",                 # Ultraviolet
    "ir" = "method",                 # Infrared
    "rna-seq" = "method",            # RNA Sequencing
    "qtof" = "method",               # Quadrupole Time-of-Flight
    "mri" = "method",                # Magnetic Resonance Imaging
    "ct" = "method",                 # Computed Tomography
    "pet" = "method",                # Positron Emission Tomography
    "spect" = "method",              # Single-Photon Emission Computed Tomography
    "ecg" = "method",                # Electrocardiogram
    "eeg" = "method",                # Electroencephalogram
    "emg" = "method",                # Electromyography
    "fmri" = "method",               # Functional Magnetic Resonance Imaging
    "qsar" = "method",               # Quantitative Structure-Activity Relationship
    "qspr" = "method",               # Quantitative Structure-Property Relationship

    # Common biostatistical methods incorrectly classified as chemicals
    "anova" = "method",              # Analysis of Variance
    "ancova" = "method",             # Analysis of Covariance
    "manova" = "method",             # Multivariate Analysis of Variance
    "pca" = "method",                # Principal Component Analysis
    "sem" = "method",                # Structural Equation Modeling
    "glm" = "method",                # Generalized Linear Model
    "lda" = "method",                # Linear Discriminant Analysis
    "svm" = "method",                # Support Vector Machine
    "ann" = "method",                # Artificial Neural Network
    "kmeans" = "method",             # K-means clustering
    "roc" = "method",                # Receiver Operating Characteristic
    "auc" = "method",                # Area Under the Curve

    # Database and algorithm acronyms
    "kegg" = "database",             # Kyoto Encyclopedia of Genes and Genomes
    "smiles" = "method",             # Simplified Molecular-Input Line-Entry System
    "blast" = "method",              # Basic Local Alignment Search Tool
    "mace" = "method",               # Major Adverse Cardiac Events

    # Correct classifications for symptoms
    "pain" = "symptom",
    "headache" = "symptom",
    "migraine" = "disease",
    "nausea" = "symptom",
    "vomiting" = "symptom",
    "dizziness" = "symptom",
    "fatigue" = "symptom",
    "weakness" = "symptom",
    "aura" = "symptom",
    "photophobia" = "symptom",
    "phonophobia" = "symptom",

    # Proteins and receptors
    "receptor" = "protein",
    "receptors" = "protein",
    "channel" = "protein",
    "channels" = "protein",
    "transporter" = "protein",
    "transporters" = "protein",

    # Biological processes
    "inflammation" = "biological_process",
    "signaling" = "biological_process",
    "activation" = "biological_process",
    "inhibition" = "biological_process",
    "regulation" = "biological_process",
    "phosphorylation" = "biological_process"
  )

  # If the term is an acronym in our dictionary, check against claimed_type
  if (term_lower %in% names(acronym_corrections)) {
    correct_type <- acronym_corrections[[term_lower]]

    # If a claimed type is provided, check if it matches our correction
    if (!is.null(claimed_type)) {
      # Return TRUE if it matches, FALSE otherwise
      return(claimed_type == correct_type ||
               # Also allow method to be diagnostic_procedure or therapeutic_procedure
               (correct_type == "method" &&
                  (claimed_type == "diagnostic_procedure" || claimed_type == "therapeutic_procedure")))
    } else {
      # If no claimed type provided, return TRUE (it is a valid biomedical term)
      return(TRUE)
    }
  }

  # STAGE 1: Basic text pattern disqualifiers

  # Very short terms are rarely valid biomedical entities unless they're acronyms
  if (nchar(term) < 3 && !grepl("^[A-Z0-9]{2}$", term)) {
    return(FALSE)
  }

  # Terms consisting solely of numbers
  if (grepl("^[0-9]+$", term)) {
    return(FALSE)
  }

  # List of geographic locations that should never be biomedical entities
  geographic_locations <- c(
    "africa", "america", "asia", "australia", "europe", "north america", "south america",
    "central america", "western europe", "eastern europe", "northern europe", "southern europe",
    "middle east", "southeast asia", "east asia", "central asia", "south asia", "north africa",
    "sub-saharan africa", "oceania", "antarctica", "arctic", "caribbean", "mediterranean",
    "scandinavia", "benelux", "balkans", "pacific", "atlantic", "central europe",
    "usa", "china", "japan", "germany", "uk", "france", "italy", "spain", "russia",
    "brazil", "india", "canada", "mexico", "australia", "switzerland", "sweden", "norway"
  )

  # Immediately reject if the term is a geographic location
  if (term_lower %in% geographic_locations) {
    return(FALSE)
  }

  # Specific terms that should be rejected based on our examples
  problematic_specific_terms <- c(
    "europe", "vehicle", "optimization", "retention", "malformation" # Terms mentioned in the issues
  )

  if (term_lower %in% problematic_specific_terms) {
    # Special exception for malformation if it's being claimed as a disease
    if (term_lower == "malformation" && !is.null(claimed_type) && claimed_type == "disease") {
      # Allow "malformation" when it's claimed to be a disease
      return(TRUE)
    }
    return(FALSE)
  }

  # Very common English words and non-scientific terms (expanded list)
  common_words <- c(
    # Original list
    "the", "be", "to", "of", "and", "a", "in", "that", "have", "i", "it", "for", "not", "on",
    "with", "he", "as", "you", "do", "at", "this", "but", "his", "by", "from", "they", "we",
    "say", "her", "she", "or", "an", "will", "my", "one", "all", "would", "there", "their",
    "what", "so", "up", "out", "if", "about", "who", "get", "which", "go", "me", "when", "make",
    "can", "like", "time", "no", "just", "him", "know", "take", "people", "into", "year", "your",
    "good", "some", "could", "them", "see", "other", "than", "then", "now", "look", "only",
    "come", "its", "over", "think", "also", "back", "after", "use", "two", "how", "our", "work",
    "first", "well", "way", "even", "new", "want", "because", "any", "these", "give", "day",
    "most", "us", "very", "although", "much", "should", "still", "something", "find", "many",

    # Extended list of non-scientific terms
    "through", "more", "before", "those", "between", "same", "another", "around", "while",
    "however", "therefore", "furthermore", "moreover", "consequently", "nevertheless",
    "accordingly", "thus", "hence", "meanwhile", "subsequently", "indeed", "instead", "likewise",
    "namely", "regardless", "similarly", "specifically", "undoubtedly", "whereas", "mean",
    "analysis", "result", "method", "find", "show", "increase", "decrease", "effect", "study",
    "research", "data", "information", "measure", "value", "level", "report", "test", "change",
    "control", "development", "management", "system", "process", "model", "determine", "identify",
    "observed", "recorded", "analyzed", "evaluated", "assessed", "examined", "investigated",
    "considered", "described", "presented", "demonstrated", "indicated", "suggested", "revealed",

    # Demographic/socioeconomic terms
    "sociodemographic", "demographic", "social", "economic", "education", "income", "status",
    "cultural", "ethical", "society", "community", "population", "questionnaire", "survey",
    "interview", "assessment", "scale", "score", "index", "measurement", "evaluation",
    "analysis", "nationality", "ethnicity", "race", "gender", "sex", "age", "occupation",
    "employment", "marital", "household", "residence", "urban", "rural", "metropolitan",
    "suburban", "literacy", "socioeconomic",

    # Statistical terms
    "significant", "correlation", "regression", "association", "relationship", "analysis",
    "statistical", "clinically", "percentage", "proportion", "ratio", "factor", "variable",
    "parameter", "confidence", "interval", "probability", "likelihood", "odds", "risk",
    "hazard", "prevalence", "incidence", "rate", "frequency", "distribution", "sample",
    "population", "cohort", "group", "control", "case", "participant", "subject", "patient",
    "individual", "person", "people", "characteristic", "feature", "aspect", "element",
    "component", "quality", "quantity", "measure", "metric", "indicator", "predictor",
    "outcome", "result", "finding", "evidence",

    # Additional problematic terms from the example
    "vehicle", "optimization", "retention", "europe", "africa", "asia", "america", "australia"
  )

  if (term_lower %in% common_words) {
    return(FALSE)
  }

  # STAGE 2: Type-specific validation (positive identification)
  if (!is.null(claimed_type)) {
    # Check if term matches characteristics of claimed type
    # This is a positive identification approach - we check if the term has
    # characteristics typical of its claimed type

    # Is an acronym (common for genes, proteins, etc.)
    is_acronym <- grepl("^[A-Z0-9]{2,}$", term)

    # Check for commonly misclassified acronyms (analytical methods, etc.)
    if (is_acronym && claimed_type == "chemical") {
      # List of commonly misclassified analytical method acronyms
      method_acronyms <- c(
        "FAERS", "BCPNN", "UPLC", "FRAP", "HPLC", "LCMS", "GCMS", "MALDI",
        "ELISA", "FTIR", "NMR", "PCR", "SEM", "TEM", "XRD", "SAXS", "UVVIS",
        "MS", "MSMS", "LC", "GC", "TGA", "DSC", "UV", "IR", "RNASEQ", "QTOF",
        "MRI", "CT", "PET", "SPECT", "ECG", "EEG", "EMG", "FMRI", "QSAR", "QSPR",
        "ANOVA", "ANCOVA", "MANOVA", "PCA", "GLM", "LDA", "SVM", "ANN"
      )

      # Check if the acronym is in our list of analytical methods
      if (toupper(term) %in% method_acronyms) {
        return(FALSE)  # This is not a chemical, but an analytical method
      }
    }

    # Type-specific checks with expanded patterns
    if (claimed_type == "gene") {
      # Genes often match these patterns
      gene_patterns <- c(
        "gene", "receptor", "factor", "kinase", "transcription", "regulatory",
        "promoter", "repressor", "activator", "enhancer", "suppressor", "oncogene",
        "family", "homolog", "ortholog", "paralog", "allele", "mutant", "variant",
        "dna", "rna", "nucleotide", "sequence", "locus", "chromosome", "genome",
        "exon", "intron", "codon", "amplification", "deletion", "insertion",
        "duplication", "polymorphism", "snp", "mutation", "translocation", "inversion"
      )

      # Check for common gene naming patterns (e.g., BRCA1, TP53)
      has_gene_pattern <- any(sapply(gene_patterns, function(p) grepl(p, term_lower)))

      # Many genes are 2-5 letter acronyms followed by numbers
      is_typical_gene_format <- grepl("^[A-Z]{2,5}[0-9]*$", term)

      return(is_acronym || has_gene_pattern || is_typical_gene_format)
    }
    else if (claimed_type == "protein") {
      # Proteins often match these patterns
      protein_patterns <- c(
        "protein", "enzyme", "receptor", "channel", "transporter", "carrier", "hormone",
        "cytokine", "chemokine", "antibody", "immunoglobulin", "kinase", "phosphatase",
        "protease", "ligase", "ase$", "in$", "globulin", "albumin", "peptide", "factor",
        "subunit", "domain", "chain", "complex", "binding", "helicase", "reductase",
        "transferase", "polymerase", "dehydrogenase", "oxidase", "integrin", "fibrinogen",
        "collagen", "elastin", "myosin", "actin", "globin", "hemoglobin", "thrombin",
        "trypsin", "pepsin", "lipase", "amylase", "catalase", "lactase", "synthetase"
      )

      has_protein_pattern <- any(sapply(protein_patterns, function(p) grepl(p, term_lower)))

      # Special case: "receptor" and "receptors" should always be proteins
      if (term_lower == "receptor" || term_lower == "receptors") {
        return(TRUE)
      }

      return(is_acronym || has_protein_pattern)
    }
    else if (claimed_type == "drug") {
      # Common drug name suffixes
      drug_suffixes <- c(
        "caine$", "mycin$", "oxacin$", "dronate$", "olol$", "pril$", "sartan$", "mab$",
        "nib$", "gliptin$", "prazole$", "vastatin$", "dine$", "zosin$", "parin$", "ide$",
        "ane$", "ene$", "azole$", "azepam$", "idine$", "dipine$", "tadine$", "rubicin$",
        "citabine$", "mustine$", "phylline$", "racil$", "profen$", "sulfa$", "micin$",
        "fungin$", "nacin$", "bicin$", "trexate$", "pamide$", "semide$", "setron$",
        "ridone$", "tidine$", "afil$", "lukast$", "xaban$", "orphan$", "tretin$",
        "stigmine$", "curium$", "parib$", "tinib$", "cycline$", "tinel$", "cereb$",
        "navir$", "stat$", "thiazide$", "fibrate$", "glumide$", "glitazone$"
      )

      # Common drug classes
      drug_classes <- c(
        "antibiotic", "inhibitor", "antagonist", "agonist", "blocker", "vaccine",
        "antidepressant", "antipsychotic", "antiepileptic", "sedative", "stimulant",
        "antihistamine", "analgesic", "hormone", "antiviral", "anticancer",
        "antihypertensive", "antiinflammatory", "antidiabetic", "anticoagulant",
        "antithrombotic", "antiemetic", "anticonvulsant", "antiarrhythmic",
        "medication", "medicine", "drug", "tablet", "capsule", "solution", "injection",
        "infusion", "suspension", "syrup", "elixir", "tincture", "suppository",
        "ointment", "cream", "lotion", "gel", "patch", "implant", "spray", "inhaler",
        "antibacterial", "antifungal", "antimalarial", "antiparasitic", "antitussive",
        "bronchodilator", "decongestant", "expectorant", "laxative", "diuretic",
        "antispasmodic", "antiseptic", "anesthetic", "anxiolytic", "hypnotic",
        "antihypertensive", "cardiotonic", "vasodilator", "vasoconstrictor", "statin",
        "cytotoxic", "immunosuppressant", "immunomodulator", "antiretroviral",
        "antiepileptic", "antiemetic", "antimigraine", "muscle relaxant"
      )

      has_drug_suffix <- any(sapply(drug_suffixes, function(s) grepl(s, term_lower)))
      has_drug_class <- any(sapply(drug_classes, function(c) grepl(c, term_lower)))

      return(has_drug_suffix || has_drug_class)
    }
    else if (claimed_type == "disease") {
      # Common disease patterns
      disease_patterns <- c(
        "disease$", "disorder$", "syndrome$", "itis$", "emia$", "pathy$", "oma$", "osis$",
        "iasis$", "itis$", "algia$", "cancer", "tumor", "tumour", "infection", "deficiency",
        "failure", "dysfunction", "lesion", "malformation", "abnormality", "poisoning",
        "injury", "trauma", "stroke", "attack", "seizure", "allergy", "addiction",
        "sclerosis", "palsy", "dystrophy", "atrophy", "hypertrophy", "hyperplasia",
        "hypoplasia", "dysplasia", "neoplasia", "carcinoma", "sarcoma", "leukemia",
        "lymphoma", "melanoma", "adenoma", "hepatoma", "nephroma", "retinopathy",
        "neuropathy", "myopathy", "encephalopathy", "vasculopathy", "arthropathy",
        "gastritis", "colitis", "hepatitis", "nephritis", "bronchitis", "sinusitis",
        "dermatitis", "meningitis", "encephalitis", "myocarditis", "pancreatitis",
        "anemia", "leukemia", "thrombocytopenia", "neutropenia", "lymphopenia",
        "hyperglycemia", "hypoglycemia", "hyperkalemia", "hypokalemia", "hypernatremia",
        "hyponatremia", "hypercalcemia", "hypocalcemia", "acidosis", "alkalosis",
        "fever", "hypertension", "hypotension", "tachycardia", "bradycardia", "arrhythmia"
      )

      has_disease_pattern <- any(sapply(disease_patterns, function(p) grepl(p, term_lower)))

      # Special case: reject if term is "receptor" or "receptors" claimed as disease
      if (term_lower == "receptor" || term_lower == "receptors") {
        return(FALSE)
      }

      # Special case: "malformation" should be allowed as a disease
      if (term_lower == "malformation") {
        return(TRUE)
      }

      # Special case: "migraine" is a disease
      if (term_lower == "migraine") {
        return(TRUE)
      }

      return(has_disease_pattern)
    }
    else if (claimed_type == "chemical") {
      # Common chemical patterns
      chemical_patterns <- c(
        "acid", "oxide", "chloride", "bromide", "iodide", "fluoride", "hydroxide",
        "carbonate", "sulfate", "nitrate", "phosphate", "acetate", "citrate", "sulfide",
        "amine", "amide", "ester", "ether", "alcohol", "phenol", "ketone", "aldehyde",
        "hydrocarbon", "lipid", "carbohydrate", "steroid", "alkaloid", "glycoside",
        "amino acid", "nucleotide", "element", "compound", "metal", "ion", "isotope",
        "molecule", "solvent", "reagent", "catalyst", "polymer", "monomer", "dimer",
        "anhydride", "anion", "cation", "salt", "base", "gas", "solution", "suspension",
        "emulsion", "colloid", "crystal", "precipitate", "solid", "liquid", "vapor",
        "distillate", "extract", "benzene", "methane", "ethane", "propane", "butane",
        "pentane", "hexane", "heptane", "octane", "nonane", "decane", "methanol",
        "ethanol", "propanol", "butanol", "glucose", "fructose", "galactose", "mannose",
        "sucrose", "lactose", "maltose", "cellulose", "starch", "glycogen", "protein",
        "peptide", "amino", "glycine", "alanine", "valine", "leucine", "isoleucine",
        "proline", "phenylalanine", "tyrosine", "tryptophan", "serine", "threonine",
        "cysteine", "methionine", "asparagine", "glutamine", "aspartate", "glutamate",
        "lysine", "arginine", "histidine", "cholesterol", "testosterone", "estrogen",
        "progesterone", "cortisol", "aldosterone", "adrenaline", "noradrenaline",
        "dopamine", "serotonin", "histamine", "acetylcholine", "adenosine", "guanosine",
        "cytidine", "thymidine", "uridine", "atp", "adp", "amp", "gtp", "gdp", "gmp"
      )

      has_chemical_pattern <- any(sapply(chemical_patterns, function(p) grepl(p, term_lower)))

      # Chemical formulas (e.g., H2O, CO2, NaCl)
      is_chemical_formula <- grepl("[A-Z][a-z]?[0-9]*", term)

      # Special case: reject terms known to be misclassified as chemicals
      if (term_lower %in% c("optimization", "retention", "vehicle", "malformation")) {
        return(FALSE)
      }

      # Special case: reject analytical method acronyms that are often misclassified as chemicals
      if (is_acronym) {
        analytical_acronyms <- c(
          "FAERS", "BCPNN", "UPLC", "FRAP", "HPLC", "LCMS", "GCMS", "MALDI",
          "ELISA", "FTIR", "NMR", "PCR", "SEM", "TEM", "XRD", "SAXS", "UV", "IR",
          "MS", "LC", "GC", "CT", "MRI", "PET", "ROC", "AUC", "ANOVA", "PCA"
        )

        if (toupper(term) %in% analytical_acronyms) {
          return(FALSE)
        }
      }

      return(has_chemical_pattern || is_chemical_formula || is_acronym)
    }
    else if (claimed_type == "pathway") {
      # Common pathway patterns
      pathway_patterns <- c(
        "pathway", "signaling", "cascade", "cycle", "biosynthesis", "metabolism",
        "degradation", "synthesis", "catabolism", "anabolism", "oxidation", "reduction",
        "phosphorylation", "glycolysis", "gluconeogenesis", "respiration", "transport",
        "signalling", "transduction", "activation", "inhibition", "regulation",
        "homeostasis", "feedback", "response", "mechanism", "process", "circuit",
        "network", "cross-talk", "interplay", "interaction", "communication",
        "transmission", "conductance", "induction", "repression", "amplification",
        "attenuation", "potentiation", "oscillation", "cycling", "recycling",
        "metabolism", "anabolism", "catabolism", "glycolysis", "gluconeogenesis",
        "glycogenolysis", "glycogenesis", "proteolysis", "proteogenesis", "lipolysis",
        "lipogenesis", "ketogenesis", "ketolysis", "thermogenesis", "hematopoiesis",
        "erythropoiesis", "leukopoiesis", "lymphopoiesis", "myelopoiesis", "thrombopoiesis",
        "apoptosis", "necroptosis", "autophagy", "pyroptosis", "ferroptosis", "senescence",
        "differentiation", "proliferation", "maturation", "migration", "chemotaxis",
        "phagocytosis", "endocytosis", "exocytosis", "transcytosis", "pinocytosis",
        "secretion", "absorption", "diffusion", "filtration", "osmosis", "reabsorption"
      )

      has_pathway_pattern <- any(sapply(pathway_patterns, function(p) grepl(p, term_lower)))

      return(has_pathway_pattern)
    }
    else if (claimed_type == "biological_process") {
      # Common biological process patterns
      bioprocess_patterns <- c(
        "process", "regulation", "activation", "inhibition", "induction", "suppression",
        "proliferation", "differentiation", "apoptosis", "necrosis", "autophagy", "growth",
        "development", "maturation", "aging", "senescence", "inflammation", "fibrosis",
        "angiogenesis", "healing", "repair", "regeneration", "immunity", "response",
        "secretion", "expression", "translation", "transcription", "replication", "binding",
        "signaling", "transduction", "transmission", "recognition", "adhesion", "migration",
        "biogenesis", "morphogenesis", "organogenesis", "embryogenesis", "hematopoiesis",
        "neurogenesis", "vasculogenesis", "myogenesis", "osteogenesis", "chondrogenesis",
        "adipogenesis", "lymphopoiesis", "erythropoiesis", "myelopoiesis", "granulopoiesis",
        "monocytopoiesis", "thrombopoiesis", "spermatogenesis", "oogenesis", "fertilization",
        "implantation", "gastrulation", "neurulation", "placentation", "parturition",
        "lactation", "respiration", "circulation", "digestion", "absorption", "assimilation",
        "excretion", "homeostasis", "thermoregulation", "osmoregulation", "metabolism",
        "catabolism", "anabolism", "glycolysis", "gluconeogenesis", "glycogenolysis",
        "glycogenesis", "lipolysis", "lipogenesis", "proteolysis", "proteogenesis",
        "detoxification", "biotransformation", "conjugation", "elimination", "oxidation",
        "reduction", "phosphorylation", "dephosphorylation", "methylation", "demethylation",
        "acetylation", "deacetylation", "ubiquitination", "deubiquitination", "glycosylation"
      )

      has_bioprocess_pattern <- any(sapply(bioprocess_patterns, function(p) grepl(p, term_lower)))

      return(has_bioprocess_pattern)
    }
    else if (claimed_type == "molecular_function") {
      # Common molecular function patterns
      molfunction_patterns <- c(
        "binding", "activity", "function", "catalysis", "hydrolysis", "synthesis",
        "polymerization", "depolymerization", "phosphorylation", "dephosphorylation",
        "methylation", "demethylation", "acetylation", "deacetylation", "ubiquitination",
        "sumoylation", "glycosylation", "transport", "uptake", "release", "secretion",
        "import", "export", "recognition", "interaction", "regulation", "activation",
        "inhibition", "transduction", "transmission", "receptor", "ligand", "cofactor",
        "coenzyme", "prosthetic", "modulator", "effector", "mediator", "transmitter",
        "antagonist", "agonist", "blocker", "inhibitor", "activator", "stimulator",
        "repressor", "inducer", "catalyst", "enzyme", "isomerase", "transferase",
        "hydrolase", "lyase", "oxidoreductase", "ligase", "kinase", "phosphatase",
        "protease", "nuclease", "glycosidase", "lipase", "transporter", "carrier",
        "channel", "pump", "exchanger", "symporter", "antiporter", "uniporter",
        "permease", "porin", "translocase", "translocator", "transferrin", "ferritin",
        "globin", "hemoglobin", "myoglobin", "albumin", "immunoglobulin", "antibody",
        "antigen", "complement", "cytokine", "chemokine", "interferon", "interleukin",
        "growth factor", "hormone", "neurotransmitter", "neuromodulator", "peptide",
        "neuropeptide", "endorphin", "enkephalin", "dynorphin", "substance"
      )

      has_molfunction_pattern <- any(sapply(molfunction_patterns, function(p) grepl(p, term_lower)))

      return(has_molfunction_pattern)
    }
    else if (claimed_type == "cellular_component" || claimed_type == "cell") {
      # Common cellular component patterns
      cell_patterns <- c(
        "cell", "membrane", "cytoplasm", "nucleus", "organelle", "mitochondrion",
        "mitochondria", "endoplasmic", "golgi", "lysosome", "vesicle", "vacuole",
        "peroxisome", "ribosome", "cytoskeleton", "microtubule", "microfilament",
        "chromosome", "chromatin", "nucleolus", "centrosome", "centriole", "cilium",
        "flagellum", "axoneme", "spindle", "cortex", "matrix", "lumen", "junction",
        "desmosome", "phagosome", "endosome", "exosome", "soma", "dendrite", "axon",
        "synapse", "neuron", "fibroblast", "macrophage", "lymphocyte", "erythrocyte",
        "platelet", "epithelial", "endothelial", "muscle", "nuclear", "nucleoplasm",
        "nucleoid", "nucleoplasm", "nucleopore", "nuclear envelope", "nuclear matrix",
        "nuclear lamina", "nuclear pore", "nuclear receptor", "chromatin", "chromosome",
        "chromatid", "centromere", "telomere", "kinetochore", "cytoplasmic", "cytosol",
        "cytoskeleton", "microfilament", "microtubule", "intermediate filament",
        "actin", "myosin", "tubulin", "kinesin", "dynein", "spectrin", "ankyrin",
        "integrin", "cadherin", "selectin", "immunoglobulin", "fibronectin", "laminin",
        "collagen", "elastin", "proteoglycan", "glycoprotein", "lipoprotein", "receptor",
        "channel", "pump", "transporter", "carrier", "anchoring", "scaffold", "matrix",
        "basal lamina", "basement membrane", "extracellular matrix", "tight junction",
        "gap junction", "adherens junction", "desmosome", "hemidesmosome", "focal adhesion",
        "zonula adherens", "zonula occludens", "macula adherens", "neuromuscular junction",
        "neuron", "neurite", "dendrite", "axon", "synapse", "presynaptic", "postsynaptic",
        "synaptic vesicle", "synaptic cleft", "neurotransmitter", "neuroreceptor"
      )

      has_cell_pattern <- any(sapply(cell_patterns, function(p) grepl(p, term_lower)))

      return(has_cell_pattern)
    }
    else if (claimed_type == "symptom") {
      # Common symptom patterns - significantly expanded
      symptom_patterns <- c(
        "pain", "ache", "fever", "cough", "rash", "swelling", "inflammation", "fatigue",
        "weakness", "dizziness", "vertigo", "nausea", "vomiting", "diarrhea", "constipation",
        "bleeding", "discharge", "lesion", "loss", "deficit", "malaise", "discomfort",
        "distress", "dyspnea", "shortness", "tachycardia", "bradycardia", "hypertension",
        "hypotension", "hyperglycemia", "hypoglycemia", "seizure", "paralysis", "spasm",
        "tremor", "headache", "anorexia", "insomnia", "anxiety", "depression", "symptom",
        "sign", "manifestation", "complaint", "condition", "syndrome", "presentation",
        "syndrome", "disorder", "palpitation", "arrhythmia", "dysphagia", "dysphonia",
        "dysarthria", "aphasia", "amnesia", "paresthesia", "dysesthesia", "anesthesia",
        "hyperesthesia", "hypoesthesia", "ataxia", "apraxia", "agnosia", "alexia",
        "agraphia", "acalculia", "hemianopia", "hemianopsia", "scotoma", "diplopia",
        "amblyopia", "amaurosis", "photophobia", "phonophobia", "hyperacusis", "tinnitus",
        "vertigo", "syncope", "presyncope", "diaphoresis", "hyperhidrosis", "anhidrosis",
        "pruritus", "urticaria", "erythema", "petechiae", "purpura", "ecchymosis",
        "jaundice", "cyanosis", "pallor", "flushing", "edema", "ascites", "pleural effusion",
        "pericardial effusion", "hemoptysis", "hematemesis", "melena", "hematochezia",
        "hematuria", "dysuria", "polyuria", "oliguria", "anuria", "polydipsia", "polyphagia",
        "dyspepsia", "bloating", "flatulence", "tenesmus", "steatorrhea", "pyrosis", "heartburn",
        "migraine", "aura", "osmophobia", "allodynia", "hypersensitivity", "paresthesia"
      )

      # Explicit checks for common symptoms that are frequently misclassified
      if (term_lower %in% c("pain", "headache", "migraine", "nausea", "vomiting",
                            "dizziness", "fatigue", "weakness", "photophobia")) {
        return(TRUE)
      }

      # General check for symptom patterns
      has_symptom_pattern <- any(sapply(symptom_patterns, function(p) grepl(paste0("\\b", p, "\\b"), term_lower)))

      return(has_symptom_pattern)
    }
    else if (claimed_type == "organism") {
      # Common organism patterns
      organism_patterns <- c(
        "bacteria", "virus", "fungus", "parasite", "microbe", "pathogen", "species", "strain",
        "genus", "family", "order", "class", "phylum", "kingdom", "domain", "organism",
        "animal", "plant", "vertebrate", "invertebrate", "mammal", "bird", "reptile",
        "amphibian", "fish", "insect", "arachnid", "crustacean", "mollusk", "worm",
        "protozoa", "algae", "archaea", "prokaryote", "eukaryote", "bacteria", "bacterium",
        "bacillus", "cocci", "coccus", "spirochete", "spirillum", "rickettsia", "mycoplasma",
        "chlamydia", "escherichia", "salmonella", "shigella", "klebsiella", "proteus",
        "pseudomonas", "acinetobacter", "enterobacter", "haemophilus", "neisseria",
        "staphylococcus", "streptococcus", "enterococcus", "pneumococcus", "mycobacterium",
        "clostridium", "bacillus", "listeria", "corynebacterium", "actinomyces",
        "nocardia", "legionella", "bordetella", "brucella", "campylobacter", "helicobacter",
        "vibrio", "yersinia", "pasteurella", "francisella", "bartonella", "virus", "viral",
        "virology", "virion", "capsid", "envelope", "glycoprotein", "adenovirus", "herpesvirus",
        "papillomavirus", "polyomavirus", "poxvirus", "hepadnavirus", "retrovirus",
        "lentivirus", "rhabdovirus", "filovirus", "paramyxovirus", "orthomyxovirus",
        "bunyavirus", "arenavirus", "reovirus", "togavirus", "flavivirus", "picornavirus",
        "coronavirus", "hepatitis", "influenza", "rhinovirus", "rotavirus", "hiv", "fungus",
        "fungi", "yeast", "mold", "mould", "mushroom", "candida", "aspergillus", "cryptococcus",
        "histoplasma", "blastomyces", "coccidioides", "paracoccidioides", "sporothrix",
        "zygomycetes", "mucor", "rhizopus", "absidia", "basidiomycetes", "ascomycetes"
      )

      has_organism_pattern <- any(sapply(organism_patterns, function(p) grepl(p, term_lower)))

      return(has_organism_pattern)
    }
    else if (claimed_type == "tissue") {
      # Common tissue patterns
      tissue_patterns <- c(
        "tissue", "epithelium", "endothelium", "mesothelium", "mesenchyme", "stroma",
        "parenchyma", "connective", "muscle", "nerve", "neural", "vasculature", "vascular",
        "gland", "glandular", "mucous", "mucosa", "submucosa", "serosa", "adventitia",
        "periosteum", "perichondrium", "synovium", "bone", "cartilage", "tendon", "ligament",
        "fascia", "adipose", "blood", "lymph", "marrow", "skin", "dermal", "epidermal",
        "cutaneous", "subcutaneous", "mucosal", "membrane", "meninges", "dura", "arachnoid",
        "pia", "pleura", "peritoneum", "pericardium", "endocardium", "myocardium", "epicardium",
        "endometrium", "myometrium", "perimetrium", "glomerulus", "tubule", "nephron",
        "hepatocyte", "bile duct", "islet", "acinus", "alveolus", "bronchus", "bronchiole",
        "trachea", "esophagus", "stomach", "intestine", "duodenum", "jejunum", "ileum",
        "colon", "rectum", "bladder", "urethra", "ureter", "prostate", "testis", "ovary",
        "uterus", "fallopian", "cervix", "vagina", "breast", "thyroid", "parathyroid",
        "adrenal", "pituitary", "thymus", "spleen", "lymph node", "tonsil", "cerebrum",
        "cerebellum", "brain stem", "spinal cord", "ganglion", "nerve", "neuron", "retina",
        "cornea", "sclera", "choroid", "iris", "lens", "vitreous", "aqueous", "cochlea",
        "vestibule", "labyrinth", "muscle", "skeletal", "cardiac", "smooth", "tendon",
        "ligament", "cartilage", "joint", "synovium", "bone", "cortical", "trabecular",
        "marrow", "osteoid", "callus", "granulation", "scar", "fibrosis", "granuloma"
      )

      has_tissue_pattern <- any(sapply(tissue_patterns, function(p) grepl(p, term_lower)))

      return(has_tissue_pattern)
    }
    else if (claimed_type == "anatomy") {
      # Common anatomy patterns
      anatomy_patterns <- c(
        "head", "neck", "thorax", "chest", "abdomen", "pelvis", "back", "extremity", "arm",
        "forearm", "wrist", "hand", "finger", "thumb", "leg", "thigh", "knee", "calf", "ankle",
        "foot", "toe", "shoulder", "elbow", "hip", "joint", "skull", "cranium", "vertebra",
        "spine", "rib", "sternum", "clavicle", "scapula", "humerus", "radius", "ulna", "carpal",
        "metacarpal", "phalanx", "pelvis", "femur", "patella", "tibia", "fibula", "tarsal",
        "metatarsal", "brain", "cerebrum", "cerebellum", "brainstem", "thalamus", "hypothalamus",
        "pituitary", "pineal", "basal ganglia", "hippocampus", "amygdala", "fornix", "corpus callosum",
        "ventricle", "meninges", "dura", "arachnoid", "pia", "spinal cord", "nerve", "ganglion",
        "plexus", "heart", "atrium", "ventricle", "septum", "valve", "mitral", "tricuspid",
        "pulmonary", "aortic", "aorta", "artery", "arteriole", "capillary", "venule", "vein",
        "vena cava", "pulmonary", "coronary", "carotid", "jugular", "subclavian", "axillary",
        "brachial", "radial", "ulnar", "femoral", "popliteal", "tibial", "lung", "bronchus",
        "bronchiole", "alveolus", "pleura", "diaphragm", "trachea", "larynx", "pharynx",
        "esophagus", "stomach", "duodenum", "jejunum", "ileum", "cecum", "colon", "appendix",
        "rectum", "anus", "liver", "gallbladder", "bile duct", "pancreas", "spleen", "kidney",
        "ureter", "bladder", "urethra", "prostate", "testis", "epididymis", "vas deferens",
        "seminal vesicle", "penis", "urethra", "ovary", "fallopian", "uterus", "cervix",
        "vagina", "vulva", "mammary", "thyroid", "parathyroid", "adrenal", "thymus", "lymph node",
        "tonsil", "eye", "orbit", "eyelid", "conjunctiva", "cornea", "sclera", "choroid",
        "retina", "lens", "iris", "pupil", "ciliary", "ear", "pinna", "external", "middle",
        "inner", "tympanic", "ossicle", "cochlea", "vestibule", "labyrinth", "nose", "nasal",
        "paranasal", "sinus", "mouth", "lip", "tongue", "teeth", "gingiva", "palate", "uvula",
        "skin", "epidermis", "dermis", "subcutaneous", "hair", "nail", "sebaceous", "sweat",
        "ligament", "tendon", "fascia", "bursa", "synovium", "cartilage", "meniscus", "disc"
      )

      has_anatomy_pattern <- any(sapply(anatomy_patterns, function(p) grepl(p, term_lower)))

      return(has_anatomy_pattern)
    }
    else if (claimed_type == "diagnostic_procedure") {
      # Common diagnostic procedure patterns
      diagnostic_procedure_patterns <- c(
        "test", "scan", "imaging", "radiograph", "ultrasound", "sonogram", "tomography",
        "resonance", "endoscopy", "biopsy", "aspiration", "culture", "assay", "analysis",
        "measurement", "examination", "evaluation", "assessment", "screening", "monitoring",
        "probe", "detection", "quantification", "identification", "diagnosis", "diagnostic",
        "radiography", "xray", "x-ray", "radiogram", "fluoroscopy", "angiography", "venography",
        "lymphangiography", "myelography", "arthrography", "mammography", "tomography",
        "ct", "cat scan", "computed tomography", "mri", "magnetic resonance", "fmri",
        "functional mri", "pet", "positron emission", "spect", "single photon", "ultrasound",
        "sonography", "doppler", "echocardiography", "electrocardiography", "ecg", "ekg",
        "electroencephalography", "eeg", "electromyography", "emg", "electroneuronography",
        "nerve conduction", "electrophysiology", "endoscopy", "colonoscopy", "sigmoidoscopy",
        "proctoscopy", "anoscopy", "esophagogastroduodenoscopy", "egds", "bronchoscopy",
        "laryngoscopy", "rhinoscopy", "otoscopy", "cystoscopy", "ureteroscopy", "nephroscopy",
        "laparoscopy", "arthroscopy", "colposcopy", "hysteroscopy", "biopsy", "aspiration",
        "culture", "gram stain", "sensitivity", "pcr", "polymerase chain reaction", "elisa",
        "western blot", "southern blot", "northern blot", "immunoassay", "immunohistochemistry",
        "serology", "titer", "flow cytometry", "karyotype", "cytogenetics", "genetic",
        "sequencing", "microarray", "histopathology", "cytology", "hematology", "chemistry",
        "metabolic", "electrolytes", "glucose", "glycemia", "lipid profile", "cholesterol",
        "triglycerides", "liver function", "renal function", "creatinine", "bun", "urea",
        "hemoglobin", "hematocrit", "complete blood count", "cbc", "coagulation", "urinalysis",
        "urine", "stool", "cerebrospinal", "fluid", "synovial", "pleural", "pericardial",
        "peritoneal", "bronchial", "lavage", "sputum", "spectrum", "spectra", "spectroscopy",
        "spectrophotometry", "chromatography", "mass spectrometry", "electrophoresis"
      )

      # Special case for analytical techniques often misclassified
      analytical_techniques <- c(
        "faers", "bcpnn", "uplc", "frap", "hplc", "lc-ms", "gc-ms", "maldi",
        "elisa", "ft-ir", "nmr", "pcr", "sem", "tem", "xrd", "saxs", "uv-vis",
        "ms", "ms/ms", "lc", "gc", "tga", "dsc", "uv", "ir", "rna-seq", "qtof",
        "mri", "ct", "pet", "spect", "ecg", "eeg", "emg", "fmri", "qsar", "qspr",
        "anova", "ancova", "manova", "pca", "sem", "glm", "lda", "svm", "ann"
      )

      # Check if term is an analytical technique
      if (term_lower %in% analytical_techniques) {
        return(TRUE)
      }

      # Check if it's a diagnostic procedure
      has_diagnostic_procedure_pattern <- any(sapply(diagnostic_procedure_patterns, function(p) grepl(p, term_lower)))

      return(has_diagnostic_procedure_pattern)
    }
    else if (claimed_type == "therapeutic_procedure") {
      # Common therapeutic procedure patterns
      therapeutic_procedure_patterns <- c(
        "therapy", "treatment", "intervention", "management", "procedure", "surgery", "operation",
        "excision", "resection", "removal", "transplantation", "implantation", "replacement",
        "repair", "reconstruction", "restoration", "augmentation", "reduction", "extraction",
        "amputation", "anastomosis", "bypass", "graft", "flap", "catheterization", "intubation",
        "cannulation", "injection", "infusion", "transfusion", "dialysis", "filtration",
        "transplant", "prosthesis", "fixation", "manipulation", "reposition", "reduction",
        "traction", "immobilization", "stimulation", "radiation", "irradiation", "ablation",
        "cauterization", "coagulation", "cryotherapy", "hyperthermia", "phototherapy", "laser",
        "ultrasound", "lithotripsy", "dilatation", "dilation", "stenting", "drainage", "aspiration",
        "lavage", "debridement", "curettage", "anesthesia", "sedation", "analgesia", "ventilation",
        "oxygenation", "suction", "suctioning", "tracheostomy", "gastrostomy", "jejunostomy",
        "ileostomy", "colostomy", "cystostomy", "nephrostomy", "thoracostomy", "thoracentesis",
        "paracentesis", "arthrocentesis", "lumbar puncture", "spinal tap", "amniocentesis",
        "chorionic villus sampling", "cordocentesis", "biopsy", "excisional", "incisional",
        "needle", "aspiration", "incision", "excision", "dissection", "ligation", "suture",
        "stapling", "amputation", "disarticulation", "arthrodesis", "fusion", "arthroplasty",
        "replacement", "osteotomy", "tenorrhaphy", "tenolysis", "tenotomy", "myotomy", "myorrhaphy",
        "neurorrhaphy", "neurolysis", "neurectomy", "sympathectomy", "vagotomy", "rhizotomy",
        "cordotomy", "tractotomy", "lobotomy", "craniotomy", "craniectomy", "cranioplasty",
        "ventriculostomy", "shunt", "laminectomy", "discectomy", "foraminotomy", "vertebroplasty",
        "kyphoplasty", "thoracotomy", "thoracoplasty", "pneumonectomy", "lobectomy", "segmentectomy",
        "wedge resection", "pleurodesis", "pleurectomy", "decortication", "pericardiectomy",
        "pericardiostomy", "valvuloplasty", "valvulotomy", "commissurotomy", "annuloplasty",
        "ventriculoplasty", "bypass", "endarterectomy", "thrombectomy", "embolectomy",
        "aneurysmectomy", "aneurysmorrhaphy", "varicectomy", "sclerotherapy", "gastrectomy",
        "vagotomy", "gastroenterostomy", "gastropexy", "fundoplication", "pyloromyotomy",
        "pyloroplasty", "esophagectomy", "esophagostomy", "esophagoplasty", "colectomy",
        "colostomy", "ileostomy", "appendectomy", "proctectomy", "hemorrhoidectomy", "hepatectomy",
        "hepatorrhaphy", "cholecystectomy", "choledochostomy", "splenectomy", "pancreatectomy",
        "nephrectomy", "nephrostomy", "nephropexy", "pyeloplasty", "ureteroplasty", "cystectomy",
        "cystostomy", "cystoplasty", "prostatectomy", "vasectomy", "vasovasostomy", "orchidectomy",
        "orchiectomy", "orchiopexy", "hydrocelectomy", "varicocelectomy", "hysterectomy", "oophorectomy"
      )

      has_therapeutic_procedure_pattern <- any(sapply(therapeutic_procedure_patterns, function(p) grepl(p, term_lower)))

      return(has_therapeutic_procedure_pattern)
    }
    else if (claimed_type == "method") {
      # Common method patterns - especially for technical terms often misclassified as chemicals
      method_patterns <- c(
        "method", "technique", "assay", "analysis", "procedure", "protocol", "algorithm",
        "approach", "workflow", "process", "measurement", "detection", "quantification",
        "identification", "determination", "evaluation", "assessment", "test", "screen",
        "monitor", "examine", "characterize", "validate", "verify", "qualify", "quantify",
        "calculate", "estimate", "predict", "model", "simulate", "standardize", "optimize",
        "chromatography", "spectroscopy", "microscopy", "spectrometry", "electrophoresis",
        "sequencing", "immunoassay", "radiography", "tomography", "ultrasound", "imaging"
      )

      # List of specific analytical methods and their acronyms
      analytical_methods <- c(
        "faers", "fda adverse event reporting system", "bcpnn", "bayesian confidence propagation neural network",
        "uplc", "ultra performance liquid chromatography", "frap", "fluorescence recovery after photobleaching",
        "hplc", "high performance liquid chromatography", "lc-ms", "liquid chromatography-mass spectrometry",
        "gc-ms", "gas chromatography-mass spectrometry", "maldi", "matrix-assisted laser desorption/ionization",
        "elisa", "enzyme-linked immunosorbent assay", "ft-ir", "fourier transform infrared spectroscopy",
        "nmr", "nuclear magnetic resonance", "pcr", "polymerase chain reaction", "sem", "scanning electron microscopy",
        "tem", "transmission electron microscopy", "xrd", "x-ray diffraction", "saxs", "small-angle x-ray scattering",
        "uv-vis", "ultraviolet-visible spectroscopy", "ms", "mass spectrometry", "ms/ms", "tandem mass spectrometry",
        "lc", "liquid chromatography", "gc", "gas chromatography", "tga", "thermogravimetric analysis",
        "dsc", "differential scanning calorimetry", "uv", "ultraviolet", "ir", "infrared", "rna-seq", "rna sequencing",
        "qtof", "quadrupole time-of-flight", "mri", "magnetic resonance imaging", "ct", "computed tomography",
        "pet", "positron emission tomography", "spect", "single-photon emission computed tomography",
        "ecg", "electrocardiogram", "eeg", "electroencephalogram", "emg", "electromyography",
        "fmri", "functional magnetic resonance imaging", "qsar", "quantitative structure-activity relationship",
        "qspr", "quantitative structure-property relationship", "anova", "analysis of variance",
        "ancova", "analysis of covariance", "manova", "multivariate analysis of variance",
        "pca", "principal component analysis", "sem", "structural equation modeling",
        "glm", "generalized linear model", "lda", "linear discriminant analysis",
        "svm", "support vector machine", "ann", "artificial neural network",
        "roc", "receiver operating characteristic", "auc", "area under the curve"
      )

      # If term is an analytical technique acronym, it's a method
      if (term_lower %in% analytical_methods || is_acronym) {
        return(TRUE)
      }

      # Check for method patterns
      has_method_pattern <- any(sapply(method_patterns, function(p) grepl(p, term_lower)))

      return(has_method_pattern)
    }

    # If no specific rules for this type, allow it to pass
    return(TRUE)
  }

  # STAGE 3: Check for general biomedical term characteristics

  # Check common biomedical characteristics that span multiple types
  # If a term has any of these characteristics, it's likely a biomedical entity

  # Is a recognized acronym pattern common in biomedicine (2-6 uppercase letters, maybe with numbers)
  is_biomedical_acronym <- grepl("^[A-Z]{2,6}[0-9]*$", term)

  # Has Latin or Greek roots common in medical terminology
  has_latin_greek_roots <- grepl("(itis|osis|emia|pathy|trophy|plasia|poiesis|genesis|lysis|ectomy|otomy|ostomy|plasty|pexy|rhaphy|graphy|scopy|metry)", term_lower)

  # Contains numbers and chemical elements (common in chemical formulas)
  is_chemical_formula <- grepl("[A-Z][a-z]?[0-9]+", term)

  # Common biomedical term endings
  biomedical_suffixes <- c("in$", "ase$", "gen$", "one$", "ide$", "ate$", "ene$", "ane$", "ole$",
                           "itis$", "osis$", "emia$", "pathy$", "trophy$", "plasia$", "poiesis$",
                           "genesis$", "lysis$", "ectomy$", "otomy$", "ostomy$", "plasty$", "pexy$",
                           "rhaphy$", "graphy$", "scopy$", "metry$", "algia$", "dynia$", "oma$",
                           "iasis$", "ismus$", "uria$", "pnea$", "emesis$", "pepsia$", "phagia$",
                           "rrhea$", "rrhage$", "sthenia$", "phobia$", "lexia$", "praxia$", "gnosis$",
                           "penia$", "cytosis$", "esthesia$", "kinesia$", "phasia$", "plegia$", "paresis$")

  has_biomedical_suffix <- any(sapply(biomedical_suffixes, function(s) grepl(s, term_lower)))

  # Check for compound terms that contain recognizable biomedical components
  biomedical_components <- c("neuro", "cardio", "gastro", "hepato", "nephro", "dermato",
                             "hemato", "immuno", "onco", "osteo", "arthro", "myelo", "cyto",
                             "histo", "patho", "pharmaco", "psycho", "toxo", "vas", "angio")

  has_biomedical_component <- any(sapply(biomedical_components, function(c) grepl(c, term_lower)))

  # Additional check for terms that should NEVER be considered biomedical entities
  never_biomedical <- c(
    # Geographic regions and locations
    "europe", "asia", "africa", "america", "australia", "us", "uk", "usa",
    # General abstract concepts
    "vehicle", "optimization", "retention",
    # Problematic general terms from example
    "malformation", "receptor", "receptors"
  )

  # Special case handling exceptions - these are only valid with specific type claims
  special_exceptions <- list(
    "malformation" = "disease",   # Malformation is valid as a disease
    "receptor" = "protein",       # Receptor is valid as a protein
    "receptors" = "protein"       # Receptors is valid as a protein
  )

  # Check if the term is in our never_biomedical list
  if (term_lower %in% never_biomedical) {
    # If it's in special_exceptions, check claimed type
    if (term_lower %in% names(special_exceptions)) {
      # Only allow if claimed type matches the exception type
      if (!is.null(claimed_type) && claimed_type == special_exceptions[[term_lower]]) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      # Not in exceptions, so reject
      return(FALSE)
    }
  }

  # Special check for analytical method acronyms that are often misclassified as chemicals
  if (is_biomedical_acronym) {
    analytical_acronyms <- c(
      "FAERS", "BCPNN", "UPLC", "FRAP", "HPLC", "LCMS", "GCMS", "MALDI",
      "ELISA", "FTIR", "NMR", "PCR", "SEM", "TEM", "XRD", "SAXS", "UV", "IR",
      "MS", "LC", "GC", "CT", "MRI", "PET", "ROC", "AUC", "ANOVA", "PCA"
    )

    if (toupper(term) %in% analytical_acronyms) {
      # If claimed as chemical, it's invalid
      if (!is.null(claimed_type) && claimed_type == "chemical") {
        return(FALSE)
      }
      # But it's valid as a method or diagnostic_procedure
      else if (!is.null(claimed_type) &&
               (claimed_type == "method" ||
                claimed_type == "diagnostic_procedure" ||
                claimed_type == "therapeutic_procedure")) {
        return(TRUE)
      }
      # If no claimed type, return TRUE as it's a valid biomedical term
      else if (is.null(claimed_type)) {
        return(TRUE)
      }
    }
  }

  # Return TRUE if term shows any of these biomedical characteristics
  if (is_biomedical_acronym || has_latin_greek_roots || is_chemical_formula ||
      has_biomedical_suffix || has_biomedical_component) {
    return(TRUE)
  }

  # Special check for "sociodemographic" and similar terms that are incorrectly labeled
  problematic_terms <- c("sociodemographic", "demographic", "social", "economic", "education",
                         "income", "status", "cultural", "ethical", "society", "community",
                         "population", "questionnaire", "survey", "interview", "assessment")

  if (term_lower %in% problematic_terms) {
    return(FALSE)
  }

  # If no positive identification was made and the term passed all filters, let it through
  # A future version could implement a machine learning classifier for more accuracy
  return(FALSE)  # Default to FALSE for terms that don't match any biomedical patterns
}


# Filter function for use in the ABC model
filter_terms_for_abc_model <- function(terms, entity_types = NULL) {
  valid_terms <- character(0)

  # Process each term
  for (i in seq_along(terms)) {
    term <- terms[i]

    # Get entity type if available
    entity_type <- NULL
    if (!is.null(entity_types) && term %in% names(entity_types)) {
      entity_type <- entity_types[term]
    }

    # Check if the term is a valid biomedical entity
    if (is_valid_biomedical_entity(term, entity_type)) {
      valid_terms <- c(valid_terms, term)
    }
  }

  return(valid_terms)
}

#' Apply the ABC model for literature-based discovery with improved filtering
#'
#' This function implements the ABC model for literature-based discovery with
#' enhanced term filtering and validation.
#'
#' @param co_matrix A co-occurrence matrix produced by create_comat().
#' @param a_term Character string, the source term (A).
#' @param c_term Character string, the target term (C). If NULL, all potential C terms will be evaluated.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#' @param scoring_method Method to use for scoring.
#' @param b_term_types Character vector of entity types allowed for B terms.
#' @param c_term_types Character vector of entity types allowed for C terms.
#' @param exclude_general_terms Logical. If TRUE, excludes common general terms.
#' @param filter_similar_terms Logical. If TRUE, filters out B-terms that are too similar to A-term.
#' @param similarity_threshold Numeric. Maximum allowed string similarity between A and B terms.
#' @param enforce_strict_typing Logical. If TRUE, enforces stricter entity type validation.
#' @param validation_method Character. Method to use for entity validation: "pattern", "nlp", "api", or "comprehensive".
#'
#' @return A data frame with ranked discovery results.
#' @export
abc_model <- function(co_matrix, a_term, c_term = NULL,
                      min_score = 0.1, n_results = 100,
                      scoring_method = c("multiplication", "average", "combined", "jaccard"),
                      b_term_types = NULL, c_term_types = NULL,
                      exclude_general_terms = TRUE,
                      filter_similar_terms = TRUE,
                      similarity_threshold = 0.8,
                      enforce_strict_typing = TRUE,
                      validation_method = "pattern") {

  # Match scoring_method argument
  scoring_method <- match.arg(scoring_method)

  # Set up validation function based on method
  validator <- switch(validation_method,
                      "pattern" = is_valid_biomedical_entity,
                      "nlp" = function(term, type) {
                        tryCatch({
                          validate_entity_with_nlp(term, type)
                        }, error = function(e) {
                          message("NLP validation failed, falling back to pattern-based: ", e$message)
                          is_valid_biomedical_entity(term, type)
                        })
                      },
                      "api" = function(term, type) {
                        tryCatch({
                          query_external_api(term, type)
                        }, error = function(e) {
                          message("API validation failed, falling back to pattern-based: ", e$message)
                          is_valid_biomedical_entity(term, type)
                        })
                      },
                      "comprehensive" = function(term, type) {
                        tryCatch({
                          validate_entity_comprehensive(term, type)
                        }, error = function(e) {
                          message("Comprehensive validation failed, falling back to pattern-based: ", e$message)
                          is_valid_biomedical_entity(term, type)
                        })
                      },
                      is_valid_biomedical_entity)  # Default to pattern-based

  # Check if the matrix has entity types and store in a local variable
  has_entity_types <- !is.null(attr(co_matrix, "entity_types"))
  entity_types_dict <- if (has_entity_types) attr(co_matrix, "entity_types") else NULL

  # Validate type constraints can be applied
  if ((!is.null(b_term_types) || !is.null(c_term_types)) && !has_entity_types) {
    warning("Entity type constraints specified but no entity types found in co-occurrence matrix. Constraints will be ignored.")
    b_term_types <- NULL
    c_term_types <- NULL
  }

  # Check if A term exists in the matrix
  if (!a_term %in% rownames(co_matrix)) {
    stop("A-term '", a_term, "' not found in the co-occurrence matrix")
  }

  # Check if C term exists (if provided)
  if (!is.null(c_term) && !c_term %in% rownames(co_matrix)) {
    stop("C-term '", c_term, "' not found in the co-occurrence matrix")
  }

  # Define explicit blacklist of problematic terms
  blacklisted_terms <- c(
    # Geographic locations
    "africa", "america", "asia", "australia", "europe", "north america", "south america",
    "central america", "western europe", "eastern europe", "northern europe", "southern europe",
    "usa", "uk", "us", "china", "japan", "germany", "france", "italy", "spain", "russia",

    # General terms that should never be B terms
    "method", "approach", "analysis", "assessment", "evaluation", "procedure", "technique",
    "protocol", "intervention", "treatment", "outcome", "result", "effect", "impact",
    "value", "study", "trial", "research", "experiment", "observation", "publication",
    "test", "measure", "detection", "identification", "classification", "characterization",
    "determination", "calculation", "examination", "investigation", "exploration",
    "screening", "monitoring", "surveillance", "survey", "review", "overview", "summary",
    "score", "grade", "rating", "ranking", "stratification", "categorization", "grouping",
    "vehicle", "optimization", "retention"
  )

  # Enhanced term-type mapping for common terms that are frequently misclassified
  term_type_mappings <- list(
    # Analytical techniques/methods that are often misclassified as chemicals
    "faers" = "method",              # FDA Adverse Event Reporting System
    "bcpnn" = "method",              # Bayesian Confidence Propagation Neural Network
    "uplc" = "method",               # Ultra Performance Liquid Chromatography
    "frap" = "method",               # Fluorescence Recovery After Photobleaching
    "hplc" = "method",               # High Performance Liquid Chromatography
    "lc-ms" = "method",              # Liquid Chromatography-Mass Spectrometry
    "gc-ms" = "method",              # Gas Chromatography-Mass Spectrometry
    "maldi" = "method",              # Matrix-Assisted Laser Desorption/Ionization
    "elisa" = "method",              # Enzyme-Linked Immunosorbent Assay
    "ft-ir" = "method",              # Fourier Transform Infrared Spectroscopy
    "nmr" = "method",                # Nuclear Magnetic Resonance
    "pcr" = "method",                # Polymerase Chain Reaction
    "sem" = "method",                # Scanning Electron Microscopy
    "tem" = "method",                # Transmission Electron Microscopy
    "xrd" = "method",                # X-Ray Diffraction
    "saxs" = "method",               # Small-Angle X-ray Scattering
    "uv-vis" = "method",             # Ultraviolet-Visible Spectroscopy
    "ms" = "method",                 # Mass Spectrometry
    "ms/ms" = "method",              # Tandem Mass Spectrometry
    "lc" = "method",                 # Liquid Chromatography
    "gc" = "method",                 # Gas Chromatography
    "tga" = "method",                # Thermogravimetric Analysis
    "dsc" = "method",                # Differential Scanning Calorimetry
    "uv" = "method",                 # Ultraviolet
    "ir" = "method",                 # Infrared
    "rna-seq" = "method",            # RNA Sequencing
    "qtof" = "method",               # Quadrupole Time-of-Flight
    "mri" = "method",                # Magnetic Resonance Imaging
    "ct" = "method",                 # Computed Tomography
    "pet" = "method",                # Positron Emission Tomography
    "spect" = "method",              # Single-Photon Emission Computed Tomography
    "ecg" = "method",                # Electrocardiogram
    "eeg" = "method",                # Electroencephalogram
    "emg" = "method",                # Electromyography
    "fmri" = "method",               # Functional Magnetic Resonance Imaging
    "qsar" = "method",               # Quantitative Structure-Activity Relationship
    "qspr" = "method",               # Quantitative Structure-Property Relationship

    # Common biostatistical methods incorrectly classified as chemicals
    "anova" = "method",              # Analysis of Variance
    "ancova" = "method",             # Analysis of Covariance
    "manova" = "method",             # Multivariate Analysis of Variance
    "pca" = "method",                # Principal Component Analysis
    "sem" = "method",                # Structural Equation Modeling
    "glm" = "method",                # Generalized Linear Model
    "lda" = "method",                # Linear Discriminant Analysis
    "svm" = "method",                # Support Vector Machine
    "ann" = "method",                # Artificial Neural Network
    "kmeans" = "method",             # K-means clustering
    "roc" = "method",                # Receiver Operating Characteristic
    "auc" = "method",                # Area Under the Curve

    # Database and algorithm acronyms
    "kegg" = "database",             # Kyoto Encyclopedia of Genes and Genomes
    "smiles" = "method",             # Simplified Molecular-Input Line-Entry System
    "blast" = "method",              # Basic Local Alignment Search Tool
    "mace" = "method",               # Major Adverse Cardiac Events

    # Proteins and receptors
    "receptor" = "protein",
    "receptors" = "protein",
    "channel" = "protein",
    "channels" = "protein",
    "transporter" = "protein",
    "transporters" = "protein",

    # Symptoms and clinical manifestations
    "pain" = "symptom",
    "headache" = "symptom",
    "migraine" = "disease",
    "nausea" = "symptom",
    "vomiting" = "symptom",
    "dizziness" = "symptom",
    "aura" = "symptom",
    "photophobia" = "symptom",
    "phonophobia" = "symptom",

    # Biological processes
    "inflammation" = "biological_process",
    "signaling" = "biological_process",
    "activation" = "biological_process",
    "inhibition" = "biological_process",
    "regulation" = "biological_process",
    "phosphorylation" = "biological_process",
    "oxidation" = "biological_process",

    # Diseases and disorders
    "malformation" = "disease",
    "disorder" = "disease",
    "syndrome" = "disease"
  )

  # Extract A-B associations
  a_associations <- co_matrix[a_term, ]

  # Filter B terms by removing terms with low association to A
  b_terms <- names(a_associations[a_associations > min_score])

  # Remove A term from potential B terms
  b_terms <- b_terms[b_terms != a_term]

  # If C term is specified, also remove it from B terms to prevent redundancy
  if (!is.null(c_term)) {
    b_terms <- b_terms[b_terms != c_term]
  }

  # Remove blacklisted terms from potential B terms
  b_terms <- b_terms[!tolower(b_terms) %in% blacklisted_terms]

  # Enforce term-type mappings
  if (has_entity_types) {
    # Apply corrections to entity types dictionary
    for (term_lower in names(term_type_mappings)) {
      # Find matches in the co-occurrence matrix (case-insensitive)
      term_matches <- grep(paste0("^", term_lower, "$"), rownames(co_matrix), ignore.case = TRUE)
      if (length(term_matches) > 0) {
        matched_terms <- rownames(co_matrix)[term_matches]
        for (matched_term in matched_terms) {
          entity_types_dict[matched_term] <- term_type_mappings[[term_lower]]
        }
      }
    }

    # Update entity types in the co-occurrence matrix
    attr(co_matrix, "entity_types") <- entity_types_dict
  }

  # Apply enhanced biomedical entity filtering to B terms if requested
  if (exclude_general_terms) {
    original_b_count <- length(b_terms)

    # Apply improved filtering that ensures B terms are valid biomedical entities
    valid_b_terms <- character(0)

    for (b_term in b_terms) {
      # Check if term is in our mapping and get the correct type
      b_type <- NULL
      term_lower <- tolower(b_term)

      if (term_lower %in% names(term_type_mappings)) {
        b_type <- term_type_mappings[[term_lower]]
      } else if (has_entity_types && b_term %in% names(entity_types_dict)) {
        b_type <- entity_types_dict[b_term]
      }

      # Apply validation using the selected validator
      if (validator(b_term, b_type)) {
        valid_b_terms <- c(valid_b_terms, b_term)
      }
    }

    # Update b_terms with filtered list
    b_terms <- valid_b_terms

    filtered_count <- original_b_count - length(b_terms)
    percent_filtered <- if (original_b_count > 0) {
      round((filtered_count / original_b_count) * 100, 1)
    } else {
      0
    }

    if (filtered_count > 0) {
      message("Filtered ", filtered_count, " B terms (", percent_filtered, "%) that weren't valid biomedical entities")
    }

    # If filtering is too aggressive, provide a warning
    if (length(b_terms) < 0.1 * original_b_count && original_b_count > 10) {
      message("Warning: Term filtering removed most B terms (", percent_filtered, "% filtered). Results may be limited.")
    }
  }

  # Apply B-term type constraints if specified
  if (!is.null(b_term_types) && has_entity_types) {
    original_b_count <- length(b_terms)
    valid_b_terms <- character(0)

    # Get B terms with matching types
    for (b_term in b_terms) {
      b_type <- NULL
      term_lower <- tolower(b_term)

      # Check if term is in our mapping first
      if (term_lower %in% names(term_type_mappings)) {
        b_type <- term_type_mappings[[term_lower]]
      }
      # Otherwise check in entity_types_dict if available
      else if (has_entity_types && b_term %in% names(entity_types_dict)) {
        b_type <- entity_types_dict[b_term]
      }

      # If we found a type, validate it
      if (!is.null(b_type)) {
        # Additional type validation for problematic terms
        if (enforce_strict_typing) {
          # Check if b_term with claimed type is valid using the selected validator
          if (!validator(b_term, b_type)) {
            next
          }
        }

        if (b_type %in% b_term_types) {
          valid_b_terms <- c(valid_b_terms, b_term)
        }
      }
    }

    # Replace b_terms with filtered list
    b_terms <- valid_b_terms

    filtered_count <- original_b_count - length(b_terms)
    if (filtered_count > 0) {
      message("Filtered ", filtered_count, " B terms that didn't match specified entity types: ",
              paste(b_term_types, collapse = ", "))
    }
  }

  # Filter out B terms that are too similar to A term if requested
  if (filter_similar_terms && length(b_terms) > 0) {
    # Function to calculate string similarity
    string_similarity <- function(a, b) {
      # Calculate Levenshtein distance
      a_lower <- tolower(a)
      b_lower <- tolower(b)

      # Check for pluralization by removing trailing 's'
      a_singular <- sub("s$", "", a_lower)
      b_singular <- sub("s$", "", b_lower)

      # Check for stemming similarity
      stem_sim <- a_singular == b_singular

      # Calculate basic string similarity ratio
      if (nchar(a_lower) == 0 || nchar(b_lower) == 0) {
        basic_sim <- 0
      } else {
        lev_dist <- adist(a_lower, b_lower)[1,1]
        max_len <- max(nchar(a_lower), nchar(b_lower))
        basic_sim <- 1 - (lev_dist / max_len)
      }

      # Return maximum similarity from the two methods
      return(max(basic_sim, as.numeric(stem_sim)))
    }

    # Calculate similarity for each B term
    original_b_count <- length(b_terms)
    similarities <- sapply(b_terms, function(b) string_similarity(a_term, b))

    # Filter out terms that are too similar to A term
    dissimilar_indices <- which(similarities < similarity_threshold)

    if (length(dissimilar_indices) > 0) {
      b_terms <- b_terms[dissimilar_indices]
      message("Filtered out ", original_b_count - length(b_terms),
              " B terms that were too similar to A term (similarity threshold: ",
              similarity_threshold, ")")
    } else if (original_b_count > 0) {
      # As a fallback, keep at least the least similar terms
      message("Warning: All B terms were filtered due to high similarity to A term. ",
              "Using reduced similarity threshold.")

      # Sort by similarity and keep the lower half
      sorted_indices <- order(similarities)
      keep_count <- max(1, floor(length(sorted_indices) / 2))
      b_terms <- b_terms[sorted_indices[1:keep_count]]
    }
  }

  # Additional filter to remove single-character and very short B terms
  b_terms <- b_terms[nchar(b_terms) >= 3]

  # If no B terms found, return empty result
  if (length(b_terms) == 0) {
    message("No suitable B terms found with association score > ", min_score, " after filtering")
    return(data.frame(
      a_term = character(),
      b_term = character(),
      c_term = character(),
      a_b_score = numeric(),
      b_c_score = numeric(),
      abc_score = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Initialize results
  results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    a_b_score = numeric(),
    b_c_score = numeric(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  # If a specific C term is provided
  if (!is.null(c_term)) {
    # Check C term type constraint if specified
    if (!is.null(c_term_types) && has_entity_types) {
      c_type <- NULL
      term_lower <- tolower(c_term)

      # Check if c_term is in our mapping
      if (term_lower %in% names(term_type_mappings)) {
        c_type <- term_type_mappings[[term_lower]]
      }
      # Otherwise check in entity_types_dict if available
      else if (c_term %in% names(entity_types_dict)) {
        c_type <- entity_types_dict[c_term]
      }

      # Validate C term against its claimed type using the selected validator
      if (!is.null(c_type)) {
        if (enforce_strict_typing && !validator(c_term, c_type)) {
          message("C term '", c_term, "' does not appear to be a valid ", c_type)
        }

        if (!(c_type %in% c_term_types)) {
          message("Specified C term '", c_term, "' does not match required entity types: ",
                  paste(c_term_types, collapse = ", "))
          return(results)
        }
      }
    }

    # Get B-C associations for the specific C term
    for (b_term in b_terms) {
      b_c_score <- co_matrix[b_term, c_term]

      # Only include if B-C association exists and meets threshold
      if (b_c_score > min_score) {
        a_b_score <- a_associations[b_term]

        # Calculate ABC score based on scoring method
        abc_score <- calculate_score(a_b_score, b_c_score, scoring_method)

        results <- rbind(results, data.frame(
          a_term = a_term,
          b_term = b_term,
          c_term = c_term,
          a_b_score = a_b_score,
          b_c_score = b_c_score,
          abc_score = abc_score,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    # Explore all potential C terms
    # Get all terms except A term and filtered B terms
    all_terms <- rownames(co_matrix)
    potential_c_terms <- setdiff(all_terms, c(a_term, b_terms))

    # Remove blacklisted terms from potential C terms
    potential_c_terms <- potential_c_terms[!tolower(potential_c_terms) %in% blacklisted_terms]

    # Apply filtering to potential C terms if requested
    if (exclude_general_terms) {
      original_c_count <- length(potential_c_terms)

      # Apply the same improved filtering for C terms using the selected validator
      valid_c_terms <- character(0)

      for (c_term_candidate in potential_c_terms) {
        c_type <- NULL
        term_lower <- tolower(c_term_candidate)

        # Check if term is in our mapping
        if (term_lower %in% names(term_type_mappings)) {
          c_type <- term_type_mappings[[term_lower]]
        }
        # Otherwise check in entity_types_dict if available
        else if (has_entity_types && c_term_candidate %in% names(entity_types_dict)) {
          c_type <- entity_types_dict[c_term_candidate]
        }

        # Apply validation using the selected validator
        if (validator(c_term_candidate, c_type)) {
          valid_c_terms <- c(valid_c_terms, c_term_candidate)
        }
      }

      # Update potential_c_terms with filtered list
      potential_c_terms <- valid_c_terms

      filtered_count <- original_c_count - length(potential_c_terms)
      if (filtered_count > 0) {
        message("Filtered ", filtered_count, " potential C terms that weren't valid biomedical entities")
      }
    }

    # Apply C-term type constraints if specified
    if (!is.null(c_term_types) && has_entity_types) {
      original_c_count <- length(potential_c_terms)
      valid_c_terms <- character(0)

      for (c_term_candidate in potential_c_terms) {
        c_type <- NULL
        term_lower <- tolower(c_term_candidate)

        # Check if term is in our mapping
        if (term_lower %in% names(term_type_mappings)) {
          c_type <- term_type_mappings[[term_lower]]
        }
        # Otherwise check in entity_types_dict if available
        else if (c_term_candidate %in% names(entity_types_dict)) {
          c_type <- entity_types_dict[c_term_candidate]
        }

        # Additional validation for C terms using the selected validator
        if (!is.null(c_type)) {
          if (enforce_strict_typing) {
            # Skip if the term with claimed type is not valid
            if (!validator(c_term_candidate, c_type)) {
              next
            }
          }

          if (c_type %in% c_term_types) {
            valid_c_terms <- c(valid_c_terms, c_term_candidate)
          }
        }
      }

      potential_c_terms <- valid_c_terms

      if (length(potential_c_terms) == 0) {
        message("No potential C terms found matching the specified entity types: ",
                paste(c_term_types, collapse = ", "))
        return(results)
      }
    }

    # For each B term, find potential C terms
    message("Identifying potential C terms via ", length(b_terms), " B terms...")
    pb <- utils::txtProgressBar(min = 0, max = length(b_terms), style = 3)

    for (i in seq_along(b_terms)) {
      utils::setTxtProgressBar(pb, i)

      b_term <- b_terms[i]

      # Get all B-C associations
      b_associations <- co_matrix[b_term, ]

      # Filter for potential C terms with sufficient association
      potential_c_for_b <- names(b_associations[b_associations > min_score])

      # Filter by potential C terms
      potential_c_for_b <- intersect(potential_c_for_b, potential_c_terms)

      # Also filter out C terms that are too similar to A term if requested
      if (filter_similar_terms && length(potential_c_for_b) > 0) {
        # Calculate similarity for each potential C term
        c_similarities <- sapply(potential_c_for_b, function(c) string_similarity(a_term, c))

        # Filter out terms that are too similar to A term
        potential_c_for_b <- potential_c_for_b[c_similarities < similarity_threshold]
      }

      # Additional filter to remove single-character and very short C terms
      potential_c_for_b <- potential_c_for_b[nchar(potential_c_for_b) >= 3]

      # For each potential C term
      for (c_term_candidate in potential_c_for_b) {
        b_c_score <- b_associations[c_term_candidate]
        a_b_score <- a_associations[b_term]

        # Calculate ABC score based on scoring method
        abc_score <- calculate_score(a_b_score, b_c_score, scoring_method)

        results <- rbind(results, data.frame(
          a_term = a_term,
          b_term = b_term,
          c_term = c_term_candidate,
          a_b_score = a_b_score,
          b_c_score = b_c_score,
          abc_score = abc_score,
          stringsAsFactors = FALSE
        ))
      }
    }

    close(pb)
  }

  # If no results found, return empty data frame
  if (nrow(results) == 0) {
    message("No ABC connections found")
    return(results)
  }

  # Sort by ABC score
  results <- results[order(-results$abc_score), ]

  # Limit to n_results
  if (nrow(results) > n_results) {
    results <- results[1:n_results, ]
  }

  # Add entity type information if available
  if (has_entity_types) {
    # Process special type mappings
    for (term in names(term_type_mappings)) {
      if (term %in% names(entity_types_dict)) {
        entity_types_dict[term] <- term_type_mappings[[term]]
      }
    }

    results$a_type <- sapply(results$a_term, function(term) {
      term_lower <- tolower(term)
      # Check if term is in our mapping
      if (term_lower %in% names(term_type_mappings)) {
        return(term_type_mappings[[term_lower]])
      }
      # Otherwise check in entity_types_dict if available
      else if (term %in% names(entity_types_dict)) {
        return(entity_types_dict[term])
      } else {
        return(NA)
      }
    })

    results$b_type <- sapply(results$b_term, function(term) {
      term_lower <- tolower(term)
      # Check if term is in our mapping
      if (term_lower %in% names(term_type_mappings)) {
        return(term_type_mappings[[term_lower]])
      }
      # Otherwise check in entity_types_dict if available
      else if (term %in% names(entity_types_dict)) {
        return(entity_types_dict[term])
      } else {
        return(NA)
      }
    })

    results$c_type <- sapply(results$c_term, function(term) {
      term_lower <- tolower(term)
      # Check if term is in our mapping
      if (term_lower %in% names(term_type_mappings)) {
        return(term_type_mappings[[term_lower]])
      }
      # Otherwise check in entity_types_dict if available
      else if (term %in% names(entity_types_dict)) {
        return(entity_types_dict[term])
      } else {
        return(NA)
      }
    })

    # Apply final type validation where needed
    if (enforce_strict_typing) {
      # Find rows with suspicious entity type assignments
      suspicious_rows <- which(
        (!is.na(results$b_type) & !sapply(1:nrow(results), function(i)
          validator(results$b_term[i], results$b_type[i]))) |
          (!is.na(results$c_type) & !sapply(1:nrow(results), function(i)
            validator(results$c_term[i], results$c_type[i])))
      )

      if (length(suspicious_rows) > 0) {
        message("Found ", length(suspicious_rows), " results with suspicious entity type assignments")
        # Remove these rows if we have enough results left
        if (nrow(results) - length(suspicious_rows) >= min(10, n_results/2)) {
          results <- results[-suspicious_rows, ]
          message("Removed suspicious rows, ", nrow(results), " results remaining")
        } else {
          message("Too many suspicious rows to remove, keeping them for now")
        }
      }
    }
  }

  return(results)
}

#' Calculate ABC score based on specified method
#'
#' @param a_b_score A-B association score
#' @param b_c_score B-C association score
#' @param method Scoring method: "multiplication", "average", "combined", or "jaccard"
#' @return Calculated score
#' @keywords internal
calculate_score <- function(a_b_score, b_c_score, method) {
  switch(method,
         "multiplication" = a_b_score * b_c_score,
         "average" = (a_b_score + b_c_score) / 2,
         "combined" = 0.7 * (a_b_score * b_c_score) + 0.3 * ((a_b_score + b_c_score) / 2),
         "jaccard" = a_b_score * b_c_score, # Actual Jaccard calculation would require the original co-occurrence data
         a_b_score * b_c_score) # Default to multiplication
}

#' Enforce diversity by selecting top connections from each B term
#'
#' @param results Data frame with ABC model results
#' @param max_per_group Maximum number of results to keep per B term
#' @return Data frame with diverse results
#' @keywords internal
diversify_b_terms <- function(results, max_per_group = 3) {
  # Identify unique B terms
  unique_b_terms <- unique(results$b_term)

  # Initialize diverse results
  diverse_results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    a_b_score = numeric(),
    b_c_score = numeric(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  # For each unique B term, get the top C term
  for (b_term in unique_b_terms) {
    b_results <- results[results$b_term == b_term, ]

    # Skip if no results for this B term
    if (nrow(b_results) == 0) next

    # Get unique C terms for this B term
    b_c_terms <- unique(b_results$c_term)

    # For each C term, take the best ABC score
    for (c_term in b_c_terms) {
      c_results <- b_results[b_results$c_term == c_term, ]

      # Get the row with maximum ABC score for this B-C pair
      best_idx <- which.max(c_results$abc_score)

      # Add to diverse results
      diverse_results <- rbind(diverse_results, c_results[best_idx, ])
    }
  }

  # Sort by ABC score
  diverse_results <- diverse_results[order(-diverse_results$abc_score), ]

  # Limit results per B term to max_per_group
  b_term_counts <- table(diverse_results$b_term)

  # For B terms with more than max_per_group results, keep only the top max_per_group
  for (b_term in names(b_term_counts)) {
    if (b_term_counts[b_term] > max_per_group) {
      # Identify rows for this B term
      b_rows <- which(diverse_results$b_term == b_term)

      # Keep only the top max_per_group based on ABC score (they're already sorted)
      rows_to_remove <- b_rows[(max_per_group + 1):length(b_rows)]

      # Remove excess rows
      if (length(rows_to_remove) > 0) {
        diverse_results <- diverse_results[-rows_to_remove, ]
      }
    }
  }

  return(diverse_results)
}

#' Add statistical significance testing based on hypergeometric tests
#'
#' @param results Data frame with ABC model results
#' @param co_matrix Co-occurrence matrix
#' @param alpha Significance level
#' @return Data frame with p-values and significance indicators
#' @keywords internal
add_statistical_significance <- function(results, co_matrix, alpha = 0.05) {
  # Initialize p-values
  results$p_value <- numeric(nrow(results))
  results$significant <- logical(nrow(results))

  # Get total number of documents (approximation from co-occurrence matrix)
  # Use the diagonal elements which represent term frequency
  term_freq <- diag(co_matrix)
  total_docs <- max(term_freq) # This is an approximation

  # For each result, calculate hypergeometric p-value
  for (i in 1:nrow(results)) {
    a_term <- results$a_term[i]
    b_term <- results$b_term[i]
    c_term <- results$c_term[i]

    # Get frequencies
    a_freq <- term_freq[a_term]
    b_freq <- term_freq[b_term]
    c_freq <- term_freq[c_term]

    # Get co-occurrence counts
    a_b_count <- co_matrix[a_term, b_term] * a_freq
    b_c_count <- co_matrix[b_term, c_term] * b_freq

    # Calculate expected A-C co-occurrence under independence assumption
    expected_a_c <- (a_freq * c_freq) / total_docs

    # Calculate observed A-C co-occurrence
    a_c_count <- co_matrix[a_term, c_term] * a_freq

    # Calculate p-value using hypergeometric test
    # We use phyper for hypergeometric distribution:
    # phyper(q, m, n, k, lower.tail = FALSE)
    # q = observed count - 1 (since we want P(X > a_c_count))
    # m = a_freq (number of "successes" in the population)
    # n = total_docs - a_freq (number of "failures" in the population)
    # k = c_freq (number of draws)

    # Since we're looking for higher than expected co-occurrence:
    p_value <- stats::phyper(a_c_count - 1, a_freq, total_docs - a_freq, c_freq, lower.tail = FALSE)

    # Store p-value and significance
    results$p_value[i] <- p_value
    results$significant[i] <- p_value < alpha
  }

  # Apply Benjamini-Hochberg false discovery rate correction
  if (nrow(results) > 1) {
    # Sort p-values
    sorted_indices <- order(results$p_value)
    sorted_p_values <- results$p_value[sorted_indices]

    # Calculate BH-adjusted p-values
    n <- length(sorted_p_values)
    adjusted_p_values <- sorted_p_values * n / seq_len(n)

    # Ensure monotonicity
    for (i in (n-1):1) {
      adjusted_p_values[i] <- min(adjusted_p_values[i], adjusted_p_values[i+1])
    }

    # Update results
    results$adjusted_p_value <- numeric(n)
    results$adjusted_p_value[sorted_indices] <- adjusted_p_values
    results$significant <- results$adjusted_p_value < alpha
  }

  return(results)
}

#' Find all potential ABC connections
#'
#' This function finds all potential ABC connections in a co-occurrence matrix.
#'
#' @param co_matrix A co-occurrence matrix produced by create_comat().
#' @param a_type Character string, the entity type for A terms.
#' @param c_type Character string, the entity type for C terms.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#'
#' @return A data frame with ranked discovery results.
#' @export
#'
#' @examples
#' \dontrun{
#' all_abc <- find_abc_all(co_matrix, a_type = "source", c_type = "target")
#' }
find_abc_all <- function(co_matrix, a_type = NULL, c_type = NULL,
                         min_score = 0.1, n_results = 1000) {
  # Check if the matrix has entity types
  has_entity_types <- !is.null(attr(co_matrix, "entity_types"))

  # If entity types are requested but not available, throw an error
  if (((!is.null(a_type) || !is.null(c_type)) && !has_entity_types)) {
    stop("Entity types requested but not available in the co-occurrence matrix")
  }

  # Get all terms
  all_terms <- rownames(co_matrix)

  # Initialize results
  all_results <- data.frame(
    a_term = character(),
    b_term = character(),
    c_term = character(),
    a_b_score = numeric(),
    b_c_score = numeric(),
    abc_score = numeric(),
    stringsAsFactors = FALSE
  )

  # Filter potential A terms by type
  potential_a_terms <- all_terms
  if (!is.null(a_type) && has_entity_types) {
    entity_types <- attr(co_matrix, "entity_types")
    a_type_terms <- names(entity_types[entity_types == a_type])
    potential_a_terms <- intersect(potential_a_terms, a_type_terms)
    if (length(potential_a_terms) == 0) {
      message("No terms found with type: ", a_type)
      return(all_results)
    }
  }

  # For each potential A term
  for (a_term in potential_a_terms) {
    message("Processing A term: ", a_term, " (", which(potential_a_terms == a_term),
            " of ", length(potential_a_terms), ")")

    # Apply ABC model with provided min_score
    abc_results <- suppressMessages(
      abc_model(co_matrix,
                a_term = a_term,
                c_term = NULL,
                min_score = min_score,
                n_results = n_results)
    )

    # Combine results
    if (nrow(abc_results) > 0) {
      all_results <- rbind(all_results, abc_results)
    }
  }

  # If no results found, return empty data frame
  if (nrow(all_results) == 0) {
    message("No ABC connections found")
    return(all_results)
  }

  # Sort by ABC score and limit to n_results
  all_results <- all_results[order(-all_results$abc_score), ]
  if (nrow(all_results) > n_results) {
    all_results <- all_results[1:n_results, ]
  }

  return(all_results)
}

#' Apply the ABC model with statistical significance testing
#'
#' This function extends the ABC model with statistical significance testing
#' to evaluate the strength of discovered connections.
#'
#' @param co_matrix A co-occurrence matrix produced by create_cooccurrence_matrix().
#' @param a_term Character string, the source term (A).
#' @param c_term Character string, the target term (C). If NULL, all potential C terms will be evaluated.
#' @param a_type Character string, the entity type for A terms. If NULL, all types are considered.
#' @param c_type Character string, the entity type for C terms. If NULL, all types are considered.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#' @param n_permutations Number of permutations for significance testing.
#' @param scoring_method Method to use for scoring ABC connections.
#'
#' @return A data frame with ranked discovery results and p-values.
#' @export
#'
#' @examples
#' \dontrun{
#' abc_results <- abc_model_sig(co_matrix, a_term = "migraine",
#'                                                 scoring_method = "combined")
#' }
abc_model_sig <- function(co_matrix, a_term, c_term = NULL,
                          a_type = NULL, c_type = NULL,
                          min_score = 0.1, n_results = 100,
                          n_permutations = 1000,
                          scoring_method = c("multiplication", "average", "combined", "jaccard")) {

  # Match scoring_method argument
  scoring_method <- match.arg(scoring_method)

  # First get standard ABC results
  results <- abc_model(co_matrix, a_term, c_term, a_type, c_type, min_score, n_results)

  # If no results, return empty data frame
  if (nrow(results) == 0) {
    return(results)
  }

  # Calculate alternative scores based on the chosen method
  if (scoring_method == "multiplication") {
    # This is already calculated as abc_score in the original function
    results$primary_score <- results$abc_score
  } else if (scoring_method == "average") {
    results$abc_score_avg <- (results$a_b_score + results$b_c_score) / 2
    results$primary_score <- results$abc_score_avg
  } else if (scoring_method == "combined") {
    # Weighted combination of multiplication and average
    results$abc_score_combined <- 0.7 * results$abc_score +
      0.3 * ((results$a_b_score + results$b_c_score) / 2)
    results$primary_score <- results$abc_score_combined
  } else if (scoring_method == "jaccard") {
    # For Jaccard, we need to recalculate from the original co-occurrence matrix
    # This assumes the co-occurrence matrix contains raw co-occurrence counts

    # Get a-term row and sum
    a_docs <- co_matrix[a_term, ]

    # Initialize jaccard scores
    results$abc_score_jaccard <- numeric(nrow(results))

    for (i in 1:nrow(results)) {
      b_term <- results$b_term[i]
      c_term_i <- results$c_term[i]

      # Get rows for b and c terms
      b_docs <- co_matrix[b_term, ]
      c_docs <- co_matrix[c_term_i, ]

      # Calculate Jaccard coefficients
      a_b_jaccard <- sum(a_docs > 0 & b_docs > 0) / sum(a_docs > 0 | b_docs > 0)
      b_c_jaccard <- sum(b_docs > 0 & c_docs > 0) / sum(b_docs > 0 | c_docs > 0)

      # Update scores
      results$abc_score_jaccard[i] <- a_b_jaccard * b_c_jaccard
    }

    results$primary_score <- results$abc_score_jaccard
  }

  # Perform significance testing through permutation
  # We'll calculate p-values for each B term

  # Store original scores
  original_scores <- results$primary_score

  # Initialize p-values
  results$p_value <- numeric(nrow(results))

  # Permutation test
  message("Performing permutation test for statistical significance...")
  permutation_scores <- matrix(0, nrow = nrow(results), ncol = n_permutations)

  for (perm in 1:n_permutations) {
    if (perm %% 100 == 0) {
      message("  Permutation ", perm, " of ", n_permutations)
    }

    # Create a permuted co-occurrence matrix
    # Just shuffle the elements within each row
    perm_matrix <- co_matrix
    for (i in 1:nrow(perm_matrix)) {
      perm_matrix[i, ] <- sample(perm_matrix[i, ])
    }

    # Calculate scores using permuted matrix
    for (i in 1:nrow(results)) {
      b_term <- results$b_term[i]
      c_term_i <- results$c_term[i]

      # Extract scores from permuted matrix
      perm_a_b_score <- perm_matrix[a_term, b_term]
      perm_b_c_score <- perm_matrix[b_term, c_term_i]

      # Calculate score based on the chosen method
      if (scoring_method == "multiplication") {
        perm_score <- perm_a_b_score * perm_b_c_score
      } else if (scoring_method == "average") {
        perm_score <- (perm_a_b_score + perm_b_c_score) / 2
      } else if (scoring_method == "combined") {
        perm_score <- 0.7 * (perm_a_b_score * perm_b_c_score) +
          0.3 * ((perm_a_b_score + perm_b_c_score) / 2)
      } else if (scoring_method == "jaccard") {
        # For Jaccard, would need more complex permutation approach
        # Simplifying here to just use the product
        perm_score <- perm_a_b_score * perm_b_c_score
      }

      permutation_scores[i, perm] <- perm_score
    }
  }

  # Calculate p-values
  for (i in 1:nrow(results)) {
    original <- original_scores[i]
    perms <- permutation_scores[i, ]

    # p-value is proportion of permutation scores >= original score
    results$p_value[i] <- sum(perms >= original) / n_permutations
  }

  # Add significance indicator
  results$significant <- results$p_value < 0.05

  # Add false discovery rate correction
  if (nrow(results) > 1) {
    # Benjamini-Hochberg procedure
    p_sorted <- sort(results$p_value)
    rank <- 1:length(p_sorted)
    q_value <- p_sorted * length(p_sorted) / rank

    # Ensure q-values are monotonically decreasing
    for (i in (length(q_value) - 1):1) {
      q_value[i] <- min(q_value[i], q_value[i + 1])
    }

    # Map q-values back to original order
    results$q_value <- q_value[match(results$p_value, p_sorted)]

    # Add FDR-corrected significance indicator
    results$significant_fdr <- results$q_value < 0.05
  } else {
    results$q_value <- results$p_value
    results$significant_fdr <- results$significant
  }

  # Sort by primary score and limit to n_results
  results <- results[order(-results$primary_score), ]
  if (nrow(results) > n_results) {
    results <- results[1:n_results, ]
  }

  return(results)
}

#' Apply time-sliced ABC model for validation
#'
#' This function implements a time-sliced ABC model for validation.
#' It uses historical data to predict connections that will appear in the future.
#'
#' @param entity_data A data frame of entity data with time information.
#' @param time_column Name of the column containing time information.
#' @param split_time Time point to split historical and future data.
#' @param a_term Character string, the source term (A).
#' @param a_type Character string, the entity type for A terms.
#' @param c_type Character string, the entity type for C terms.
#' @param min_score Minimum score threshold for results.
#' @param n_results Maximum number of results to return.
#'
#' @return A list with prediction results and validation metrics.
#' @export
#'
#' @examples
#' \dontrun{
#' validation <- abc_timeslice(entity_data,
#'                             time_column = "publication_year",
#'                             split_time = 2010,
#'                             a_term = "migraine")
#' }
abc_timeslice <- function(entity_data, time_column = "publication_year",
                          split_time, a_term, a_type = NULL, c_type = NULL,
                          min_score = 0.1, n_results = 100) {

  # Check if time column exists
  if (!time_column %in% colnames(entity_data)) {
    stop("Time column '", time_column, "' not found in the data")
  }

  # Split data into historical and future sets
  historical_data <- entity_data[entity_data[[time_column]] < split_time, ]
  future_data <- entity_data[entity_data[[time_column]] >= split_time, ]

  message("Split data: ", nrow(historical_data), " historical records, ",
          nrow(future_data), " future records")

  # Create co-occurrence matrix for historical data
  historical_matrix <- create_comat(historical_data,
                                    normalize = TRUE)

  # Run ABC model on historical data
  historical_results <- abc_model(historical_matrix,
                                  a_term = a_term,
                                  min_score = min_score,
                                  n_results = n_results)

  # Extract predicted A-C connections
  predicted_connections <- unique(data.frame(
    a_term = historical_results$a_term,
    c_term = historical_results$c_term,
    stringsAsFactors = FALSE
  ))

  # Create co-occurrence matrix for future data
  future_matrix <- create_comat(future_data,
                                normalize = TRUE)

  # Check which predicted connections appear in future data
  validated_connections <- data.frame(
    a_term = character(),
    c_term = character(),
    predicted_score = numeric(),
    future_score = numeric(),
    validated = logical(),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(predicted_connections)) {
    a <- predicted_connections$a_term[i]
    c <- predicted_connections$c_term[i]

    # Get prediction score from historical results
    idx <- which(historical_results$a_term == a & historical_results$c_term == c)
    if (length(idx) > 0) {
      predicted_score <- max(historical_results$abc_score[idx])
    } else {
      predicted_score <- NA
    }

    # Check if a and c appear together in future data
    future_score <- NA
    validated <- FALSE

    if (a %in% rownames(future_matrix) && c %in% rownames(future_matrix)) {
      # Check for direct connection in future
      direct_score <- future_matrix[a, c]

      if (direct_score > 0) {
        future_score <- direct_score
        validated <- TRUE
      } else {
        # Check for indirect connections through any B term
        for (b in rownames(future_matrix)) {
          if (b != a && b != c) {
            a_b_score <- future_matrix[a, b]
            b_c_score <- future_matrix[b, c]

            if (a_b_score > 0 && b_c_score > 0) {
              if (is.na(future_score) || a_b_score * b_c_score > future_score) {
                future_score <- a_b_score * b_c_score
                validated <- TRUE
              }
            }
          }
        }
      }
    }

    validated_connections <- rbind(validated_connections, data.frame(
      a_term = a,
      c_term = c,
      predicted_score = predicted_score,
      future_score = future_score,
      validated = validated,
      stringsAsFactors = FALSE
    ))
  }

  # Calculate validation metrics
  total_predictions <- nrow(validated_connections)
  total_validated <- sum(validated_connections$validated)
  validation_rate <- total_validated / total_predictions

  # Return results
  return(list(
    predictions = historical_results,
    validations = validated_connections,
    validation_metrics = list(
      total_predictions = total_predictions,
      total_validated = total_validated,
      validation_rate = validation_rate
    )
  ))
}

#' Apply statistical validation to ABC model results with support for large matrices
#'
#' This function performs statistical tests to validate ABC model results.
#' It calculates p-values using hypergeometric tests and applies correction for multiple testing.
#' The function is optimized to work with very large co-occurrence matrices.
#'
#' @param abc_results A data frame containing ABC results.
#' @param co_matrix The co-occurrence matrix used to generate the ABC results.
#' @param alpha Significance level (p-value threshold).
#' @param correction Method for multiple testing correction.
#' @param filter_by_significance Logical. If TRUE, only returns significant results.
#'
#' @return A data frame with ABC results and statistical significance measures.
#' @export
#'
#' @examples
#' \dontrun{
#' validated_results <- validate_abc(abc_results, co_matrix)
#' }
validate_abc <- function(abc_results, co_matrix,
                         alpha = 0.05,
                         correction = c("BH", "bonferroni", "none"),
                         filter_by_significance = FALSE) {

  # Match correction argument
  correction <- match.arg(correction)

  # Check if results are empty
  if (nrow(abc_results) == 0) {
    message("ABC results are empty")
    return(abc_results)
  }

  # Add p-value and significance columns
  results <- abc_results
  results$p_value <- numeric(nrow(results))
  results$significant <- logical(nrow(results))

  # Instead of relying on diag(), we'll estimate frequencies directly
  message("Using optimized approach for large matrix validation...")

  # Try to get document count from metadata if available
  if (!is.null(attr(co_matrix, "metadata")) && !is.null(attr(co_matrix, "metadata")$n_docs)) {
    total_docs <- attr(co_matrix, "metadata")$n_docs
    message("Using metadata for document count: ", total_docs)
  } else {
    # If not in metadata, use a reasonable estimate based on matrix properties
    # We'll use the maximum self-co-occurrence (diagonal element) as an approximation

    # Get unique terms from the ABC results to limit our search
    unique_terms <- unique(c(results$a_term, results$b_term, results$c_term))

    # Find the maximum frequency by checking diagonal elements for these terms
    max_freq <- 0
    for (term in unique_terms) {
      if (term %in% rownames(co_matrix)) {
        term_freq <- co_matrix[term, term]
        max_freq <- max(max_freq, term_freq)
      }
    }

    # If we found a reasonable max frequency, use it
    if (max_freq > 0) {
      total_docs <- ceiling(max_freq)
    } else {
      # As a fallback, use a reasonable default or estimate from matrix size
      total_docs <- max(100, ceiling(sqrt(nrow(co_matrix))))
    }

    message("Estimated document count: ", total_docs)
  }

  # Calculate p-values using hypergeometric test
  message("Calculating statistical significance using hypergeometric test...")

  for (i in 1:nrow(results)) {
    a_term <- results$a_term[i]
    b_term <- results$b_term[i]
    c_term <- results$c_term[i]

    # Skip calculation if any term is missing from the matrix
    if (!all(c(a_term, b_term, c_term) %in% rownames(co_matrix))) {
      results$p_value[i] <- 1.0  # Set to 1.0 to indicate no significance
      next
    }

    # Get term frequencies from diagonal elements (one at a time)
    a_freq <- co_matrix[a_term, a_term]
    b_freq <- co_matrix[b_term, b_term]
    c_freq <- co_matrix[c_term, c_term]

    # Get co-occurrence values
    a_b_score <- co_matrix[a_term, b_term]
    b_c_score <- co_matrix[b_term, c_term]
    a_c_score <- co_matrix[a_term, c_term]

    # Handle normalized scores (if values are between 0 and 1)
    if (max(a_b_score, b_c_score, a_c_score) <= 1) {
      # Convert normalized scores to approximate counts
      # Use minimum to avoid over-counting
      a_b_count <- round(a_b_score * min(a_freq, b_freq))
      b_c_count <- round(b_c_score * min(b_freq, c_freq))
      a_c_count <- round(a_c_score * min(a_freq, c_freq))
    } else {
      # Assume these are already counts
      a_b_count <- a_b_score
      b_c_count <- b_c_score
      a_c_count <- a_c_score
    }

    # Calculate expected A-C co-occurrence under independence
    expected_a_c <- (a_freq * c_freq) / total_docs

    # Skip calculation if we don't have valid frequencies
    if (a_freq <= 0 || c_freq <= 0 || total_docs <= 0) {
      results$p_value[i] <- 1.0
      next
    }

    # Calculate p-value using hypergeometric test
    # Test for over-representation (common in LBD)
    p_value <- tryCatch({
      stats::phyper(a_c_count - 1, a_freq, total_docs - a_freq, c_freq,
                    lower.tail = FALSE)
    }, error = function(e) {
      # If the hypergeometric test fails, use a simple approximation
      # based on the ratio of observed to expected
      if (expected_a_c > 0) {
        ratio <- a_c_count / expected_a_c
        # Convert ratio to a p-value (higher ratio = lower p-value)
        1 / (1 + ratio^2)
      } else {
        1.0  # Default to no significance
      }
    })

    # Store p-value
    results$p_value[i] <- p_value
  }

  # Apply multiple testing correction
  if (correction == "BH") {
    # Benjamini-Hochberg procedure (controls false discovery rate)
    adjusted_p <- stats::p.adjust(results$p_value, method = "BH")
    results$adjusted_p_value <- adjusted_p
    results$significant <- adjusted_p < alpha
  } else if (correction == "bonferroni") {
    # Bonferroni correction (controls family-wise error rate)
    adjusted_p <- stats::p.adjust(results$p_value, method = "bonferroni")
    results$adjusted_p_value <- adjusted_p
    results$significant <- adjusted_p < alpha
  } else {
    # No correction (just use raw p-values)
    results$adjusted_p_value <- results$p_value
    results$significant <- results$p_value < alpha
  }

  # Calculate percentage of significant results
  percent_significant <- sum(results$significant) / nrow(results) * 100
  message(sprintf("%.1f%% of connections are statistically significant (p < %.2f%s)",
                  percent_significant, alpha,
                  ifelse(correction == "none", "", paste0(", ", correction, " correction"))))

  # Filter by significance if requested
  if (filter_by_significance) {
    results <- results[results$significant, ]
    if (nrow(results) == 0) {
      warning("No statistically significant results found. Returning unfiltered results.")
      return(abc_results)
    }
  }

  # Sort by adjusted p-value (or raw p-value if no correction)
  results <- results[order(results$adjusted_p_value, -results$abc_score), ]

  return(results)
}

#' Standard validation method using hypergeometric tests
#' @keywords internal
standard_validation <- function(abc_results, co_matrix, alpha, correction) {
  # Add p-value and significance columns
  results <- abc_results
  results$p_value <- numeric(nrow(results))
  results$significant <- logical(nrow(results))

  # Get total number of documents from co-occurrence matrix
  # This is an approximation based on term frequency
  term_freq <- diag(co_matrix)
  total_docs <- max(term_freq) # This is an approximation

  # Calculate p-values using hypergeometric test
  message("Calculating statistical significance using hypergeometric test...")

  for (i in 1:nrow(results)) {
    a_term <- results$a_term[i]
    b_term <- results$b_term[i]
    c_term <- results$c_term[i]

    # Get frequencies
    a_freq <- term_freq[a_term]
    b_freq <- term_freq[b_term]
    c_freq <- term_freq[c_term]

    # Get co-occurrence counts (actual co-occurrence * frequency)
    a_b_count <- co_matrix[a_term, b_term] * min(a_freq, b_freq)
    b_c_count <- co_matrix[b_term, c_term] * min(b_freq, c_freq)

    # Calculate observed A-C co-occurrence
    a_c_count <- co_matrix[a_term, c_term] * min(a_freq, c_freq)

    # Calculate expected A-C co-occurrence under independence
    expected_a_c <- (a_freq * c_freq) / total_docs

    # Calculate p-value using hypergeometric test
    # Evaluate the probability of observing a_c_count or more co-occurrences by chance
    p_value <- stats::phyper(a_c_count - 1, a_freq, total_docs - a_freq, c_freq,
                             lower.tail = FALSE)

    # Store p-value
    results$p_value[i] <- p_value
  }

  # Apply multiple testing correction
  return(apply_correction(results, correction, alpha))
}

#' Alternative validation for large matrices
#' @keywords internal
alternative_validation <- function(abc_results, co_matrix, alpha, correction) {
  # Add columns to results
  results <- abc_results
  results$p_value <- numeric(nrow(results))
  results$significant <- logical(nrow(results))

  # For large matrices, use a simplified approach that avoids diag() operations
  message("Using alternative statistical validation for large matrix...")

  # Instead of extracting all diagonal elements at once, process terms individually
  for (i in 1:nrow(results)) {
    a_term <- results$a_term[i]
    b_term <- results$b_term[i]
    c_term <- results$c_term[i]

    # Get individual frequencies directly (avoids extracting full diagonal)
    # Note: In a co-occurrence matrix, diagonal element [i,i] represents term i's frequency
    tryCatch({
      a_freq <- co_matrix[a_term, a_term]
      b_freq <- co_matrix[b_term, b_term]
      c_freq <- co_matrix[c_term, c_term]

      # Calculate total docs (approximate)
      total_docs <- max(a_freq, b_freq, c_freq) * 10  # Multiplier as a heuristic

      # Get connection scores
      a_b_score <- co_matrix[a_term, b_term]
      b_c_score <- co_matrix[b_term, c_term]
      a_c_score <- co_matrix[a_term, c_term]

      # Calculate approximate co-occurrence counts
      a_b_count <- a_b_score * min(a_freq, b_freq)
      b_c_count <- b_c_score * min(b_freq, c_freq)
      a_c_count <- a_c_score * min(a_freq, c_freq)

      # Calculate expected co-occurrence under independence
      expected_a_c <- (a_freq * c_freq) / total_docs

      # Calculate p-value using hypergeometric test
      p_value <- stats::phyper(max(1, a_c_count) - 1, a_freq, total_docs - a_freq, c_freq,
                               lower.tail = FALSE)

      # Prevent very low p-values from becoming 0
      p_value <- max(p_value, .Machine$double.eps)

      # Store p-value
      results$p_value[i] <- p_value
    }, error = function(e) {
      # If even individual access fails, use abc score as proxy
      message("Error accessing matrix elements for terms. Using score-based approximation.")
      results$p_value[i] <<- 1 - results$abc_score[i] / max(results$abc_score)
    })
  }

  # Apply multiple testing correction
  return(apply_correction(results, correction, alpha))
}

#' Apply correction to p-values
#' @keywords internal
apply_correction <- function(results, correction, alpha) {
  # Apply multiple testing correction
  if (correction == "BH") {
    # Benjamini-Hochberg procedure (controls false discovery rate)
    adjusted_p <- stats::p.adjust(results$p_value, method = "BH")
    results$adjusted_p_value <- adjusted_p
    results$significant <- adjusted_p < alpha
  } else if (correction == "bonferroni") {
    # Bonferroni correction (controls family-wise error rate)
    adjusted_p <- stats::p.adjust(results$p_value, method = "bonferroni")
    results$adjusted_p_value <- adjusted_p
    results$significant <- adjusted_p < alpha
  } else {
    # No correction (just use raw p-values)
    results$adjusted_p_value <- results$p_value
    results$significant <- results$p_value < alpha
  }

  # Calculate percentage of significant results
  percent_significant <- sum(results$significant) / nrow(results) * 100
  message(sprintf("%.1f%% of connections are statistically significant (p < %.2f%s)",
                  percent_significant, alpha,
                  ifelse(correction == "none", "", paste0(", ", correction, " correction"))))

  # Sort by adjusted p-value (or raw p-value if no correction), then by abc_score
  results <- results[order(results$adjusted_p_value, -results$abc_score), ]

  return(results)
}

#' Perform randomization test for ABC model
#'
#' This function assesses the significance of ABC model results through randomization.
#' It generates a null distribution by permuting the co-occurrence matrix.
#'
#' @param abc_results A data frame containing ABC results.
#' @param co_matrix The co-occurrence matrix used to generate the ABC results.
#' @param n_permutations Number of permutations to perform.
#' @param alpha Significance level.
#'
#' @return A data frame with ABC results and permutation-based significance measures.
#' @export
#'
#' @examples
#' \dontrun{
#' randomized_results <- perm_test_abc(abc_results, co_matrix, n_permutations = 1000)
#' }
perm_test_abc <- function(abc_results, co_matrix, n_permutations = 1000, alpha = 0.05) {
  # Check if results are empty
  if (nrow(abc_results) == 0) {
    message("ABC results are empty")
    return(abc_results)
  }

  # Initialize results with permutation p-values
  results <- abc_results
  results$perm_p_value <- numeric(nrow(results))
  results$perm_significant <- logical(nrow(results))

  # Store original ABC scores
  original_scores <- results$abc_score

  # Initialize matrix to store permutation scores
  perm_scores <- matrix(0, nrow = nrow(results), ncol = n_permutations)

  # Run permutations
  message("Performing randomization test with ", n_permutations, " permutations...")
  pb <- utils::txtProgressBar(min = 0, max = n_permutations, style = 3)

  for (p in 1:n_permutations) {
    # Create a permuted co-occurrence matrix by shuffling cells
    perm_matrix <- co_matrix

    # Shuffle within each row to preserve row sums
    for (i in 1:nrow(perm_matrix)) {
      # Get non-zero, non-diagonal elements in this row
      values <- perm_matrix[i, ]
      values[i] <- 0  # Exclude diagonal
      idx <- which(values > 0)

      if (length(idx) > 1) {
        # Shuffle the non-zero values
        values[idx] <- sample(values[idx])
        perm_matrix[i, ] <- values
      }
    }

    # Calculate permuted ABC scores
    for (i in 1:nrow(results)) {
      a_term <- results$a_term[i]
      b_term <- results$b_term[i]
      c_term <- results$c_term[i]

      # Calculate permuted scores
      a_b_score <- perm_matrix[a_term, b_term]
      b_c_score <- perm_matrix[b_term, c_term]
      abc_score <- a_b_score * b_c_score

      # Store permuted score
      perm_scores[i, p] <- abc_score
    }

    utils::setTxtProgressBar(pb, p)
  }

  close(pb)

  # Calculate permutation p-values
  # For each result, p-value is the proportion of permuted scores >= original score
  for (i in 1:nrow(results)) {
    # Calculate p-value from permutation distribution
    perm_p_value <- sum(perm_scores[i, ] >= original_scores[i]) / n_permutations

    # Store permutation p-value
    results$perm_p_value[i] <- perm_p_value
    results$perm_significant[i] <- perm_p_value < alpha
  }

  # Apply Benjamini-Hochberg correction to permutation p-values
  results$perm_adjusted_p <- stats::p.adjust(results$perm_p_value, method = "BH")
  results$perm_significant_adj <- results$perm_adjusted_p < alpha

  # Report percentage of significant results after permutation test
  percent_significant <- sum(results$perm_significant_adj) / nrow(results) * 100
  message(sprintf("%.1f%% of connections are significant after randomization test (p < %.2f, BH correction)",
                  percent_significant, alpha))

  # Sort by permutation p-value
  results <- results[order(results$perm_adjusted_p, -results$abc_score), ]

  return(results)
}

#' Enforce diversity in ABC model results
#'
#' This function applies diversity enforcement to ABC model results by:
#' 1. Removing duplicate paths to the same C term
#' 2. Ensuring B term diversity by selecting top results from each B term group
#' 3. Preventing A and C terms from appearing as B terms
#'
#' @param abc_results A data frame containing ABC results.
#' @param diversity_method Method for enforcing diversity: "b_term_groups", "unique_c_paths", or "both".
#' @param max_per_group Maximum number of results to keep per B term or C term.
#' @param min_score Minimum score threshold for including connections.
#'
#' @return A data frame with diverse ABC results.
#' @export
#'
#' @examples
#' \dontrun{
#' diverse_results <- diversify_abc(abc_results)
#' }
diversify_abc <- function(abc_results,
                          diversity_method = c("both", "b_term_groups", "unique_c_paths"),
                          max_per_group = 3,
                          min_score = 0.1) {

  # Match diversity_method argument
  diversity_method <- match.arg(diversity_method)

  # Check if results are empty
  if (nrow(abc_results) == 0) {
    message("ABC results are empty")
    return(abc_results)
  }

  # Filter results by minimum score
  results <- abc_results[abc_results$abc_score >= min_score, ]

  # If no results after filtering, return empty data frame
  if (nrow(results) == 0) {
    message("No results remain after filtering by minimum score")
    return(results)
  }

  # Sort by ABC score
  results <- results[order(-results$abc_score), ]

  # Initialize diverse results
  diverse_results <- results

  # Apply selected diversity methods
  if (diversity_method %in% c("both", "b_term_groups")) {
    diverse_results <- diversify_b_terms(diverse_results, max_per_group)
  }

  if (diversity_method %in% c("both", "unique_c_paths")) {
    diverse_results <- diversify_c_paths(diverse_results, max_per_group)
  }

  # Remove A and C terms that appear as B terms
  diverse_results <- remove_ac_terms(diverse_results)

  return(diverse_results)
}

#' Enforce diversity for C term paths
#'
#' @param results Data frame with ABC model results
#' @param max_per_c Maximum number of paths to keep per C term
#' @return Data frame with C term path diversity enforced
#' @keywords internal
diversify_c_paths <- function(results, max_per_c = 3) {
  # Group results by C term
  unique_c_terms <- unique(results$c_term)

  # Initialize diverse results
  diverse_results <- data.frame()

  # For each C term, select top paths (limited by max_per_c)
  for (c_term in unique_c_terms) {
    c_results <- results[results$c_term == c_term, ]

    # Select the best paths using unique B terms
    selected_paths <- data.frame()

    # Track which B terms we've already used for this C term
    used_b_terms <- character(0)

    # Add paths until we reach max_per_c or run out of options
    for (i in 1:nrow(c_results)) {
      # If we've reached max_per_c paths or used all B terms, break
      if (nrow(selected_paths) >= max_per_c || i > nrow(c_results)) {
        break
      }

      b_term <- c_results$b_term[i]

      # Skip if we've already used this B term for this C term
      if (b_term %in% used_b_terms) {
        next
      }

      # Add path and mark B term as used
      selected_paths <- rbind(selected_paths, c_results[i, ])
      used_b_terms <- c(used_b_terms, b_term)
    }

    # Add to diverse results
    diverse_results <- rbind(diverse_results, selected_paths)
  }

  # Re-sort by ABC score
  diverse_results <- diverse_results[order(-diverse_results$abc_score), ]

  return(diverse_results)
}

#' Remove A and C terms that appear as B terms
#'
#' @param results Data frame with ABC model results
#' @return Data frame with A and C terms removed from B terms
#' @keywords internal
remove_ac_terms <- function(results) {
  # Collect A and C terms
  a_terms <- unique(results$a_term)
  c_terms <- unique(results$c_term)
  ac_terms <- c(a_terms, c_terms)

  # Filter out results where B terms are also A or C terms
  filtered_results <- results[!results$b_term %in% ac_terms, ]

  # If we removed all results, return the original to prevent empty result
  if (nrow(filtered_results) == 0) {
    warning("All B terms were also A or C terms. Returning original results.")
    return(results)
  }

  return(filtered_results)
}

#' Get entity type distribution from co-occurrence matrix
#'
#' @param co_matrix A co-occurrence matrix produced by create_typed_comat().
#'
#' @return A data frame with entity type counts and percentages.
#' @export
#'
#' @examples
#' \dontrun{
#' type_dist <- get_type_dist(co_matrix)
#' }
get_type_dist <- function(co_matrix) {
  # Check if matrix has entity types
  if (is.null(attr(co_matrix, "entity_types"))) {
    stop("Co-occurrence matrix does not have entity type information")
  }

  # Get entity types
  entity_types <- attr(co_matrix, "entity_types")

  # Count entities by type
  type_counts <- table(entity_types)

  # Create data frame
  result <- data.frame(
    entity_type = names(type_counts),
    count = as.numeric(type_counts),
    percentage = round(as.numeric(type_counts) / length(entity_types) * 100, 2),
    stringsAsFactors = FALSE
  )

  # Sort by count
  result <- result[order(-result$count), ]

  return(result)
}

#' Filter a co-occurrence matrix by entity type
#'
#' @param co_matrix A co-occurrence matrix produced by create_typed_comat().
#' @param types Character vector of entity types to include.
#'
#' @return A filtered co-occurrence matrix.
#' @export
#'
#' @examples
#' \dontrun{
#' # Keep only disease and drug entities
#' filtered_matrix <- filter_by_type(co_matrix, types = c("disease", "drug"))
#' }
filter_by_type <- function(co_matrix, types) {
  # Check if matrix has entity types
  if (is.null(attr(co_matrix, "entity_types"))) {
    stop("Co-occurrence matrix does not have entity type information")
  }

  # Get entity types
  entity_types <- attr(co_matrix, "entity_types")

  # Get entities of the specified types
  entities_to_keep <- names(entity_types[entity_types %in% types])

  # Filter the matrix
  filtered_matrix <- co_matrix[entities_to_keep, entities_to_keep, drop = FALSE]

  # Preserve attributes
  attr(filtered_matrix, "entity_types") <- entity_types[entities_to_keep]

  if (!is.null(attr(co_matrix, "entity_freq"))) {
    attr(filtered_matrix, "entity_freq") <- attr(co_matrix, "entity_freq")[entities_to_keep]
  }

  if (!is.null(attr(co_matrix, "metadata"))) {
    metadata <- attr(co_matrix, "metadata")
    metadata$n_entities <- length(entities_to_keep)
    attr(filtered_matrix, "metadata") <- metadata
  }

  return(filtered_matrix)
}

#' Validate entity types using NLP-based entity recognition with improved accuracy
#'
#' @param term Character string, the term to validate
#' @param claimed_type Character string, the claimed entity type
#' @param nlp_model The loaded NLP model to use for validation
#' @return Logical indicating if the term is likely of the claimed type
validate_entity_with_nlp <- function(term, claimed_type, nlp_model = NULL) {
  # Load required packages
  if (!requireNamespace("spacyr", quietly = TRUE)) {
    message("Installing spacyr package for NLP-based entity recognition")
    # Initialize spaCy
    spacyr::spacy_initialize()
  }

  # Convert term and claimed_type to lowercase for better matching
  term_lower <- tolower(term)
  if (!is.null(claimed_type)) {
    claimed_type <- tolower(claimed_type)
  }

  # Check for common general terms that should be excluded
  general_terms <- c(
    # Geographic locations
    "europe", "asia", "africa", "america", "australia", "united states", "canada", "uk", "china", "japan",
    "germany", "france", "italy", "spain", "russia", "brazil", "india", "mexico", "germany", "switzerland",

    # Common verbs and generic concepts
    "optimization", "retention", "vehicle", "publication", "test", "review", "experiment",
    "analysis", "development", "application", "investigation", "evaluation", "protocol",
    "survey", "interview", "questionnaire", "scale", "assessment",

    # Other non-biomedical terms
    "method", "model", "approach", "strategy", "design", "value", "cost", "benefit", "risk",
    "measure", "calculate", "determine", "perform", "conduct", "report", "describe", "discuss",
    "recommend", "suggest", "propose", "prove", "demonstrate", "argue", "claim", "state",
    "conclude", "summarize", "clarify", "classification", "categorization", "identification",
    "characterization", "qualification", "quantification", "estimation", "calculation",
    "determination", "measurement", "assessment", "evaluation", "analysis", "interpretation",
    "explanation", "description", "discussion", "recommendation", "conclusion", "review"
  )

  # List of regions, countries, and cities that should be rejected
  geographic_locations <- c(
    "africa", "america", "asia", "australia", "europe", "north america", "south america",
    "central america", "western europe", "eastern europe", "northern europe", "southern europe",
    "middle east", "southeast asia", "east asia", "central asia", "south asia", "north africa",
    "sub-saharan africa", "oceania", "antarctica", "arctic", "caribbean", "mediterranean",
    "scandinavia", "benelux", "balkans", "pacific", "atlantic", "central europe"
  )

  # Immediately reject if the term is a geographic location
  if (term_lower %in% geographic_locations) {
    return(FALSE)
  }

  # Immediately reject if the term is in our general terms list
  if (term_lower %in% general_terms) {
    return(FALSE)
  }

  # Process the term with spaCy
  parsed <- spacyr::spacy_parse(term)

  # Expanded and improved mapping of entity types to spaCy entity types
  type_mapping <- list(
    # Chemical entities
    "chemical" = c("CHEMICAL", "ORG", "PRODUCT", "SUBSTANCE", "FAC"),

    # Disease entities
    "disease" = c("DISEASE", "CONDITION", "SYNDROME", "DISORDER", "PATHOLOGY",
                  "DIAGNOSIS", "ILLNESS", "HEALTH_CONDITION"),

    # Gene entities
    "gene" = c("GENE", "DNA", "RNA", "GENOMIC", "GENETIC_MARKER"),

    # Protein entities - extremely important to get right
    "protein" = c("PROTEIN", "ENZYME", "PEPTIDE", "ANTIBODY", "HORMONE",
                  "RECEPTOR", "GLYCOPROTEIN", "LIPOPROTEIN", "CHANNEL"),

    # Pathway entities
    "pathway" = c("PATHWAY", "PROCESS", "MECHANISM", "SIGNALING", "CASCADE",
                  "METABOLISM", "CYCLE", "NETWORK"),

    # Symptom entities
    "symptom" = c("SYMPTOM", "SIGN", "MANIFESTATION", "PRESENTATION", "COMPLAINT"),

    # Drug entities
    "drug" = c("CHEMICAL", "SUBSTANCE", "MEDICATION", "PRODUCT", "PHARMACEUTICAL",
               "DRUG", "COMPOUND", "MEDICINE"),

    # Biological process entities
    "biological_process" = c("PROCESS", "FUNCTION", "MECHANISM", "ACTIVITY",
                             "PATHWAY", "REGULATION"),

    # Cell entities
    "cell" = c("ANATOMY", "CELL", "TISSUE", "STRUCTURE", "ORG", "ENTITY"),

    # Tissue entities
    "tissue" = c("ANATOMY", "TISSUE", "STRUCTURE", "ORGAN", "ORG"),

    # Organism entities
    "organism" = c("ORG", "ORGANISM", "SPECIES", "BACTERIA", "VIRUS",
                   "MICROORGANISM", "FUNGUS", "PLANT", "ANIMAL"),

    # Molecular function entities
    "molecular_function" = c("FUNCTION", "PROCESS", "ACTIVITY", "MECHANISM"),

    # Cellular component entities
    "cellular_component" = c("CELL", "COMPONENT", "STRUCTURE", "ORGANELLE",
                             "MEMBRANE", "COMPARTMENT"),

    # Diagnostic procedure entities
    "diagnostic_procedure" = c("PROCEDURE", "TEST", "EXAMINATION", "TECHNIQUE",
                               "DIAGNOSIS", "IMAGING"),

    # Therapeutic procedure entities
    "therapeutic_procedure" = c("PROCEDURE", "THERAPY", "TREATMENT", "INTERVENTION",
                                "SURGERY", "MEDICATION"),

    # Anatomy entities
    "anatomy" = c("ANATOMY", "BODY", "STRUCTURE", "ORGAN", "SYSTEM", "TISSUE")
  )

  # Get expected spaCy entity types for claimed type
  expected_types <- type_mapping[[claimed_type]]
  if (is.null(expected_types)) {
    # If we don't have a mapping, be conservative and return FALSE
    return(FALSE)
  }

  # Domain-specific pattern checks (more comprehensive than before)
  type_patterns <- list(
    # Chemical patterns with more specific terms and avoiding general concepts
    "chemical" = "\\b(acid|oxide|ester|amine|compound|element|ion|molecule|solvent|reagent|catalyst|inhibitor|activator|hydroxide|chloride|phosphate|sulfate|nitrate|carbonate)\\b",

    # Disease patterns focusing on explicit disease terminology
    "disease" = "\\b(disease|disorder|syndrome|itis|emia|pathy|oma|infection|deficiency|failure|dysfunction|lesion|malignancy|neoplasm|tumor|cancer|fibrosis|inflammation|sclerosis|atrophy|dystrophy)\\b",

    # Gene patterns with specific genetic terminology
    "gene" = "\\b(gene|allele|locus|promoter|repressor|transcription|expression|mutation|variant|polymorphism|genotype|phenotype|hereditary|dna|chromosome|genomic|rna|mrna|nucleotide)\\b",

    # Protein patterns focusing on protein-specific terminology - very important for fixing the receptor issue
    "protein" = "\\b(protein|enzyme|receptor|antibody|hormone|kinase|phosphatase|transporter|factor|channel|carrier|ase\\b|globulin|albumin|transferase|reductase|oxidase|ligase|protease|peptidase|hydrolase)\\b",

    # Pathway patterns
    "pathway" = "\\b(pathway|cascade|signaling|transduction|regulation|metabolism|synthesis|biosynthesis|degradation|catabolism|anabolism|cycle|flux|transport|secretion|activation|inhibition|phosphorylation)\\b",

    # Symptom patterns
    "symptom" = "\\b(pain|ache|discomfort|swelling|redness|fatigue|weakness|fever|nausea|vomiting|dizziness|vertigo|headache|cough|dyspnea|tachycardia|bradycardia|edema|pallor|cyanosis)\\b",

    # Drug patterns
    "drug" = "\\b(drug|medication|therapy|treatment|compound|dose|inhibitor|agonist|antagonist|blocker|stimulant|suppressant|antidepressant|antibiotic|analgesic|sedative|hypnotic|vaccine|antiviral|antifungal)\\b",

    # Biological process patterns
    "biological_process" = "\\b(process|function|regulation|activity|response|mechanism|homeostasis|apoptosis|autophagy|proliferation|differentiation|migration|adhesion|division|fusion|cycle|phagocytosis|endocytosis|exocytosis)\\b",

    # Cell patterns
    "cell" = "\\b(cell|neuron|microglia|astrocyte|fibroblast|macrophage|lymphocyte|erythrocyte|platelet|epithelial|endothelial|muscle|myocyte|adipocyte|hepatocyte|keratinocyte|melanocyte|osteocyte|chondrocyte)\\b",

    # Tissue patterns
    "tissue" = "\\b(tissue|membrane|epithelium|endothelium|mucosa|connective|muscle|nerve|vessel|artery|vein|capillary|ligament|tendon|cartilage|bone|stroma|parenchyma|dermis|epidermis)\\b",

    # Organism patterns
    "organism" = "\\b(bacteria|virus|fungus|parasite|pathogen|microbe|species|strain|microorganism|prokaryote|eukaryote|archaea|protozoa|helminth|bacillus|coccus|spirochete|mycoplasma|chlamydia|rickettsia)\\b"
  )

  # Check if term matches any patterns for its claimed type
  pattern_match <- FALSE
  if (!is.null(type_patterns[[claimed_type]])) {
    pattern_match <- grepl(type_patterns[[claimed_type]], term_lower)
  }

  # Check if any token has an expected entity type
  entity_types <- parsed$entity_type
  has_expected_type <- any(entity_types %in% expected_types)

  # Check for explicit special cases - ensuring common issues are fixed
  special_case_check <- FALSE

  # Special case: Ensure "receptor" and related terms are recognized as proteins
  if (claimed_type == "protein" &&
      (grepl("receptor", term_lower) ||
       grepl("channel", term_lower) ||
       grepl("transporter", term_lower))) {
    special_case_check <- TRUE
  }

  # Special case: Ensure "malformation" is recognized as a disease/disorder
  if (claimed_type == "disease" && grepl("malformation", term_lower)) {
    special_case_check <- TRUE
  }

  # Special case: Ensure "optimization" is not classified as a chemical
  if (claimed_type == "chemical" &&
      (grepl("optimization", term_lower) ||
       grepl("retention", term_lower) ||
       grepl("vehicle", term_lower))) {
    return(FALSE)
  }

  # List of terms commonly misclassified, expanded with specific examples
  misclassified_terms <- list(
    "chemical" = c("sociodemographic", "demographic", "social", "economic", "education",
                   "income", "status", "cultural", "ethical", "society", "community",
                   "population", "questionnaire", "survey", "interview", "assessment",
                   "scale", "score", "index", "measurement", "evaluation", "analysis",
                   "methodology", "approach", "strategy", "procedure", "protocol",
                   "optimization", "retention", "vehicle", "europe", "usa", "africa"),

    "gene" = c("family", "type", "group", "class", "series", "variety", "category",
               "classification", "collection", "list", "set", "batch", "assortment"),

    "protein" = c("factor", "element", "component", "ingredient", "constituent",
                  "parameter", "variable", "characteristic", "feature", "aspect", "attribute"),

    "pathway" = c("approach", "method", "technique", "procedure", "course", "direction",
                  "route", "channel", "corridor", "passage", "street", "road", "track"),

    "disease" = c("europe", "asia", "africa", "america", "australia", "vehicle",
                  "optimization", "retention", "procedure", "method", "technique")
  )

  # Check if the term is commonly misclassified for its claimed type
  is_misclassified <- FALSE
  if (!is.null(misclassified_terms[[claimed_type]])) {
    if (any(sapply(misclassified_terms[[claimed_type]], function(t)
      grepl(paste0("\\b", t, "\\b"), term_lower)))) {
      is_misclassified <- TRUE
    }
  }

  # Special checks for specific entity types
  is_valid_by_special_check <- FALSE

  # Check if term is a chemical formula or has chemical structure
  if (claimed_type == "chemical" && grepl("[A-Z][a-z]?[0-9]+", term)) {
    is_valid_by_special_check <- TRUE
  }

  # Check if term is a likely gene name (e.g., BRCA1, TP53)
  if (claimed_type == "gene" && grepl("^[A-Z0-9]{2,}[0-9]*$", term)) {
    is_valid_by_special_check <- TRUE
  }

  # Check if term is a likely protein or enzyme
  if (claimed_type == "protein" && grepl("(ase|in)$", term_lower)) {
    is_valid_by_special_check <- TRUE
  }

  # Check if term is a likely drug name
  if (claimed_type == "drug" && grepl("(mab|nib|olol|pril|sartan|prazole|statin)$", term_lower)) {
    is_valid_by_special_check <- TRUE
  }

  # Check if term is a likely disease
  if (claimed_type == "disease" && grepl("(itis|osis|emia|oma|pathy)$", term_lower)) {
    is_valid_by_special_check <- TRUE
  }

  # Return TRUE if any validation method confirms the type and it's not misclassified
  # Also include special_case_check in the condition
  return((has_expected_type || pattern_match || is_valid_by_special_check || special_case_check) && !is_misclassified)
}

#' Validate biomedical entities using BioBERT or other ML models
#'
#' @param term Character string, the term to validate
#' @param claimed_type Character string, the claimed entity type
#' @return Logical indicating if the term is validated
validate_biomedical_entity <- function(term, claimed_type) {
  # If reticulate and Python environment are available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    message("Installing reticulate package for Python integration")
  }

  # First, perform basic regex-based validation before trying more complex ML-based validation
  # This helps filter out obviously incorrect terms efficiently
  term_lower <- tolower(term)

  # Extended list of non-biomedical terms by category
  non_biomedical_terms <- list(
    # Academic/research terms
    "academic" = c("introduction", "method", "methodology", "results", "discussion",
                   "conclusion", "abstract", "background", "objective", "aim", "purpose",
                   "significance", "rationale", "approach", "analysis", "evaluation",
                   "assessment", "measurement", "investigation", "experiment", "observation",
                   "survey", "review", "study", "research", "trial", "test", "examination",
                   "procedure", "technique", "protocol", "design", "framework", "model",
                   "theory", "concept", "paradigm", "hypothesis", "assumption", "limitation",
                   "implication", "recommendation", "reference", "citation", "bibliography",
                   "appendix", "table", "figure", "illustration", "chart", "graph", "diagram",
                   "plot", "map", "image", "photograph", "picture", "drawing", "report",
                   "paper", "article", "manuscript", "publication", "journal", "chapter",
                   "section", "subsection", "paragraph", "sentence", "word", "phrase",
                   "term", "definition", "explanation", "clarification", "interpretation",
                   "understanding", "knowledge", "information", "fact", "finding", "evidence",
                   "data", "statistic", "number", "figure", "calculation", "computation",
                   "algorithm", "formula", "equation", "variable", "parameter", "constant",
                   "coefficient", "factor", "element", "component", "aspect", "dimension",
                   "feature", "characteristic", "property", "attribute", "quality",
                   "recruitment", "benefit"),

    # Statistical/analytical terms
    "statistical" = c("mean", "median", "mode", "average", "standard", "deviation",
                      "variance", "range", "distribution", "normal", "poisson", "binomial",
                      "skewness", "kurtosis", "quartile", "percentile", "correlation",
                      "regression", "covariance", "confidence", "significance", "probability",
                      "likelihood", "hypothesis", "null", "alternative", "p-value", "t-test",
                      "f-test", "chi-square", "anova", "ancova", "manova", "parameter",
                      "statistic", "predictor", "outcome", "dependent", "independent",
                      "variable", "factor", "covariate", "confounder", "moderator", "mediator",
                      "interaction", "random", "fixed", "mixed", "linear", "nonlinear",
                      "parametric", "nonparametric", "univariate", "bivariate", "multivariate",
                      "sample", "population", "estimate", "estimator", "bias", "error",
                      "residual", "outlier", "power", "effect", "size", "sensitivity",
                      "specificity"),

    # Demographic/socioeconomic terms
    "demographic" = c("sociodemographic", "demographic", "social", "economic", "education",
                      "income", "status", "cultural", "ethical", "society", "community",
                      "population", "questionnaire", "survey", "interview", "assessment",
                      "scale", "score", "index", "measurement", "evaluation", "analysis",
                      "nationality", "ethnicity", "race", "gender", "sex", "age", "occupation",
                      "employment", "marital", "household", "residence", "urban", "rural",
                      "metropolitan", "suburban", "literacy", "socioeconomic"),

    # General descriptive terms
    "descriptive" = c("high", "low", "normal", "abnormal", "increase", "decrease",
                      "change", "difference", "similar", "different", "significant",
                      "insignificant", "positive", "negative", "higher", "lower",
                      "larger", "smaller", "greater", "lesser", "better", "worse",
                      "improvement", "deterioration", "mild", "moderate", "severe",
                      "acute", "chronic", "transient", "persistent", "intermittent",
                      "continuous", "progressive", "regressive", "stable", "unstable",
                      "regular", "irregular", "frequent", "infrequent", "rare", "common",
                      "typical", "atypical", "classic", "nonspecific", "specific",
                      "general", "special", "primary", "secondary", "tertiary", "initial",
                      "final", "early", "late", "short", "long", "brief", "extended",
                      "rapid", "slow", "fast", "delay", "immediate", "sudden", "gradual",
                      "partial", "complete", "total", "partial", "focal", "diffuse",
                      "localized", "generalized", "unilateral", "bilateral", "central",
                      "peripheral")
  )

  # Check if term matches any non-biomedical terms
  for (category in names(non_biomedical_terms)) {
    if (term_lower %in% non_biomedical_terms[[category]]) {
      return(FALSE)
    }
  }

  # Try to use Python-based biomedical NER
  tryCatch({
    # Import the necessary Python modules
    transformers <- reticulate::import("transformers")
    torch <- reticulate::import("torch")

    # Load BioBERT NER model
    tokenizer <- transformers$AutoTokenizer$from_pretrained("dmis-lab/biobert-base-cased-v1.1")
    model <- transformers$AutoModelForTokenClassification$from_pretrained("dmis-lab/biobert-base-cased-v1.1-ner")

    # Process the term
    encoded_input <- tokenizer(term, return_tensors = "pt")
    outputs <- model$forward(input_ids = encoded_input$input_ids)
    predictions <- torch$argmax(outputs$logits, dim = 2L)

    # Get predicted labels
    predicted_labels <- tokenizer$batch_decode(predictions)

    # Extended mapping of BioBERT labels to entity types
    biobert_mapping <- list(
      "chemical" = c("B-CHEM", "I-CHEM"),
      "disease" = c("B-DISO", "I-DISO"),
      "gene" = c("B-GENE", "I-GENE"),
      "protein" = c("B-PROT", "I-PROT"),
      "cell" = c("B-CELL", "I-CELL"),
      "species" = c("B-SPEC", "I-SPEC"),
      "pathway" = c("B-PATH", "I-PATH"),
      "drug" = c("B-DRUG", "I-DRUG", "B-CHEM", "I-CHEM"),
      "biological_process" = c("B-PROC", "I-PROC"),
      "tissue" = c("B-TISS", "I-TISS"),
      "cellular_component" = c("B-COMP", "I-COMP"),
      "anatomy" = c("B-ANAT", "I-ANAT"),
      "molecular_function" = c("B-FUNC", "I-FUNC"),
      "organism" = c("B-ORG", "I-ORG", "B-SPEC", "I-SPEC"),
      "symptom" = c("B-SYMPT", "I-SYMPT", "B-DISO", "I-DISO"),
      "diagnostic_procedure" = c("B-PROC", "I-PROC"),
      "therapeutic_procedure" = c("B-PROC", "I-PROC", "B-TREAT", "I-TREAT")
    )

    expected_labels <- biobert_mapping[[claimed_type]]

    # If no mapping exists for this type, fallback to pattern matching
    if (is.null(expected_labels)) {
      return(is_valid_biomedical_entity(term, claimed_type))
    }

    is_valid <- any(sapply(expected_labels, function(label) grepl(label, predicted_labels)))

    return(is_valid)
  }, error = function(e) {
    message("BioBERT validation failed: ", e$message)
    # Fall back to basic validation if BioBERT fails
    return(is_valid_biomedical_entity(term, claimed_type))
  })
}

#' Comprehensive entity validation using multiple techniques
#'
#' @param term Character string, the term to validate
#' @param claimed_type Character string, the claimed entity type
#' @param use_nlp Logical, whether to use NLP-based validation
#' @param use_pattern Logical, whether to use pattern-based validation
#' @param use_external_api Logical, whether to query external APIs
#' @return Logical indicating if the term is validated
validate_entity_comprehensive <- function(term, claimed_type,
                                          use_nlp = TRUE,
                                          use_pattern = TRUE,
                                          use_external_api = FALSE) {

  # Convert type to lowercase for consistent comparison
  if (!is.null(claimed_type)) {
    claimed_type <- tolower(claimed_type)
  }

  # Skip validation for terms too short (e.g., single characters)
  if (is.null(term) || is.na(term) || nchar(term) < 2) {
    return(FALSE)
  }

  # Special case for problematic terms
  problematic_terms <- c(
    "sociodemographic", "demographic", "social", "economic", "education",
    "income", "status", "cultural", "ethical", "society", "community",
    "population", "questionnaire", "survey", "interview", "assessment",
    "scale", "score", "index", "measurement", "evaluation", "analysis",
    "methodology", "approach", "strategy", "procedure", "protocol",
    "high", "low", "normal", "abnormal", "increase", "decrease",
    "change", "difference", "significant", "insignificant", "positive",
    "higher", "lower", "larger", "smaller", "greater", "lesser", "better"
  )

  if (tolower(term) %in% problematic_terms) {
    return(FALSE)
  }

  # Initialize validation results
  results <- logical(0)

  # Pattern-based validation (our original approach)
  if (use_pattern) {
    pattern_result <- is_valid_biomedical_entity(term, claimed_type)
    results <- c(results, pattern_result)
  }

  # NLP-based validation
  if (use_nlp) {
    nlp_result <- tryCatch({
      validate_entity_with_nlp(term, claimed_type)
    }, error = function(e) {
      message("NLP validation failed: ", e$message)
      return(is_valid_biomedical_entity(term, claimed_type))  # Fallback to pattern-based
    })
    results <- c(results, nlp_result)
  }

  # External API validation (e.g., PubChem, UniProt)
  if (use_external_api) {
    api_result <- tryCatch({
      validate_biomedical_entity(term, claimed_type)
    }, error = function(e) {
      message("API validation failed: ", e$message)
      return(is_valid_biomedical_entity(term, claimed_type))  # Fallback to pattern-based
    })
    results <- c(results, api_result)
  }

  # Special handling for known categories of terms
  term_lower <- tolower(term)

  # Check for statistical or methodological terms that aren't biomedical entities
  stat_method_terms <- c("significant", "analysis", "result", "correlation", "association",
                         "outcome", "variable", "factor", "parameter", "cohort", "group",
                         "control", "case", "study", "research", "trial", "experiment",
                         "observation", "model", "algorithm", "data", "sample", "population")

  if (term_lower %in% stat_method_terms) {
    return(FALSE)
  }

  # If any method validates the term, consider it valid
  # But if the term is explicitly rejected by any method, reject it
  if (length(results) > 0) {
    return(any(results) && !all(!results))
  }

  # Default to FALSE for terms that couldn't be validated
  return(FALSE)
}

#' Query external biomedical APIs to validate entity types
#'
#' @param term Character string, the term to validate
#' @param claimed_type Character string, the claimed entity type
#' @return Logical indicating if the term was found in the appropriate database
query_external_api <- function(term, claimed_type) {
  # Define API endpoints based on entity type
  api_endpoints <- list(
    "chemical" = "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/name/",
    "gene" = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gene&term=",
    "protein" = "https://www.uniprot.org/uniprot/?query=",
    "disease" = "https://www.diseaseontology.org/api/search?q="
  )

  endpoint <- api_endpoints[[claimed_type]]
  if (is.null(endpoint)) {
    return(TRUE)  # No endpoint for this type, be conservative
  }

  # Prepare URL-safe term
  safe_term <- utils::URLencode(term)

  # Query the API
  response <- tryCatch({
    httr::GET(paste0(endpoint, safe_term))
  }, error = function(e) {
    message("API query failed: ", e$message)
    return(NULL)
  })

  # Check if we got a valid response
  if (is.null(response) || httr::status_code(response) != 200) {
    return(FALSE)
  }

  # Different APIs have different response formats
  # This is a simplified check - production code would need more parsing
  content <- httr::content(response, "text")
  return(!grepl("No items found", content) && !grepl("0 results", content))
}
