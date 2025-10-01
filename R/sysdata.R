# sysdata.R - Create comprehensive static data list for LBDiscover package

# Create comprehensive static data list
static_data <- list(

  # =============================================================================
  # ACRONYM CORRECTIONS (from is_valid_biomedical_entity)
  # =============================================================================
  acronym_corrections = list(
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
    "spect" = "method",              # Single-Photron Emission Computed Tomography
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

    # Common diseases
    "migraine" = "disease",
    "headache" = "symptom",
    "pain" = "symptom",
    "nausea" = "symptom",
    "vomiting" = "symptom",
    "dizziness" = "symptom",
    "fatigue" = "symptom",
    "weakness" = "symptom",
    "aura" = "symptom",
    "photophobia" = "symptom",
    "phonophobia" = "symptom",
    "malformation" = "disease",
    "cardiomyopathy" = "disease",

    # Proteins and receptors
    "receptor" = "protein",
    "receptors" = "protein",
    "channel" = "protein",
    "channels" = "protein",
    "transporter" = "protein",
    "transporters" = "protein",

    # Common drugs
    "sumatriptan" = "drug",
    "aspirin" = "drug",
    "ibuprofen" = "drug",

    # Biological processes
    "inflammation" = "biological_process",
    "signaling" = "biological_process",
    "activation" = "biological_process",
    "inhibition" = "biological_process",
    "regulation" = "biological_process",
    "phosphorylation" = "biological_process"
  ),

  # =============================================================================
  # TERM TYPE MAPPINGS (same as acronym_corrections, maintained for compatibility)
  # =============================================================================
  term_type_mappings = list(
    "migraine" = "disease",
    "receptor" = "protein",
    "receptors" = "protein",
    "channel" = "protein",
    "channels" = "protein",
    "malformation" = "disease",
    "inflammation" = "biological_process",
    "sumatriptan" = "drug",
    "cardiomyopathy" = "disease"
  ),

  # =============================================================================
  # GEOGRAPHIC LOCATIONS
  # =============================================================================
  geographic_locations = c(
    "africa", "america", "asia", "australia", "europe", "north america", "south america",
    "central america", "western europe", "eastern europe", "northern europe", "southern europe",
    "middle east", "southeast asia", "east asia", "central asia", "south asia", "north africa",
    "sub-saharan africa", "oceania", "antarctica", "arctic", "caribbean", "mediterranean",
    "scandinavia", "benelux", "balkans", "pacific", "atlantic", "central europe",
    "usa", "china", "japan", "germany", "uk", "france", "italy", "spain", "russia",
    "brazil", "india", "canada", "mexico", "australia", "switzerland", "sweden", "norway"
  ),

  # =============================================================================
  # PROBLEMATIC SPECIFIC TERMS
  # =============================================================================
  problematic_specific_terms = c(
    "europe", "vehicle", "optimization", "retention"
  ),

  # =============================================================================
  # COMMON WORDS (from multiple sources)
  # =============================================================================
  common_words = c(
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
    "outcome", "result", "finding", "evidence"
  ),

  # =============================================================================
  # BLACKLISTED TERMS (comprehensive from multiple sources)
  # =============================================================================
  blacklisted_terms = c(
    # Common language terms that aren't biomedical
    "the", "of", "and", "in", "to", "a", "is", "that", "for", "it", "with", "as", "was",
    "on", "be", "by", "this", "an", "we", "our", "these", "those", "which", "or", "if",
    "have", "has", "had", "are", "were", "can", "could", "would", "should", "may", "might",
    "will", "must", "also", "only", "very", "such", "so", "but", "than", "when", "where",
    "how", "what", "who", "whom", "whose", "why", "not", "all", "any", "each", "every",
    "some", "many", "few", "most", "more", "less", "other", "another", "same", "different",
    "CENTRAL", "remain", "attention", "Delay", "highlight", "highlights", "highlighting",
    "indicate", "suggests", "demonstrated", "show", "shown", "shows", "reveal", "revealed",
    "further", "findings", "results", "find", "found", "into", "across", "both", "several",
    "provide", "provides", "provided", "crucial", "critical", "important", "notably",
    "particularly", "specific", "associated", "related", "linked", "while", "however",
    "advanced", "advancing", "development", "potential", "potentially", "novel", "insights",

    # Academic language and research paper terminology - EXPANDED
    "demonstrate", "within", "alongside", "investigate", "explain", "integrate", "elucidate",
    "certain", "debate", "state", "recapitulate", "phase", "translate", "modulate",
    "ultimate", "whole", "varied", "role", "speculate", "side", "academia",
    "considerable", "consistent", "substantial", "significant", "relevant", "important",
    "interesting", "promising", "similar", "different", "distinct", "specific", "particular",
    "major", "minor", "key", "main", "essential", "necessary", "sufficient", "adequate",
    "proper", "appropriate", "suitable", "consecutive", "simultaneous", "various",
    "variable", "concurrent", "concomitant", "overall", "entire", "whole", "optimum",
    "optimal", "ideal", "better", "best", "worse", "worst", "efficacious", "limited",
    "extensive", "intensive", "widespread", "reliable", "reproducible", "repeatable",
    "comparable", "varied", "useful", "valuable", "successful", "unsuccessful",
    "effective", "ineffective", "minimum", "maximum",

    # Generic research terms
    "method", "approach", "analysis", "assessment", "evaluation", "procedure", "technique",
    "protocol", "intervention", "treatment", "outcome", "result", "effect", "impact",
    "value", "study", "trial", "research", "experiment", "observation", "publication",
    "test", "measure", "detection", "identification", "classification", "characterization",
    "determination", "calculation", "examination", "investigation", "exploration",
    "screening", "monitoring", "surveillance", "survey", "review", "overview", "summary",
    "score", "grade", "rating", "ranking", "stratification", "categorization", "grouping"
  ),

  # =============================================================================
  # MESH QUERY MAPPING
  # =============================================================================
  mesh_query_map = list(
    "disease" = "disease[MeSH]",
    "drug" = "pharmaceutical preparations[MeSH]",
    "gene" = "genes[MeSH]",
    "protein" = "proteins[MeSH]",
    "chemical" = "chemicals[MeSH]",
    "pathway" = "metabolic networks and pathways[MeSH]",
    "anatomy" = "anatomy[MeSH]",
    "organism" = "organisms[MeSH]",
    "biological_process" = "biological phenomena[MeSH]",
    "cell" = "cells[MeSH]",
    "tissue" = "tissues[MeSH]",
    "symptom" = "signs and symptoms[MeSH]",
    "diagnostic_procedure" = "diagnostic techniques and procedures[MeSH]",
    "therapeutic_procedure" = "therapeutics[MeSH]",
    "phenotype" = "phenotype[MeSH]",
    "molecular_function" = "molecular biology[MeSH]"
  ),

  # =============================================================================
  # UMLS SEMANTIC TYPES MAPPING
  # =============================================================================
  umls_semantic_types = list(
    # Original mappings
    "disease" = c("T047", "T048", "T019", "T046"), # Disease and Syndrome, Mental Disorder, Congenital Abnormality, Pathologic Function
    "drug" = c("T116", "T121", "T195", "T200"), # Amino Acid, Lipid, Antibiotic, Clinical Drug
    "gene" = c("T028", "T087", "T123"), # Gene, Amino Acid Sequence, Nucleotide Sequence

    # New expanded mappings
    "protein" = c("T116", "T126", "T125"), # Amino Acid, Peptide/Protein, Enzyme
    "chemical" = c("T103", "T104", "T109", "T196"), # Chemical, Chemical Viewed Structurally, Organic Chemical, Element, Ion, or Isotope
    "pathway" = c("T044", "T042", "T045"), # Molecular Function, Organ or Tissue Function, Genetic Function
    "anatomy" = c("T017", "T018", "T023", "T024"), # Anatomical Structure, Embryonic Structure, Body Part, Organ, or Organ Component, Tissue
    "organism" = c("T001", "T002", "T004", "T005", "T007"), # Organism, Plant, Fungus, Virus, Bacterium
    "biological_process" = c("T038", "T039", "T040", "T041"), # Biologic Function, Physiologic Function, Organism Function, Mental Process
    "cellular_component" = c("T026", "T025", "T029"), # Cell Component, Cell, Body Location or Region
    "molecular_function" = c("T044", "T045", "T118"), # Molecular Function, Genetic Function, Carbohydrate
    "diagnostic_procedure" = c("T059", "T060"), # Laboratory Procedure, Diagnostic Procedure
    "therapeutic_procedure" = c("T061", "T058"), # Therapeutic or Preventive Procedure, Health Care Activity
    "phenotype" = c("T046", "T047", "T048", "T020"), # Pathologic Function, Disease or Syndrome, Mental or Behavioral Dysfunction, Acquired Abnormality
    "symptom" = c("T184", "T046", "T048"), # Sign or Symptom, Pathologic Function, Mental or Behavioral Dysfunction
    "cell" = c("T025"), # Cell
    "tissue" = c("T024"), # Tissue
    "mental_process" = c("T041", "T048"), # Mental Process, Mental or Behavioral Dysfunction
    "physiologic_function" = c("T039", "T040"), # Physiologic Function, Organism Function
    "laboratory_procedure" = c("T059") # Laboratory Procedure
  ),

  # =============================================================================
  # BIOMEDICAL PATTERNS BY TYPE
  # =============================================================================
  biomedical_patterns = list(
    # Chemical patterns with more specific terms and avoiding general concepts
    "chemical" = "\\b(acid|oxide|ester|amine|compound|element|ion|molecule|solvent|reagent|catalyst|inhibitor|activator|hydroxide|chloride|phosphate|sulfate|nitrate|carbonate)\\b",

    # Disease patterns - split into whole words and suffixes for better matching
    "disease" = "\\b(disease|disorder|syndrome|infection|deficiency|failure|dysfunction|lesion|malignancy|neoplasm|tumor|cancer|fibrosis|inflammation|sclerosis|atrophy|dystrophy)\\b|(itis|emia|pathy|oma)($|\\b)",

    # Gene patterns with specific genetic terminology
    "gene" = "\\b(gene|allele|locus|promoter|repressor|transcription|expression|mutation|variant|polymorphism|genotype|phenotype|hereditary|dna|chromosome|genomic|rna|mrna|nucleotide)\\b",

    # Protein patterns focusing on protein-specific terminology
    "protein" = "\\b(protein|enzyme|receptor|antibody|hormone|kinase|phosphatase|transporter|factor|channel|carrier|globulin|albumin|transferase|reductase|oxidase|ligase|protease|peptidase|hydrolase)\\b|ase($|\\b)",

    # Pathway patterns
    "pathway" = "\\b(pathway|cascade|signaling|transduction|regulation|metabolism|synthesis|biosynthesis|degradation|catabolism|anabolism|cycle|flux|transport|secretion|activation|inhibition|phosphorylation)\\b",

    # Symptom patterns
    "symptom" = "\\b(pain|ache|discomfort|swelling|redness|fatigue|weakness|fever|nausea|vomiting|dizziness|vertigo|headache|cough|dyspnea|tachycardia|bradycardia|edema|pallor|cyanosis)\\b",

    # Drug patterns - include common drug suffixes
    "drug" = "\\b(drug|medication|therapy|treatment|compound|dose|inhibitor|agonist|antagonist|blocker|stimulant|suppressant|antidepressant|antibiotic|analgesic|sedative|hypnotic|vaccine|antiviral|antifungal)\\b|(triptan|statin|pril|sartan|olol|pine|caine|mab|nib|prazole|tidine|oxetine|pam|cycline|cillin|floxacin|mycin|vir)($|\\b)",

    # Biological process patterns
    "biological_process" = "\\b(process|function|regulation|activity|response|mechanism|homeostasis|apoptosis|autophagy|proliferation|differentiation|migration|adhesion|division|fusion|cycle|phagocytosis|endocytosis|exocytosis)\\b",

    # Cell patterns
    "cell" = "\\b(cell|neuron|microglia|astrocyte|fibroblast|macrophage|lymphocyte|erythrocyte|platelet|epithelial|endothelial|muscle|myocyte|adipocyte|hepatocyte|keratinocyte|melanocyte|osteocyte|chondrocyte|cyte)\\b",

    # Tissue patterns
    "tissue" = "\\b(tissue|membrane|epithelium|endothelium|mucosa|connective|muscle|nerve|vessel|artery|vein|capillary|ligament|tendon|cartilage|bone|stroma|parenchyma|dermis|epidermis)\\b",

    # Organism patterns
    "organism" = "\\b(bacteria|virus|fungus|parasite|pathogen|microbe|species|strain|microorganism|prokaryote|eukaryote|archaea|protozoa|helminth|bacillus|coccus|spirochete|mycoplasma|chlamydia|rickettsia)\\b"
  ),

  # =============================================================================
  # MISCLASSIFIED TERMS BY TYPE
  # =============================================================================
  misclassified_terms = list(
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
  ),

  # =============================================================================
  # BIOMEDICAL SUFFIXES
  # =============================================================================
  biomedical_suffixes = c("in$", "ase$", "gen$", "one$", "ide$", "ate$", "ene$", "ane$", "ole$",
                          "itis$", "osis$", "emia$", "pathy$", "trophy$", "plasia$", "poiesis$",
                          "genesis$", "lysis$", "ectomy$", "otomy$", "ostomy$", "plasty$", "pexy$",
                          "rhaphy$", "graphy$", "scopy$", "metry$", "algia$", "dynia$", "oma$",
                          "iasis$", "ismus$", "uria$", "pnea$", "emesis$", "pepsia$", "phagia$",
                          "rrhea$", "rrhage$", "sthenia$", "phobia$", "lexia$", "praxia$", "gnosis$",
                          "penia$", "cytosis$", "esthesia$", "kinesia$", "phasia$", "plegia$", "paresis$"),

  # =============================================================================
  # BIOMEDICAL COMPONENTS
  # =============================================================================
  biomedical_components = c("neuro", "cardio", "gastro", "hepato", "nephro", "dermato",
                            "hemato", "immuno", "onco", "osteo", "arthro", "myelo", "cyto",
                            "histo", "patho", "pharmaco", "psycho", "toxo", "vas", "angio"),

  # Terms that are never biomedical
  never_biomedical = c(
    "optimization", "retention", "vehicle", "europe", "asia", "africa", "america",
    "australia", "recruitment", "benefit", "significant", "analysis", "result",
    "correlation", "association", "outcome", "variable", "factor", "parameter",
    "cohort", "group", "control", "case", "study", "research", "trial", "experiment",
    "observation", "model", "algorithm", "data", "sample", "population"
  ),

  # Special exceptions that may be biomedical in certain contexts
  special_exceptions = list(
    "malformation" = "disease",
    "receptor" = "protein",
    "channel" = "protein"
  ),

  # Entity type colors for visualization
  entity_type_colors = list(
    "disease" = "#E31A1C",    # Red
    "drug" = "#1F78B4",       # Blue
    "gene" = "#33A02C",       # Green
    "protein" = "#FF7F00",    # Orange
    "chemical" = "#6A3D9A",   # Purple
    "pathway" = "#B15928",    # Brown
    "symptom" = "#FB9A99",    # Light Red
    "cell" = "#A6CEE3",       # Light Blue
    "tissue" = "#B2DF8A",     # Light Green
    "organism" = "#FDBF6F",   # Light Orange
    "anatomy" = "#CAB2D6",    # Light Purple
    "biological_process" = "#FFFF99", # Yellow
    "A" = "#E31A1C",          # Red for A nodes
    "B" = "#33A02C",          # Green for B nodes
    "C" = "#1F78B4"           # Blue for C nodes
  ),

  # Dummy dictionaries for testing/fallback
  dummy_dictionaries = list(
    "disease" = data.frame(
      term = c("migraine", "headache", "pain", "disorder", "syndrome"),
      id = paste0("DISEASE_", 1:5),
      type = rep("disease", 5),
      source = rep("dummy", 5),
      stringsAsFactors = FALSE
    ),
    "drug" = data.frame(
      term = c("sumatriptan", "aspirin", "ibuprofen", "medication", "treatment"),
      id = paste0("DRUG_", 1:5),
      type = rep("drug", 5),
      source = rep("dummy", 5),
      stringsAsFactors = FALSE
    ),
    "gene" = data.frame(
      term = c("CACNA1A", "SCN1A", "TRPV1", "gene", "allele"),
      id = paste0("GENE_", 1:5),
      type = rep("gene", 5),
      source = rep("dummy", 5),
      stringsAsFactors = FALSE
    ),
    "protein" = data.frame(
      term = c("receptor", "channel", "transporter", "enzyme", "kinase"),
      id = paste0("PROTEIN_", 1:5),
      type = rep("protein", 5),
      source = rep("dummy", 5),
      stringsAsFactors = FALSE
    ),
    "chemical" = data.frame(
      term = c("serotonin", "dopamine", "GABA", "glutamate", "acetylcholine"),
      id = paste0("CHEMICAL_", 1:5),
      type = rep("chemical", 5),
      source = rep("dummy", 5),
      stringsAsFactors = FALSE
    ),
    "pathway" = data.frame(
      term = c("signaling", "metabolism", "transport", "regulation", "cascade"),
      id = paste0("PATHWAY_", 1:5),
      type = rep("pathway", 5),
      source = rep("dummy", 5),
      stringsAsFactors = FALSE
    )
  )
)

# Make mesh_query_map available at global level for backwards compatibility
mesh_query_map <- static_data$mesh_query_map

# Save the data using usethis if available, otherwise use base save
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(static_data, internal = TRUE, overwrite = TRUE)
} else {
  save(static_data, file = "R/sysdata.rda", compress = "xz")
}
