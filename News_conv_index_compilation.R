############################################################
# News Conventionality Index (NCI) — Compilation Script
#
# PURPOSE
#   Builds directly on the output from:
#     News_Conv_Features.R
#
#   This script:
#     1) Defines NCI feature groups (dimensions)
#     2) Z-standardises component features (document-level)
#     3) Computes five dimension scores (mean of z-features per group)
#     4) Computes the final NCI and a z-standardised NCI_z
#
# REQUIRED INPUT OBJECT
#   `conventionality_df` (a data frame created by News_Conv_Features.R)
#
# REQUIRED COLUMNS IN `conventionality_df`
#   At minimum, the feature columns referenced in the feature groups below.
#   If any are missing, the script will warn and compute scores from the
#   remaining available features (where possible).
#
# OUTPUT
#   The input `conventionality_df` is augmented with:
#     epistemic_score, subjectivity_score, style_score,
#     temporal_anchoring_score, discourse_score, NCI, NCI_z
#
# OPTIONAL FILE OUTPUT
#   Writes an output CSV if WRITE_CSV is TRUE.
#
# NOTE
#   This script does not assume anything about your original corpus object
#   (`text_corpus_df`). It only requires the feature table `conventionality_df`.
############################################################

# ------------------------------
# Toggles
# ------------------------------
WRITE_CSV  <- FALSE
OUT_FILE   <- "news_conventionality_index.csv"
DROP_Z_COLS <- TRUE   # set FALSE to keep z_* columns for inspection

# ------------------------------
# Packages
# ------------------------------
pkgs <- c("dplyr", "readr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ------------------------------
# Safety checks
# ------------------------------
if (!exists("conventionality_df")) {
  stop("Object `conventionality_df` not found. Run News_Conv_Features.R first.")
}
if (!is.data.frame(conventionality_df)) {
  stop("`conventionality_df` exists but is not a data.frame.")
}

# ------------------------------
# Helper: safe z-score
# ------------------------------
safe_z <- function(v) {
  v <- as.numeric(v)
  m <- mean(v, na.rm = TRUE)
  s <- stats::sd(v, na.rm = TRUE)
  if (!is.finite(s) || s == 0) rep(0, length(v)) else (v - m) / s
}

# Helper: rowMeans that tolerates empty feature sets
rowMeanSafe <- function(df, cols) {
  if (!length(cols)) return(rep(NA_real_, nrow(df)))
  as.numeric(rowMeans(df[, cols, drop = FALSE], na.rm = TRUE))
}

# ==========================================================
# 1) DEFINE FEATURE GROUPS (dimensions)
#    Convention: higher NCI = more "conventional hard news" style.
# ==========================================================

# A) Epistemic conventionality (expected + for NCI)
feat_epistemic <- c(
  "modal_per100",
  "hedge_per100",
  "reporting_per100",
  "short_sent_ratio"
)

# B) Subjectivity / evaluative stance (expected - for NCI)
#    NOTE: If your sentiment lexicons are not available (e.g., non-Norwegian),
#    polarity_net and subjectivity may be 0 (from the feature script) or absent.
feat_subjectivity <- c(
  "subjectivity",
  "polarity_net"
)

# C) Style / emotional intensity (expected - for NCI)
#    Includes both low-level punctuation/intensification and lexical elaboration
#    (the same set used to compute `colour_index` in the feature script).
feat_style <- c(
  "intensifier_per100",
  "exclam_per1k",
  "adj_per100",
  "adv_per100",
  "hyphen_per1k",
  "rhet_punc_per1k",
  "vivid_mwe_per100",
  "avg_idf",
  "rare_share",
  "phrasal_per1k"
)

# D) Temporal anchoring (expected + for NCI)
feat_temporal_anchor <- c(
  "deictic_time_per100"
)

# E) Discourse mode (argumentative + narrative density; expected - for NCI)
feat_discourse <- c(
  "arg_conn_per100",
  "narr_mark_per100"
)

# ---- Resolve missing features gracefully ----
all_feats <- unique(c(
  feat_epistemic,
  feat_subjectivity,
  feat_style,
  feat_temporal_anchor,
  feat_discourse
))

missing_feats <- setdiff(all_feats, names(conventionality_df))
if (length(missing_feats)) {
  warning(
    "Missing feature column(s) in conventionality_df (will be ignored):\n  - ",
    paste(missing_feats, collapse = "\n  - ")
  )
}

# Keep only those that exist
feat_epistemic       <- intersect(feat_epistemic,       names(conventionality_df))
feat_subjectivity    <- intersect(feat_subjectivity,    names(conventionality_df))
feat_style           <- intersect(feat_style,           names(conventionality_df))
feat_temporal_anchor <- intersect(feat_temporal_anchor, names(conventionality_df))
feat_discourse       <- intersect(feat_discourse,       names(conventionality_df))

# ==========================================================
# 2) Z-STANDARDISE ALL COMPONENT FEATURES
# ==========================================================
z_targets <- unique(c(
  feat_epistemic,
  feat_subjectivity,
  feat_style,
  feat_temporal_anchor,
  feat_discourse
))

for (f in z_targets) {
  conventionality_df[[paste0("z_", f)]] <- safe_z(conventionality_df[[f]])
}

# ==========================================================
# 3) DIMENSION SCORES (mean of z-features in each group)
# ==========================================================
conventionality_df <- conventionality_df %>%
  mutate(
    epistemic_score          = rowMeanSafe(., paste0("z_", feat_epistemic)),
    subjectivity_score       = rowMeanSafe(., paste0("z_", feat_subjectivity)),
    style_score              = rowMeanSafe(., paste0("z_", feat_style)),
    temporal_anchoring_score = rowMeanSafe(., paste0("z_", feat_temporal_anchor)),
    discourse_score          = rowMeanSafe(., paste0("z_", feat_discourse))
  )

# ==========================================================
# 4) FINAL NCI AND NCI_z
#
#    Sign convention:
#      + epistemic_score
#      + temporal_anchoring_score
#      - subjectivity_score
#      - style_score
#      - discourse_score
#
#    Note: This creates an additive index. If you prefer the final index to be
#    the mean of signed dimensions instead of a sum, replace the line for NCI.
# ==========================================================
conventionality_df <- conventionality_df %>%
  mutate(
    NCI = epistemic_score +
      temporal_anchoring_score -
      subjectivity_score -
      style_score -
      discourse_score,
    NCI_z = safe_z(NCI)
  )

# ---- Optional cleanup: drop z_ columns ----
if (DROP_Z_COLS) {
  conventionality_df <- conventionality_df %>% select(-starts_with("z_"))
}

cat(
  "Done. Added columns:\n",
  "  epistemic_score, subjectivity_score, style_score,\n",
  "  temporal_anchoring_score, discourse_score, NCI, NCI_z\n",
  sep = ""
)

# ---- Optional save ----
if (WRITE_CSV) {
  readr::write_csv(conventionality_df, OUT_FILE)
  cat("Wrote:", OUT_FILE, "\n")
}
