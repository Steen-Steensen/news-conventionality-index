# ============================================================
# News Conventionality Feature Extraction
# (Norwegian example implementation; language-agnostic core)
#
# PURPOSE
#   Extract linguistic features used to compute the News Conventionality Index (NCI).
#   The *feature extraction logic is language-agnostic*, but several features depend
#   on *language-specific lexicons* (e.g., hedges, connectives, temporal deictics,
#   reporting constructions, etc.). This script is configured with a Norwegian
#   lexicon setup as an example.
#
# INPUT OBJECT (required)
#   A data frame named `text_corpus_df` in the R environment.
#
# REQUIRED COLUMNS in `text_corpus_df`
#   - id         : unique text identifier (character/integer)
#   - headline   : headline text (character; can be NA)
#   - lead       : lead/intro text (character; can be NA)
#   - body_text  : main text body (character; can be NA)
#
# OPTIONAL METADATA COLUMNS (carried through to the output if present)
#   - newsroom, type, url, genre
#   (If missing, they are created as NA and the script still runs.)
#
# LEXICONS
#   By default, lexicons are expected under:
#     ./lexicons/
#
#   Norwegian sentiment features (polarity_net, subjectivity) are *optionally*
#   computed using fullform sentiment lexicons from NorSentLex:
#     https://github.com/ltgoslo/norsentlex
#
#   IMPORTANT:
#     - NorSentLex is Norwegian-specific and is only relevant when applying the
#       NCI in a Norwegian context.
#     - The NorSentLex lexicon files are NOT included in this repository.
#     - For other languages, you must supply or construct an appropriate sentiment
#       lexicon (or disable sentiment features).
#
# OUTPUT OBJECT
#   `conventionality_df` with extracted feature columns + `colour_index`
#
# OPTIONAL FILE OUTPUT
#   Writes "news_conventionality_features.csv" if WRITE_CSV is TRUE
# ============================================================

# --------------------------------------------
# Toggles
# --------------------------------------------
RUN_SAMPLE <- TRUE      # TRUE: sample up to 5,000 docs for quick tests
WRITE_CSV  <- FALSE     # TRUE: write conventionality_df to CSV
set.seed(1234)

# --------------------------------------------
# 0. Packages
# --------------------------------------------
pkgs <- c(
  "dplyr", "stringr", "tibble", "quanteda",
  "quanteda.textstats", "purrr", "tidyr",
  "Matrix", "readr"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# --------------------------------------------
# 1. Input checks + column contract
# --------------------------------------------
if (!exists("text_corpus_df")) {
  stop("Object `text_corpus_df` not found. Please load your corpus as a data frame named `text_corpus_df`.")
}

req_cols <- c("id", "headline", "lead", "body_text")
missing_req <- setdiff(req_cols, names(text_corpus_df))
if (length(missing_req)) {
  stop(
    "Missing required column(s) in `text_corpus_df`: ",
    paste(missing_req, collapse = ", "),
    ".\nRequired columns are: ", paste(req_cols, collapse = ", ")
  )
}

# Optional metadata columns (keep if present; otherwise create NA)
opt_cols <- c("newsroom", "type", "url", "genre")
for (nm in opt_cols) {
  if (!nm %in% names(text_corpus_df)) text_corpus_df[[nm]] <- NA
}

# --------------------------------------------
# 2. Select input (sample or full)
# --------------------------------------------
if (RUN_SAMPLE) {
  n_total <- nrow(text_corpus_df)
  n_samp  <- min(5000L, n_total)
  idx_samp <- sample.int(n_total, size = n_samp, replace = FALSE)
  input_df <- text_corpus_df[idx_samp, , drop = FALSE]
  cat("Testing NCI feature extraction on a random sample of", nrow(input_df), "texts (of", n_total, ").\n")
} else {
  input_df <- text_corpus_df
  cat("Running NCI feature extraction on FULL dataset:", nrow(input_df), "texts.\n")
}

# --------------------------------------------
# 3. Helpers
# --------------------------------------------
vfill <- function(x, fill = "") { x <- as.character(x); x[is.na(x)] <- fill; x }
split_sentences <- function(x) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  unlist(stringr::str_split(x, "(?<=[.!?])\\s+"))
}
safe_z <- function(v) {
  m <- mean(v, na.rm = TRUE); s <- stats::sd(v, na.rm = TRUE)
  if (!is.finite(s) || s == 0) rep(0, length(v)) else (v - m) / s
}

# Lexicon loader (one entry per line; ignores blank lines and # comments)
LEX_DIR <- file.path("lexicons")
load_lex <- function(fname) {
  readr::read_lines(file.path(LEX_DIR, fname)) |>
    stringr::str_trim() |>
    purrr::discard(~ .x == "" || stringr::str_starts(.x, "#")) |>
    tolower() |>
    unique()
}

# Small dfm → counts + per100 helper
dfm_to_counts <- function(dfm_obj, feat_name, tok_vec) {
  n_doc <- length(tok_vec)
  if (!quanteda::ndoc(dfm_obj)) {
    out <- tibble::tibble(row = seq_len(n_doc), tokens = tok_vec)
    out[[feat_name]] <- 0
    out[[paste0(feat_name, "_per100")]] <- 0
    return(out)
  }
  mat <- as.matrix(dfm_obj)
  counts <- if (feat_name %in% colnames(mat)) as.numeric(mat[, feat_name]) else rep(0, nrow(mat))
  out <- tibble::tibble(row = seq_along(counts), tokens = tok_vec)
  out[[feat_name]] <- counts
  out[[paste0(feat_name, "_per100")]] <- 100 * counts / pmax(tok_vec, 1)
  out
}

# --------------------------------------------
# 4. Build article text
# --------------------------------------------
dat <- input_df |>
  dplyr::mutate(
    headline  = vfill(headline),
    lead      = vfill(lead),
    body_text = vfill(body_text),
    article_text = stringr::str_squish(paste(headline, lead, body_text, sep = "\n\n"))
  )

# --------------------------------------------
# 5. Load lexicons
# --------------------------------------------
cat("Loading lexicons from:", normalizePath(LEX_DIR, winslash = "/", mustWork = FALSE), "\n")

# 5.1 NorSentLex sentiment lexicons (Norwegian-only, external)
#     Repo: https://github.com/ltgoslo/norsentlex
#
# These files are not included in this repository. If they are absent,
# polarity_net and subjectivity are set to 0 (script will still run).
POS_FILE <- file.path(LEX_DIR, "Fullform_Positive_lexicon.txt")
NEG_FILE <- file.path(LEX_DIR, "Fullform_Negative_lexicon.txt")

has_sent_lex <- file.exists(POS_FILE) && file.exists(NEG_FILE)
if (!has_sent_lex) {
  warning(
    "NorSentLex fullform sentiment lexicons not found in ", LEX_DIR, ".\n",
    "Norwegian sentiment features (polarity_net, subjectivity) will be set to 0.\n",
    "If you are analysing Norwegian texts and want these features, download the fullform lexicons from:\n",
    "  https://github.com/ltgoslo/norsentlex\n",
    "and place them as:\n",
    "  - ", POS_FILE, "\n  - ", NEG_FILE, "\n",
    "For other languages, supply a different sentiment lexicon (or keep sentiment features disabled)."
  )
  lex_pos <- character(0)
  lex_neg <- character(0)
} else {
  lex_pos <- readr::read_tsv(POS_FILE, col_names = FALSE, show_col_types = FALSE)$X1 |> tolower() |> unique()
  lex_neg <- readr::read_tsv(NEG_FILE, col_names = FALSE, show_col_types = FALSE)$X1 |> tolower() |> unique()
}

# 5.2 Modals, hedges, reporting verbs (Norwegian example lists)
modal_terms   <- load_lex("modal_terms.txt")
hedge_single  <- load_lex("hedge_single.txt")
hedge_multi   <- load_lex("hedge_multi.txt")
report_single <- load_lex("report_single.txt")
report_multi  <- load_lex("report_multi.txt")

# 5.3 Discourse connectives (argumentative)
disc_single <- load_lex("discourse_connectives_single.txt")
disc_multi  <- load_lex("discourse_connectives_multi.txt")

# 5.4 Narrativity markers
narr_single <- load_lex("narrativity_single.txt")
narr_multi  <- load_lex("narrativity_multi.txt")

# 5.5 Intensifiers + particles
intensifiers <- load_lex("intensifiers.txt")
particles    <- load_lex("particles.txt")

# 5.6 Temporal deixis (anchoring) + temporal sequencing (used for narrativity)
deixis_temp_single <- load_lex("deixis_temporal_single.txt")
deixis_temp_multi  <- load_lex("deixis_temporal_multi.txt")
temp_seq_single    <- load_lex("temporal_seq_single.txt")
temp_seq_multi     <- load_lex("temporal_seq_multi.txt")

# --------------------------------------------
# 6. Corpus & tokenisation
# --------------------------------------------
cat("Building corpus and tokenising...\n")

corp <- quanteda::corpus(dat, text_field = "article_text")

quanteda::docvars(corp) <- dat |>
  dplyr::select(id, newsroom, type, url, headline, lead, body_text, genre)

toks_simple <- quanteda::tokens(
  corp, what = "word",
  remove_punct = TRUE, remove_symbols = TRUE
) |>
  quanteda::tokens_tolower()

# Compound MWEs (convert spaces to underscores before compounding)
phr_hedge       <- quanteda::phrase(gsub(" ", "_", hedge_multi))
phr_report      <- quanteda::phrase(gsub(" ", "_", report_multi))
phr_disc        <- quanteda::phrase(gsub(" ", "_", disc_multi))
phr_narr        <- quanteda::phrase(gsub(" ", "_", narr_multi))
phr_temp_seq    <- quanteda::phrase(gsub(" ", "_", temp_seq_multi))
phr_temp_deixis <- quanteda::phrase(gsub(" ", "_", deixis_temp_multi))

cat("Compounding multiword expressions...\n")
toks_mwe <- toks_simple |>
  quanteda::tokens_compound(phr_hedge) |>
  quanteda::tokens_compound(phr_report) |>
  quanteda::tokens_compound(phr_disc) |>
  quanteda::tokens_compound(phr_narr) |>
  quanteda::tokens_compound(phr_temp_seq) |>
  quanteda::tokens_compound(phr_temp_deixis)

cat("Building document-feature matrices...\n")
dfm_base  <- quanteda::dfm(toks_mwe)
dfm_words <- quanteda::dfm(toks_simple)

tok_n        <- as.numeric(quanteda::ntoken(dfm_base))
tok_n_simple <- as.numeric(quanteda::ntoken(dfm_words))

# --------------------------------------------
# 7. Epistemic / genre features
# --------------------------------------------
dict_modal  <- quanteda::dictionary(list(modal = modal_terms))
dict_hedge  <- quanteda::dictionary(list(hedge = c(hedge_single, gsub(" ", "_", hedge_multi))))
dict_report <- quanteda::dictionary(list(reporting = c(report_single, gsub(" ", "_", report_multi))))

cat("Computing modal / hedge / reporting features...\n")
dfm_modal <- quanteda::dfm_lookup(dfm_base, dict_modal,  valuetype = "fixed")
dfm_hedge <- quanteda::dfm_lookup(dfm_base, dict_hedge,  valuetype = "fixed")
dfm_rept  <- quanteda::dfm_lookup(dfm_base, dict_report, valuetype = "fixed")

feat_modal  <- dfm_to_counts(dfm_modal, "modal",     tok_n)
feat_hedge  <- dfm_to_counts(dfm_hedge, "hedge",     tok_n)
feat_report <- dfm_to_counts(dfm_rept,  "reporting", tok_n)

# Short sentence ratio
txts  <- as.character(corp)
n_txt <- length(txts)
short_sent_ratio <- numeric(n_txt)

cat("Computing short sentence ratio...\n")
pb <- utils::txtProgressBar(min = 0, max = n_txt, style = 3)
for (i in seq_len(n_txt)) {
  s <- split_sentences(txts[i])
  if (!length(s)) {
    short_sent_ratio[i] <- 0
  } else {
    tok_per_sent <- stringr::str_count(s, "\\S+")
    short_sent_ratio[i] <- mean(tok_per_sent <= 15, na.rm = TRUE)
  }
  if (i %% 1000 == 0L || i == n_txt) utils::setTxtProgressBar(pb, i)
}
close(pb)

feat_short <- tibble::tibble(row = seq_len(n_txt), short_sent_ratio = short_sent_ratio)

# --------------------------------------------
# 8. Subjectivity / Emotionality (sentiment optional; Norwegian uses NorSentLex)
# --------------------------------------------
cat("Computing sentiment and emotionality features...\n")

if (length(lex_pos) && length(lex_neg)) {
  dict_sent <- quanteda::dictionary(list(positive = lex_pos, negative = lex_neg))
  dfm_sent  <- quanteda::dfm_lookup(dfm_words, dict_sent, valuetype = "fixed")
  mat_sent  <- as.matrix(dfm_sent)
  
  pos_counts <- as.numeric(if ("positive" %in% colnames(mat_sent)) mat_sent[,"positive"] else 0)
  neg_counts <- as.numeric(if ("negative" %in% colnames(mat_sent)) mat_sent[,"negative"] else 0)
  
  polarity_net <- (pos_counts - neg_counts) / pmax(tok_n_simple, 1)
  subj_total   <- (pos_counts + neg_counts) / pmax(tok_n_simple, 1)
} else {
  polarity_net <- rep(0, nrow(dat))
  subj_total   <- rep(0, nrow(dat))
}

feat_subj <- tibble::tibble(
  row          = seq_len(nrow(dat)),
  polarity_net = polarity_net,
  subjectivity = subj_total
)

# Intensifiers
dict_int <- quanteda::dictionary(list(intens = intensifiers))
dfm_int  <- quanteda::dfm_lookup(dfm_words, dict_int, valuetype = "fixed")
mat_int  <- as.matrix(dfm_int)
int_counts <- as.numeric(if ("intens" %in% colnames(mat_int)) mat_int[,"intens"] else 0)

int_rate <- 100 * int_counts / pmax(tok_n_simple, 1)
exclam_cnt  <- stringr::str_count(dat$article_text, "!")
exclam_rate <- 1000 * exclam_cnt / pmax(tok_n_simple, 1)

feat_emot <- tibble::tibble(
  row = seq_len(nrow(dat)),
  intensifier_per100 = int_rate,
  exclam_per1k       = exclam_rate
)

# --------------------------------------------
# 9. Colour / style features
# --------------------------------------------
cat("Computing lexical style / colour features...\n")

terms    <- quanteda::featnames(dfm_words)
adj_mask <- stringr::str_ends(terms, "(al|ous|ful|iv|isk|aktig|bar|leg)$")
adv_mask <- stringr::str_ends(terms, "(lig|vis)$")

dfm_adj <- dfm_words[, adj_mask, drop = FALSE]
dfm_adv <- dfm_words[, adv_mask, drop = FALSE]

adj_rate <- 100 * Matrix::rowSums(dfm_adj) / pmax(tok_n_simple, 1)
adv_rate <- 100 * Matrix::rowSums(dfm_adv) / pmax(tok_n_simple, 1)

hyphen_counts <- stringr::str_count(dat$article_text, "\\b[[:alpha:]]+-[[:alpha:]]+\\b")
hyphen_rate   <- 1000 * hyphen_counts / pmax(tok_n_simple, 1)

punc_counts <- stringr::str_count(dat$article_text, "(;|—|–|:|\\.\\.\\.)")
punc_rate   <- 1000 * punc_counts / pmax(tok_n_simple, 1)

# Collocation-based vividness (cached)
VIVID_CACHE_FILE <- file.path(LEX_DIR, "vivid_mwe_from_collocations.txt")
cat("Vividness MWEs: checking cache...\n")

if (file.exists(VIVID_CACHE_FILE)) {
  vivid <- readr::read_lines(VIVID_CACHE_FILE) |>
    stringr::str_trim() |>
    purrr::discard(~ .x == "" || stringr::str_starts(.x, "#")) |>
    unique()
  cat("Loaded", length(vivid), "vivid MWEs from cache.\n")
} else {
  cat("No cache found. Computing collocations (may take time)...\n")
  N_docs <- quanteda::ndoc(toks_simple)
  set.seed(1234)
  idx_sample <- if (N_docs > 20000) sample(N_docs, 20000) else seq_len(N_docs)
  coll_max_keep <- 20000
  
  vivid <- character(0)
  collocation_ok <- TRUE
  
  tryCatch({
    coll2 <- quanteda.textstats::textstat_collocations(toks_simple[idx_sample], size = 2, min_count = 20)
    coll3 <- quanteda.textstats::textstat_collocations(toks_simple[idx_sample], size = 3, min_count = 12)
    colls <- dplyr::bind_rows(coll2, coll3)
    rm(coll2, coll3); invisible(gc())
    
    if (!is.null(colls) && nrow(colls)) {
      colls <- colls |>
        dplyr::mutate(mwe = gsub(" ", "_", collocation)) |>
        dplyr::arrange(dplyr::desc(lambda))
      
      if (nrow(colls) > coll_max_keep) colls <- colls[seq_len(coll_max_keep), , drop = FALSE]
      
      thr_pmi <- stats::quantile(colls$lambda, 0.85, na.rm = TRUE)
      vivid   <- unique(colls$mwe[colls$lambda >= thr_pmi])
    }
  }, error = function(e) {
    message("Collocation computation failed: ", conditionMessage(e))
    message("Setting vivid_mwe_per100 to 0.")
    collocation_ok <<- FALSE
  })
  
  if (collocation_ok && length(vivid)) {
    readr::write_lines(vivid, VIVID_CACHE_FILE)
    cat("Saved", length(vivid), "vivid MWEs to cache:", VIVID_CACHE_FILE, "\n")
  } else {
    vivid <- character(0)
  }
}

if (length(vivid)) {
  toks_comp_vivid <- quanteda::tokens_compound(toks_simple, quanteda::phrase(vivid))
  dfm_comp_vivid  <- quanteda::dfm(toks_comp_vivid)
  vivid_cols <- if (quanteda::nfeat(dfm_comp_vivid))
    grepl("_", quanteda::featnames(dfm_comp_vivid), fixed = TRUE) else rep(FALSE, 0)
  
  vivid_rate <- if (any(vivid_cols)) {
    100 * Matrix::rowSums(dfm_comp_vivid[, vivid_cols, drop = FALSE]) /
      pmax(quanteda::ntoken(dfm_comp_vivid), 1)
  } else rep(0, nrow(dat))
} else {
  vivid_rate <- rep(0, nrow(dat))
}

# IDF + rare share
N_doc <- nrow(dfm_words)
df    <- quanteda::docfreq(dfm_words)
idf   <- log(N_doc / pmax(1, df))
idf_thresh <- stats::quantile(idf, 0.75, na.rm = TRUE)

tfidf   <- quanteda::dfm_tfidf(dfm_words, scheme_tf = "prop", scheme_df = "inverse", base = exp(1))
avg_idf <- Matrix::rowSums(tfidf)

bin_rare <- dfm_words
if (length(bin_rare@x)) bin_rare@x <- ifelse(idf[bin_rare@i + 1L] >= idf_thresh, 1, 0)
rare_share <- Matrix::rowSums(bin_rare) / pmax(tok_n_simple, 1)

# Particles (phrasal proxies)
dict_part <- quanteda::dictionary(list(particle = particles))
part_dfm  <- quanteda::dfm_lookup(dfm_words, dict_part, valuetype = "fixed")
mat_part  <- as.matrix(part_dfm)
part_counts <- as.numeric(if ("particle" %in% colnames(mat_part)) mat_part[,"particle"] else 0)
phrasal_per1k <- 1000 * part_counts / pmax(tok_n_simple, 1)

feat_flourish <- tibble::tibble(
  row = seq_len(nrow(dat)),
  adj_per100       = as.numeric(adj_rate),
  adv_per100       = as.numeric(adv_rate),
  hyphen_per1k     = as.numeric(hyphen_rate),
  rhet_punc_per1k  = as.numeric(punc_rate),
  vivid_mwe_per100 = as.numeric(vivid_rate),
  avg_idf          = as.numeric(avg_idf),
  rare_share       = as.numeric(rare_share),
  phrasal_per1k    = as.numeric(phrasal_per1k)
)

# --------------------------------------------
# 10. Temporal anchoring (deictic time)
# --------------------------------------------
cat("Computing temporal anchoring (deictic_time_per100)...\n")

dict_time <- quanteda::dictionary(list(
  time_u = c(deixis_temp_single, gsub(" ", "_", deixis_temp_multi))
))

dfm_time <- quanteda::dfm_lookup(dfm_base, dict_time, valuetype = "fixed")
mat_time <- as.matrix(dfm_time)
time_c <- as.numeric(if ("time_u" %in% colnames(mat_time)) mat_time[,"time_u"] else 0)

# extra regex boost for temporal MWEs (in case compounding missed some)
count_phrase <- function(txt, phrase) {
  patt <- paste0("\\b", gsub("\\s+", "\\\\s+", phrase), "\\b")
  stringr::str_count(txt, stringr::regex(patt, ignore_case = TRUE))
}
time_rx <- vapply(dat$article_text, function(x) {
  sum(vapply(deixis_temp_multi, function(p) count_phrase(x, p), integer(1L)))
}, integer(1L))

feat_time <- tibble::tibble(
  row = seq_len(nrow(dat)),
  deictic_time_per100 = 100 * (time_c + time_rx) / pmax(tok_n_simple, 1)
)

# --------------------------------------------
# 11. Argumentation & Narrativity densities
# --------------------------------------------
cat("Computing argumentation and narrativity features...\n")

dict_disc <- quanteda::dictionary(list(arg_conn = c(disc_single, gsub(" ", "_", disc_multi))))
dict_narr <- quanteda::dictionary(list(
  narr_mark = c(narr_single, gsub(" ", "_", narr_multi), temp_seq_single, gsub(" ", "_", temp_seq_multi))
))

dfm_disc <- quanteda::dfm_lookup(dfm_base, dict_disc, valuetype = "fixed")
dfm_narr <- quanteda::dfm_lookup(dfm_base, dict_narr, valuetype = "fixed")

disc_mat <- as.matrix(dfm_disc)
narr_mat <- as.matrix(dfm_narr)

disc_counts <- as.numeric(if ("arg_conn" %in% colnames(disc_mat)) disc_mat[,"arg_conn"] else 0)
narr_counts <- as.numeric(if ("narr_mark" %in% colnames(narr_mat)) narr_mat[,"narr_mark"] else 0)

feat_arg  <- tibble::tibble(row = seq_len(nrow(dat)), arg_conn_per100  = 100 * disc_counts / pmax(tok_n, 1))
feat_narr <- tibble::tibble(row = seq_len(nrow(dat)), narr_mark_per100 = 100 * narr_counts / pmax(tok_n, 1))

# --------------------------------------------
# 12. Assemble + colour_index
# --------------------------------------------
cat("Assembling feature table and computing colour_index...\n")

base_cols <- dat |>
  dplyr::transmute(
    row = dplyr::row_number(),
    id, newsroom, type, url, headline, lead, body_text, genre
  )

features <- base_cols |>
  dplyr::left_join(feat_modal,    by = "row") |>
  dplyr::left_join(feat_hedge,    by = "row") |>
  dplyr::left_join(feat_report,   by = "row") |>
  dplyr::left_join(feat_short,    by = "row") |>
  dplyr::left_join(feat_subj,     by = "row") |>
  dplyr::left_join(feat_emot,     by = "row") |>
  dplyr::left_join(feat_flourish, by = "row") |>
  dplyr::left_join(feat_time,     by = "row") |>
  dplyr::left_join(feat_arg,      by = "row") |>
  dplyr::left_join(feat_narr,     by = "row")

cols_ci <- c(
  "adj_per100","adv_per100","hyphen_per1k","rhet_punc_per1k",
  "vivid_mwe_per100","avg_idf","rare_share","phrasal_per1k",
  "intensifier_per100","exclam_per1k"
)

for (nm in intersect(cols_ci, names(features))) {
  features[[paste0("z_", nm)]] <- safe_z(features[[nm]])
}

zcols <- grep("^z_", names(features), value = TRUE)
features$colour_index <- if (length(zcols)) rowMeans(as.matrix(features[, zcols, drop = FALSE]), na.rm = TRUE) else NA_real_
features <- features |>
  dplyr::select(-dplyr::starts_with("z_"))

# --------------------------------------------
# 13. Final conventionality_df
# --------------------------------------------
conventionality_df <- features |>
  dplyr::select(
    id, newsroom, type, url, headline, lead, body_text, genre,
    # epistemic / genre
    modal_per100, hedge_per100, reporting_per100, short_sent_ratio,
    # subjectivity / emotionality (sentiment optional; Norwegian uses NorSentLex)
    polarity_net, subjectivity, intensifier_per100, exclam_per1k,
    # temporal anchoring
    deictic_time_per100,
    # colour/style
    adj_per100, adv_per100, hyphen_per1k, rhet_punc_per1k,
    vivid_mwe_per100, avg_idf, rare_share, phrasal_per1k,
    # discourse mode
    arg_conn_per100, narr_mark_per100,
    # composite style index
    colour_index
  )

cat("Done. conventionality_df has", nrow(conventionality_df), "rows.\n")
print(dplyr::glimpse(conventionality_df))

if (WRITE_CSV) {
  readr::write_csv(conventionality_df, "news_conventionality_features.csv")
  cat("Wrote: news_conventionality_features.csv\n")
}
