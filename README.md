# News Conventionality Index (NCI)

This repository contains R scripts for constructing a **News Conventionality Index (NCI)**: a composite measure of stylistic and epistemic conventionality in news texts.

The feature extraction logic is **language-agnostic**, but several features depend on **language-specific lexicons** (e.g., hedges, discourse connectives, temporal deictics, sentiment terms). The repository includes a Norwegian-oriented lexicon configuration as an example implementation.


---

## Conceptual framework

The NCI consists of five theoretically defined dimensions:

1. **Epistemic conventionality**
2. **Subjectivity and evaluative stance**
3. **Stylistic and emotional intensity**
4. **Temporal anchoring**
5. **Discourse mode**

### Unweighted construction

- All component features within each dimension are combined using an **unweighted mean** (after z-standardisation).
- The five dimension scores are combined using an **unweighted additive formula**.
- The final index is z-standardised to produce `NCI_z`.

Higher NCI values indicate stronger alignment with conventional hard-news style.

---

## Repository structure

- `News_Conv_Features.R`  
  Extracts linguistic feature variables from a text corpus and creates `conventionality_df`.

- `News_conv_index_compilation.R`  
  Builds on `conventionality_df`, z-standardises component features, computes dimension scores, and computes:
  - `epistemic_score`
  - `subjectivity_score`
  - `style_score`
  - `temporal_anchoring_score`
  - `discourse_score`
  - `NCI`
  - `NCI_z`

- `lexicons/`  
  Contains all lexicon files required by the feature extraction script.

---

## Input data requirements

The feature extraction script expects an object named `text_corpus_df` in the R environment.

### Required columns

- `id` (unique identifier)
- `headline`
- `lead`
- `body_text`

These text fields may be `NA`; missing values are treated as empty strings.

### Optional metadata columns (carried through if present)

- `newsroom`
- `type`
- `url`
- `genre`

If these columns are missing, they are created as `NA` and the script will still run.

---

## Lexicons

All lexicons are expected directly under:

```
lexicons/
```

Each lexicon file should contain one lexical item per line. Blank lines and lines beginning with `#` are ignored.

The repository currently includes a Norwegian-oriented lexicon configuration as an example. To apply the NCI to another language:

1. Replace the lexicon files in `lexicons/` with equivalent lists for the target language.
2. Keep the same filenames (so the scripts can locate them).
3. Adjust any language-specific heuristics in the feature script if necessary (e.g., suffix-based adjective/adverb detection).

The NCI framework itself is language-agnostic; only the lexicons are language-specific.

---

## NorSentLex (Norwegian sentiment lexicons)

The Norwegian implementation optionally uses fullform sentiment lexicons from **NorSentLex**:

https://github.com/ltgoslo/norsentlex

Important:

- NorSentLex is **Norwegian-specific**.
- The NorSentLex fullform lexicon files are **not included** in this repository.
- If they are not present locally, sentiment-based features (`polarity_net`, `subjectivity`) are set to 0 and the script will issue a warning.
- For other languages, you must supply or construct an appropriate sentiment lexicon, or omit sentiment-based features.

If using NorSentLex fullform lexicons, the script expects the following files in:

```
lexicons/
```

- `Fullform_Positive_lexicon.txt`
- `Fullform_Negative_lexicon.txt`

Please cite NorSentLex when using those resources.

---

## Typical workflow

1) Load your dataset into the R environment as `text_corpus_df`.

2) Extract features:

```r
source("News_Conv_Features.R")
```

This creates:

```
conventionality_df
```

3) Compile the index:

```r
source("News_conv_index_compilation.R")
```

This adds:

```
epistemic_score
subjectivity_score
style_score
temporal_anchoring_score
discourse_score
NCI
NCI_z
```

---

## NCI computation (sign convention)

The final NCI is computed as:

+ epistemic_score  
+ temporal_anchoring_score  
− subjectivity_score  
− style_score  
− discourse_score  

All component features and dimensions are combined using unweighted means.

---

## Collocation-based vividness list (generated per corpus)

The feature `vivid_mwe_per100` relies on a list of high-PMI multiword expressions that is **built from the corpus being analysed**. On the first run, `News_Conv_Features.R` identifies these collocations and caches them as:

```
lexicons/vivid_mwe_from_collocations.txt
```

This file is not included in the repository, as it is corpus-specific. Later runs reuse the cached list. **Delete the file before running the scripts on a different corpus**, so that the list is rebuilt from the new texts.

---

## Citation

If you use or adapt these scripts, please cite:

Steensen, S., Olsen, G. R., Kavtaradze, L., & Kalsnes, B. (2026). How Newsy is News? Development and Initial Validation of a Computational News Conventionality Index. *Journalism Studies*. https://doi.org/10.1080/1461670X.2026.2764924

---

## License

The scripts and lexicon files in this repository are released under the MIT License. See [`LICENSE`](LICENSE) for details.

The NorSentLex sentiment lexicons are not part of this repository and are subject to their own license terms.
