# A systematic review of Variational Autoencoders in Single-Cell RNA Sequencing Data

This repository contains the R scripts used to (1) merge and deduplicate the records retrieved from the searches, and (2) apply the automated screening criteria and produce the Excel templates used by the reviewers.

## Workflow overview

| Step | Script | What it does | Input | Output |
|------|--------|--------------|-------|--------|
| 1 | `merge/ieee_merge.R`, `merge/pubmed_merge.R`, `merge/scopus_merge.R`, `merge/wos_merge.R` | Per-database merge and deduplication | Search exports from each database (one file per query) | `<db>_12DEC2025_merged.rda` (+ `.csv`), `<db>_n_papers.rda` |
| 2 | `merge/combine_data.R` | Cross-database merge and deduplication | The four `*_12DEC2025_merged.rda` | `data_overview.Rda`, `data_overview.csv` |
| 3 | `make_review_templates.R` | Automated exclusion by document type, and creation of the full-text screening sheet: `review_eligibility.xlsx` (fields: document type, full-text availability, VAE method, application paper, main purpose, data type, language) | `data_overview.csv` | `data_overview_reviewer_eligibility.xlsx`, `review_eligibility.xlsx` |
| - | *Manual* | Full-text eligibility screening by reviewers | `review_eligibility.xlsx` | `review_eligibility_filled.xlsx` |
| 4 | `make_review_templates_from_review_eligibility.R` | Applies criteria E1-E8 (see Figure 1 in the paper) and creates the data-extraction sheets for the included papers: `review_methods.xlsx` (model design, evaluation, and challenges addressed) and `code_review.xlsx` (code availability, documentation, whether it runs, and quality) | `review_eligibility_filled.xlsx` | `review_methods.xlsx`, `code_review.xlsx` |

## Repository contents

```
README.md
merge/
  ieee_merge.R        Step 1 - IEEE Xplore
  pubmed_merge.R      Step 1 - PubMed
  scopus_merge.R      Step 1 - Scopus
  wos_merge.R         Step 1 - Web of Science
  combine_data.R      Step 2 - cross-database deduplication
make_review_templates.R                          Step 3
make_review_templates_from_review_eligibility.R  Step 4
```
