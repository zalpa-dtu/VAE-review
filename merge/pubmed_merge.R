library(dplyr)
library(readxl)
library(stringr)
library(purrr)

clean_string <- function(s) {
  s <- gsub("[\u2011\u2013\u2014\u2012\u2010\u002D]", " ", s) # change all types of dash to spaces
  s <- gsub("ö", "o", s)
  s <- gsub("ä", "a", s)
  s <- gsub("[[:space:]]+", " ", s) 
  s <- stringi::stri_replace_all_regex(s, "\\p{Zs}+", " ")  # Unicode spaces
  s <- gsub("[\u00A0\t\r\n]+", " ", s)  # NBSP, tab, newline
  s <- gsub(" +", " ", s)  # collapse multiple spaces
  trimws(s)                           # remove leading/trailing space
}

# This script reads the csv files from Scopus and combines them to avoid duplicates
setwd("/Users/zalpa/Desktop/new search - 12.12/Papers/pubmed")

queries = c('clustering','cellidentification','dimensionalityreduction','imputation','batchcorrection')

data_merged = data.frame('Pubmed.Id' = integer(),
                         'Title' = character(),
                         'Authors' = character(),
                         "Journal" = character(),
                         'Year' = integer(),
                         "DOI" = character(),
                         "query" = character(),
                         'Document.Type' = character())

n_papers_raw = c()

for (q in queries){
      
  file_path = paste0('pubmed_12_12_2025_',q,'.csv')
  if (file.exists(file_path)) {
    data = read.csv(file_path)
    
    # Remove unnecessary cols
    data$Create.Date = NULL
    data$PMCID = NULL
    data$NIHMS.ID = NULL
    data$First.Author = NULL
    
    # Add queries
    data$query = q
    
    # Save number of observations
    n_papers_raw[q] = nrow(data)
    
    # Remove complete duplicates in raw data
    dups_rawdata = duplicated(data)
    data = data[!dups_rawdata,]
    
    # Standardize title (lower letters and no comma, punctuation)
    # Standardize journal categories
    # Standardize document type
    data = data %>%
      mutate(Authors_ = clean_string(str_replace_all(str_to_lower(Authors), "[.,]", ""))) %>%
      mutate(Title_ = clean_string(str_replace_all(str_to_lower(Title), "[.,]",""))) %>%
      mutate(Journal_ = clean_string(str_replace_all(str_to_lower(Journal.Book), "[.,]",""))) %>%
      mutate(DOI_ = clean_string(str_to_lower(DOI))) %>%
      select(-c(Authors,Title,Journal.Book,DOI))
    
    # Create document type by Citations column
    data = data %>%
      mutate(Document.Type = ifelse(grepl("preprint", str_to_lower(Citation)),'preprint','')) %>%
      select(-Citation)
    
    
    # Hardcodings to correct spelling mistakes.
    # Hardcodings to simplify categories
    # Hardcodings to simplify journal
    data = data %>%
      mutate(Title__ =
               case_when(Title_ == 'buddi: bulkdeconvolution withdomaininvariance to predict cell type specific perturbations from bulk' ~ 'buddi: bulk deconvolution with domain invariance to predict cell type specific perturbations from bulk',
                         Title_ == 'single cell multi omic topic embedding reveals cell type specific and covid 19 severity related immune signatures' ~ 'single cell multi omics topic embedding reveals cell type specific and covid 19 severity related immune signatures',
                         Title_ == 'scnode : generative model for temporal single cell transcriptomic data prediction' ~ 'scnode: generative model for temporal single cell transcriptomic data prediction',
                         .default = Title_)) %>%
      mutate(Journal__ = case_when(Journal_ == 'brief bioinform' ~ 'briefings in bioinformatics',
                                   Journal_ == 'proc mach learn res' ~ 'proceedings of machine learning research',
                                   Journal_ == 'ieee trans comput biol bioinform' ~ 'ieee transactions on computational biology and bioinformatics',
                                   Journal_ == 'comput biol chem' ~ 'computational biology and chemistry',
                                   Journal_ == 'pac symp biocomput' ~ 'pacific symposium on biocomputing',
                                   Journal_ == 'biophys rev' ~ 'biophysical reviews',
                                   Journal_ == 'commun biol' ~ 'communications biology',
                                   Journal_ == 'bioinform biol insights' ~ 'bioinformatics and biology insights',
                                   Journal_ == 'int j mol sci' ~ 'international journal of molecular sciences',
                                   Journal_ == 'nat commun' ~ 'nature communications',
                                   Journal_ == 'plos comput biol' ~ 'plos computational biology',
                                   Journal_ == 'brief funct genomics' ~ 'briefings in functional genomics',
                                   Journal_ == 'sci rep' ~ 'scientific reports',
                                   Journal_ == 'nat biotechnol' ~ 'nature biotechnology',
                                   Journal_ == 'cell rep methods' ~ 'cell reports methods',
                                   Journal_ == 'nat methods' ~ 'nature methods',
                                   Journal_ == 'genome biol' ~ 'genome biology',
                                   Journal_ == 'ieee/acm trans comput biol bioinform' ~ 'ieee transactions on computational biology and bioinformatics',
                                   Journal_ == 'cell genom' ~ 'cell genomics',
                                   Journal_ == 'j comput biol' ~ 'journal of computational biology',
                                   Journal_ == 'eur j immunol' ~ 'european journal of immunology',
                                   Journal_ == 'front genet' ~ 'frontiers in genetics',
                                   Journal_ == 'genomics proteomics bioinformatics' ~ 'genomics proteomics and bioinformatics',
                                   Journal_ == 'cell syst' ~ 'cell systems',
                                   Journal_ == 'micromachines (basel)' ~ 'micromachines',
                                   Journal_ == 'bioinform adv' ~ 'bioinformatics advances',
                                   Journal_ == 'ieee j biomed health inform' ~ 'ieee journal of biomedical and health informatics',
                                   Journal_ == 'j adv res' ~ 'journal of advanced research',
                                   Journal_ == 'comput struct biotechnol j' ~ 'computational and structural biotechnology journal',
                                   Journal_ == 'nat comput sci' ~ 'nature computational science',
                                   Journal_ == 'nucleic acids res' ~ 'nucleic acids research',
                                   Journal_ == 'patterns (n y)' ~ 'patterns',
                                   Journal_ == 'bmc biol' ~ 'bmc biology',
                                   Journal_ == 'nat mach intell' ~ 'nature machine intelligence',
                                   Journal_ == 'proc natl acad sci u s a' ~ 'proceedings of the national academy of sciences of the united states of america',
                                   Journal_ == 'interdiscip sci' ~ 'interdisciplinary sciences computational life sciences',
                                   Journal_ == 'biodata min' ~ 'biodata mining',
                                   Journal_ == 'comput biol med' ~ 'computers in biology and medicine',
                                   Journal_ == 'j theor biol' ~ 'journal of theoretical biology',
                                   Journal_ == 'star protoc' ~ 'star protocols',
                                   Journal_ == 'mol syst biol' ~ 'molecular systems biology',
                                   Journal_ == 'phys life rev' ~ 'physics of life reviews',
                                   Journal_ == 'neural comput appl' ~ 'neural computing and applications',
                                   Journal_ == 'cytometry a' ~ 'cytometry part a',
                                   Journal_ == 'biology (basel)' ~ 'biology',
                                   Journal_ == 'bioengineering (basel)' ~ 'bioengineering',
                                   .default = Journal_)) #%>%
    #   mutate(Document.Type__ = case_when(Document.Type_ == 
    #                                      .default = Document.Type_)) %>%
    #   mutate(Authors__ =
    #            case_when(Authors_ == 
    #                      .default = Authors_))
    data = data %>% select(-c(Title_,Journal_))
    
    
    data_merged = full_join(data_merged,data,by=c('Authors' = 'Authors_',
                                                  'Title' = 'Title__',
                                                  'Journal' = 'Journal__',
                                                  'Document.Type',
                                                  "DOI" = 'DOI_',
                                                  'Year' = "Publication.Year",
                                                  'Pubmed.Id' = 'PMID'))
    
    data_merged <- data_merged %>%
      mutate(query = map2(query.x, query.y, ~ union(.x, .y) |> discard(is.na))) %>% # union = unique merge
      select(-query.x, -query.y)
    
  } else {
    print(paste0('File with query ',q,' does not exist.'))
  }
  
}

# Add search engine
data_merged$Search.Engine = 'pubmed'


# Some titles are duplicate where there is one unpublished from preprints and one from a journal.
# If a title is exactly the same, we assume it is the same authors. Authors are simply formatted differently 
# We will remove the ones from preprints and make a note


data_merged_v1 = data_merged %>%
  mutate(preprint_name = ifelse(Document.Type == 'preprint',Journal,NA)) %>%
  arrange(Title) %>%
  group_by(Title) %>%
  mutate(
    duplicate = n() > 1, # Ensure that there is a duplicate
    has_preprint = any(Document.Type == "preprint"),
    preprint_name_concat = list(unique(preprint_name[!is.na(preprint_name)])),
    n_duplicate_preprint = sum(Document.Type == 'preprint'),
    has_journal = any(Document.Type != "preprint"),
    merged_query_preprints = list({
      preprint_queries <- query[Document.Type == "preprint"]
      if (length(preprint_queries) == 0) character(0)
      else purrr::reduce(preprint_queries, union) |> discard(is.na)
    })
  ) %>%
  filter(
    (!duplicate) | (duplicate & Document.Type != "preprint") | (duplicate & !has_journal)
  ) %>%
  mutate(
    exist_preprint = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ 'Y',
      TRUE ~ NA_character_
    ),
    preprint = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ preprint_name_concat,
      TRUE ~ list(character(0))
    ),
    query = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ map2(query, merged_query_preprints, ~ union(.x, .y) |> discard(is.na)),
      TRUE ~ query
    )
  ) %>%
  ungroup() %>%
  select(-duplicate, -has_preprint, -preprint_name_concat, -n_duplicate_preprint, -preprint_name, -has_journal, -merged_query_preprints)

# !!!!!!! if the authors column is empty or NA and DOI column is empty or NA, then delete
data_merged_v1 <- data_merged_v1 %>%
  filter(!((Authors == "" | is.na(Authors)) & (DOI == "" | is.na(DOI))))

# Check if there are duplicates on title and author
duplicates_title_author = data_merged_v1 %>% arrange(Title,Authors)  %>% filter(duplicated(Title,Authors) | duplicated(Title,Authors, fromLast = TRUE))
duplicates_title = data_merged_v1 %>% arrange(Title) %>%  filter(duplicated(Title) | duplicated(Title, fromLast = TRUE))
duplicates_author = data_merged_v1 %>% arrange(Authors) %>%  filter(duplicated(Authors) | duplicated(Authors, fromLast = TRUE))

# Save merged reading list
saveRDS(data_merged_v1, file = 'pubmed_12DEC2025_merged.rda')

# Save number of observations
saveRDS(n_papers_raw, file = 'pubmed_n_papers.rda')

# Save as csv
data_merged_v1_csv <- data_merged_v1 %>%
  mutate(
    query = map_chr(query, ~ paste(.x, collapse = ", ")),
    preprint = map_chr(preprint, ~ paste(.x, collapse = ", "))
  )
write.csv(data_merged_v1_csv,'pubmed_12DEC2025_merged.csv',row.names=F)
