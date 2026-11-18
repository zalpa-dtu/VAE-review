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
setwd("/Users/zalpa/Desktop/new search - 12.12/Papers/webofscience")

queries = c('clustering','cellidentification','dimensionalityreduction','imputation','batchcorrection')

n_papers_raw = c()

data_merged = data.frame('Authors' = character(),
                         'Title' = character(),
                         'Journal' = character(),
                         "DOI" = character(),
                         "Document.Type" = character(),
                         "Year" = integer(),
                         'Pubmed.Id' = integer(),
                         'query' = character())

for (q in queries){
      
  file_path = paste0('webofscience_12_12_2025_',q,'.xls')
  if (file.exists(file_path)) {
    # Load workbook
    data = read_excel(file_path)
    data = data.frame(data)
    
    # Excel read blank as NA
    data[is.na(data)] <- ""
    
    # Ensure correct formatting of integer/non-integer
    if (typeof(data$DOI) != 'character'){
      data$DOI = ifelse(is.na(data$DOI), "", as.character(data$DOI))
    }
    if (typeof(data$Pubmed.Id) != 'integer'){
      data$Pubmed.Id = as.integer(data$Pubmed.Id)
    }
    if (typeof(data$Publication.Year) != 'integer'){
      data$Publication.Year = as.integer(data$Publication.Year)
    }
    
    
    # Remove unnecessary cols
    data[,c(1,3,4,5,6,7,8,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,48,49,50,51,52,53,54,55,56,58,59,60,61,62,63,64,65,67,68,69,70,72)] = NULL
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
      mutate(Title_ = clean_string(str_replace_all(str_to_lower(Article.Title), "[.,]",""))) %>%
      mutate(Journal_ = clean_string(str_replace_all(str_to_lower(Source.Title), "[.,]",""))) %>%
      mutate(Document.Type_ = clean_string(str_to_lower(Document.Type))) %>%
      mutate(DOI_ = ifelse(is.na(DOI),'',clean_string(str_to_lower(DOI)))) %>%
      select(-c(Authors,Article.Title,Source.Title,Document.Type,DOI))
    
    
    # Hardcodings to correct spelling mistakes.
    # Hardcodings to simplify categories
    # Hardcodings to simplify journal
    data = data %>%
      mutate(Title__ =
               case_when(Title_ == 'singlecell multiomic topic embedding reveals celltypespecific and covid19 severityrelated immune signatures' ~ 'singlecell multiomics topic embedding reveals celltypespecific and covid19 severityrelated immune signatures',
                         Title_ == 'buddi: bulkdeconvolution withdomaininvariance to predict cell type specific perturbations from bulk' ~ 'buddi: bulk deconvolution with domain invariance to predict cell type specific perturbations from bulk',
                         Title_ == 'single cell multi omic topic embedding reveals cell type specific and covid 19 severity related immune signatures' ~ 'single cell multi omics topic embedding reveals cell type specific and covid 19 severity related immune signatures',
                         Title_ == 'the deep generative decoder: map estimation of representations improves modeling of single cell rna data' ~ 'the deep generative decoder: map estimation of representations improves modelling of single cell rna data',
                         Title_ == 'a deep generative model for estimating single cell rna splicing and degradation rates' ~ 'deepkinet: a deep generative model for estimating single cell rna splicing and degradation rates',
                         Title_ == 'jamie: joint variational autoencoders for multi modal imputation and embedding' ~ 'joint variational autoencoders for multimodal imputation and embedding',
                         Title_ == 'mofa plus : a statistical framework for comprehensive integration of multi modal single cell data' ~ 'mofa+: a statistical framework for comprehensive integration of multi modal single cell data',
                         Title_ == 'abstract 1932: pollock: fishing for cell states' ~ 'pollock: fishing for cell states',
                         Title_ == 'abstract 349: gene regulatory modules regulate cardiac lineage specification revealed by single cell rna sequencing' ~ 'gene regulatory modules regulate cardiac lineage specification revealed by single cell rna sequencing',
                         Title_ == 'mixture representation learning with coupled autoencoding agents' ~ 'mixture representation learning with coupled autoencoders',
                         Title_ == 'learning interpretable cellular embedding for inferring biological mechanisms underlying single cell transcriptomics' ~ 'igtp: learning interpretable cellular embedding for inferring biological mechanisms underlying single cell transcriptomics',
                         Title_ == 'calibrated identification of feature dependencies in single cell multiomics' ~ 'vi vs: calibrated identification of feature dependencies in single cell multiomics',
                         .default = Title_)) %>%
      mutate(Journal__ = case_when(Journal_ == 'biorxiv : the preprint server for biology' ~ 'biorxiv',
                                   Journal_ == 'medrxiv : the preprint server for health sciences' ~ 'medrxiv',
                                   Journal_ == 'pacific symposium on biocomputing. pacific symposium on biocomputing' ~ 'pacific symposium on biocomputing',
                                   Journal_ == 'pacific symposium on biocomputing 2019' ~ 'pacific symposium on biocomputing',
                                   Journal_ == 'genomics proteomics & bioinformatics' ~ 'genomics proteomics and bioinformatics',
                                   Journal_ == 'ieee-acm transactions on computational biology and bioinformatics' ~ 'ieee transactions on computational biology and bioinformatics',
                                   Journal_ == 'interdisciplinary sciences-computational life sciences' ~ 'interdisciplinary sciences - computational life sciences',
                                   Journal_ == 'neural computing & applications' ~ 'neural computing and applications',
                                   Journal_ == 'biology basel' ~ 'biology',
                                   Journal_ == 'bioengineering basel' ~ 'bioengineering',
                                   Journal_ == 'ieee acm transactions on computational biology and bioinformatics' ~ 'ieee transactions on computational biology and bioinformatics',
                                   Journal_ == 'pacific symposium on biocomputing pacific symposium on biocomputing' ~ 'pacific symposium on biocomputing',
                                   Journal_ == 'machine learning science and technology' ~ 'machine learning: science and technology',
                                   Journal_ == '2024 ieee conference on artificial intelligence cai 2024' ~ '2024 ieee conference on artificial intelligence (cai)',
                                   Journal_ == '32nd ieee signal processing and communications applications conference siu 2024' ~ '2024 32nd signal processing and communications applications conference (siu)',
                                   Journal_ == '2023 ieee/cvf international conference on computer vision workshops iccvw' ~ '2023 ieee/cvf international conference on computer vision workshops (iccvw)',
                                   Journal_ == 'advances in neural information processing systems 35 neurips 2022' ~ 'advances in neural information processing systems',
                                   Journal_ == '2012 ieee 51st annual conference on decision and control (cdc)' ~ 'ieee conference on decision and control',
                                   Journal_ == '2024 international joint conference on neural networks ijcnn 2024' ~ 'international joint conference on neural networks',
                                   Journal_ == 'advances in neural information processing systems 36 (neurips 2023)' ~ 'advances in neural information processing systems',
                                   Journal_ == 'thirty ninth aaai conference on artificial intelligence aaai 25 vol 39 no 15' ~ 'aaai conference on artificial intelligence',
                                   .default = Journal_)) %>%
      mutate(Document.Type__ = case_when(Document.Type_ == 'journal article; preprint' ~ 'preprint',
                                         Document.Type_ == 'preprint; journal article' ~ 'preprint',
                                         Document.Type_ == 'article; early access' ~ 'article',
                                         Document.Type_ == 'journal article' ~ 'article',
                                         Document.Type_ == 'journal article; review; research support, non u.s. gov\'t' ~ 'review',
                                         Document.Type_ == 'journal article; research support, non u.s. gov\'t' ~ 'article',
                                         Document.Type_ == 'journal article; research support, n.i.h., extramural; research support, non u.s. gov\'t' ~ 'article',
                                         Document.Type_ == 'journal article; research support, n.i.h., extramural' ~ 'article',
                                         Document.Type_ == 'article; proceedings paper' ~ 'article',
                                         .default = Document.Type_)) %>%
      mutate(Authors__ =
               case_when(Authors_ == 'zhou, manqi; zhang, hao; baii, zilong; mann-krzisnik, dylan; wang, fei; li, yue' ~ 'zhou, manqi; zhang, hao; bai, zilong; mann-krzisnik, dylan; wang, fei; li, yue',
                         Authors_ == 'tu, xinming; hutter, jan-christian; jerrywang, zitong; kudo, takamasa; regev, aviv; lopez, romain' ~ 'tu, xinming; hutter, jan-christian; wang, zitong jerry; kudo, takamasa; regev, aviv; lopez, romain',
                         .default = Authors_))
    data = data %>% select(-c(Journal_,Document.Type_,Authors_,Title_))
    
    
    data_merged = full_join(data_merged,data,by=c('Authors' = 'Authors__',
                                                  'Title' = 'Title__',
                                                  'Journal' = 'Journal__',
                                                  "DOI" = 'DOI_',
                                                  "Document.Type" = 'Document.Type__',
                                                  'Year' = "Publication.Year",
                                                  'Pubmed.Id'))
    
    data_merged <- data_merged %>%
      mutate(query = map2(query.x, query.y, ~ union(.x, .y) |> discard(is.na))) %>% # union = unique merge
      select(-query.x, -query.y)
    
  } else {
    print(paste0('File with query ',q,' does not exist.'))
  }
  
}

# Remove data types
# data_merged = data_merged %>%
#   filter(!(Document.Type == 'awarded grant' & is.na(Authors)) &
#            !(Document.Type == 'data set') & 
#            !(Document.Type == 'data study') & 
#            !(Document.Type == 'dissertation/thesis') & 
#            !(Document.Type == 'meeting abstract') & 
#            !(Document.Type == 'software') &
#            !(Document.Type == 'data study' & is.na(Authors)))


# Remove paper without title
data_merged = data_merged %>%
  filter(!is.na(Title))



# Add search engine
data_merged$Search.Engine = 'web_of_science'



# Some titles are duplicate where there is one unpublished from preprints and one from a journal.
# If a title is exactly the same, we assume it is the same authors. Authors are simply formatted differently 
# We will remove the ones from preprints and make a note


data_merged_v1 = data_merged %>%
  mutate(preprint_name = ifelse(Document.Type == 'preprint',Journal,NA)) %>%
  mutate(proceeding_name = ifelse(Document.Type == 'proceedings paper',Journal,NA)) %>%
  arrange(Title) %>%
  group_by(Title) %>%
  mutate(
    duplicate = n() > 1, # Ensure that there is a duplicate
    has_preprint = any(Document.Type == "preprint"),
    has_proceeding = any(Document.Type == "proceedings paper"),
    preprint_name_concat = list(unique(preprint_name[!is.na(preprint_name)])),
    proceeding_name_concat = list(unique(proceeding_name[!is.na(proceeding_name)])),
    has_journal = any(Document.Type == "article"),
    has_non_preprint = any(Document.Type != "preprint"),
    merged_query_preprints = list({
      preprint_queries <- query[Document.Type == "preprint"]
      if (length(preprint_queries) == 0) character(0)
      else purrr::reduce(preprint_queries, union) |> discard(is.na)
    }),
    merged_query_proceeding = list({
      proceeding_queries <- query[Document.Type == "proceedings paper"]
      if (length(proceeding_queries) == 0) character(0)
      else purrr::reduce(proceeding_queries, union) |> discard(is.na)
    })
  ) %>%
  filter(
    !((duplicate & has_non_preprint & Document.Type == 'preprint') | 
        (duplicate & has_journal & Document.Type == 'proceedings paper'))
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
    exist_proceeding = case_when(
      duplicate & has_proceeding & Document.Type != "proceedings paper" ~ 'Y',
      TRUE ~ NA_character_
    ),
    proceeding = case_when(
      duplicate & has_proceeding & Document.Type != "proceedings paper" ~ proceeding_name_concat,
      TRUE ~ list(character(0))
    ),
    query = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ map2(query, merged_query_preprints, ~ union(.x, .y) |> discard(is.na)),
      TRUE ~ query
    ),
    query = case_when(
      duplicate & has_proceeding & Document.Type != "proceedings paper" ~ map2(query, merged_query_proceeding, ~ union(.x, .y) |> discard(is.na)),
      TRUE ~ query
    )
  ) %>%
  ungroup() %>%
  select(-duplicate, -has_preprint, -has_proceeding, -preprint_name_concat, -proceeding_name_concat, -preprint_name, -proceeding_name, -has_journal, -has_non_preprint, -merged_query_preprints, -merged_query_proceeding)


# !!!!!!! if the authors column is empty or NA and DOI column is empty or NA, then delete
data_merged_v1 <- data_merged_v1 %>%
  filter(!((Authors == "" | is.na(Authors)) & (DOI == "" | is.na(DOI))))

# Check if there are duplicates on title and author
duplicates_title_author = data_merged_v1 %>% arrange(Title,Authors)  %>% filter(duplicated(Title,Authors) | duplicated(Title,Authors, fromLast = TRUE))
duplicates_title = data_merged_v1 %>% arrange(Title) %>%  filter(duplicated(Title) | duplicated(Title, fromLast = TRUE))
duplicates_author = data_merged_v1 %>% arrange(Authors) %>%  filter(duplicated(Authors) | duplicated(Authors, fromLast = TRUE))



# Save merged reading list
saveRDS(data_merged_v1, file = 'wos_12DEC2025_merged.rda')

# Save number of observations
saveRDS(n_papers_raw, file = 'wos_n_papers.rda')

# Save as csv
data_merged_v1_csv <- data_merged_v1 %>%
  mutate(
    query = map_chr(query, ~ paste(.x, collapse = ", ")),
    preprint = map_chr(preprint, ~ paste(.x, collapse = ", ")),
    proceeding = map_chr(proceeding, ~ paste(.x, collapse = ", "))
  )
write.csv(data_merged_v1_csv,'wos_12DEC2025_merged.csv',row.names=F)

