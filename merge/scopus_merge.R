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
setwd("/Users/zalpa/Desktop/new search - 12.12/Papers/scopus")

queries = c('clustering','cellidentification','dimensionalityreduction','imputation','batchcorrection')

n_papers_raw = c()

data_merged = data.frame('Pubmed.Id' = integer(),
                         'Authors' = character(),
                         'Title' = character(),
                         'Year' = integer(),
                         'Journal' = character(),
                         "DOI" = character(),
                         "Document.Type" = character(),
                         "Search.Engine" = character(),
                         "query" = character())

for (q in queries){
      
  file_path = paste0('scopus_12_12_2025_',q,'.csv')
  if (file.exists(file_path)) {
    data = read.csv(file_path)
    
    # Ensure correct formatting of Journal
    if (typeof(data$Source.title) != 'character'){
      data$Source.title = ifelse(is.na(data$Source.title), "", as.character(data$Source.title))
    }
    
    # Remove unnecessary cols
    data$Author.full.names = NULL
    data$Art..No. = NULL
    data$Page.start = NULL
    data$Page.end = NULL
    data$Page.count = NULL
    data$Volume = NULL
    data$Issue = NULL
    data$Open.Access = NULL
    data$EID = NULL
    data$Abstract = NULL
    data$Author.Keywords = NULL
    data$Index.Keywords = NULL
    data$Author.s..ID = NULL
    data$Publication.Stage = NULL
    data$Link = NULL
    data$Cited.by = NULL
    
    
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
      mutate(Journal_ = clean_string(str_replace_all(str_to_lower(Source.title), "[.,]",""))) %>%
      mutate(Document.Type_ = clean_string(str_to_lower(Document.Type))) %>%
      mutate(Source_ = clean_string(str_to_lower(Source))) %>%
      mutate(DOI_ = clean_string(str_to_lower(DOI))) %>%
      select(-c(Authors,Title,Source.title,Document.Type,Source,DOI))
    
    
    # Hardcodings to correct spelling mistakes.
    # Hardcodings to simplify categories
    # Hardcodings to simplify journal
    data = data %>%
      mutate(Title__ =
               case_when(Title_ == 'a deep embedded clustering method based on βvariational autoencoder for single cell rna sequencing data' ~ 'a deep embedded clustering method based on beta variational autoencoder for single cell rna sequencing data',
                         Title_ == 'mixture of experts variational autoencoder for clustering and generating from similaritybased representations on single cell data andreas kopfid' ~ 'mixture of experts variational autoencoder for clustering and generating from similarity based representations on single cell data',
                         Title_ == 'comparative analysis of fusion techniques for integrating single cell multiomics datasets; tek hücre çoklu omik veri kümelerini bütünleştirmede füzyon tekniklerinin karşılaştırmalı analizi' ~ 'comparative analysis of fusion techniques for integrating single cell multiomics datasets',
                         .default = Title_)) %>%
      mutate(Journal__ = case_when(Journal_ == 'ieee/acm transactions on computational biology and bioinformatics' ~ 'ieee transactions on computational biology and bioinformatics',
                                   Journal_ == 'proceedings of the ieee conference on decision and control' ~ 'ieee conference on decision and control',
                                   Journal_ == 'proceedings of the international joint conference on neural networks' ~ 'international joint conference on neural networks',
                                   Journal_ == 'proceedings of the aaai conference on artificial intelligence' ~ 'aaai conference on artificial intelligence',
                                   .default = Journal_)) %>%
      mutate(Document.Type__ = case_when(Document.Type_ == 'conference paper' ~ 'proceedings paper',
                                         .default = Document.Type_)) %>%
      mutate(Authors__ =
               case_when(Authors_ == 'v fortuin vincent; vr somnath vignesh ram; m claassen manfred' ~ 'a kopf andreas; v fortuin vincent; vr somnath vignesh ram; m claassen manfred',
                         Authors_ == 'wang h y; zhao j p; zheng c h; su y s' ~ 'wang h; zhao j; zheng c; su y s',
                         .default = Authors_))
    data = data %>% select(-c(Title_,Journal_,Authors_,Document.Type_))
    
    
    data_merged = full_join(data_merged,data,by=c('Authors' = 'Authors__',
                                                  'Title' = 'Title__',
                                                  'Journal' = 'Journal__',
                                                  "DOI" = 'DOI_',
                                                  "Document.Type" = 'Document.Type__',
                                                  'Year',
                                                  "Search.Engine" = "Source_",
                                                  'Pubmed.Id' = 'PubMed.ID'))
    
    data_merged <- data_merged %>%
      mutate(query = map2(query.x, query.y, ~ union(.x, .y) |> discard(is.na))) %>% # union = unique merge
      select(-query.x, -query.y)
    
    
  } else {
    print(paste0('File with query ',q,' does not exist.'))
  }
  
}


# !!!!!!! if the authors column is empty or NA and DOI column is empty or NA, then delete
data_merged <- data_merged %>%
  filter(!((Authors == "" | is.na(Authors)) & (DOI == "" | is.na(DOI)) &
             (
               Title == "10th international conference on computational advances in bio and medical sciences iccabs 2020" |
                 Title == "14th international conference on bioinformatics and computational biology bicob 2022"
             )
  ))

# Check if there are duplicates on title and author
duplicates_title_author = data_merged %>% arrange(Title,Authors)  %>% filter(duplicated(Title,Authors) | duplicated(Title,Authors, fromLast = TRUE))
duplicates_title = data_merged %>% arrange(Title) %>%  filter(duplicated(Title) | duplicated(Title, fromLast = TRUE))
duplicates_author = data_merged %>% arrange(Authors) %>%  filter(duplicated(Authors) | duplicated(Authors, fromLast = TRUE))



# Save merged reading list
saveRDS(data_merged, file = 'scopus_12DEC2025_merged.rda')
#write.csv(data_merged_v1,'scopus_19092025_merged.csv',row.names=F)

# Save number of observations
saveRDS(n_papers_raw, file = 'scopus_n_papers.rda')

# Save as csv
data_merged_csv <- data_merged %>%
  mutate(
    query = map_chr(query, ~ paste(.x, collapse = ", "))
  )
write.csv(data_merged_csv,'scopus_12DEC2025_merged.csv',row.names=F)

