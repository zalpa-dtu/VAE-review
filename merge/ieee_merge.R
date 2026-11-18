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
setwd("/Users/zalpa/Desktop/new search - 12.12/Papers/ieee")

queries = c('clustering','cellidentification','dimensionalityreduction','imputation','batchcorrection')

n_papers_raw = c()

data_merged = data.frame('Title' = character(),
                         'Authors' = character(),
                         'Journal' = character(),
                         'Year' = integer(),
                         "DOI" = character(),
                         "Document.Type" = character(),
                         "query" = character())

for (q in queries){
      
  file_path = paste0('ieee_12_12_2025_',q,'.csv')
  if (file.exists(file_path)) {
    data = read.csv(file_path)
    
    
    # Remove unnecessary cols
    data$Date.Added.To.Xplore = NULL
    data$Author.Affiliations = NULL
    data$Volume = NULL
    data$Issue = NULL
    data$ISSN = NULL
    data$ISBNs = NULL
    data$Start.Page = NULL
    data$End.Page = NULL
    data$Abstract = NULL
    data$Funding.Information = NULL
    data$PDF.Link = NULL
    data$IEEE.Terms = NULL
    data$Mesh_Terms = NULL
    data$Patent.Citation.Count = NULL
    data$License = NULL
    data$Online.Date = NULL
    data$Issue.Date = NULL
    data$Meeting.Date = NULL
    data$Author.Keywords = NULL
    data$Reference.Count = NULL
    data$Publisher = NULL
    data$Article.Citation.Count = NULL
    
    
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
      mutate(Title_ = clean_string(str_replace_all(str_to_lower(Document.Title), "[.,]",""))) %>%
      mutate(Journal_ = clean_string(str_replace_all(str_to_lower(Publication.Title), "[.,]",""))) %>%
      mutate(Document.Type_ = clean_string(str_to_lower(Document.Identifier))) %>%
      mutate(DOI_ = clean_string(str_to_lower(DOI))) %>%
      select(-c(Authors,Document.Title,Publication.Title,Document.Identifier,DOI))
    
    
    # Hardcodings to correct spelling mistakes.
    # Hardcodings to simplify categories
    # Hardcodings to simplify journal
    data = data %>%
      mutate(Title__ =
               case_when(Title_ == 'a deep embedded clustering method based on $\\beta$ variational autoencoder for single cell rna sequencing data' ~ 'a deep embedded clustering method based on beta variational autoencoder for single cell rna sequencing data',
                         .default = Title_)) %>%
      mutate(Journal__ = case_when(Journal_ == 'ieee/acm transactions on computational biology and bioinformatics' ~ 'ieee transactions on computational biology and bioinformatics',
                                   Journal_ == '2020 ieee 20th international conference on bioinformatics and bioengineering (bibe)' ~ '2020 ieee 20th international conference on bioinformatics and bioengineering (bibe 2020)',
                                   Journal_ == '2012 ieee 51st ieee conference on decision and control (cdc)' ~ 'ieee conference on decision and control',
                                   Journal_ == '2024 international joint conference on neural networks (ijcnn)' ~ 'international joint conference on neural networks',
                                   .default = Journal_)) %>%
      mutate(Document.Type__ = case_when(Document.Type_ == 'ieee conferences' ~ 'proceedings paper',
                                         Document.Type_ == 'ieee journals' ~ 'article',
                                         Document.Type_ == 'ieee early access articles' ~ 'article', # these articles are accepted and peer-reviewed, but might miss editing
                                         .default = Document.Type_)) #%>%
    #   mutate(Authors__ =
    #            case_when(Authors_ == 
    #                        .default = Authors_))
    data = data %>% select(-c(Document.Type_,Title_,Journal_))
    
    
    data_merged = full_join(data_merged,data,by=c('Authors' = 'Authors_',
                                                  'Title' = 'Title__',
                                                  'Journal' = 'Journal__',
                                                  "DOI" = 'DOI_',
                                                  "Document.Type" = 'Document.Type__',
                                                  'Year' = "Publication.Year"))
    
    data_merged <- data_merged %>%
      mutate(query = map2(query.x, query.y, ~ union(.x, .y) |> discard(is.na))) %>% # union = unique merge
      select(-query.x, -query.y)
    
  } else {
    print(paste0('File with query ',q,' does not exist.'))
  }
  
}


# Add search engine
data_merged$Search.Engine = 'ieee'

# !!!!!!! if the authors column is empty or NA and DOI column is empty or NA, then delete
data_merged <- data_merged %>%
  filter(!((Authors == "" | is.na(Authors)) & (DOI == "" | is.na(DOI))))

# Check if there are duplicates on title and author
duplicates_title_author = data_merged %>% arrange(Title,Authors)  %>% filter(duplicated(Title,Authors) | duplicated(Title,Authors, fromLast = TRUE))
duplicates_title = data_merged %>% arrange(Title) %>%  filter(duplicated(Title) | duplicated(Title, fromLast = TRUE))
duplicates_author = data_merged %>% arrange(Authors) %>%  filter(duplicated(Authors) | duplicated(Authors, fromLast = TRUE))

# Save merged reading list
saveRDS(data_merged, file = 'ieee_12DEC2025_merged.rda')
#write.csv(data_merged,'ieee_19092025_merged.csv',row.names=F)

# Save number of observations
saveRDS(n_papers_raw, file = 'ieee_n_papers.rda')

# Save as csv
data_merged_csv <- data_merged %>%
  mutate(
    query = map_chr(query, ~ paste(.x, collapse = ", "))
  )
write.csv(data_merged_csv,'ieee_12DEC2025_merged.csv',row.names=F)


