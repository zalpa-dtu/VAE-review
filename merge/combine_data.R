library(dplyr)
library(tidyr)
library(purrr)


# This document merges all searches IEEE Xplore, Scopus, WOS and Pubmed
setwd("/Users/zalpa/Desktop/new search - 12.12/Papers/")
data_pubmed = readRDS('pubmed/pubmed_12DEC2025_merged.rda')
data_ieee   = readRDS('ieee/ieee_12DEC2025_merged.rda')
data_scopus = readRDS('scopus/scopus_12DEC2025_merged.rda')
data_wos    = readRDS('webofscience/wos_12DEC2025_merged.rda')

data = bind_rows(data_pubmed,data_ieee,data_scopus,data_wos)  %>%
  mutate(Y = "Y") %>%
  pivot_wider(
    names_from = Search.Engine,
    values_from = Y,
    values_fill = list(Y = "")
  ) 


# If data has duplicates on title and there is a pre-print. 
# This preprint is just on the same record.
data_preprint = data %>%
  arrange(Title) %>%
  mutate(preprint_name = ifelse(Document.Type == 'preprint',Journal,NA)) %>%
  group_by(Title) %>%
  mutate(
    duplicate = n() > 1, # Ensure that there is a duplicate
    has_preprint = any(Document.Type == "preprint"),
    preprint_name_concat = list(unique(preprint_name[!is.na(preprint_name)])),
    has_non_preprint = any(Document.Type != "preprint"),
    merged_query_preprints = list({
      preprint_queries <- query[Document.Type == "preprint"]
      if (length(preprint_queries) == 0) character(0)
      else purrr::reduce(preprint_queries, union) |> discard(is.na)
    })
  ) %>%
  filter(
    !(duplicate & has_non_preprint & Document.Type == 'preprint')
  ) %>%
  mutate(
    exist_preprint_ = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ 'Y',
      TRUE ~ exist_preprint
    ),
    preprint_ = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ map2(preprint, preprint_name_concat, ~ union(.x, .y) |> discard(is.na)),
      TRUE ~ preprint
    ),
    query_ = case_when(
      duplicate & has_preprint & Document.Type != "preprint" ~ map2(query, merged_query_preprints, ~ union(.x, .y) |> discard(is.na)),
      TRUE ~ query
    )
  ) %>%
  ungroup() %>%
  select(Title,
         Authors,
         Document.Type,
         DOI,
         Journal,
         Pubmed.Id,
         Year,
         scopus,ieee,web_of_science,pubmed,
         exist_preprint_,exist_proceeding,
         preprint_,proceeding,query_) %>%
  dplyr::rename(query = query_,
                exist_preprint = exist_preprint_,
                preprint = preprint_
  ) 




data_final = data_preprint %>% 
  arrange(Title) %>%
  group_by(Title) %>%
  mutate(Authors_match       = n_distinct(Authors[Authors != '']) <= 1,
         Document.Type_match = n_distinct(Document.Type[Document.Type != '']) <= 1,
         DOI_match           = n_distinct(DOI[DOI != '']) <= 1,
         Journal_match       = n_distinct(Journal[Journal != '']) <= 1,
         Pubmed.Id_match     = n_distinct(Pubmed.Id,na.rm=T) <= 1,
         Title_match         = n_distinct(Title) <= 1,
         Year_match          = n_distinct(Year[!is.na(Year)]) <= 1)

data_v1 = data_final


# Do not need to match on author
titles_move_v1 = data_v1 %>%
  group_by(Title) %>%
  filter(Document.Type_match == T &
           DOI_match == T &      
           Journal_match == T &  
           Pubmed.Id_match == T &
           Title_match == T &    
           Year_match == T) %>%
  pull(Title) %>%
  unique()

# Step 2: Aggregate valid groups into new dataset X
final_data_v1 = data_v1 %>%
  arrange(Title) %>%
  filter(Title %in% titles_move_v1) %>%
  group_by(Title) %>%
  summarise(Authors_                  = dplyr::first(na.omit(Authors)),
            Document.Type_            = dplyr::first(na.omit(Document.Type)),
            DOI_                      = dplyr::first(na.omit(DOI)),
            Journal_                  = dplyr::first(na.omit(Journal)),
            Pubmed.Id_                = dplyr::first(na.omit(Pubmed.Id)),
            Year_                     = dplyr::first(na.omit(Year)),
            scopus                   = paste(scopus, collapse = ""),
            ieee                     = paste(ieee, collapse = ""),
            web_of_science           = paste(web_of_science, collapse = ""),
            pubmed                   = paste(pubmed, collapse = ""),
            exist_preprint_          = paste(unique(exist_preprint[!is.na(exist_preprint)]),collapse=""),
            exist_proceeding_        = paste(unique(exist_proceeding[!is.na(exist_proceeding)]),collapse=""),
            preprint_                = list(preprint %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            proceeding_              = list(proceeding %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            query_                   = list(query %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            note_                    = '')

# Step 3: Flag these rows in the original dataset
data_final = data_final %>%
  mutate(moved = ifelse(Title %in% titles_move_v1, 'v1', '')) %>%
  arrange(moved,Title)

data_v2 = data_final %>%
  filter(moved == '')


# Move where everything is the same, but document type is editorial material and note -> editorial material/note
titles_move_v2 = data_v2 %>%
  group_by(Title) %>%
  filter((length(Document.Type) == 2) & (any(Document.Type == 'editorial material')) & (any(Document.Type == 'note'))  &
           DOI_match == T &
           Journal_match == T &
           Pubmed.Id_match == T &
           Title_match == T &
           Year_match == T) %>%
  pull(Title) %>%
  unique()

final_data_v2 = data_v2 %>%
  arrange(Title) %>%
  filter(Title %in% titles_move_v2) %>%
  group_by(Title) %>%
  summarise(Authors_                  = dplyr::first(na.omit(Authors)),
            Document.Type_            = paste0(na.omit(Document.Type),collapse="/"),
            DOI_                      = dplyr::first(na.omit(DOI)),
            Journal_                  = dplyr::first(na.omit(Journal)),
            Pubmed.Id_                = dplyr::first(na.omit(Pubmed.Id)),
            Year_                     = dplyr::first(na.omit(Year)),
            scopus                   = paste(scopus, collapse = ""),
            ieee                     = paste(ieee, collapse = ""),
            web_of_science           = paste(web_of_science, collapse = ""),
            pubmed                   = paste(pubmed, collapse = ""),
            exist_preprint_          = paste(unique(exist_preprint[!is.na(exist_preprint)]),collapse=""),
            exist_proceeding_        = paste(unique(exist_proceeding[!is.na(exist_proceeding)]),collapse=""),
            preprint_                = list(preprint %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            proceeding_              = list(proceeding %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            query_                   = list(query %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            note_                    = '')

data_final = data_final %>%
  mutate(moved = ifelse(Title %in% titles_move_v2, paste0(moved,'v2',collapse = ' '), moved)) %>%
  arrange(moved,Title)

data_v3 = data_final %>%
  filter(moved == '')


# Move where everything is the same, but year is different
titles_move_v3 = data_v3 %>%
  group_by(Title) %>%
  filter(Document.Type_match == T  &
           DOI_match == T &
           Journal_match == T &
           Pubmed.Id_match == T &
           Title_match == T &
           Year_match == F) %>%
  pull(Title) %>%
  unique()

final_data_v3 = data_v3 %>%
  arrange(Title) %>%
  filter(Title %in% titles_move_v3) %>%
  group_by(Title) %>%
  summarise(Authors_                  = dplyr::first(na.omit(Authors)),
            Document.Type_            = dplyr::first(na.omit(Document.Type)),
            DOI_                      = dplyr::first(na.omit(DOI)),
            Journal_                  = dplyr::first(na.omit(Journal)),
            Pubmed.Id_                = dplyr::first(na.omit(Pubmed.Id)),
            Year_                     = max(na.omit(Year)),
            scopus                   = paste(scopus, collapse = ""),
            ieee                     = paste(ieee, collapse = ""),
            web_of_science           = paste(web_of_science, collapse = ""),
            pubmed                   = paste(pubmed, collapse = ""),
            exist_preprint_          = paste(unique(exist_preprint[!is.na(exist_preprint)]),collapse=""),
            exist_proceeding_        = paste(unique(exist_proceeding[!is.na(exist_proceeding)]),collapse=""),
            preprint_                = list(preprint %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            proceeding_              = list(proceeding %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            query_                   = list(query %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            note_                    = 'Year differs.')

data_final = data_final %>%
  mutate(moved = ifelse(Title %in% titles_move_v3, paste0(moved,'v3',collapse = ' '), moved)) %>%
  arrange(moved,Title)

data_v4 = data_final %>%
  filter(moved == '')



# Move where everything is the same, but Journal differs
# Concatenate with a "/"
titles_move_v4 = data_v4 %>%
  group_by(Title) %>%
  filter(Document.Type_match == T  &
           DOI_match == T &
           Journal_match == F &
           Pubmed.Id_match == T &
           Title_match == T &
           Year_match == T) %>%
  pull(Title) %>%
  unique()

final_data_v4 = data_v4 %>%
  arrange(Title) %>%
  filter(Title %in% titles_move_v4) %>%
  group_by(Title) %>%
  summarise(Authors_                  = dplyr::first(na.omit(Authors)),
            Document.Type_            = dplyr::first(na.omit(Document.Type)),
            DOI_                      = dplyr::first(na.omit(DOI)),
            Journal_                  = paste0(na.omit(Journal),collapse="/"),
            Pubmed.Id_                = dplyr::first(na.omit(Pubmed.Id)),
            Year_                     = dplyr::first(na.omit(Year)),
            scopus                   = paste(scopus, collapse = ""),
            ieee                     = paste(ieee, collapse = ""),
            web_of_science           = paste(web_of_science, collapse = ""),
            pubmed                   = paste(pubmed, collapse = ""),
            exist_preprint_          = paste(unique(exist_preprint[!is.na(exist_preprint)]),collapse=""),
            exist_proceeding_        = paste(unique(exist_proceeding[!is.na(exist_proceeding)]),collapse=""),
            preprint_                = list(preprint %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            proceeding_              = list(proceeding %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            query_                   = list(query %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            note_                    = '')

data_final = data_final %>%
  mutate(moved = ifelse(Title %in% titles_move_v4, paste0(moved,'v4',collapse = ' '), moved)) %>%
  arrange(moved,Title)

data_v5 = data_final %>%
  filter(moved == '')


# Hardcode the last few
final_data_v5 = data_v5 %>%
  arrange(Title) %>%
  mutate(keep = ifelse((Title == 'a survey of autoencoder algorithms to pave the diagnosis of rare diseases' & Document.Type == 'review') |
                         (Title == 'single cell multi omics sequencing: application trends covid 19 data analysis issues and prospects' & Document.Type == 'review') |
                         (Title == 'parameter tuning is a key part of dimensionality reduction via deep variational autoencoders for single cell rna transcriptomics' & Document.Type == 'article') |
                         (Title == 'shallow sparsely connected autoencoders for gene set projection' & Document.Type == 'article'),'Y','')) %>%
  group_by(Title) %>%
  summarise(Authors_                  = Authors[keep == 'Y'],
            Document.Type_            = Document.Type[keep == 'Y'],
            DOI_                      = DOI[keep == 'Y'],
            Journal_                  = Journal[keep == 'Y'],
            Pubmed.Id_                = Pubmed.Id[keep == 'Y'],
            Year_                     = Year[keep == 'Y'],
            scopus                    = paste(scopus, collapse = ""),
            ieee                      = paste(ieee, collapse = ""),
            web_of_science            = paste(web_of_science, collapse = ""),
            pubmed                    = paste(pubmed, collapse = ""),
            exist_preprint_           = paste(unique(exist_preprint[!is.na(exist_preprint)]),collapse=""),
            exist_proceeding_         = paste(unique(exist_proceeding[!is.na(exist_proceeding)]),collapse=""),
            preprint_                 = list(preprint %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            proceeding_               = list(proceeding %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            query_                    = list(query %>% compact() %>% purrr::reduce(union, .init = character(0)) %>% discard(is.na)),
            note_                     = '')



final_data_ = rbind(final_data_v1,final_data_v2,final_data_v3,final_data_v4,final_data_v5) %>% arrange(Title)

# Rename
final_data = final_data_ %>%
  dplyr::rename(Authors = Authors_,
                Document.Type = Document.Type_,
                DOI = DOI_,
                Journal = Journal_,
                Pubmed.Id = Pubmed.Id_,
                Year = Year_,
                exist_preprint = exist_preprint_,
                exist_proceeding = exist_proceeding_,
                preprint = preprint_,
                proceeding = proceeding_,
                query = query_) %>%
  select(-note_)

saveRDS(final_data,file='data_overview.Rda')

final_data_csv <- final_data %>%
  mutate(
    query = map_chr(query, ~ paste(.x, collapse = ", ")),
    preprint = map_chr(preprint, ~ paste(.x, collapse = ", ")),
    proceeding = map_chr(proceeding, ~ paste(.x, collapse = ", "))
  )
write.csv(final_data_csv,'data_overview.csv',row.names=F)



duplicates_title_author = final_data %>% arrange(Title,Authors)  %>% filter(duplicated(Title,Authors) | duplicated(Title,Authors, fromLast = TRUE))
duplicates_title = final_data %>% arrange(Title) %>%  filter(duplicated(Title) | duplicated(Title, fromLast = TRUE))
duplicates_author = final_data %>% arrange(Authors) %>%  filter(duplicated(Authors) | duplicated(Authors, fromLast = TRUE))

