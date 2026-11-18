library(openxlsx)
library(dplyr)


create_standard_xlsx = function(df,worksheet_name,wrap_cols,create_workbook=T){
  
  if(create_workbook){
    wb <- createWorkbook()
  }
  
  # Add a worksheet
  addWorksheet(wb, worksheet_name)
  
  # Write the data to the sheet
  writeData(wb, worksheet_name, df)
  
  # Create a style with text wrapping enabled
  header_style <- createStyle(
    fontSize = 11,
    fontColour = "white",
    fgFill = "#4F81BD",  # light blue background
    halign = "center",
    textDecoration = "bold",
    border = "Bottom",
    borderColour = "black",
    wrapText = TRUE, 
    valign = "top"
  )
  addStyle(
    wb,
    sheet = worksheet_name,
    style = header_style,
    rows = 1,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )
  
  wrap_style <- createStyle(wrapText = TRUE, valign = "top")
  addStyle(
    wb,
    sheet = worksheet_name,
    style = wrap_style,
    rows = 2:(nrow(df)+1),  # +1 to include header
    cols = wrap_cols,
    gridExpand = TRUE
  )
  freezePane(wb, sheet = worksheet_name, firstActiveRow = 2)
  
  return(wb)
}
# --------------------------- Read base data (metadata) --------------------------- 
setwd("/Users/zalpa/Desktop/new search - 12.12/Papers/")
base_data = read.csv('data_overview.csv')

# --------------------------- Review Eligibility (metadata) --------------------------- 

# Data for reviewer
eligibility_metadata_review <- base_data %>%
  mutate(E12_metadata = ifelse((Document.Type != 'preprint' & 
                                  Document.Type != 'review' &
                                  Document.Type != 'conference review' & 
                                  Document.Type != 'awarded grant' &
                                  Document.Type != 'dissertation/thesis' &
                                  Document.Type != 'software' &
                                  Document.Type != 'data set' &
                                  Document.Type != 'meeting abstract'),'','Y'),
         Document.Type.Reviewer = '',
         Flag = '',
         Initials = '',
         Date = '')


wrap_cols = which(!colnames(eligibility_metadata_review) %in% c('query'))
wb = create_standard_xlsx(eligibility_metadata_review,'Metadata',wrap_cols)
# Set col width
small_size = which(!colnames(eligibility_metadata_review) %in% c('Pubmed.Id','Year','ieee','pubmed','scopus','web_of_science','exist_preprint','exist_proceeding','E12_metadata'))
large_size = which(!colnames(eligibility_metadata_review) %in% c('Pubmed.Id','Year','ieee','pubmed','scopus','web_of_science','exist_preprint','exist_proceeding','E12_metadata'))
setColWidths(wb, 'Metadata', cols = small_size, widths = 6)
setColWidths(wb, 'Metadata', cols = large_size, widths = 30)
saveWorkbook(wb, 'review_templates/data_overview_reviewer_eligibility.xlsx', overwrite = TRUE)
rm(ls='wb')

# --------------------------- Remove E1 and E2 metadata review ---------------------------

base_data_final = base_data %>% filter(Document.Type != 'preprint' & 
                                         Document.Type != 'review' &
                                         Document.Type != 'conference review' & 
                                         Document.Type != 'awarded grant' &
                                         Document.Type != 'dissertation/thesis' &
                                         Document.Type != 'software' &
                                         Document.Type != 'data set' &
                                         Document.Type != 'meeting abstract') # 106 papers

base_data_final_identifier = base_data_final %>% 
  select(Title,Authors,Year,DOI)

# --------------------------- Draw papers for calibration ---------------------------

set.seed(4321)
# 10% for all
calibration10 = sample(1:nrow(base_data_final),size=0.1*nrow(base_data_final),replace=F)
calibration10_add = sample(1:nrow(base_data_final)[-calibration10],size=0.1*nrow(base_data_final),replace=F)
calibration20 = c(calibration10,calibration10_add)

# --------------------------- Review Eligibility (reading) --------------------------- 

eligibility_reading_review <- base_data_final_identifier %>%
  mutate(Calibration.Paper = ifelse(row_number() %in% calibration10,'Y',''),
         Document.Type.Reviewer = '',
         Full.Text.Available = '',
         VAE.Method = '',
         Application.Paper = '',
         Main.Purpose = '',
         Data.Type = '',
         Language = '',
         Methodology.Application.Paper = '',
         Flag = '',
         Reviewer = '',
         Date = '')


wb = create_standard_xlsx(eligibility_reading_review,'Eligibility_review',1:ncol(eligibility_reading_review))
addWorksheet(wb,'CatYN')
writeData(wb, sheet = 'CatYN', x = c('Y','N'))
apply_cols = which(colnames(eligibility_reading_review) %in% c('Full.Text.Available','VAE.Method','Application.Paper','Flag'))
for (c in apply_cols){
  dataValidation(
    wb,
    sheet = "Eligibility_review",
    cols = c,
    rows = 2:(nrow(eligibility_reading_review) + 1),  # future rows too
    type = "list",
    operator = 'equal',
    value = "'CatYN'!$A$1:$A$2"
  )
}

addWorksheet(wb,'CatPType')
writeData(wb, sheet = 'CatPType', x = c('Review','Journal','Conference','Preprint','Other'))
dataValidation(
  wb,
  sheet = "Eligibility_review",
  cols = which(colnames(eligibility_reading_review) %in% c('Document.Type.Reviewer')),
  rows = 2:(nrow(eligibility_reading_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatPType'!$A$1:$A$5"
)
# Set col width
small_size = which(!colnames(eligibility_reading_review) %in% c('Year'))
large_size = which(!colnames(eligibility_reading_review) %in% c('Year'))
setColWidths(wb, 'Eligibility_review', cols = small_size, widths = 6)
setColWidths(wb, 'Eligibility_review', cols = large_size, widths = 30)
saveWorkbook(wb, 'review_templates/review_eligibility.xlsx', overwrite = TRUE)
rm(ls='wb')

# --------------------------- Review method (reading) --------------------------- 

technical_review <- base_data_final_identifier %>%
  mutate(Calibration.Paper = ifelse(row_number() %in% calibration10,'Y',''),
         Name = '',
         Main.Purpose = '',
         Network.Encoder = '',
         Network.Decoder = '',
         Input.Distribution = '',
         Data.Type = '',
         Preprocessing.Method = '',
         Latent.Space.Prior = '',
         Clustering = '',
         Clustering.VAE = '',
         Loss = '',
         Technical.Advancemnet = '',
         Flag = '',
         Reviewer = '',
         Date = '')

evaluation_review <- base_data_final_identifier %>%
  mutate(Calibration.Paper = ifelse(row_number() %in% calibration10,'Y',''),
         Number.of.Datasets = '',
         Dataset.Names = '',
         Min.Cells = '',
         Max.Cells = '',
         Synthetic.Data = '',
         Hyperparameter.Tuning = '',
         Evaluation.Metric = '',
         Compared.Methods = '',
         Flag = '',
         Reviewer = '',
         Date = '')


challenges_review <- base_data_final_identifier %>%
  mutate(Calibration.Paper = ifelse(row_number() %in% calibration10,'Y',''),
         Correlation = '',
         How.Correlation = '',
         Explainability = '',
         How.Explainability = '',
         OOD = '',
         How.OOD = '',
         Prior.Knowledge = '',
         How.Prior.Knowledge = '',
         Robustness = '',
         How.Robustness = '',
         Sparsity = '',
         How.Sparsity = '',
         Uncertainty = '',
         How.Uncertainty = '',
         Flag = '',
         Reviewer = '',
         Date = '')



wb = create_standard_xlsx(technical_review,'Technical_review',1:ncol(technical_review))
wb = create_standard_xlsx(evaluation_review,'Evaluation_review',1:ncol(evaluation_review),create_workbook=F)
wb = create_standard_xlsx(challenges_review,'Challenges_review',1:ncol(challenges_review),create_workbook=F)
addWorksheet(wb,'CatYN')
writeData(wb, sheet = 'CatYN', x = c('Y','N'))
dataValidation(
  wb,
  sheet = "Technical_review",
  cols = which(colnames(technical_review) %in% c('Clustering.VAE')),
  rows = 2:(nrow(technical_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatYN'!$A$1:$A$2"
)
apply_cols = which(colnames(evaluation_review) %in% c('Synthetic.Data','Hyperparameter.Tuning'))
for (c in apply_cols){
  dataValidation(
    wb,
    sheet = "Evaluation_review",
    cols = c,
    rows = 2:(nrow(evaluation_review) + 1),  # future rows too
    type = "list",
    operator = 'equal',
    value = "'CatYN'!$A$1:$A$2"
  )
}
apply_cols = which(colnames(challenges_review) %in% c('Correlation','Explainability','Imbalance','OOD','Prior.Knowledge','Robustness','Sparsity','Uncertainty'))
for (c in apply_cols){
  dataValidation(
    wb,
    sheet = "Challenges_review",
    cols = c,
    rows = 2:(nrow(challenges_review) + 1),  # future rows too
    type = "list",
    operator = 'equal',
    value = "'CatYN'!$A$1:$A$2"
  )
}
addWorksheet(wb,'CatNetwork')
writeData(wb, sheet = 'CatNetwork', x = c('FFN','CNN','GNN'))
dataValidation(
  wb,
  sheet = "Technical_review",
  cols = which(colnames(technical_review) %in% c('Network.Encoder','Network.Decoder')),
  rows = 2:(nrow(technical_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatNetwork'!$A$1:$A$3"
)
addWorksheet(wb,'CatDistribution')
writeData(wb, sheet = 'CatDistribution', x = c('N','P','ZIP','NB','ZINB','G'))
dataValidation(
  wb,
  sheet = "Technical_review",
  cols = which(colnames(technical_review) %in% c('Input.Distribution')),
  rows = 2:(nrow(technical_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatDistribution'!$A$1:$A$6"
)
addWorksheet(wb,'CatDataType')
writeData(wb, sheet = 'CatDataType', x = c('pre-processed','raw counts'))
dataValidation(
  wb,
  sheet = "Technical_review",
  cols = which(colnames(technical_review) %in% c('Data.Type')),
  rows = 2:(nrow(technical_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatDataType'!$A$1:$A$2"
)
addWorksheet(wb,'CatPrior')
writeData(wb, sheet = 'CatPrior', x = c('N','GMM'))
dataValidation(
  wb,
  sheet = "Technical_review",
  cols = which(colnames(technical_review) %in% c('Latent.Space.Prior')),
  rows = 2:(nrow(technical_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatPrior'!$A$1:$A$2"
)

# Set col width
small_size = which(!colnames(technical_review) %in% c('Year','Flag'))
large_size = which(!colnames(technical_review) %in% c('Year','Flag'))
setColWidths(wb, 'Technical_review', cols = small_size, widths = 6)
setColWidths(wb, 'Technical_review', cols = large_size, widths = 30)
small_size = which(!colnames(evaluation_review) %in% c('Year','Flag'))
large_size = which(!colnames(evaluation_review) %in% c('Year','Flag'))
setColWidths(wb, 'Evaluation_review', cols = small_size, widths = 6)
setColWidths(wb, 'Evaluation_review', cols = large_size, widths = 30)
small_size = which(!colnames(challenges_review) %in% c('Year','Flag'))
large_size = which(!colnames(challenges_review) %in% c('Year','Flag'))
setColWidths(wb, 'Challenges_review', cols = small_size, widths = 6)
setColWidths(wb, 'Challenges_review', cols = large_size, widths = 30)
saveWorkbook(wb, 'review_templates/review_methods.xlsx', overwrite = TRUE)
rm(ls='wb')






# --------------------------- Review code --------------------------- 

code_review <- base_data_final_identifier %>%
  mutate(Calibration.Paper = ifelse(row_number() %in% calibration10,'Y',''),
         Availability = '',
         Environment = '',
         Tutorial = '',
         Working = '',
         Quality = '',
         Code.Comments = '',
         Link = '',
         Reviewer.Comments = '',
         Flag = '',
         Initials = '',
         Date = '') 



wb = create_standard_xlsx(code_review,'Code_review',1:ncol(code_review))
addWorksheet(wb,'CatYN')
writeData(wb, sheet = 'CatYN', x = c('Y','N'))
dataValidation(
  wb,
  sheet = "Code_review",
  cols = which(colnames(code_review) %in% c('Availablity','Tutorial','Working')),
  rows = 2:(nrow(code_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatYN'!$A$1:$A$2"
)
addWorksheet(wb,'CatQual')
writeData(wb, sheet = 'CatQual', x = c('Good','Medium','Bad'))
dataValidation(
  wb,
  sheet = "Code_review",
  cols = which(colnames(code_review) %in% c('Quality')),
  rows = 2:(nrow(code_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatQual'!$A$1:$A$3"
)
addWorksheet(wb,'CatCodeComm')
writeData(wb, sheet = 'CatCodeComm', x = c('Good','Medium/Good','Medium/Bad','Bad'))
dataValidation(
  wb,
  sheet = "Code_review",
  cols = which(colnames(code_review) %in% c('Code.Comments')),
  rows = 2:(nrow(code_review) + 1),  # future rows too
  type = "list",
  operator = 'equal',
  value = "'CatCodeComm'!$A$1:$A$4"
)
# Set col width
small_size = which(!colnames(code_review) %in% c('Year','Flag'))
large_size = which(!colnames(code_review) %in% c('Year','Flag'))
setColWidths(wb, 'Code_review', cols = small_size, widths = 6)
setColWidths(wb, 'Code_review', cols = large_size, widths = 30)
saveWorkbook(wb, 'review_templates/code_review.xlsx', overwrite = TRUE)
rm(ls='wb')  






