# Purpose:
# This script processes a filled review eligibility Excel file and prepares
# structured review templates for downstream analysis.

library(openxlsx)
library(dplyr)
library(readxl)


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
# --------------------------- Read eligibility review data ----------------------------------------- 
setwd("/Users/zalpa/Downloads/")
base_data <- read_excel("review_eligibility_filled.xlsx")

# --------------------------- Remove E1 and E2 ----------------------------------

base_data_without_E1_E2 = base_data %>% filter(Document.Type.Reviewer != "Preprint" & 
                                         Document.Type.Reviewer != "Review" &
                                         Document.Type.Reviewer != "Other")

# --------------------------- Remove E3 ----------------------------------

base_data_without_E1_E2_E3 <- base_data_without_E1_E2 %>%
  filter(Full.Text.Available == "Y")

# --------------------------- Remove E4 ----------------------------------

base_data_without_E1_E2_E3_E4 <- base_data_without_E1_E2_E3 %>%
  filter(VAE.Method == "Y")

# --------------------------- Remove E5 ----------------------------------

base_data_without_E1_E2_E3_E4_E5 <- base_data_without_E1_E2_E3_E4 %>%
  filter(Application.Paper == "N")

# --------------------------- Remove E6 ----------------------------------

base_data_without_E1_E2_E3_E4_E5_E6 <- base_data_without_E1_E2_E3_E4_E5 %>%
  filter(Main.Purpose != "All computational methods" &
    Main.Purpose != "Batch correction; Prediction of treatment response" & 
    Main.Purpose != "Data generation" &
    Main.Purpose != "Deconvolving variations in treatment-control" &
    Main.Purpose != "DEG detection" &
    Main.Purpose != "Disentangle biological and technical effects; Correcting technical batch effects" &
    Main.Purpose != "Estimation of kinetic rates (mRNA splicing and degradation)" &
    Main.Purpose != "Gene embeddings" &
    Main.Purpose != "Gene regulatory network inference" &
    Main.Purpose != "Gene set projection" &
    Main.Purpose != "Identifying cell-cell communication" &
    Main.Purpose != "Imputation; Graph construction" &
    Main.Purpose != "Inferring TF activity; Dimensionality reduction" &
    Main.Purpose != "Integration" &
    Main.Purpose != "Integration; Imputation; Dimensionality reduction" &
    Main.Purpose != "Integration; Representation learning" &
    Main.Purpose != "Normalization" &
    Main.Purpose != "Prediction of differentiation" &
    Main.Purpose != "Prediction of treatment response" &
    Main.Purpose != "Review" &
    Main.Purpose != "Spatial reconstruction" &
    Main.Purpose != "Time dependent data (transcriptome dynamics and splicing)" &
    Main.Purpose != "Trajectory inference" &
    Main.Purpose != "Trajectory inference;  Integration"
    )
# --------------------------- Remove E7 ----------------------------------

base_data_without_E1_E2_E3_E4_E5_E6_E7 <- base_data_without_E1_E2_E3_E4_E5_E6 %>%
  filter(Language == "English")

# --------------------------- Remove E8 ----------------------------------

base_data_without_E1_E2_E3_E4_E5_E6_E7_E8 <- base_data_without_E1_E2_E3_E4_E5_E6_E7 %>%
  filter(Data.Type != "Drug and protein interaction data" &
    Data.Type != "Exon-level data" &
    Data.Type != "Gene-expression data" &
    Data.Type != "Multi-modal data (CITE-seq data)" &
    Data.Type != "Multi-modal data (scRNA-seq + protein-protein interaction sources)" &
    Data.Type != "Multi-modal data (scRNA-seq + scATAC-seq)" &
    Data.Type != "Multi-modal data (scRNA-seq + scTCR-seq)" &
    Data.Type != "Multi-modal data (scRNA-seq + spatial transcriptomics data)" &
    Data.Type != "Multi-modal data (scRNA-seq data + differentiation information)" &
    Data.Type != "Multi-modal data (scRNA-seq data + drug/celltype/batch information)" &
    Data.Type != "Multi-modal data (scRNA-seq data + gene ontology data)" &
    Data.Type != "Multi-modal data (scRNA-seq data + gene regulatory networks)" &
    Data.Type != "Multi-modal data (scRNA-seq data + proteomics data (ADT))" &
    Data.Type != "Multi-modal data (single-cell omics data + gene-gene interaction networks)" &
    Data.Type != "Multi-modal data (single-cell omics data)" &
    Data.Type != "Multi-modal data (single-cell omics data) and spatial transcriptomics" &
    Data.Type != "Multi-modal data (spatial transcriptomics + histology image)" &
    Data.Type != "Pool-seq data" &
    Data.Type != "scATAC-seq data" &
    Data.Type != "scRNA-seq data (annotated with batch information)" &
    Data.Type != "scRNA-seq data (batched) [and scATAC-seq (batched) and Multi-modal data (scRNA-seq + scATAC-seq)]" &
    Data.Type != "scRNA-seq data (from multiple samples)" &
    Data.Type != "scRNA-seq data (labelled and unlabelled samples)" &
    Data.Type != "scRNA-seq data (normal and perturbed)" &
    Data.Type != "scRNA-seq data (treatment-control)" &
    Data.Type != "scRNA-seq data + metadata (patient/sample)" &
    Data.Type != "Spatial scRNA-seq data" &
    Data.Type != "Spatial transcriptomics (imaging-based)" &
    Data.Type != "spatial transcriptomics" &
    Data.Type != "Time-series data (scRNA-seq data (spliced and unspliced counts))" &
    Data.Type != "Time-series data (scRNA-seq data and scATAC-seq data (spliced and unspliced counts))" &
    Data.Type != "Time-series data (scRNA-seq)" &
    Data.Type != "Time-series data (single-cell)"
  )

base_data_final_identifier = base_data_without_E1_E2_E3_E4_E5_E6_E7_E8 %>% 
  select(Title,Authors,Year,DOI)

# --------------------------- Draw papers for calibration ---------------------------

set.seed(4321)
# 10% for all
calibration10 = sample(1:nrow(base_data_without_E1_E2_E3_E4_E5_E6_E7_E8),size=0.1*nrow(base_data_without_E1_E2_E3_E4_E5_E6_E7_E8),replace=F)
calibration10_add = sample(1:nrow(base_data_without_E1_E2_E3_E4_E5_E6_E7_E8)[-calibration10],size=0.1*nrow(base_data_without_E1_E2_E3_E4_E5_E6_E7_E8),replace=F)
calibration20 = c(calibration10,calibration10_add)

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
saveWorkbook(wb, 'review_methods.xlsx', overwrite = TRUE)
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
saveWorkbook(wb, 'code_review.xlsx', overwrite = TRUE)
rm(ls='wb')  
