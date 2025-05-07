library(biomaRt) # loading needed packages
library(readr)

table <- read_excel("C:/Users/Alipa/Downloads/nerd ass data.xlsx")

common <- filter

probe_ids <- read_csv("C:/Users/Alipa/Downloads/probe_ids.csv") #reading probe id list

affyids <- as.vector(as.matrix(probe_ids)) #takes list from probe ids and turns it into a vector

mart <- useMart("ensembl", dataset = "mmusculus_gene_ensembl", host = "https://www.ensembl.org") #fetching the dataset of mouse genes


annotated_ids <- getBM(attributes = c('affy_mg_u74av2','name_1006','definition_1006','namespace_1003'), #tells getBM to fetch the required attributes of each gene
      filters = 'affy_mg_u74av2', 
      values = affyids, #tells it to use the list of probe IDs 
      mart = mart,
      uniqueRows = TRUE)



