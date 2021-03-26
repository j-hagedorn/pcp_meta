
library(tidyverse);library(easyPubMed)

query <- '(((person-centered) OR (person-centered planning)) OR (person-centred)) OR (patient-centered)'
entrez_id <- get_pubmed_ids(query)

batch_pubmed_download(query,dest_dir = "batch",dest_file_prefix = "pcp_")

files <- list.files("batch/",full.names = T)
df <- data.frame()

for (i in 1:length(files)){
  x <- table_articles_byAuth(files[i])
  df <- bind_rows(df,x)
}

write_csv(df,"pcp_df.csv")

# remove the files
file.remove(list.files("batch", include.dirs = F, full.names = T, recursive = T))
