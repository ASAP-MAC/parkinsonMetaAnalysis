library(tidyverse)

# parquet_raw <- arrow::read_parquet("~/Downloads/genefamilies_unstratified_uuid.parquet")
parquet_raw <- arrow::read_parquet(
  file = "~/Downloads/genefamilies_unstratified_uuid.parquet", col_select = c("gene_family","rpk_abundance", "uuid")
  )


filter(parquet_raw, uuid %in% )
# columns are 
## gene_family
## rpk_abundance
## uuid
## humann_header 

# the last one is not necessary for data retrieval

head(parquet_raw)