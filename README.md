# parkinsonMetaAnalysis
A package for the meta-analysis of shotgun datasets

# 2025-02-21

This repository will contain meta-analysis contents and concepts 
for publicly available shotgun metagenomics datasets. Part of the 
difficulty in gathering all is that metadata is almost always 
omitted, making incomplete datasets virtually useless.

## Data collection and retrieval

Data were collected with the scope of finding Parkinson disease datasets 
with publicly available FASTQ and per-sample metadata.

## Data processing

Data were uniformly processed with curatedMetagenomicNextflow and stored in Google 
Cloud bucket. Parquet files were generated and hosted on huggingface.
Data retrieval is made available via `parkinsonsMetagenomicData`, a convenience 
R packages that expands functionalities of curatedMetagenomicData.


## Download data from `parkinsonsMetagenomicData`

First, to use gene families, one must download the data locally

```
curl https://huggingface.co/datasets/waldronlab/metagenomics_mac/resolve/main/genefamilies_unstratified_uuid.parquet -o ~/Downloads/genefamilies_unstratified.parquet
```

```
quarto render 02-download-data.qmd -P relative_abundance
quarto render 02-download-data.qmd -P pathabundance_unstratified
# this last step doesn't work yet
quarto render 02-download-data.qmd -P genefamilies_unstratified
``` 
