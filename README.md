# primeDB: R Client for PRIME Database API

An R package for accessing the PRIME ([PRIME Database](https://primedb.sjtu.edu.cn)) microbiome data through its RESTful API. This package provides functions to retrieve project metadata, sample information, taxonomic data, abundance data, and statistical analyses from the PRIME database.

## Features

- **Complete metadata access** for projects and samples
- **Multi-dimensional statistical analysis** across phenotypes, geography, and systems
- **Taxonomic data** from Silva 138.2 and Greengenes2 2024.09 databases
- **Abundance data** at multiple taxonomic levels
- **Easy-to-use functions** with comprehensive error handling
- **Consistent naming conventions** following camelCase
- **Automatic visualization** of distribution data with pie charts

## Installation

```r
# Install from GitHub
if (!require(devtools)) install.packages("devtools")
devtools::install_github("reckonzzz/primeDB")
```

## Quick Start

```r
library(primeDB)

# Check API health
health <- health_check()
print(health$status)

# Get database statistics
stats <- get_stats()
print(stats$data$basicStats$totalProjects)

# Get project metadata
metadata <- get_project_metadata(c("PRJDB13875", "PRJDB16847"))
print(metadata$summary$found)

# Get abundance data
abundance <- get_abundance_project("gg", "phylum", "absolute", 
                                       c("PRJDB13875", "PRJDB16847"))
print(dim(abundance))
```

## Main Functions

### Core Functions
- `health_check()` - Check API service status
- `get_stats()` - Get database statistics

### Metadata Functions
- `get_project_metadata(project_accessions)` - Get project metadata
- `get_sample_metadata(runs, show_progress = TRUE)` - Get sample metadata with batch processing

### Project Functions
- `get_projects()` - Get project list
- `get_project_samples(project_accession)` - Get project samples
- `get_project_stats(project_accession)` - Get project statistics

### Sample Functions
- `get_samples()` - Get sample list
- `get_sample_project(sample_run)` - Get sample project
- `get_sample_stats(sample_run)` - Get sample statistics

### Phenotype Functions
- `get_phenotypes()` - Get phenotype list
- `get_phenotype_projects(phenotype_id)` - Get phenotype projects
- `get_phenotype_samples(phenotype_id)` - Get phenotype samples
- `get_phenotype_stats(phenotype_id)` - Get phenotype statistics

### Country Functions
- `get_countries()` - Get country list
- `get_country_projects(country_id)` - Get country projects
- `get_country_samples(country_id)` - Get country samples
- `get_country_stats(country_id)` - Get country statistics

### Body Site Functions
- `get_bodysites()` - Get body site list (returns matrix)
- `get_bodysite_samples(body_site_id)` - Get body site samples
- `get_bodysite_projects(body_site_id)` - Get body site projects
- `get_bodysite_system(body_site_id)` - Get body site system
- `get_bodysite_stats(body_site_id)` - Get body site statistics

### System Functions
- `get_systems()` - Get system list
- `get_system_projects(short_name)` - Get system projects
- `get_system_samples(short_name)` - Get system samples
- `get_system_stats(short_name)` - Get system statistics

### Taxonomy Functions
- `get_taxonomy_list(db, rank = 'phylum')` - Get taxonomy list
- `get_taxonomy_projects(db, taxon_id)` - Get taxonomy projects
- `get_taxonomy_samples(db, taxon_id)` - Get taxonomy samples
- `get_taxonomy_stats(db, taxon_id)` - Get taxonomy statistics
- `get_taxonomy_parent(db, taxon_id)` - Get taxonomy parent
- `get_taxonomy_children(db, taxon_id)` - Get taxonomy children

### Abundance Functions
- `get_abundance_project(db, rank, type, project_accessions, show_progress = TRUE)` - Get project abundance data with batch processing
- `get_abundance_sample(db, rank, type, runs, show_progress = TRUE)` - Get sample abundance data with batch processing
- `get_abundance_cache(db, rank, type, show_progress = TRUE)` - Get cached abundance data with progress monitoring

### Visualization Functions
- `plot_distributions(stats_data, plot_names = NULL)` - Plot all (or selected) distribution pie charts

## Examples

### Working with Projects

```r
# Get all projects
projects <- get_projects()
print(length(projects))

# Get samples for a specific project
samples <- get_project_samples("PRJDB13875")
print(length(samples))

# Get detailed statistics for a project
stats <- get_project_stats("PRJDB13875")
print(stats$statistics$totalSamples)
```

### Working with Body Sites

```r
# Get all body sites (returns matrix)
bodysites <- get_bodysites()
print(dim(bodysites))
print(head(bodysites))

# Get samples for a specific body site
samples <- get_bodysite_samples(2)
print(length(samples))
print(dim(samples))  # runs matrix dimensions
```

### Working with Abundance Data

```r
# Get abundance data for projects
abundance <- get_abundance_project("gg", "phylum", "absolute", 
                                       c("PRJDB13875", "PRJDB16847"))

# Data is returned as a matrix with samples as rows and taxa as columns
print(dim(abundance))
print(head(abundance))

# Get abundance data for specific samples
sample_abundance <- get_abundance_sample("gg", "phylum", "absolute", 
                                             c("DRR396974", "DRR396975"))
print(dim(sample_abundance))
```

### Working with Visualizations

```r
# Get project statistics for visualization
stats <- get_project_stats("PRJDB13875")

# Plot all distributions
plots <- plot_distributions(stats)

# Plot only specific distributions
plots_selected <- plot_distributions(stats, plot_names = c("countries", "systems"))
```

## Data Structure

All functions return standardized data structures:

- **List data**: Most functions return lists with `data`, `count`, and `summary` fields
- **Matrix data**: Distribution data is automatically converted to matrix format for easy analysis
- **Error handling**: Invalid inputs return descriptive error messages

## Error Handling

All functions include comprehensive error handling:

```r
# Invalid database type
tryCatch({
  result <- get_taxonomy_list("invalid", "phylum")
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
```

## Dependencies

Required packages:
- `httr` - HTTP requests
- `jsonlite` - JSON parsing
- `dplyr` - Data manipulation
- `readr` - Data reading
- `ggplot2` - Plotting
- `ggrepel` - Label positioning

Suggested packages:
- `patchwork` - Plot combination
- `testthat` - Testing
- `knitr` - Documentation
- `rmarkdown` - Reports

## Rate Limiting

The API currently has no rate limiting, but it's recommended to:
- Make no more than 10 requests per second
- Implement appropriate delays between requests for large datasets
- Handle errors gracefully

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is licensed under the MIT License.

## Citation

If you use this package in your research, please cite:

```
Zhizhuo Zhang, Hongyu Zhao, Tao Wang, PRIME: a database for 16S rRNA microbiome data with phenotypic reference and comprehensive metadata, Nucleic Acids Research, 2025;, gkaf1057, https://doi.org/10.1093/nar/gkaf1057
```

bibtex:

```bibtex
@article{10.1093/nar/gkaf1057,
    author = {Zhang, Zhizhuo and Zhao, Hongyu and Wang, Tao},
    title = {PRIME: a database for 16S rRNA microbiome data with phenotypic reference and comprehensive metadata},
    journal = {Nucleic Acids Research},
    pages = {gkaf1057},
    year = {2025},
    month = {10},
    abstract = {PRIME (Phenotypic Reference for Integrated Microbiome Enrichment) is a curated and standardized database of human microbiome 16S rRNA amplicon sequencing data, designed to facilitate cross-study analysis, reproducibility, and phenotype-driven discovery. PRIME aggregates 53 449 samples from 111 public studies, covering 93 body sites and 101 phenotypic categories, with detailed harmonization of sample-level metadata such as disease status, demographics, body sites, sequencing protocols, and experimental design. Each sample includes taxonomic abundance profiles generated via a consistent pipeline using both SILVA (138.2) and Greengenes2 (2024.09) reference databases, with results reported at multiple taxonomic levels as observed abundances (read counts) and relative abundances (proportions). A major strength of PRIME is its extensive manual curation, which standardizes phenotypic and contextual metadata across studies, enabling precise querying and robust phenotype-based comparisons. Users can interactively explore the database through a modern web interface, filter and visualize data by metadata fields, and download customized subsets. Programmatic access is supported via RESTful APIs and R package. PRIME aims to advance microbiome data integration and is continuously updated to incorporate new studies and features. The database is freely available at https://primedb.sjtu.edu.cn.},
    issn = {1362-4962},
    doi = {10.1093/nar/gkaf1057},
    url = {https://doi.org/10.1093/nar/gkaf1057},
    eprint = {https://academic.oup.com/nar/advance-article-pdf/doi/10.1093/nar/gkaf1057/65048637/gkaf1057.pdf},
}
```

## Support

For support and questions:
- Check the [documentation](https://primedb.sjtu.edu.cn/help)
- Open an issue on [GitHub](https://github.com/reckonzzz/primeDB/issues)
- Contact the development team 
