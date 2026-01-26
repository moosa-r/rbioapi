# Changelog

## rbioapi 0.8.3 (Current CRAN version)

CRAN release: 2025-07-03

- Improved internal stability of Enrichr functions

- Minor improvements and fixes.

## rbioapi 0.8.2

CRAN release: 2025-02-07

- Update functions to the latest corresponding API endpoints:

  - Added STRING function:

    rba_string_enrichment_image()

  - Updated Enrichr function:

    Added option to supply background genes

  - Updated PANTHER function:

    added option to supply data frame to the
    enrichment/over-representation function.

  - Added UniProt functions:

    rba_uniprot_epitope(), rba_uniprot_epitope_search(),
    rba_uniprot_coordinates_location_genome(),
    rba_uniprot_proteomics_hpp(), rba_uniprot_proteomics_hpp_search(),
    rba_uniprot_proteomics_non_ptm(),
    rba_uniprot_proteomics_non_ptm_search(),
    rba_uniprot_proteomics_ptm(), rba_uniprot_proteomics_ptm_search(),
    rba_uniprot_proteomics_species(), rba_uniprot_rna_edit(),
    rba_uniprot_rna_edit_search()

  - Deprecated UniProt functions:

    rba_uniprot_proteomics(), rba_uniprot_proteomics_search(),
    rba_uniprot_ptm(), rba_uniprot_ptm_search()

  - New arguments added to existing UniProt functions:

    rba_uniprot_features_search(),
    rba_uniprot_coordinates_location_protein(), rba_uniprot_features()

- All scripts were reformatted to enhance readability.

- Minor improvements and fixes.

## rbioapi 0.8.1

CRAN release: 2024-03-30

- Move to JASPAR 2024.

- Minor improvements and fixes.

## rbioapi 0.8.0

CRAN release: 2023-09-30

- Move to STRING v12.

- Incorporate the recent changes in PANTHER and MIEAA API.

- Minor improvements and fixes.

## rbioapi 0.7.9

CRAN release: 2023-05-09

- Stability improvements.

## rbioapi 0.7.8

CRAN release: 2023-05-02

- Bug fixes and minor improvements.

## rbioapi 0.7.7

CRAN release: 2022-08-08

- Bug fixes and minor improvements.

## rbioapi 0.7.6

CRAN release: 2022-04-05

- Submitted the paper to Bioinformatics journal (DOI:
  10.1093/bioinformatics/btac172).

- Added vignette article: “Over-Representation (Enrichment) Analysis””

- Updated citations information.

- Moved to JASPAR 2022.

- Added new API endpoints of UniProt and PANTHER.

- Minor internal improvements.

## rbioapi 0.7.5

- Moved to STRING database version 11.5

## rbioapi 0.7.4

CRAN release: 2021-06-22

- Bug fixes and minor improvements.

## rbioapi 0.7.3

- Improved internal functions.

## rbioapi 0.7.2

- JASPAR is supported.

## rbioapi 0.7.1

- Enrichr is supported.

## rbioapi 0.7.0

CRAN release: 2021-04-30

- The package was submitted to CRAN
