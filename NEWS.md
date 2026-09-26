# rbioapi (development version)

## New features

* `rba_metadata()` retrieves optional request metadata attached to rbioapi
  results. Metadata can be enabled for one call with `metadata = TRUE` or for
  all later calls with `rba_options(metadata = TRUE)`. It records the rbioapi
  version and, for each request, its time, call, original `httr` response, and
  the exact function(s) used to parse that response. Multi-request functions, 
  or multiple calls due to retries preserve request order. Metadata remains off 
  by default because it can substantially increase the size of returned
  R objects.

* `rba_pages()` now supports inclusive `"pages:start:end"` ranges in either
  direction and arbitrary ordered page sets through `page_arg` and `pages`.
  It accepts at most 100 unique positive whole-number pages and waits at least
  two seconds between requests. Results preserve the requested order, retain
  `NULL` pages, and use names such as `page_1`. The `skip_error`,
  `progress`, and `verbose` arguments now provide explicit control over the
  complete operation.

* `rba_reactome_search()` searches Reactome by text with filters for species,
  entry type, compartment, keyword, and scope, together with grouping and page
  controls.

* `rba_enrichr_gene_sets()` retrieves all gene sets, or one exact term, from
  an organism-specific Enrichr library and can save the original GMT file.

* `rba_panther_genome()` retrieves one page of genes from a PANTHER genome
  and reports the current page, total pages, and number of genes in the genome.

* `rba_string_functional_terms()` searches STRING functional terms by ID or
  descriptive text for a species.

* `rba_uniprot_features_type()` searches UniProt feature descriptions for a
  chosen feature type, with optional annotation filters.

* `rba_uniprot_variation_locations()` retrieves UniProt variants for paired
  accession and sequence-position values. Results are returned as a list named
  by accession or saved as PEFF.

* `rba_panther_enrich()` gains `request_mapped_genes` and can return genes
  mapped from the input, genes mapped from the reference set, or no mapped
  genes. Input-gene mappings are returned by default.

* `rba_string_interactions_network()` and `rba_string_network_image()` can
  request a network with `network_term_id` instead of a list of IDs.
  `rba_string_annotations()` gains `only_pubmed`; when `TRUE`, it takes
  priority over `allow_pubmed`.

* `rba_reactome_event_hierarchy()` gains pathway-only output and filters
  based on an analysis token. `rba_reactome_query()` gains controls for
  relationships, reference-entity summaries, and diseases.
  `rba_reactome_xref()` now accepts several IDs and supports database filters
  and result pages.

* `rba_mieaa_cats()` gains a `mode` argument for all, default, or expert
  categories. `rba_mieaa_enrich()` gains `poll_interval` and
  `poll_timeout` to control how often a job is checked and how long polling
  continues.

## Changes to existing behavior

* rbioapi now rejects `NA` and `NaN` unless a function explicitly permits
  them. Numeric identifiers, counts, sequence positions, page numbers, and
  retry counts must be finite whole numbers where applicable. Invalid values
  passed to `rba_options()` now produce an error instead of silently restoring
  package defaults.

* `rba_mieaa_enrich_submit()` no longer assumes human input and now requires
  `species` as a supported abbreviation, scientific name, or taxonomy ID.
  
* `rba_panther_info(what = "families")` now names its family result
  `family`, correcting the previous `familiy` spelling.
  
* `rba_reactome_pathways_events()` now returns complete contained-event
  records as a named list rather than a data frame. Requests for one event
  attribute preserve empty and structured values; if the response cannot be
  separated reliably by event, the original response is returned with a
  warning.

* `rba_string_map_ids()` now uses STRING v12's `get_string_ids` service and
  returns at most one match for each input ID. `echo_query` now defaults to
  `TRUE`, `queryIndex` is returned as an integer, and saved responses use
  `.tsv`. The `limit` argument is deprecated and has no effect.

* `rba_string_network_image()` and `rba_string_enrichment_image()` now
  return PNG results as decoded image arrays and SVG results as raw content.

* `rba_uniprot_rna_edit_search()` renames `variantlocation` to
  `variant_location` to match UniProt.

## Other updates by service

* Enrichr functions now use the correct export and Speedrichr view web
  addresses, accept positive whole-number list IDs, and use `.tsv` files for
  standard enrichment and `.json` files for background enrichment.
  Custom-background results preserve one row per enriched term instead of
  creating extra rows from the returned gene list (#20).
  
* JASPAR functions now default to and accept the 2026 release. Requests for
  collections, matrices, releases, sites, species, taxon groups, and TFFMs use
  current query values and supported taxon groups. The TFFM search alias,
  saved file names, and JASPAR error details are also corrected.
  
* miEAA functions now use the current `/mieaa/` web addresses and check
  category names before submitting a job. Conversion results use `.txt` or
  `.tsv` files to match their content. Over-representation analysis (ORA),
  gene set enrichment analysis (GSEA), empty results, and numeric sorting are
  handled correctly. Failed, malformed, or timed-out jobs stop promptly while
  still respecting `skip_error`.
  
* STRING requests now follow the current routes for ID mapping, interactions,
  homology, enrichment, annotations, and images. Set-based operations remove
  duplicate IDs and backgrounds, while ID mapping preserves duplicates and
  input order. Functions subject to STRING's species limit now require
  `species` for more than 10 unique IDs. Empty enrichment results and image
  file names are also handled correctly.

* PANTHER requests now follow the current service limits. Mapping accepts up to
  5,000 IDs, enrichment checks the 100,000-ID limit, and TreeGrafter accepts
  sequences up to 50,000 characters. Paralog requests, family page counts,
  out-of-range pages, mapped-gene results, and temporary-file cleanup are
  corrected. Empty, single-record, and multi-record table-like results now
  have consistent data-frame forms while preserving nested fields.
  
* Reactome analysis functions now use the current submission, mapping, report,
  and download routes. Scalar IDs, vectors, tables, local files, and HTTP(S)
  URLs are handled consistently, and local files use Reactome's current form
  upload. Direct, token, and species analysis results now represent
  `pathways` as a data frame, including a zero-row data frame when no pathways
  are found.

* Reactome content functions now use the current hierarchy, enhanced-query,
  interaction, orthology, cross-reference, and exporter resources.
  `rba_reactome_analysis_token()` makes `species` optional and defaults
  `p_value` to `1`. `rba_reactome_exporter_diagram()` now defaults
  `ehld` to `TRUE`, matching Reactome's current behavior.

* UniProt protein, feature, variation, proteomics, proteome, gene-centric,
  coordinate, taxonomy, and UniParc functions now use current web addresses,
  argument names, filters, page handling, and returned data. Search results are
  named by accession more reliably, and deprecated proteomics functions call
  their supported replacements.

## Other fixes and improvements

* rbioapi now detects errors reported in a response body with HTTP 200 and
  applies `skip_error` in the same way as for regular HTTP errors.

* Error messages from Enrichr, JASPAR, miEAA, PANTHER, Reactome, STRING, and
  UniProt now include the HTTP status and useful server details. Unrecognized
  response formats return the original text instead of hiding it. UniProt
  server errors are handled, and 4xx and 5xx responses are correctly described
  as client and server errors.

* Response saving now more robustly supports Windows and Unix-like file paths
  containing backslashes, spaces, or quotes. Missing targets are handled 
  consistently, and invalid paths produce clearer errors.

* Required-argument detection, optional `NULL` handling, retry validation, and
  request-header construction now behave consistently and report invalid
  inputs more clearly.

## Documentation and maintenance

* Function manuals, argument and return descriptions, examples, vignettes,
  external links, and service citations are updated and standardized. Slow
  examples use smaller requests so checks and documentation builds take
  less time.
  
* `png` moves from `Suggests` to `Imports` because STRING image functions
  use it while the package is running. The minimum suggested `testthat`
  version is now 3.1.7. Tests cover the new input checks, service errors,
  metadata, multiple pages, and file saving, and the GitHub Actions files use
  current templates.

* Documentation is regenerated with roxygen2 8.0.0. The pkgdown reference lists
  the new functions, and README badges, links, examples, and development
  notices are updated.

# rbioapi 0.8.3 (Current CRAN version)

* Improved internal stability of Enrichr functions

* Minor improvements and fixes.

# rbioapi 0.8.2

* Update functions to the latest corresponding API endpoints:

  * Added STRING function:
  
    rba_string_enrichment_image()
    
  * Updated Enrichr function:
  
    Added option to supply background genes
    
  * Updated PANTHER function:
  
    added option to supply data frame to the enrichment/over-representation function.

  * Added UniProt functions:
  
    rba_uniprot_epitope(), rba_uniprot_epitope_search(),
    rba_uniprot_coordinates_location_genome(), rba_uniprot_proteomics_hpp(),
    rba_uniprot_proteomics_hpp_search(), rba_uniprot_proteomics_non_ptm(),
    rba_uniprot_proteomics_non_ptm_search(), rba_uniprot_proteomics_ptm(),
    rba_uniprot_proteomics_ptm_search(), rba_uniprot_proteomics_species(),
    rba_uniprot_rna_edit(), rba_uniprot_rna_edit_search()
  
  * Deprecated UniProt functions:
  
    rba_uniprot_proteomics(), rba_uniprot_proteomics_search(),
    rba_uniprot_ptm(), rba_uniprot_ptm_search()
  
  * New arguments added to existing UniProt functions:
  
    rba_uniprot_features_search(),
    rba_uniprot_coordinates_location_protein(), rba_uniprot_features()

* All scripts were reformatted to enhance readability.

* Minor improvements and fixes.

# rbioapi 0.8.1

* Move to JASPAR 2024.

* Minor improvements and fixes.

# rbioapi 0.8.0

* Move to STRING v12.

* Incorporate the recent changes in PANTHER and miEAA API.

* Minor improvements and fixes.

# rbioapi 0.7.9

* Stability improvements.

# rbioapi 0.7.8

* Bug fixes and minor improvements.

# rbioapi 0.7.7

* Bug fixes and minor improvements.

# rbioapi 0.7.6

* Submitted the paper to Bioinformatics journal (DOI: 10.1093/bioinformatics/btac172).

* Added vignette article: "Over-Representation (Enrichment) Analysis""

* Updated citations information.

* Moved to JASPAR 2022.

* Added new API endpoints of UniProt and PANTHER.

* Minor internal improvements.

# rbioapi 0.7.5

* Moved to STRING database version 11.5

# rbioapi 0.7.4

* Bug fixes and minor improvements.

# rbioapi 0.7.3

* Improved internal functions.

# rbioapi 0.7.2

* JASPAR is supported.

# rbioapi 0.7.1

* Enrichr is supported.

# rbioapi 0.7.0 

* The package was submitted to CRAN
