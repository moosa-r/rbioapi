# Search HPP Proteomics data in UniProt

Using this function, you can search for HPP (Human Proteome Project)
proteomics features that has been map to UniProt proteins. You may also
refine your search with modifiers such as data_source, peptide etc. See
"Arguments section" for more information.

## Usage

``` r
rba_uniprot_proteomics_hpp_search(
  accession = NULL,
  taxid = NULL,
  data_source = NULL,
  upid = NULL,
  peptide = NULL,
  unique = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

- data_source:

  Proteomics data source. In addition to manual curation, UniProt also
  import PTM annotations from the following databases:

  - ["HPP"](https://hupo.org/Initiatives/)

  Please use \`rba_uniprot_proteomics_species()\` for more information
  on the available data sources for a given species.

- upid:

  [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- peptide:

  Peptide sequence(s). You can supply up to 20 sequences.

- unique:

  Logical: Should the results be filtered based on the Peptide's
  uniqueness (the fact that a peptide maps to only 1 protein). If TRUE,
  Only unique peptides will be returned, if FALSE only un-unique
  peptides will be returned; If NULL (default) the results will not be
  filtered based on this.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list Where each element correspond to a UniProt protein and proteomics
data are organized under the "features" sub-list.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.

UniProt categorizes proteomics data sources into three main data
categories: PTM (Post-Translational Modification), non-PTM, and HPP
(Human Proteome Project); each with corresponding API endpoints, and
thus, rbioapi functions.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm"

## References

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010

- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, Nucleic Acids Research, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
  https://doi.org/10.1093/nar/gkx237

- [Proteins API Documentation](https://www.ebi.ac.uk/proteins/api/doc/)

- [Citations note on UniProt
  website](https://www.uniprot.org/help/publications)

## See also

Other "UniProt - Proteomics":
[`rba_uniprot_proteomics_hpp()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp.md),
[`rba_uniprot_proteomics_non_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm.md),
[`rba_uniprot_proteomics_non_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm_search.md),
[`rba_uniprot_proteomics_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm.md),
[`rba_uniprot_proteomics_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm_search.md),
[`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)

## Examples

``` r
# \donttest{
  rba_uniprot_proteomics_hpp_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
# }
```
