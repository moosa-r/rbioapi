# Search Post-Translational Modification in UniProt (Deprecated)

This function is Deprecated. Please use
[`rba_uniprot_proteomics_ptm_search`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm_search.md)
instead.  
UniProt maps proteomics peptides from different sources to the proteins'
sequences. Using this function, you can search for proteomics peptides
that has been map to UniProt proteins. You may also refine your search
with modifiers such as data_source, peptide etc. See "Arguments section"
for more information.

## Usage

``` r
rba_uniprot_ptm_search(
  accession = NULL,
  ptm = NULL,
  data_source = NULL,
  taxid = NULL,
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

- ptm:

  Post-translational modification name

- data_source:

  Proteomics data source. You can choose up to two of:

  - ["MaxQB"](https://www.uniprot.org/database/DB-0186)

  - ["PeptideAtlas"](https://www.uniprot.org/database/DB-0071)

  - ["EPD"](https://www.uniprot.org/database/DB-0205)

  - ["ProteomicsDB"](https://www.uniprot.org/database/DB-0229)

- taxid:

  NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You can supply
  up to 20 taxon IDs.

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

A list Where each element correspond to a UniProt protein and
post-translational modification are organized under the "features"
sub-list.

## Details

Note that this is a search function. Thus, you are not required to fill
every argument; You may use whatever combinations of arguments you see
fit for your query.  
see also: [PTM / Processing section in
UniProtKB](https://www.uniprot.org/help/ptm_processing_section)

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteomics-ptm"

## References

- The UniProt Consortium, UniProt: the universal protein knowledgebase
  in 2021, Nucleic Acids Research, Volume 49, Issue D1, 8 January 2021,
  Pages D480–D489, https://doi.org/10.1093/nar/gkaa1100

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

Other "Deprecated functions":
[`rba_uniprot_proteomics()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics.md),
[`rba_uniprot_proteomics_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_search.md),
[`rba_uniprot_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_ptm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
#Deprecated
rba_uniprot_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
} # }
```
