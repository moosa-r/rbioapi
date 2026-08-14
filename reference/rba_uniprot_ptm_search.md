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

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- ptm:

  Character: (optional) Post-translational modification name.

- data_source:

  Character: (optional) Proteomics data source. You can supply up to two
  values. Use
  [`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)
  to retrieve the sources currently available for each species and
  category.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- upid:

  Character: (optional) [UniProt Proteome identifier
  (UPID)](https://www.uniprot.org/help/proteome_id). You can supply up
  to 100 UPIDs.

- peptide:

  Character: (optional) Peptide sequence(s). You can supply up to 20
  sequences.

- unique:

  Logical: (optional) Filter by peptide uniqueness. If `TRUE`, return
  peptides mapping to one protein; if `FALSE`, return non-unique
  peptides; if `NULL`, do not apply this filter.

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

"GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm"

## References

- The UniProt Consortium. (2025). UniProt: the Universal Protein
  Knowledgebase in 2025. Nucleic Acids Research, 53(D1), D609–D617.
  https://doi.org/10.1093/nar/gkae1010

- Nightingale, A., Antunes, R., Alpi, E., Bursteinas, B., Gonzales, L.,
  Liu, W., Luo, J., Qi, G., Turner, E., & Martin, M. (2017). The
  Proteins API: Accessing key integrated protein and genome information.
  Nucleic Acids Research, 45(W1), W539–W544.
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
