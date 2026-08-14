# Search Post-Translational Modification Proteomics in UniProt

Search for post-translational-modification proteomics features mapped to
UniProt proteins. Refine the search by data source, peptide, or other
supported criteria.

## Usage

``` r
rba_uniprot_proteomics_ptm_search(
  accession = NULL,
  ptm = NULL,
  taxid = NULL,
  data_source = NULL,
  upid = NULL,
  peptide = NULL,
  unique = NULL,
  confidence_score = NULL,
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- ptm:

  Character: (optional) Post-translational modification name used by the
  Proteomics API, such as "Phosphorylation" or "SUMOylation".

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- data_source:

  Character: (optional) Proteomics data source. You can supply up to two
  values. Use
  [`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)
  to retrieve the sources currently available for each species and
  category.

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

- confidence_score:

  Character: (optional) One or more of "Bronze", "Silver", or "Gold";
  you can supply all three values. UniProt classifies modified residues
  by false localization rate across multiple datasets. See [Large-scale
  modified residues](https://www.uniprot.org/help/mod_res_large_scale)
  for more information.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list in which each element corresponds to a UniProt protein and
post-translational modifications are stored under the `features`
element.

## Details

At least one search criterion is required; `unique` only refines another
criterion.

See also: [PTM / Processing section in
UniProtKB](https://www.uniprot.org/help/post-translational_modification)

UniProt categorizes proteomics data sources into three main data
categories: PTM (Post-Translational Modification), non-PTM, and HPP
(Human Proteome Project); each with corresponding API endpoints, and
thus, rbioapi functions.

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

Other "UniProt - Proteomics":
[`rba_uniprot_proteomics_hpp()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp.md),
[`rba_uniprot_proteomics_hpp_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp_search.md),
[`rba_uniprot_proteomics_non_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm.md),
[`rba_uniprot_proteomics_non_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm_search.md),
[`rba_uniprot_proteomics_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm.md),
[`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)

## Examples

``` r
# \donttest{
  rba_uniprot_proteomics_ptm_search(peptide = "NDQVYQPLRDRDDAQYSHLGGNWAR")
# }
```
