# Get Post-Translational Modification of UniProt Protein

UniProt maps post-translational modification proteomics data from
different sources to the proteins' sequences. Using this function, you
can retrieve all the post-translational-modification features mapped to
a given UniProt protein's sequence.

## Usage

``` r
rba_uniprot_proteomics_ptm(accession, confidence_score = NULL, ...)
```

## Arguments

- accession:

  Character: [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

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

A list containing the post-translational modification features of your
supplied UniProt protein's sequence.

## Details

see also: [PTM / Processing section in
UniProtKB](https://www.uniprot.org/help/post-translational_modification)

UniProt categorizes proteomics data sources into three main data
categories: PTM (Post-Translational Modification), non-PTM, and HPP
(Human Proteome Project); each with corresponding API endpoints, and
thus, rbioapi functions.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteomics/ptm/{accession}"

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
[`rba_uniprot_proteomics_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm_search.md),
[`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)

## Examples

``` r
# \donttest{
  rba_uniprot_proteomics_ptm(accession = "P04234")
# }
```
