# Search Antigens in UniProt

UniProt maps antigenic (antibody-binding) features from several sources
to protein sequences. Search those mappings using one or more criteria.
At least one of `accession`, `antigen_sequence`, `antigen_id`,
`ensembl_id`, or `match_score` is required.

## Usage

``` r
rba_uniprot_antigens_search(
  accession = NULL,
  antigen_sequence = NULL,
  antigen_id = NULL,
  ensembl_id = NULL,
  match_score = NULL,
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- antigen_sequence:

  Character: (optional) A single antigenic protein sequence of at least
  four residues.

- antigen_id:

  Character: (optional) Human Protein Atlas (HPA) antigen ID. You can
  supply up to 20 IDs.

- ensembl_id:

  Character: (optional) Ensembl stable transcript ID. You can supply up
  to 20 IDs.

- match_score:

  Numeric: (optional) A whole number from 0 to 100 giving the minimum
  alignment score between the antigen sequence and target protein
  sequence.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list in which each element represents a matching UniProt entry, named
by accession when available. Antigenic annotations are stored in the
entry's `features` element.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/antigen"

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

Other "UniProt - Antigen":
[`rba_uniprot_antigens()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens.md)

## Examples

``` r
# \donttest{
rba_uniprot_antigens_search(antigen_id = "HPA001060")
# }
```
