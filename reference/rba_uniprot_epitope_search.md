# Search UniProt Epitopes

Use this function to search epitope data associated with UniProt
entries, using various criteria such as UniProt accession, epitope
sequence, IEDB ID, and match score. At least one search criterion is
required.

## Usage

``` r
rba_uniprot_epitope_search(
  accession = NULL,
  epitope_sequence = NULL,
  iedb_id = NULL,
  match_score = NULL,
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- epitope_sequence:

  Character: (optional) A single epitope protein sequence.

- iedb_id:

  Character or Numeric: (optional) [IEDB](https://www.iedb.org/) epitope
  identifier(s). You can supply up to 20 identifiers.

- match_score:

  Numeric: (optional) A whole number from 0 to 100 giving the minimum
  alignment score between the epitope sequence and target protein
  sequence.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list in which each element represents a matching UniProt entry and is
named by accession when available.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/epitope"

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

Other "UniProt - Epitopes":
[`rba_uniprot_epitope()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope.md)

## Examples

``` r
# \donttest{
  rba_uniprot_epitope_search(accession = c("Q84ZX5", "P36222"))
# }
# \donttest{
  rba_uniprot_epitope_search(epitope_sequence = "DKKCIEWEKAQHGA")
# }
# \donttest{
  rba_uniprot_epitope_search(iedb_id = 20354)
# }
```
