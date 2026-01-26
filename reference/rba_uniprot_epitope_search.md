# Search UniProt Epitopes

Use this function to search epitope data associated to UniProt entities,
using various criteria such as UniProt accession, epitope sequence, IEDB
ID, and match score.

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

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- epitope_sequence:

  (Character) Epitope's proteins sequence

- iedb_id:

  (Numeric) [EIDB](https://www.iedb.org/) epitope Identifier(s). You can
  supply up to 20 accession numbers.

- match_score:

  Integer: Minimum alignment score for the antigen sequence and the
  target protein sequence.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A List where each element corresponds to one UniProt entity returned by
your search query. The element itself is a sub-list containing all
information that UniProt has about that entity.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/epitope"

## See also

Other "UniProt - Epitopes":
[`rba_uniprot_epitope()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope.md),
[`rba_uniprot_rna_edit()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit.md)

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
