# Retrieve UniProt Natural Variants by Sequence Position

Retrieve natural variants annotated at specified amino-acid positions in
UniProt protein sequences. Each supplied accession is paired with the
corresponding element of `locations`.

## Usage

``` r
rba_uniprot_variation_locations(accession, locations, save_peff = FALSE, ...)
```

## Arguments

- accession:

  Character: [UniProtKB
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accessions. Each accession is paired with the
  corresponding element of `locations`.

- locations:

  Character or Numeric: Amino-acid position(s) within each protein
  sequence. Each element should be a positive whole number or a
  character string of comma-separated positions, such as `"5,7"`. You
  can supply up to 100 elements, and their number should equal the
  number of supplied accessions.

- save_peff:

  Logical or Character: (default = `FALSE`)

  - FALSE: Return the parsed JSON response.

  - TRUE: Save the PEFF response to an automatically generated path.

  - Character string: A valid file path to save the PEFF file.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

With `save_peff = FALSE`, a list named by UniProt accession. Each
element contains entry metadata, the protein sequence, and matching
variants in its `features` element. Repeated groups for the same
accession are combined by the API, and `features` can be empty when no
variant is annotated at the requested positions. Otherwise, the PEFF
response is written to disk and returned as a character string.

## Details

A `locations` element can specify one position or several
comma-separated positions. The returned records are grouped by UniProt
accession and include the protein sequence and variant annotations found
at the requested positions.

## Corresponding API Resources

"GET
https://www.ebi.ac.uk/proteins/api/variation/accession_locations/{accession_locations}"

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

Other "UniProt - Variation":
[`rba_uniprot_variation()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation.md),
[`rba_uniprot_variation_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_variation_locations(
    accession = c("P05067", "Q99616"),
    locations = c("5,7", "5,29"))
# }
```
