# Get UniProt entry by accession

Retrieve a UniProtKB entry by accession. Alternatively, retrieve its
isoforms or interaction partners by setting `isoforms = TRUE` or
`interaction = TRUE`. These two modes are mutually exclusive.

## Usage

``` r
rba_uniprot_proteins(accession, interaction = FALSE, isoforms = FALSE, ...)
```

## Arguments

- accession:

  Character: [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- interaction:

  Logical: (default = `FALSE`) Retrieve
  [interaction](https://www.uniprot.org/help/interaction_section)
  partners instead of the entry itself?

- isoforms:

  Logical: (default = `FALSE`) Retrieve
  [isoforms](https://www.uniprot.org/help/alternative_products) of your
  supplied UniProt entry instead of the canonical entry itself?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the requested UniProtKB entry. Isoform and interaction
results are lists named by accession.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/proteins/{accession}"  
"GET
https://www.ebi.ac.uk/proteins/api/proteins/interaction/{accession}"  
"GET https://www.ebi.ac.uk/proteins/api/proteins/{accession}/isoforms"

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

Other "UniProt - Proteins":
[`rba_uniprot_proteins_crossref()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_crossref.md),
[`rba_uniprot_proteins_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteins(accession = "P01730")
# }
# \donttest{
rba_uniprot_proteins(accession = "P01730", interaction = TRUE)
# }
# \donttest{
rba_uniprot_proteins(accession = "Q29983", isoforms = TRUE)
# }
```
