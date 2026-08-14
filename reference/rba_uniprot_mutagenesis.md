# Get Mutagenesis by UniProt Accession

UniProt describes how sequence mutations affect the biological
properties of a protein, cell, or organism. Retrieve the [mutagenesis
annotations](https://www.uniprot.org/help/mutagen) mapped to one UniProt
protein.

## Usage

``` r
rba_uniprot_mutagenesis(accession, location = NULL, ...)
```

## Arguments

- accession:

  Character: [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- location:

  Character: (optional) A valid amino acid range (e.g. 10-25) within the
  sequence of the given protein.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the mutagenesis description of your supplied UniProt
protein's sequence.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/mutagenesis/{accession}"

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

Other "UniProt - Mutagenesis":
[`rba_uniprot_mutagenesis_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_mutagenesis(accession = "P0DTC2", location = "300-400")
# }
```
