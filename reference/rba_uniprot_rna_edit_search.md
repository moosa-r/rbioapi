# Search RNA Editing in UniProt

UniProt curates [RNA-editing
events](https://www.uniprot.org/help/rna_editing) (conversion,
insertion, deletion of nucleotides). Use this function to search RNA
editing records in UniProt using various criteria such as accession,
taxon ID, or protein-level variant location. At least one criterion is
required.

## Usage

``` r
rba_uniprot_rna_edit_search(
  accession = NULL,
  taxid = NULL,
  variant_location = NULL,
  ...
)
```

## Arguments

- accession:

  Character: (optional) [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- taxid:

  Numeric: (optional) NIH-NCBI [Taxon
  ID](https://www.uniprot.org/taxonomy/). You can supply up to 20 taxon
  IDs.

- variant_location:

  Character: (optional) Up to four protein-level variant locations, for
  example `"p.Leu336Pro"`.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list in which each element represents a matching UniProt entry and is
named by accession when available.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/rna-editing"

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

Other "UniProt - RNA Editing":
[`rba_uniprot_rna_edit()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit.md)

## Examples

``` r
# \donttest{
  rba_uniprot_rna_edit_search(accession = c("Q16851", "Q16849"))
# }
```
