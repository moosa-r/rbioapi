# Search Mutagenesis in UniProt

UniProt describes how sequence mutations affect the biological
properties of a protein, cell, or organism. Use this function to search
for [mutagenesis annotations](https://www.uniprot.org/help/mutagen)
using at least one of `accession`, `taxid`, or `db_id`.

## Usage

``` r
rba_uniprot_mutagenesis_search(
  accession = NULL,
  taxid = NULL,
  db_id = NULL,
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

- db_id:

  Character: (optional) The ID in a cross-reference database. You can
  supply up to 20 values.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list in which each element represents a matching UniProt entry, named
by accession when available. Mutagenesis annotations are stored in the
entry's `features` element.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/mutagenesis"

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
[`rba_uniprot_mutagenesis()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis.md)

## Examples

``` r
# \donttest{
#search all mutations in COVID19 proteins
rba_uniprot_mutagenesis_search(taxid = 2697049)
# }
```
