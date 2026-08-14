# Traverse UniProt Taxonomic Tree Path

Traverse upward or downward from a supplied node in the [UniProt
Taxonomy database tree](https://www.uniprot.org/help/taxonomy).

## Usage

``` r
rba_uniprot_taxonomy_path(id, direction, depth = 5, ...)
```

## Arguments

- id:

  Numeric: An [NCBI taxonomic
  identifier](https://www.uniprot.org/help/taxonomic_identifier).

- direction:

  Character: Direction of the taxonomic path, either "TOP" or "BOTTOM".

- depth:

  Numeric: (default = `5`) Number of taxonomic-tree levels to traverse,
  from 1 to 5.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A nested list containing the requested taxonomic path.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/taxonomy/path"

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

Other "UniProt - Taxonomy":
[`rba_uniprot_taxonomy()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy.md),
[`rba_uniprot_taxonomy_lca()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lca.md),
[`rba_uniprot_taxonomy_lineage()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lineage.md),
[`rba_uniprot_taxonomy_name()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_name.md),
[`rba_uniprot_taxonomy_relationship()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_relationship.md)

## Examples

``` r
# \donttest{
rba_uniprot_taxonomy_path(id = 9606, direction = "TOP", depth = 3)
# }
# \donttest{
rba_uniprot_taxonomy_path(id = 207598, direction = "BOTTOM", depth = 3)
# }
```
