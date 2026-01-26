# Get Shortest Path Between Two Taxonomy Nodes

Use this function to retrieve the shortest path between two nodes in the
taxonomy tree of [UniProt Taxonomy
database](https://www.uniprot.org/help/taxonomy).

## Usage

``` r
rba_uniprot_taxonomy_relationship(from, to, ...)
```

## Arguments

- from:

  [NCBI taxonomic
  identifier](https://www.uniprot.org/help/taxonomic_identifier) of your
  initial node.

- to:

  [NCBI taxonomic
  identifier](https://www.uniprot.org/help/taxonomic_identifier) of your
  final node.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a nested list containing the node which are in the shortest path between
your supplied nodes.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/relationship"

## References

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010

- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, Nucleic Acids Research, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
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
[`rba_uniprot_taxonomy_path()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_path.md)

## Examples

``` r
# \donttest{
rba_uniprot_taxonomy_relationship(from = 9606, to = 10090)
# }
```
