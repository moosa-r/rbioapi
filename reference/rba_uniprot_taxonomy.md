# Get UniProt Taxonomy Nodes

Retrieve taxonomic-node information using [NCBI taxonomic
identifiers](https://www.uniprot.org/help/taxonomic_identifier). You can
also retrieve nodes related to one supplied node in [UniProt Taxonomy
database](https://www.uniprot.org/help/taxonomy). Child and sibling
results are paginated.

## Usage

``` r
rba_uniprot_taxonomy(
  ids,
  hierarchy = NULL,
  node_only = TRUE,
  page_size = 200,
  page_number = 1,
  ...
)
```

## Arguments

- ids:

  Numeric: One or more [NCBI taxonomic
  identifiers](https://www.uniprot.org/help/taxonomic_identifier).

- hierarchy:

  Character: (optional) Retrieve nodes related to one supplied node. One
  of "children", "parent", or "siblings".

- node_only:

  Logical: (default = `TRUE`) If `TRUE`, return node information without
  links to parent, sibling, and child nodes.

- page_size:

  Numeric: (default = `200`) Number of child or sibling nodes per page.
  The maximum is 200.

- page_number:

  Numeric: (default = `1`) Page of child or sibling nodes to retrieve.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing taxonomy information for the requested nodes or their
related nodes.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/ids/{ids}"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/ids/{ids}/node"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/node"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/children"  
"GET
https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/children/node"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/parent"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/parent/node"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/siblings"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/id/{id}/siblings/node"

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
[`rba_uniprot_taxonomy_lca()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lca.md),
[`rba_uniprot_taxonomy_lineage()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lineage.md),
[`rba_uniprot_taxonomy_name()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_name.md),
[`rba_uniprot_taxonomy_path()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_path.md),
[`rba_uniprot_taxonomy_relationship()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_relationship.md)

## Examples

``` r
# \donttest{
rba_uniprot_taxonomy(ids = c(9606, 10090))
# }
# \donttest{
rba_uniprot_taxonomy(ids = 9989, hierarchy = "children")
# }
```
