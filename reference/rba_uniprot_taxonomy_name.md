# Search UniProt Taxonomic Names

Search and retrieve taxonomic nodes by name from the [UniProt Taxonomy
database](https://www.uniprot.org/help/taxonomy). Search results are
paginated.

## Usage

``` r
rba_uniprot_taxonomy_name(
  name,
  field = "scientific",
  search_type = "equal_to",
  node_only = TRUE,
  page_size = 200,
  page_number = 1,
  ...
)
```

## Arguments

- name:

  Character: Taxonomic name to search.

- field:

  Character: (default = `"scientific"`) Name field to search. One of
  "scientific", "common", or "mnemonic".

- search_type:

  Character: (default = `"equal_to"`) Relationship between the query and
  taxonomic name. One of "equal_to", "start_with", "end_with", or
  "contain".

- node_only:

  Logical: (default = `TRUE`) If `TRUE`, return node information without
  links to parent, sibling, and child nodes.

- page_size:

  Numeric: (default = `200`) Number of results per page. The maximum is
  200.

- page_number:

  Numeric: (default = `1`) Page to retrieve.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing matching taxonomic nodes and page information.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/taxonomy/name/{name}"  
"GET https://www.ebi.ac.uk/proteins/api/taxonomy/name/{name}/node"

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
[`rba_uniprot_taxonomy_path()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_path.md),
[`rba_uniprot_taxonomy_relationship()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_relationship.md)

## Examples

``` r
# \donttest{
rba_uniprot_taxonomy_name(name = "homo", field = "scientific",
    search_type = "start_with")
# }
# \donttest{
rba_uniprot_taxonomy_name(name = "adenovirus", field = "scientific",
    search_type = "contain", page_size = 200, page_number = 2)
# }
```
