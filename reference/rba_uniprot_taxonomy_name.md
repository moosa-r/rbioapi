# Search UniProt Taxonomic Names

Using this function, you can search and retrieve taxonomic nodes using
their names from [UniProt Taxonomy
database](https://www.uniprot.org/help/taxonomy).

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

  a name to to be used as search query.

- field:

  Specify the field that your supplied name should be searched. It
  should be one of : "scientific" (default), "common" or "mnemonic".

- search_type:

  The logical relationship between your supplied search query and the
  taxonomic name field. It should be one of "equal_to" (default),
  "start_with", "end_with" or "contain".

- node_only:

  (logical) Retrieve only the node(s) information and exclude URL links
  to parents, siblings and children nodes. default = TRUE

- page_size:

  (numeric) Your search results may be very long, thus UniProt API will
  paginate the results, you may use this argument to control the
  pagination. maximum value is 200.

- page_number:

  (numeric) Your search results may be very long, thus UniProt API will
  paginate the results, you may use this argument to control the
  pagination. maximum value is 200.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a list containing taxonomic nodes that match your supplied inputs.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/name/{name}"  
"GET https://ebi.ac.uk/proteins/api/name/{name}/node"

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
