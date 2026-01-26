# Get UniProt Taxonomy Nodes

Using this function, you can retrieve taxonomic nodes information by
providing their [NCBI taxonomic
identifiers](https://www.uniprot.org/help/taxonomic_identifier). also,
you can explicitly retrieve other nodes in relation to your supplied
node's hierarchy in [UniProt Taxonomy
database](https://www.uniprot.org/help/taxonomy).

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

  (numeric) a single or a numeric vector of [NCBI taxonomic
  identifier(s)](https://www.uniprot.org/help/taxonomic_identifier)

- hierarchy:

  Retrieve taxonomic nodes that have specific hierarchical relation to
  your supplied taxonomic node. should be one of: "children", "parent"
  or "siblings".

- node_only:

  Retrieve only the node(s) information and exclude URL links to
  parents, siblings and children nodes.

- page_size:

  (numeric) Only when hierarchy is supplied. hierarchy information may
  be very long, thus UniProt API will paginate the results, you may use
  this argument to control the pagination. maximum value is 200.

- page_number:

  (numeric) Only when hierarchy is supplied. hierarchy information may
  be very long, thus UniProt API will paginate the results, you may use
  this argument to control the pagination.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

a list containing your supplied nodes or their related nodes taxonomic
information.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/ids/{ids}"  
"GET https://ebi.ac.uk/proteins/api/ids/id/{id}/node"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/node"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/children"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/children/node"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/parent"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/parent/node"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/siblings"  
"GET https://ebi.ac.uk/proteins/api/id/{id}/siblings/node"

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
