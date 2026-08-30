# Get PANTHER Families and Sub-Families

Using this function, you can retrieve Orthologs, MSA or Tree topology
information of a given PANTHER family.

## Usage

``` r
rba_panther_family(id, what, target_organisms = NULL, ...)
```

## Arguments

- id:

  Character: Panther family id.

- what:

  Character: What to retrieve? One of:

  - "ortholog": Orthologs ('LDO' for least diverged and 'O' for more
    diverged).

  - "msa": Multiple Sequence Alignment Information,

  - "tree": Tree topology and nodes attributes.

- target_organisms:

  Numeric: (optional) NCBI taxon ID(s) to filter the results. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

For trees a list and otherwise a data frame with the requested family's
information.

## Corresponding API Resources

"POST https://www.pantherdb.org/services/oai/pantherdb/familyortholog"  
"POST https://www.pantherdb.org/services/oai/pantherdb/familymsa"  
"POST https://www.pantherdb.org/services/oai/pantherdb/treeinfo"

## References

- Thomas PD, Ebert D, Muruganujan A, Mushayahama T, Albou L-P,
  Mi H. (2022) PANTHER: Making genome-scale phylogenetics accessible to
  all. Protein Science, 31(1), 8–22. https://doi.org/10.1002/pro.4218

- [PANTHER Services
  Details](https://www.pantherdb.org/services/details.jsp)

- [Citations note on PANTHER
  website](https://www.pantherdb.org/publications.jsp#HowToCitePANTHER)

## See also

Other "PANTHER":
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md),
[`rba_panther_genome()`](https://rbioapi.moosa-r.com/reference/rba_panther_genome.md),
[`rba_panther_homolog()`](https://rbioapi.moosa-r.com/reference/rba_panther_homolog.md),
[`rba_panther_info()`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md),
[`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md),
[`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md),
[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)

## Examples

``` r
# \donttest{
rba_panther_family("PTHR10000", what = "ortholog")
# }
```
