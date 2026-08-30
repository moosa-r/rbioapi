# Get PANTHER database Information

Using this function you can retrieve a list of available organisms,
annotation datasets, families, and pathways which are supported in
PANTHER.

## Usage

``` r
rba_panther_info(what, organism_chr_loc = FALSE, families_page = 1, ...)
```

## Arguments

- what:

  Character: what information to retrieve? should be one of:

  - "organisms": Retrieve supported organisms in PANTHER.

  - "datasets": Retrieve available annotation datasets.

  - "families": Retrieve available family IDs.

  - "species_tree": Retrieve PANTHER's species tree.

  - "pathways" Retrieve available pathway IDs.

- organism_chr_loc:

  Logical: (default = `FALSE`) (only when 'what = "organisms"') If TRUE,
  only organisms with chromosome location will be returned. If FALSE
  (default), all organisms will be returned.

- families_page:

  Numeric: (default = `1`) (only when 'what = "families"') Family
  information is very long, so results are returned in pages of up to
  1,000 families. Use a positive whole number to define the page to
  retrieve.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

For families, a list containing family information, the requested page,
and the total number of pages. For the species tree, a list; otherwise a
data frame with pertinent information.

## Corresponding API Resources

"GET
https://www.pantherdb.org/services/oai/pantherdb/supportedgenomes"  
"GET
https://www.pantherdb.org/services/oai/pantherdb/supportedannotdatasets"  
"GET
https://www.pantherdb.org/services/oai/pantherdb/supportedpantherfamilies"  
"GET
https://www.pantherdb.org/services/oai/pantherdb/supportedpantherpathways"  
"GET https://www.pantherdb.org/services/oai/pantherdb/speciestree"

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
[`rba_panther_family()`](https://rbioapi.moosa-r.com/reference/rba_panther_family.md),
[`rba_panther_genome()`](https://rbioapi.moosa-r.com/reference/rba_panther_genome.md),
[`rba_panther_homolog()`](https://rbioapi.moosa-r.com/reference/rba_panther_homolog.md),
[`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md),
[`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md),
[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)

## Examples

``` r
# \donttest{
rba_panther_info(what = "organisms")
# }
# \donttest{
rba_panther_info(what = "families", families_page = 4)
# }
```
