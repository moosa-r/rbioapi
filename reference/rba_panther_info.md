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

  what information to retrieve? should be one of:

  - "organisms": Retrieve supported organisms in PANTHER.

  - "datasets": Retrieve available annotation datasets.

  - "families" Retrieve available family IDs.

  - "species_tree" Retrieve the PANThER's species tree.

  - "pathways" Retrieve available pathway IDs.

- organism_chr_loc:

  (Logical) (only when 'what = "organisms"') If TRUE, only organisms
  with chromosome location will be returned. If FALSE (default) every
  organisms will be returned.

- families_page:

  (Numeric) (only when 'what = "families"') Family information is very
  long, so results are paginated. Use this argument to define the page
  to retrieve.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

For families and species tree, a list and otherwise a data frame with
pertinent information.

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

- Huaiyu Mi, Dustin Ebert, Anushya Muruganujan, Caitlin Mills,
  Laurent-Philippe Albou, Tremayne Mushayamaha, Paul D Thomas, PANTHER
  version 16: a revised family classification, tree-based classification
  tool, enhancer regions and extensive API, Nucleic Acids Research,
  Volume 49, Issue D1, 8 January 2021, Pages D394–D403,
  https://doi.org/10.1093/nar/gkaa1106

- [PANTHER Services
  Details](https://www.pantherdb.org/services/details.jsp)

- [Citations note on PANTHER
  website](https://www.pantherdb.org/publications.jsp#HowToCitePANTHER)

## See also

Other "PANTHER":
[`rba_panther_enrich()`](https://rbioapi.moosa-r.com/reference/rba_panther_enrich.md),
[`rba_panther_family()`](https://rbioapi.moosa-r.com/reference/rba_panther_family.md),
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
