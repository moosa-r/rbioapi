# Retrieve Genes from a PANTHER Genome

Retrieve one page of genes and their associated information for a genome
supported by PANTHER. Each page contains up to 1,000 genes.

## Usage

``` r
rba_panther_genome(organism, page, ...)
```

## Arguments

- organism:

  (numeric) NCBI taxon ID. Run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- page:

  (numeric) The results page to retrieve. Pages contain up to 1,000
  genes and are numbered starting from 1.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list with the following elements:

- gene:

  A data frame with one row per returned gene. Fields are kept as
  returned by PANTHER, with annotation information in nested columns.

- page:

  The retrieved page.

- pages_count:

  The total number of available pages.

- number_of_genes_in_genome:

  The total number of genes in the genome.

- product:

  PANTHER product source and version information.

- search_type:

  The search type reported by PANTHER.

## Corresponding API Resources

"POST https://www.pantherdb.org/services/oai/pantherdb/downloadgenome"

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
[`rba_panther_homolog()`](https://rbioapi.moosa-r.com/reference/rba_panther_homolog.md),
[`rba_panther_info()`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md),
[`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md),
[`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md),
[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)

## Examples

``` r
# \donttest{
rba_panther_genome(organism = 9606, page = 1)
# }
```
