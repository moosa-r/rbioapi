# Search PANTHER for Homologs of Gene(s)

Using this function you can search and retrieve homolog of given
gene(s).

## Usage

``` r
rba_panther_homolog(genes, organism, type = "P", target_organisms = NULL, ...)
```

## Arguments

- genes:

  Character or Numeric: A vector of gene identifiers with maximum length
  of 10. Can be any of: Ensembl gene ID, Ensembl protein ID, Ensembl
  transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
  International protein index ID, NCBI UniGene ID, UniProt accession
  and/or UniProt ID.

- organism:

  Numeric: NCBI taxon ID of the organism of your supplied genes. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- type:

  Character: (default = `"P"`) Homolog types to return. either "P"
  (default) for paralogs, "X" for horizontal gene transfer and "LDX" for
  least diverged horizontal gene transfer.

- target_organisms:

  Numeric: (optional) NCBI taxon ID(s) to filter the results. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms. This argument is ignored for paralogs, which are
  searched within the input organism. For horizontal gene transfers,
  target organisms should differ from the input organism.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with homolog information.

## Corresponding API Resources

"POST
https://www.pantherdb.org/services/oai/pantherdb/ortholog/homologOther"

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
[`rba_panther_info()`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md),
[`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md),
[`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md),
[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)

## Examples

``` r
# \donttest{
rba_panther_homolog("OR4F5", organism = 9606, type = "P")
# }
```
