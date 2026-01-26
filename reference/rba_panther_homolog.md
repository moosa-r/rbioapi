# Search PANTHER for Homologs of Gene(s)

Using this function you can search and retrieve homolog of given
gene(s).

## Usage

``` r
rba_panther_homolog(genes, organism, type = "P", target_organisms = NULL, ...)
```

## Arguments

- genes:

  Character vector of genes identifiers with maximum length of 10 or
  only one if seq_pos is supplied. Can be any of: Ensemble gene ID,
  Ensemble protein ID, Ensemble transcript ID, Entrez gene ID, gene
  symbol, NCBI GI, HGNC ID, International protein index ID, NCBI UniGene
  ID, UniProt accession and/or UniProt ID.

- organism:

  (numeric) NCBI taxon ID of the organism of your supplied genes. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- type:

  Homolog types to return. either "P" (default) for paralogs, "X" for
  horizontal gene transfer and "LDX" for diverged horizontal gene
  transfer.

- target_organisms:

  (numeric) NCBI taxon ID(s) to filter the results. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms. For Paralog, target organism and organism should
  be the same; Otherwise, the target organism should be different from
  the input organism.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A dataframe with homologs information.

## Corresponding API Resources

"GET
https://www.pantherdb.org/services/oai/pantherdb/ortholog/homologOther"

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
