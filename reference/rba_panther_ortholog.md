# Search PANTHER for Orthologs of Gene(s)

Using this function you can search and retrieve orthologs of given
gene(s), and optionally return the corresponding position in the target
organisms' protein sequences.

## Usage

``` r
rba_panther_ortholog(
  genes,
  organism,
  type = "all",
  target_organisms = NULL,
  seq_pos = NULL,
  include_msa = NULL,
  ...
)
```

## Arguments

- genes:

  Character or numeric vector of gene identifiers with maximum length of
  10, or only one if `seq_pos` is supplied. Can be any of: Ensembl gene
  ID, Ensembl protein ID, Ensembl transcript ID, Entrez gene ID, gene
  symbol, NCBI GI, HGNC ID, International protein index ID, NCBI UniGene
  ID, UniProt accession and/or UniProt ID.

- organism:

  (numeric) NCBI taxon ID of the organism of your supplied genes. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- type:

  Ortholog types to return. either "all" (default) or "LDO" to only
  return least diverged orthologs.

- target_organisms:

  (numeric) NCBI taxon ID(s) to filter the results. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- seq_pos:

  (Numeric) A position in the protein's sequence of the supplied gene.
  should be in the range of the protein's length.

- include_msa:

  (Logical) Only if a sequence position is supplied, should MSA
  (Multiple Sequence Alignment) information be included in the results?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A data frame with Orthologs information.

## Corresponding API Resources

"POST
https://www.pantherdb.org/services/oai/pantherdb/ortholog/matchortho"  
"POST
https://www.pantherdb.org/services/oai/pantherdb/ortholog/homologpos"

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
[`rba_panther_info()`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md),
[`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md),
[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)

## Examples

``` r
# \donttest{
rba_panther_ortholog("CD40", organism = 9606, type = "LDO")
# }
```
