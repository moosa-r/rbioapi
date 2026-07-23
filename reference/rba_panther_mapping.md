# Map A Gene-set to PANTHER Database

Using this function, you can search your genes in PANTHER database and
retrieve attributes and annotations associated to your genes.

## Usage

``` r
rba_panther_mapping(genes, organism, ...)
```

## Arguments

- genes:

  Character or numeric vector of gene identifiers with maximum length of
  5,000. Can be any of: Ensembl gene ID, Ensembl protein ID, Ensembl
  transcript ID, Entrez gene ID, gene symbol, NCBI GI, HGNC ID,
  International protein index ID, NCBI UniGene ID, UniProt accession
  and/or UniProt ID.

- organism:

  (numeric) NCBI taxon ID. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing your unmapped inputs and mapped genes with pertinent
information.

## Corresponding API Resources

"POST https://www.pantherdb.org/services/oai/pantherdb/geneinfo"

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
[`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md),
[`rba_panther_tree_grafter()`](https://rbioapi.moosa-r.com/reference/rba_panther_tree_grafter.md)

## Examples

``` r
# \donttest{
rba_panther_mapping(genes = c("Cd40", 7124, "ENSG00000203747", "P33681"),
    organism = 9606)
# }
```
