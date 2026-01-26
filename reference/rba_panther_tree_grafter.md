# PANTHER Tree Grafter

Use this function to retrieve a PANTHER family's tree topology
information with a node corresponding to your sequence grafted in the
best location in that tree.

## Usage

``` r
rba_panther_tree_grafter(protein_seq, target_organisms = NULL, ...)
```

## Arguments

- protein_seq:

  A character string with the protein's sequence. Maximum allowed
  sequence length is 50kb.

- target_organisms:

  (numeric) NCBI taxon ID(s) to filter the results. run
  [`rba_panther_info`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md)
  with argument 'what = "organisms"' to get a list of PANTHER's
  supported organisms.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing PANTHER tree topology information.

## Details

For more information, see: Haiming Tang, Robert D Finn, Paul D Thomas,
TreeGrafter: phylogenetic tree-based annotation of proteins with Gene
Ontology terms and other annotations, Bioinformatics, Volume 35, Issue
3, February 2019, Pages 518–520,
[doi:10.1093/bioinformatics/bty625](https://doi.org/10.1093/bioinformatics/bty625)

## Corresponding API Resources

"GET https://www.pantherdb.org/services/oai/pantherdb/graftsequence"

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
[`rba_panther_info()`](https://rbioapi.moosa-r.com/reference/rba_panther_info.md),
[`rba_panther_mapping()`](https://rbioapi.moosa-r.com/reference/rba_panther_mapping.md),
[`rba_panther_ortholog()`](https://rbioapi.moosa-r.com/reference/rba_panther_ortholog.md)

## Examples

``` r
# \donttest{
rba_panther_tree_grafter("MKVLWAALLVTFLAGCQAKVEQAVETE")
# }
```
