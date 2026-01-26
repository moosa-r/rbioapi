# Map Cross References IDs to Reactome ReferenceEntity

Use this function To retrieve a list of Reactome ReferenceEntity
associated to your supplied Cross Reference (i.e. External) ID.

## Usage

``` r
rba_reactome_xref(xref_id, ...)
```

## Arguments

- xref_id:

  molecule's cross-reference (external) identifier.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing the ReferenceEntity corresponding to your supplied
cross-reference (external) ID.

## Details

Reactome cross-references external database's identifiers to it's
database Entries named ReferenceEntity, which resembles the invariant
aspect of a molecule. Thus there is a one-to-many relationship between
Reactome's ReferenceEntity object and the molecule's ID in external
databases, which in Reactome's terms is called Cross Reference.  
See
[`rba_reactome_participants`](https://rbioapi.moosa-r.com/reference/rba_reactome_participants.md)'s
"Details section" to learn more about how Reactome classifies molecules.

## Corresponding API Resources

"GET
https://reactome.org/ContentService/references/mapping/{identifier}"

## References

- Marc Gillespie, Bijay Jassal, Ralf Stephan, Marija Milacic, Karen
  Rothfels, Andrea Senff-Ribeiro, Johannes Griss, Cristoffer Sevilla,
  Lisa Matthews, Chuqiao Gong, Chuan Deng, Thawfeek Varusai, Eliot
  Ragueneau, Yusra Haider, Bruce May, Veronica Shamovsky, Joel Weiser,
  Timothy Brunson, Nasim Sanati, Liam Beckman, Xiang Shao, Antonio
  Fabregat, Konstantinos Sidiropoulos, Julieth Murillo, Guilherme
  Viteri, Justin Cook, Solomon Shorser, Gary Bader, Emek Demir, Chris
  Sander, Robin Haw, Guanming Wu, Lincoln Stein, Henning Hermjakob,
  Peter D’Eustachio, The reactome pathway knowledgebase 2022, Nucleic
  Acids Research, 2021;, kab1028, https://doi.org/10.1093/nar/gkab1028

- Griss J, Viteri G, Sidiropoulos K, Nguyen V, Fabregat A, Hermjakob H.
  ReactomeGSA - Efficient Multi-Omics Comparative Pathway Analysis. Mol
  Cell Proteomics. 2020 Sep 9. doi: 10.1074/mcp. PubMed PMID: 32907876.

- [Reactome Content Services API
  Documentation](https://reactome.org/ContentService/)

- [Citations note on Reactome website](https://reactome.org/cite/)

## Examples

``` r
# \donttest{
rba_reactome_xref("CD40")
# }
# \donttest{
rba_reactome_xref("ENSP00000361350")
# }
```
