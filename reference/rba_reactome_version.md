# The version number of current database

Returns the current version of Reactome database.

## Usage

``` r
rba_reactome_version(...)
```

## Arguments

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Character string containing the version of Reactome database.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/database/version"

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
rba_reactome_version()
# }
```
