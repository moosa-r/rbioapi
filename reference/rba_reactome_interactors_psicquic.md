# The interface From Reactome to PSICQUIC

You can call this function in two scenarios: 1- To retrieve information
of all available PSICQUIC resources, call the function without providing
any argument; i.e rba_reactome_interactors_psicquic(). 2-To retrieve a
list of interactors of specific protein(s), fill out the function's
arguments.

## Usage

``` r
rba_reactome_interactors_psicquic(
  proteins = NULL,
  resource = NULL,
  details = TRUE,
  ...
)
```

## Arguments

- proteins:

  Proteins to retrieve PSICQUIC interactors.

- resource:

  The PSICQUIC resource for your supplied proteins. Call
  rba_reactome_interactors_psicquic() without argument to get the
  available options.

- details:

  Logical: If TRUE (default) a detailed list of interactors will be
  returned. If FALSE, only a summary of available interactors will be
  returned.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Depending your input, a list containing the detailed or summary of
PSICQUIC interactions or a data frame of all registered PSICQUIC
resources.

## Corresponding API Resources

"POST
https://reactome.org/ContentService/interactors/psicquic/molecules/
{resource}/details"  
"POST
https://reactome.org/ContentService/interactors/psicquic/molecules/
{resource}/summary"  
"GET https://reactome.org/ContentService/interactors/psicquic/resources"

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

## See also

Other "Reactome Content Service - Molecule Interactors":
[`rba_reactome_interactors_static()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_static.md)

## Examples

``` r
# \donttest{
rba_reactome_interactors_psicquic()
# }
# \donttest{
rba_reactome_interactors_psicquic(proteins = c("TP53", "MYC"),
    resource = "BioGrid",
    details = FALSE)
# }
# \donttest{
rba_reactome_interactors_psicquic(proteins = c("TP53", "MYC"),
    resource = "BioGrid",
    details = TRUE)
# }
```
