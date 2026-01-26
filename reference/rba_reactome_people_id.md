# A person by his identifiers

A person by his identifiers

## Usage

``` r
rba_reactome_people_id(
  person_id,
  authored_pathways = FALSE,
  publications = FALSE,
  attribute_name = NULL,
  ...
)
```

## Arguments

- person_id:

  Reactome database ID (DbId) or ORCHID ID

- authored_pathways:

  Logical: Only return Pathway list authored by the person? (default =
  FALSE)

- publications:

  Logical: Only return publications list authored by the person? (Defalt
  = FALSE)

- attribute_name:

  (optional) A Reactome person attribute to return only. see [Reactome
  Data Schema: person](https://reactome.org/content/schema/Person/) for
  available options.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing the requested informations of your supplied person.

## Corresponding API Resources

"GET https://reactome.org/ContentService"

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

Other "Reactome Content Service - Person Queries":
[`rba_reactome_people_name()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_name.md)

## Examples

``` r
# \donttest{
rba_reactome_people_id("391309")
# }
# \donttest{
rba_reactome_people_id(person_id = "391309", authored_pathways = TRUE)
# }
```
