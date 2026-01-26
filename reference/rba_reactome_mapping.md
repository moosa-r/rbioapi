# Map External ID to Reactome Pathways/Reactions

By providing an external identifier from a given resource, you can
retrieve a list of pathways/reactions that include your supplied ID.

## Usage

``` r
rba_reactome_mapping(id, resource, map_to, species = "Homo sapiens", ...)
```

## Arguments

- id:

  Molecule's external Identifier

- resource:

  What is the resource of your supplied ID? see: [Reactome External
  Identifiers](https://reactome.org/content/schema/objects/ReferenceDatabase/)

- map_to:

  Either "pathways" or "reactions".

- species:

  Numeric or Character: NCBI Taxonomy identifier (Human is 9606),
  species name (e.g. "Homo sapiens") or Reactome DbId (e.g Homo sapiens
  is 48887). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

Data frame where each row is a pathway/reaction and columns are
pertinent information.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/mapping/{resource}/
{identifier}/pathways"  
"GET https://reactome.org/ContentService/data/mapping/{resource}/
{identifier}/reactions"

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
rba_reactome_mapping(id = "PTEN", resource =  "UniProt",
    map_to = "reactions", species = 9606)
# }
```
