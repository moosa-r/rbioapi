# Get Static(IntAct) Interaction Information of a Protein

Reactome maintain a locally host a version of IntAct(Static)
interactions database. Using this function, you can retrieve IntAct
information of a protein(s) in two scenarios:

1.  If endpoint = "details" or "summary": Retrieve a detailed/summary
    information of your supplied protein accession(s) from IntAct
    database.

2.  If endpoint = "pathway", Retrieve a list of Reactome pathways which
    include your supplied protein accession. Pathways with the class
    "TopLevelPathway" will be excluded.

## Usage

``` r
rba_reactome_interactors_static(
  proteins,
  endpoint = "details",
  only_diagrammed = FALSE,
  species = NULL,
  ...
)
```

## Arguments

- proteins:

  Uniprot proteins accession(s). If endpoint = "pathway", only a single
  protein accession can be supplied.

- endpoint:

  Can be one of:

  1.  "details": To return a detailed information of your supplied
      protein(s) accession.

  2.  "summary": To return a summary of your supplied protein(s)
      accession

  3.  "pathway": To return a list of pathways containing the interacting
      molecules (excluding TopLevelPathway class).

- only_diagrammed:

  Logical: (only when "endpoint = "pathway") If TRUE, pathways without
  diagram will be excluded. (default = FALSE)

- species:

  Only when "endpoint = "pathway", The scientific name of the species to
  search for the pathways. See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List which it's content varies based on the supplied "endpoint"
argument.

## Corresponding API Resources

"POST https://reactome.org/ContentService/interactors/static/
molecules/details"  
"POST https://reactome.org/ContentService/interactors/static/
molecules/summary"  
"GET https://reactome.org/ContentService/interactors/static/
molecules/pathways"

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
[`rba_reactome_interactors_psicquic()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_psicquic.md)

## Examples

``` r
# \donttest{
rba_reactome_interactors_static(proteins = "Q9BXM7-1",
    endpoint = "pathways", species = "Homo sapiens")
# }
# \donttest{
rba_reactome_interactors_static(proteins = c("Q9BXM7-1", "Q13501"),
    endpoint = "details")
# }
# \donttest{
rba_reactome_interactors_static(proteins = c("Q9BXM7-1", "Q13501"),
    endpoint = "summary")
# }
```
