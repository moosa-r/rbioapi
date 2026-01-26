# Get Orthologous (Computationally Inferred) Events

Reactome incorporate manually curated human reactions and PANTHER's
protein homology data to Computationally infer events in other
eukaryotic species.

## Usage

``` r
rba_reactome_orthology(event_ids, species_dbid, ...)
```

## Arguments

- event_ids:

  Human Reactome event ID(s) to retrieve their orthologous events.

- species_dbid:

  Reactome database ID (DbId) of the target species. (e.g Mus musculus
  is 48892). See
  [`rba_reactome_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md)
  or [Reactome Data Schema: Entries:
  Species](https://reactome.org/content/schema/objects/Species/).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List containing found Orthologous event(s) in your supplied species and
their pertinent information.

## Details

In version 73 (11 June 2020), using an orthology-based approach, Homo
sapiens events was projected to 18,654 orthologous pathways (with 81,835
orthologous proteins) in 15 non-human species.  
See [Reactome Computationally Inferred
Events](https://reactome.org/documentation/inferred-events/) for more
information.

## Corresponding API Resources

"POST https://reactome.org/ContentService/data/orthologies/ids/
species/{speciesId}"

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

[`rba_reactome_analysis_species`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md)

## Examples

``` r
# \donttest{
rba_reactome_orthology(event_ids = c("R-HSA-6799198", " R-HSA-72764"),
    species_dbid = 49633)
# }
```
