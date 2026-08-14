# Get Participants of a Reactome Event

Participating molecules in a Reactome comprises set of 'Physical Entity'
and 'Reference Entities' class objects. Use this function to retrieve
all, only 'Physical Entity' or only 'Reference Entities' participants of
given event.

## Usage

``` r
rba_reactome_participants(
  event_id,
  only_physical_entities = FALSE,
  only_reference_entities = FALSE,
  ...
)
```

## Arguments

- event_id:

  Character or Numeric: Reactome event's database ID (DbId) or Stable ID
  (StId).

- only_physical_entities:

  Logical: (default = `FALSE`) If TRUE, only participating 'Physical
  Entities' will be returned.

- only_reference_entities:

  Logical: (default = `FALSE`) If TRUE, only participating 'Reference
  Entities' will be returned.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

List with the participant of your supplied Event ID. A Data frame if
only physical or 'Reference Entities' was requested.

## Details

A 'Physical Entity' Instance could include an individual molecule, a
multi-molecular complex or a set of molecule forming a group based on
some characteristics. a single molecule can have different 'Physical
Entity' instances based on it's associated attributes. For example, IgK
Ig kappa chain, has two 'Physical Entity' instances; one, with ID
["R-HSA-197041"](https://reactome.org/content/schema/instance/browser/R-HSA-197041)
refers to the secreted antibody protein to the extra-cellular region;
And the second one is with ID
["R-HSA-2038819"](https://reactome.org/content/schema/instance/browser/R-HSA-2038819)
and refers to the plasma-membrane-integrated form of the antibody
protein.  
To make it possible to link multiple 'Physical Entity' instances of a
molecule, Reactome uses a data class named "'Reference Entities'" which
correspond to the invariant attribute of a molecule. for example, both
of the above-mentioned 'Physical Entities' see a 'Reference Entities'
named ["UniProt:P01834
IGKC](https://reactome.org/content/schema/instance/browser/57819).  
See [Reactome Data
Model](https://reactome.org/documentation/data-model/) for more
information about the data model and Physical Entities.

## Corresponding API Resources

"GET https://reactome.org/ContentService/data/participants/{id}"  
"GET https://reactome.org/ContentService/data/participants/{id}/
participatingPhysicalEntities"  
"GET https://reactome.org/ContentService/data/participants/{id}/
referenceEntities"

## References

- Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
  Grentner, A., ... D’Eustachio, P. (2026). The Reactome
  Knowledgebase 2026. Nucleic Acids Res., 54(D1), D673–D681. doi:
  10.1093/nar/gkaf1223

- Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A., &
  Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
  Pathway Analysis. Molecular & Cellular Proteomics, 19(12), 2115–2125.
  doi: 10.1074/mcp.TIR120.002155

- [Reactome Content Services API
  Documentation](https://reactome.org/ContentService/)

- [Citations note on Reactome website](https://reactome.org/cite/)

## See also

[`rba_reactome_participant_of`](https://rbioapi.moosa-r.com/reference/rba_reactome_participant_of.md)

## Examples

``` r
# \donttest{
rba_reactome_participants("R-HSA-5682012")
# }
# \donttest{
rba_reactome_participants("R-HSA-5682012", only_physical_entities = TRUE)
# }
# \donttest{
rba_reactome_participants("R-HSA-5682012", only_reference_entities = TRUE)
# }
```
