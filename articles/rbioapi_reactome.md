# 2.E: Reactome & rbioapi

## Introduction

Directly quoting from [Reactome](https://reactome.org/):

> REACTOME is an open-source, open access, manually curated and
> peer-reviewed pathway database. Our goal is to provide intuitive
> bioinformatics tools for the visualization, interpretation and
> analysis of pathway knowledge to support basic and clinical research,
> genome analysis, modeling, systems biology and education. Founded in
> 2003, the Reactome project is led by Lincoln Stein
> of [OICR](https://oicr.on.ca/), Peter D’Eustachio
> of [NYULMC](https://nyulangone.org/), Henning Hermjakob
> of [EMBL-EBI](https://www.ebi.ac.uk/), and Guanming Wu
> of [OHSU](https://www.ohsu.edu/).
>
> (source: <https://reactome.org/what-is-reactome>)

Reactome provides two RESTful API services: Reactome content services
and Reactome analysis services. In rbioapi, the naming schema is that
any function which belongs to analysis services starts with
rba_reactome_analysis\* . Other rba_reactome\_\* functions without the
‘analysis’ infix correspond to content services API.

Before continuing reading this article, it is a good idea to read
[Reactome Data Model](https://reactome.org/documentation/data-model)
page.

------------------------------------------------------------------------

## Reactome analysis services

This section mostly revolves around
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)
function. So, naturally, we will start with that. As explained in the
function’s manual, you have considerable freedom in providing the main
input for this function; You can supply an R object (as a data frame,
matrix, or simple vector), a URL, or a local file path. Note that the
type of analysis will be decided based on whether your input is
1-dimensional or 2-dimensional. This has been explained in detail in the
manual of
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md),
see that for more information.  
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)
is the API equivalent of Reactome’s [analyse gene
list](https://reactome.org/PathwayBrowser/#TOOL=AT) tool. You can see
that the function’s arguments correspond to what would you choose in the
webpage’s wizard.

``` r

## 1 We create a simple vector with our genes
genes <- c(
  "p53", "BRCA1", "cdk2", "Q99835", "CDC42", "CDK1", "KIF23", "PLK1", "RAC2",
  "RACGAP1", "RHOA", "RHOB", "MSL1", "PHF21A", "INSR", "JADE2", "P2RX7",
  "CCDC101", "PPM1B", "ANAPC16", "CDH8", "HSPA1L", "CUL2", "ZNF302", "CUX1",
  "CYTH2", "SEC22C", "EIF4E3", "ROBO2", "CXXC1", "LINC01314", "ATP5F1"
)

## 2 We call reactome analysis with the default parameters
analyzed <- rba_reactome_analysis(
  input = genes,
  projection = TRUE,
  p_value = 0.01
)

## 3 As always, we use str() to inspect the resutls
str(analyzed, 1)
#> List of 8
#>  $ summary            :List of 7
#>  $ expression         :List of 1
#>  $ identifiersNotFound: int 1
#>  $ pathwaysFound      : int 80
#>  $ pathways           :'data.frame': 80 obs. of  19 variables:
#>  $ resourceSummary    :'data.frame': 3 obs. of  3 variables:
#>  $ speciesSummary     :'data.frame': 1 obs. of  5 variables:
#>  $ warnings           : list()

## 4 Note that in the summary element: (analyzed$summary)
### 4.a because we supplied a simple vector, the analysis type was: over-representation
### 4.b You need the token for other rba_reactome_analysis_* functions

## 5 Analsis results are in the pathways data frame:
```

As mentioned, some of
[`rba_reactome_analysis()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis.md)’s
arguments correspond to the wizard of [analyse gene
list](https://reactome.org/PathwayBrowser/#TOOL=AT) tool; Other
arguments corresponds to the contents of “Filter your results” tab in
the results page.

Having the analysis’s token, you can retrieve the analysis results in
many formats using
[`rba_reactome_analysis_pdf()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_pdf.md)
and
[`rba_reactome_analysis_download()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_download.md):

``` r

# download a full pdf report
rba_reactome_analysis_pdf(
  token = analyzed$summary$token,
  species = 9606
)

# download the result in compressed json.gz format
rba_reactome_analysis_download(
  token = analyzed$summary$token,
  request = "results",
  save_to = "reactome_results.json"
)
```

Your token is only guaranteed to be stored for 7 days. After that, you
can upload the JSON file you have downloaded using
`rba_reactome_analysis_download` and get a token for that:

``` r

re_uploaded <- rba_reactome_analysis_import(input = "reactome_results.json")
```

**Please Note:** Other services supported by rbioapi also provide
Over-representation analysis tools. Please see the vignette article [Do
with rbioapi: Over-Representation (Enrichment) Analysis in
R](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.md) ([link to
the documentation
site](https://rbioapi.moosa-r.com/articles/rbioapi_do_enrich.html)) for
an in-depth review.

### See also in Functions’ manuals

Some rbioapi Reactome analysis functions were not covered in this
vignette, be sure to check their manuals:

- [`rba_reactome_analysis_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_mapping.md)

- [`rba_reactome_analysis_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_species.md)

- [`rba_reactome_analysis_token()`](https://rbioapi.moosa-r.com/reference/rba_reactome_analysis_token.md)

------------------------------------------------------------------------

## Reactome contents services

rbioapi functions that correspond to Reactome content services are those
starting with rba_reactome\_\* but without “\_analysis” infix. These
functions cover what you can do with objects in Reactome knowledge-base.
In simpler terms, most -but not all of them- correspond to what you can
find in [Reactome Pathway
Browser](https://reactome.org/PathwayBrowser/ "Reactome Pathway Browser")
and [search
results](https://reactome.org/content/query?q=apoptosis&species=Homo+sapiens "Search results for apoptosis").
(e.g. a
[pathway](https://reactome.org/content/detail/R-HSA-109581 "Apoptosis Homo sapiens"),
a
[reaction](https://reactome.org/content/detail/R-HSA-202939 "Caspase-mediated cleavage of E-Cadherin"),
a [physical
Entity](https://reactome.org/content/detail/R-HSA-350870 "Caspase-3 [cytosol]"),
etc.)

### Search the Reactome knowledge-base

If you do not already know the Reactome identifier of an entry, use
[`rba_reactome_search()`](https://rbioapi.moosa-r.com/reference/rba_reactome_search.md)
to search by name or descriptive text. The results can be limited by
species, entry type, cellular compartment, and keyword. By default,
matches are grouped by their result type. In the following example, we
search for TP53 across four representative human entry types and request
one match from each group.

``` r

search_results <- rba_reactome_search(
  query = "TP53",
  species = "Homo sapiens",
  types = c("Protein", "Complex", "Reaction", "Pathway"),
  page_size = 1,
  force_filters = TRUE
)

str(search_results, 2)
#> List of 4
#>  $ results        :'data.frame': 4 obs. of  4 variables:
#>   ..$ entries     :List of 4
#>   ..$ typeName    : chr [1:4] "Protein" "Complex" "Reaction" "Pathway"
#>   ..$ entriesCount: int [1:4] 1341 143 261 55
#>   ..$ rowCount    : int [1:4] 1 1 1 1
#>  $ rowCount       : int 4
#>  $ numberOfGroups : int 4
#>  $ numberOfMatches: int 1800
```

The matching entries are stored in the `entries` column of the returned
result groups. Below, we combine the four groups and display selected
fields:

### Retrieve any object from Reactome knowledge-base

Using
[`rba_reactome_query()`](https://rbioapi.moosa-r.com/reference/rba_reactome_query.md),
you can retrieve any object to which Reactome has assigned a database or
stable identifier. This includes proteins, reactions, pathways, species,
people, and many other entries described in [Reactome’s data
schema](https://reactome.org/content/schema/DatabaseObject "Graph Database :: Data Schema").
A standard query accepts one or more identifiers. An enhanced query
accepts one identifier and adds related regulations and catalysts; it
also lets you control incoming relationships and disease-specific
information, or request a shorter summary for a reference entity.

``` r

## 1 Query a pathway entry
pathway <- rba_reactome_query(
  ids = "R-HSA-109581",
  enhanced = TRUE
)

## 2 As always we use str() to inspect the output's structure
str(pathway, 2)
#> List of 29
#>  $ dbId               : int 109581
#>  $ displayName        : chr "Apoptosis"
#>  $ stId               : chr "R-HSA-109581"
#>  $ stIdVersion        : chr "R-HSA-109581.6"
#>  $ created            :List of 6
#>   ..$ dbId       : int 109608
#>   ..$ displayName: chr "Alnemri, E, Hengartner, Michael, Tschopp, Jürg, Tsujimoto, Yoshihide, Hardwick, JM, 2004-01-16"
#>   ..$ dateTime   : chr "2004-01-16 21:01:51"
#>   ..$ author     :List of 5
#>   ..$ className  : chr "InstanceEdit"
#>   ..$ schemaClass: chr "InstanceEdit"
#>  $ modified           :List of 7
#>   ..$ dbId       : int 11116865
#>   ..$ displayName: chr "Weiser, Joel, 2026-06-12"
#>   ..$ dateTime   : chr "2026-06-12 07:49:47"
#>   ..$ note       : chr "Inserted by org.reactome.orthoinference"
#>   ..$ author     :List of 1
#>   ..$ className  : chr "InstanceEdit"
#>   ..$ schemaClass: chr "InstanceEdit"
#>  $ isInDisease        : logi FALSE
#>  $ isInferred         : logi FALSE
#>  $ maxDepth           : int 7
#>  $ name               :List of 1
#>   ..$ : chr "Apoptosis"
#>  $ releaseDate        : chr "2004-09-20"
#>  $ speciesName        : chr "Homo sapiens"
#>  $ authored           :List of 1
#>   ..$ : int 109608
#>  $ edited             :List of 1
#>   ..$ :List of 6
#>  $ eventOf            :List of 1
#>   ..$ :List of 20
#>  $ figure             :List of 1
#>   ..$ :List of 5
#>  $ goBiologicalProcess:List of 9
#>   ..$ dbId        : int 2273
#>   ..$ displayName : chr "apoptotic process"
#>   ..$ accession   : chr "0006915"
#>   ..$ databaseName: chr "GO"
#>   ..$ definition  : chr "A programmed cell death process which begins when a cell receives an internal (e.g. DNA damage) or external sig"| __truncated__
#>   ..$ name        : chr "apoptotic process"
#>   ..$ url         : chr "https://www.ebi.ac.uk/QuickGO/term/GO:0006915"
#>   ..$ className   : chr "GO_BiologicalProcess"
#>   ..$ schemaClass : chr "GO_BiologicalProcess"
#>  $ literatureReference:List of 7
#>   ..$ :List of 12
#>   ..$ :List of 12
#>   ..$ :List of 12
#>   ..$ : int 140368
#>   ..$ : int 140372
#>   ..$ : int 141241
#>   ..$ :List of 12
#>  $ orthologousEvent   :List of 14
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>   ..$ :List of 17
#>  $ reviewed           :List of 1
#>   ..$ :List of 6
#>  $ species            :List of 1
#>   ..$ : int 48887
#>  $ summation          :List of 1
#>   ..$ :List of 5
#>  $ reviewStatus       :List of 6
#>   ..$ dbId       : int 9821382
#>   ..$ displayName: chr "five stars"
#>   ..$ definition : chr "externally reviewed"
#>   ..$ name       :List of 1
#>   ..$ className  : chr "ReviewStatus"
#>   ..$ schemaClass: chr "ReviewStatus"
#>  $ hasDiagram         : logi TRUE
#>  $ hasEHLD            : logi TRUE
#>  $ lastUpdatedDate    : chr "2022-06-09"
#>  $ hasEvent           :List of 4
#>   ..$ :List of 19
#>   ..$ :List of 20
#>   ..$ :List of 20
#>   ..$ :List of 19
#>  $ schemaClass        : chr "Pathway"
#>  $ className          : chr "Pathway"



## 3 Compare the result with the pathway's Reactome page
# https://reactome.org/content/detail/R-HSA-109581
```

``` r

## 1 Query a reference entity and summarize its physical forms
protein <- rba_reactome_query(
  ids = 66247,
  enhanced = TRUE,
  summarize_reference_entity = TRUE
)

## 2 As always we use str() to inspect the output's structure
str(protein, 1)
#> List of 33
#>  $ dbId               : int 66247
#>  $ displayName        : chr "UniProt:P25942-1 CD40"
#>  $ stId               : chr "uniprot:P25942-1"
#>  $ name               :List of 1
#>  $ compartment        :List of 1
#>  $ componentOf        :List of 1
#>  $ crossReference     :List of 38
#>  $ inferredTo         :List of 8
#>  $ summarisedEntities :List of 1
#>  $ moleculeType       : chr "Protein"
#>  $ databaseName       : chr "UniProt"
#>  $ identifier         : chr "P25942"
#>  $ otherIdentifier    :List of 119
#>  $ url                : chr "http://purl.uniprot.org/uniprot/P25942-1"
#>  $ referenceDatabase  : int 2
#>  $ checksum           : chr "BC8776EC2C4A5680"
#>  $ comment            :List of 1
#>  $ description        :List of 1
#>  $ geneName           :List of 2
#>  $ isSequenceChanged  : logi FALSE
#>  $ keyword            :List of 17
#>  $ secondaryIdentifier:List of 8
#>  $ sequenceLength     : int 277
#>  $ species            : int 48887
#>  $ chain              :List of 2
#>  $ referenceGene      :List of 11
#>  $ referenceTranscript:List of 4
#>  $ variantIdentifier  : chr "P25942-1"
#>  $ isoformParent      :List of 1
#>  $ referenceType      : chr "ReferenceIsoform"
#>  $ referenceEntity    : int 66247
#>  $ className          : chr "SummaryEntity"
#>  $ schemaClass        : chr "SummaryEntity"



## 3 Compare the result with the entry's Reactome page
# https://reactome.org/content/detail/66247
```

### Find Cross-Reference IDs in Reactome

In the second example, we used Reactome’s database identifier `66247` to
query the CD40 reference entity.
[`rba_reactome_xref()`](https://rbioapi.moosa-r.com/reference/rba_reactome_xref.md)
can map an external identifier, such as a gene symbol, to the
corresponding Reactome reference entity.

``` r

## 1 Supply an HGNC symbol to find the corresponding Reactome database ID
xref_protein <- rba_reactome_xref("CD40")

## 2 As always, use str() to inspect the output's structure
str(xref_protein, 1)
#> List of 21
#>  $ dbId               : int 66247
#>  $ displayName        : chr "UniProt:P25942-1 CD40"
#>  $ stId               : chr "uniprot:P25942-1"
#>  $ databaseName       : chr "UniProt"
#>  $ identifier         : chr "P25942"
#>  $ name               :List of 1
#>  $ otherIdentifier    :List of 1
#>  $ url                : chr "http://purl.uniprot.org/uniprot/P25942-1"
#>  $ moleculeType       : chr "Protein"
#>  $ checksum           : chr "BC8776EC2C4A5680"
#>  $ comment            :List of 1
#>  $ description        :List of 1
#>  $ geneName           :List of 1
#>  $ isSequenceChanged  : logi FALSE
#>  $ keyword            :List of 1
#>  $ secondaryIdentifier:List of 1
#>  $ sequenceLength     : int 277
#>  $ chain              :List of 1
#>  $ variantIdentifier  : chr "P25942-1"
#>  $ className          : chr "ReferenceIsoform"
#>  $ schemaClass        : chr "ReferenceIsoform"
```

Set `expanded = TRUE` to also retrieve other external identifiers
associated with the reference entity and the stable identifiers of its
physical forms. Expanded queries can accept a vector of identifiers;
`page` and `page_size` select which supplied identifiers Reactome
processes in each call. The optional database filter uses Reactome’s
database names.

``` r

## Retrieve the ENSEMBL cross-references and associated physical forms
xref_details <- rba_reactome_xref(
  "P36897",
  expanded = TRUE,
  db_filter = "ENSEMBL"
)

str(xref_details, 2)
#> List of 1
#>  $ :List of 3
#>   ..$ reference       : chr "P36897"
#>   ..$ physicalEntities:List of 13
#>   ..$ crossReferences :List of 7
```

### Map Cross-Reference IDs to Reactome

While we are at the cross-reference topic, here is another useful
resource. Using `rba_reactome_mapping` you can find the Reactome
pathways or reactions which include your external ID:

``` r

## 1 Again, consider CD40 protein:
xref_mapping <- rba_reactome_mapping(
  id = "CD40",
  resource = "hgnc",
  map_to = "pathways"
)
```

------------------------------------------------------------------------

## See also in function manuals

Several rbioapi Reactome content functions are not covered in this
vignette. The following overview links them by purpose; see each
function’s manual for details and examples.

### Retrieve Reactome database information

- [`rba_reactome_version()`](https://rbioapi.moosa-r.com/reference/rba_reactome_version.md):
  Return current Reactome version

- [`rba_reactome_diseases()`](https://rbioapi.moosa-r.com/reference/rba_reactome_diseases.md):
  Retrieve a list of disease annotated in Reactome.

- [`rba_reactome_species()`](https://rbioapi.moosa-r.com/reference/rba_reactome_species.md):
  Retrieve a list of species annotated in Reactome.

### General Mapping/Querying

- [`rba_reactome_search()`](https://rbioapi.moosa-r.com/reference/rba_reactome_search.md):
  Search Reactome entries by text and optional filters.

- [`rba_reactome_query()`](https://rbioapi.moosa-r.com/reference/rba_reactome_query.md):
  Retrieve Reactome objects, optionally with enhanced relationships or a
  selected attribute.

- [`rba_reactome_mapping()`](https://rbioapi.moosa-r.com/reference/rba_reactome_mapping.md)

- [`rba_reactome_xref()`](https://rbioapi.moosa-r.com/reference/rba_reactome_xref.md):
  Map external identifiers to Reactome reference entities and,
  optionally, their other cross-references and physical forms.

### Things you can do with entities

- [`rba_reactome_complex_list()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_list.md):
  Get a list of complexes that have your molecule in them.

- [`rba_reactome_complex_subunits()`](https://rbioapi.moosa-r.com/reference/rba_reactome_complex_subunits.md):
  Get the list of subunits in your complex

- [`rba_reactome_participant_of()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participant_of.md):
  Get a list of Reactome sets and complexes that your entity (event,
  molecule, reaction, pathway etc.) is a participant in them.

- [`rba_reactome_entity_other_forms()`](https://rbioapi.moosa-r.com/reference/rba_reactome_entity_other_forms.md)

### Things you can do with Events

- [`rba_reactome_event_ancestors()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_ancestors.md)

- [`rba_reactome_participants()`](https://rbioapi.moosa-r.com/reference/rba_reactome_participants.md)

- [`rba_reactome_pathways_events()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_events.md)

- [`rba_reactome_event_ancestors()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_ancestors.md)

- [`rba_reactome_orthology()`](https://rbioapi.moosa-r.com/reference/rba_reactome_orthology.md)

- [`rba_reactome_event_hierarchy()`](https://rbioapi.moosa-r.com/reference/rba_reactome_event_hierarchy.md):
  Retrieve a species’s event or pathway hierarchy, optionally with
  analysis results added.

### Pathways

- [`rba_reactome_pathways_low()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_low.md)

- [`rba_reactome_pathways_events()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_events.md)

- [`rba_reactome_pathways_top()`](https://rbioapi.moosa-r.com/reference/rba_reactome_pathways_top.md)

### Interactors

- [`rba_reactome_interactors_psicquic()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_psicquic.md)

- [`rba_reactome_interactors_static()`](https://rbioapi.moosa-r.com/reference/rba_reactome_interactors_static.md):
  Retrieve Reactome’s static IntAct interaction details, summaries, or
  associated pathways.

### People

- [`rba_reactome_people_name()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_name.md)

- [`rba_reactome_people_id()`](https://rbioapi.moosa-r.com/reference/rba_reactome_people_id.md)

### Export diagrams and events

- [`rba_reactome_exporter_diagram()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_diagram.md)

- [`rba_reactome_exporter_overview()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_overview.md)

- [`rba_reactome_exporter_reaction()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_reaction.md)

- [`rba_reactome_exporter_event()`](https://rbioapi.moosa-r.com/reference/rba_reactome_exporter_event.md)

------------------------------------------------------------------------

## How to Cite?

To cite Reactome, please reference one or more of the following
publications (see <https://reactome.org/cite>):

- Ragueneau, E., Gong, C., Sinquin, P., Sevilla, C., Beavers, D.,
  Grentner, A., … D’Eustachio, P. (2026). The Reactome
  Knowledgebase 2026. *Nucleic Acids Research*, 54(D1), D673–D681.
  <https://doi.org/10.1093/nar/gkaf1223>.
- Griss, J., Viteri, G., Sidiropoulos, K., Nguyen, V., Fabregat, A., &
  Hermjakob, H. (2020). ReactomeGSA—Efficient Multi-Omics Comparative
  Pathway Analysis. *Molecular & Cellular Proteomics*, 19(12),
  2115–2125. <https://doi.org/10.1074/mcp.TIR120.002155>.
- Fabregat, A., Korninger, F., Viteri, G., Sidiropoulos, K.,
  Marin-Garcia, P., Ping, P., Wu, G., Stein, L., D’Eustachio, P., &
  Hermjakob, H. (2018). Reactome graph database: Efficient access to
  complex pathway data. *PLOS Computational Biology*, 14(1), e1005968.
  <https://doi.org/10.1371/journal.pcbi.1005968>.
- Fabregat, A., Sidiropoulos, K., Viteri, G., Marin-Garcia, P., Ping,
  P., Stein, L., D’Eustachio, P., & Hermjakob, H. (2018). Reactome
  diagram viewer: Data structures and strategies to boost performance.
  *Bioinformatics*, 34(7), 1208–1214.
  <https://doi.org/10.1093/bioinformatics/btx752>.
- Fabregat, A., Sidiropoulos, K., Viteri, G., Forner, O., Marin-Garcia,
  P., Arnau, V., D’Eustachio, P., Stein, L., & Hermjakob, H. (2017).
  Reactome pathway analysis: A high-performance in-memory approach. *BMC
  Bioinformatics*, 18(1), 142.
  <https://doi.org/10.1186/s12859-017-1559-2>.
- Wu, G., & Haw, R. (2017). Functional Interaction Network Construction
  and Analysis for Disease Discovery. *Methods in Molecular Biology*,
  1558, 235–253. <https://doi.org/10.1007/978-1-4939-6783-4_11>.

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Links

- [This article in rbioapi
  documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_reactome.html "2.D: Reactome & rbioapi")

- [Functions references in rbioapi
  documentation site](https://rbioapi.moosa-r.com/reference/index.html#section-reactome-analysis-services-rba-reactome- "rbioapi reference")

- [rbioapi vignette
  index](https://rbioapi.moosa-r.com/articles/rbioapi.md "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

------------------------------------------------------------------------

## Session info

    #> R version 4.6.1 (2026-06-24)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.4 LTS
    #> 
    #> Matrix products: default
    #> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    #> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    #> 
    #> locale:
    #>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    #>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    #>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    #> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    #> 
    #> time zone: UTC
    #> tzcode source: system (glibc)
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #> [1] rbioapi_0.8.3.9000
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] httr_1.4.9        cli_3.6.6         knitr_1.51        rlang_1.3.0      
    #>  [5] xfun_0.60         otel_0.2.0        textshaping_1.0.5 jsonlite_2.0.0   
    #>  [9] DT_0.34.0         htmltools_0.5.9   ragg_1.5.2        sass_0.4.10      
    #> [13] rmarkdown_2.32    crosstalk_1.2.2   evaluate_1.0.5    jquerylib_0.1.4  
    #> [17] fastmap_1.2.0     yaml_2.3.12       lifecycle_1.0.5   compiler_4.6.1   
    #> [21] fs_2.1.0          htmlwidgets_1.6.4 systemfonts_1.3.2 digest_0.6.39    
    #> [25] R6_2.6.1          curl_8.0.0        magrittr_2.0.5    bslib_0.12.0     
    #> [29] tools_4.6.1       mime_0.13         pkgdown_2.2.1     cachem_1.1.0     
    #> [33] desc_1.4.3
