# 2.G: UniProt & rbioapi

## Introduction

Directly quoting from
[UniProt](https://www.uniprot.org "Universal Protein Resource (UniProt)"):

> The Universal Protein Resource (UniProt) is a comprehensive resource
> for protein sequence and annotation data. The UniProt databases are
> the [UniProt Knowledgebase
> (UniProtKB)](https://www.uniprot.org/help/uniprotkb), the [UniProt
> Reference Clusters (UniRef)](https://www.uniprot.org/help/uniref), and
> the [UniProt Archive (UniParc)](https://www.uniprot.org/help/uniparc).
> The UniProt consortium and host institutions EMBL-EBI, SIB and PIR are
> committed to the long-term preservation of the UniProt databases.
>
> (source: <https://www.uniprot.org/help/about>)

------------------------------------------------------------------------

## Search vs Retrieve

Most of rbioapi UniProt functions have two variants. one to retrieve
data using a proper accession, and the second one (which have a
`_search` suffix) is to search using any combination of arguments. We
first demonstrate this using an example, then provide a list of such
functions in rbioapi.

Suppose we are interested in Human CD40 ligand protein, and we know that
it’s UniProt accession is “P29965”. We can simply do as the following:

``` r
## 1 We can retrieve CD40 protein's information by qurying it's UniProt accession:
cd40 <- rba_uniprot_proteins(accession = "P29965")

## 2 We use str() to inspect our object's structure
str(cd40, 1)
#> List of 13
#>  $ accession       : chr "P29965"
#>  $ id              : chr "CD40L_HUMAN"
#>  $ proteinExistence: chr "Evidence at protein level"
#>  $ info            :List of 4
#>  $ organism        :List of 3
#>  $ protein         :List of 4
#>  $ gene            :'data.frame':    1 obs. of  2 variables:
#>  $ comments        :'data.frame':    12 obs. of  10 variables:
#>  $ features        :'data.frame':    64 obs. of  9 variables:
#>  $ dbReferences    :'data.frame':    138 obs. of  4 variables:
#>  $ keywords        :'data.frame':    14 obs. of  1 variable:
#>  $ references      :'data.frame':    25 obs. of  3 variables:
#>  $ sequence        :List of 5
```

This is the equivalent of the page that UniProt have on this accession
([UniProtKB -
P29965](https://www.uniprot.org/uniprot/P29965 "UniProtKB - P29965 (CD40L_HUMAN)")).
But what if we didn’t know the UniProt accession? Or simply want to
perform a search using certain parameters? We can use the function with
`_search` suffix:

``` r
## 1 From the available arguments, we fill only those which we think is pertinent
cd40_search <- rba_uniprot_proteins_search(
  protein = "CD40 ligand",
  organism = "human",
  reviewed = TRUE
)

## 2 As always, we use str() to inspect our object's structure
str(cd40_search, 2)
#> List of 1
#>  $ P29965:List of 13
#>   ..$ accession       : chr "P29965"
#>   ..$ id              : chr "CD40L_HUMAN"
#>   ..$ proteinExistence: chr "Evidence at protein level"
#>   ..$ info            :List of 4
#>   ..$ organism        :List of 3
#>   ..$ protein         :List of 4
#>   ..$ gene            :List of 1
#>   ..$ comments        :List of 12
#>   ..$ features        :List of 64
#>   ..$ dbReferences    :List of 138
#>   ..$ keywords        :List of 14
#>   ..$ references      :List of 25
#>   ..$ sequence        :List of 5
```

This is the equivalent of ‘[advanced
search](https://www.uniprot.org/help/advanced_search "UniProtKB advanced search options")’
in UniProt web portal. See function `rba_uniprot_proteins_search`’s
manual for more information. Remember that in `*_search` functions, you
are not required to fill every argument, you can use any combination of
arguments you see fit to build your search query.

The applications of `*_search` variants are not limited to what the
title ‘search’ implies. These functions will also retrieve the search
hits in their response; Thus you can use them for mass-retrieving. If
you see the “argument” section in the functions’ manuals, you would see
that many arguments accept a vector of values. consider the following
examples:

``` r
## 1 As the simplest scenario, we can retrieve multiple proteins in one call
multi_prs1 <- rba_uniprot_proteins_search(
  accession = c("P04637", "P38398", "P24941", "P60953", "P06493", "Q02241")
)
## As always, we use str() to inspect our object's structure
str(multi_prs1, 1)
#> List of 6
#>  $ P24941:List of 14
#>  $ P04637:List of 14
#>  $ P60953:List of 14
#>  $ P06493:List of 14
#>  $ Q02241:List of 14
#>  $ P38398:List of 14

## 2 Or alternatively, search using Gene names, also we want to exclude isoforms and only retrieve swiss-prot entries
multi_prs2 <- rba_uniprot_proteins_search(
  gene = c("KIF23", "BRCA1", "TP53", "CDC42"),
  reviewed = TRUE,
  taxid = 9606,
  isoform = 0
)

str(multi_prs2, 1)
#> List of 29
#>  $ Q9ULZ0:List of 14
#>  $ Q02241:List of 14
#>  $ P38398:List of 14
#>  $ Q12888:List of 14
#>  $ Q9HCN2:List of 14
#>  $ P04637:List of 14
#>  $ Q9Y2B4:List of 13
#>  $ Q8IXH6:List of 14
#>  $ Q53FA7:List of 14
#>  $ A1A5B4:List of 14
#>  $ Q9NS56:List of 14
#>  $ Q96A56:List of 14
#>  $ O14683:List of 14
#>  $ Q96S44:List of 14
#>  $ Q8NBR0:List of 14
#>  $ Q13625:List of 14
#>  $ Q5VT25:List of 14
#>  $ Q7L0Q8:List of 14
#>  $ Q00587:List of 14
#>  $ O14613:List of 14
#>  $ Q6DT37:List of 14
#>  $ Q9NRR8:List of 14
#>  $ Q9NRR3:List of 14
#>  $ Q9H3Q1:List of 14
#>  $ Q07960:List of 14
#>  $ Q9UKI2:List of 14
#>  $ Q6NZY7:List of 14
#>  $ Q9Y5S2:List of 14
#>  $ P60953:List of 14
```

``` r
## 3 Search for every proteins with chemokines keyword
multi_prs3 <- rba_uniprot_proteins_search(
  keyword = "chemokines"
)

str(multi_prs3, 1)
#> List of 17
#>  $ P09703:List of 15
#>  $ P0DTM9:List of 14
#>  $ Q6SW98:List of 15
#>  $ P19063:List of 14
#>  $ P16849:List of 15
#>  $ P33854:List of 14
#>  $ P0DSV7:List of 15
#>  $ P69332:List of 15
#>  $ P24766:List of 15
#>  $ P34016:List of 14
#>  $ P21064:List of 14
#>  $ P0DSV8:List of 15
#>  $ F5HBX1:List of 14
#>  $ Q98314:List of 15
#>  $ P69333:List of 15
#>  $ P07562:List of 14
#>  $ F5HF62:List of 14
```

``` r
## 4 Search for every protein of "SARS-CoV-2" virus in Swiss-Prot
multi_prs4 <- rba_uniprot_proteins_search(
  organism = "SARS-CoV-2",
  reviewed = TRUE
)

str(multi_prs4, 1)
#> List of 17
#>  $ P0DTD3    :List of 14
#>  $ P0DTC6    :List of 14
#>  $ P0DTG0    :List of 13
#>  $ P0DTC7    :List of 14
#>  $ P0DTC2    :List of 14
#>  $ P0DTC8    :List of 14
#>  $ P0DTC4    :List of 14
#>  $ A0A663DJA2:List of 14
#>  $ P0DTC9    :List of 14
#>  $ P0DTC5    :List of 14
#>  $ P0DTC3    :List of 14
#>  $ P0DTD8    :List of 14
#>  $ P0DTD2    :List of 14
#>  $ P0DTF1    :List of 13
#>  $ P0DTD1    :List of 14
#>  $ P0DTC1    :List of 13
#>  $ P0DTG1    :List of 13
```

------------------------------------------------------------------------

## Functions with `*_search` variant

The search variants are not limited to the above. Here is a list of the
function that have both a retrieve and search variants. See their
manuals for detailed guides and examples.

1.  [`rba_uniprot_proteins()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins.md)
    &
    [`rba_uniprot_proteins_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_search.md)

2.  [`rba_uniprot_features()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features.md)
    &
    [`rba_uniprot_features_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_search.md)

3.  [`rba_uniprot_variation()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation.md)
    &
    [`rba_uniprot_variation_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_search.md)

4.  [`rba_uniprot_proteomics()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics.md)
    &
    [`rba_uniprot_proteomics_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_search.md)

5.  [`rba_uniprot_antigens()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens.md)
    &
    [`rba_uniprot_antigens_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens_search.md)

6.  [`rba_uniprot_epitope()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope.md)
    &
    [`rba_uniprot_epitope_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope_search.md)

7.  [`rba_uniprot_rna_edit()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit.md)
    &
    [`rba_uniprot_rna_edit_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit_search.md)

8.  [`rba_uniprot_proteomes()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes.md)
    &
    [`rba_uniprot_proteomes_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes_search.md)

9.  [`rba_uniprot_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_ptm.md)
    &
    [`rba_uniprot_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_ptm_search.md)

10. [`rba_uniprot_mutagenesis()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis.md)
    &
    [`rba_uniprot_mutagenesis_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis_search.md)

11. [`rba_uniprot_proteomics_hpp()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp.md)
    &
    [`rba_uniprot_proteomics_hpp_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp_search.md)

12. [`rba_uniprot_proteomics_non_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm.md)
    &
    [`rba_uniprot_proteomics_non_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm_search.md)

13. [`rba_uniprot_proteomics_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm.md)
    &
    [`rba_uniprot_proteomics_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm_search.md)

14. [`rba_uniprot_genecentric()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric.md)
    &
    [`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md)

15. [`rba_uniprot_uniparc()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc.md)
    &
    [`rba_uniprot_uniparc_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_search.md)

------------------------------------------------------------------------

## UniProt functions categories

The UniProt API endpoints are organized into 5 group. Here are those
categories and rbioapi functions that correspond to each one. See the
functions’ manuals for more details.

### Proteins

#### Proteins:

- [`rba_uniprot_proteins()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins.md)

- [`rba_uniprot_proteins_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_search.md)

- [`rba_uniprot_proteins_crossref()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_crossref.md)

#### Features

- [`rba_uniprot_features()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features.md)

- [`rba_uniprot_features_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_features_search.md)

#### Variation

[`rba_uniprot_variation()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation.md)

[`rba_uniprot_variation_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_variation_search.md)

#### Antigens

- [`rba_uniprot_antigens()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens.md)

- [`rba_uniprot_antigens_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_antigens_search.md)

#### Epitopes

- [`rba_uniprot_epitope()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope.md)

- [`rba_uniprot_epitope_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope_search.md)

#### Mutagenesis

- [`rba_uniprot_mutagenesis()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis.md)

- [`rba_uniprot_mutagenesis_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_mutagenesis_search.md)

#### RNA-Editing

- [`rba_uniprot_rna_edit()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit.md)

- [`rba_uniprot_rna_edit_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit_search.md)

### Proteomics

- [`rba_uniprot_proteomics_species()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_species.md)

- [`rba_uniprot_proteomics_hpp()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp.md)

- [`rba_uniprot_proteomics_hpp_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_hpp_search.md)

- [`rba_uniprot_proteomics_non_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm.md)

- [`rba_uniprot_proteomics_non_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_non_ptm_search.md)

- [`rba_uniprot_proteomics_ptm()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm.md)

- [`rba_uniprot_proteomics_ptm_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomics_ptm_search.md)

### Proteomes

- [`rba_uniprot_proteomes()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes.md)

- [`rba_uniprot_proteomes_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteomes_search.md)

- [`rba_uniprot_genecentric()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric.md)

- [`rba_uniprot_genecentric_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_genecentric_search.md)

### Taxonomy

- [`rba_uniprot_taxonomy()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy.md)

- [`rba_uniprot_taxonomy_lca()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lca.md)

- [`rba_uniprot_taxonomy_lineage()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_lineage.md)

- [`rba_uniprot_taxonomy_name()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_name.md)

- [`rba_uniprot_taxonomy_path()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_path.md)

- [`rba_uniprot_taxonomy_relationship()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_taxonomy_relationship.md)

### Coordinates

- [`rba_uniprot_coordinates()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates.md)

- [`rba_uniprot_coordinates_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_search.md)

- [`rba_uniprot_coordinates_location()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_coordinates_location.md)

- `rba_uniprot_coordinates_sequence()`

- `rba_uniprot_genome_coordinates_sequence()`

### UniParc

- [`rba_uniprot_uniparc()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc.md)

- [`rba_uniprot_uniparc_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_search.md)

- [`rba_uniprot_uniparc_bestguess()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_bestguess.md)

- [`rba_uniprot_uniparc_sequence()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_uniparc_sequence.md)

------------------------------------------------------------------------

## How to Cite?

To cite UniProt (Please see
<https://www.uniprot.org/help/publications>):

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, *Nucleic Acids Research*, 2024;, gkae1010,
  <https://doi.org/10.1093/nar/gkae1010>
- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, *Nucleic Acids Research*, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
  <https://doi.org/10.1093/nar/gkx237>

To cite rbioapi:

- Moosa Rezwani, Ali Akbar Pourfathollah, Farshid Noorbakhsh, rbioapi:
  user-friendly R interface to biologic web services’ API,
  Bioinformatics, Volume 38, Issue 10, 15 May 2022, Pages 2952–2953,
  <https://doi.org/10.1093/bioinformatics/btac172>

------------------------------------------------------------------------

## Links

- [This article in rbioapi
  documentation site](https://rbioapi.moosa-r.com/articles/rbioapi_uniprot.html "2.F: UniProt & rbioapi")

- [Functions references in rbioapi
  documentation site](https://rbioapi.moosa-r.com/reference/index.html#section-uniprot-rba-uniprot- "rbioapi reference")

- [rbioapi vignette
  index](https://rbioapi.moosa-r.com/articles/rbioapi.md "rbioapi: User-Friendly R Interface to Biologic Web Services' API")

------------------------------------------------------------------------

## Session info

    #> R version 4.5.2 (2025-10-31)
    #> Platform: x86_64-pc-linux-gnu
    #> Running under: Ubuntu 24.04.3 LTS
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
    #> [1] rbioapi_0.8.3
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
    #>  [5] xfun_0.56         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
    #>  [9] rmarkdown_2.30    lifecycle_1.0.5   cli_3.6.5         sass_0.4.10      
    #> [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
    #> [17] compiler_4.5.2    httr_1.4.7        tools_4.5.2       ragg_1.5.0       
    #> [21] curl_7.0.0        bslib_0.9.0       evaluate_1.0.5    yaml_2.3.12      
    #> [25] otel_0.2.0        jsonlite_2.0.0    rlang_1.1.7       fs_1.6.6         
    #> [29] htmlwidgets_1.6.4
