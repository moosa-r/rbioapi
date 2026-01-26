# Get UniProt entry by accession

Use this function to retrieve a UniProt Entry by it's UniProt accession.
You can also use "isoform" or "interaction" arguments to retrieve
isoforms or interactor proteins of that entry. Note that in one function
call you can only set none or only one of "isoform" or "interaction" as
TRUE, not both of them.

## Usage

``` r
rba_uniprot_proteins(accession, interaction = FALSE, isoforms = FALSE, ...)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- interaction:

  Logical: (default = FALSE) Only retrieve
  [interaction](https://www.uniprot.org/help/interaction_section)
  information of your supplied UniProt entity?

- isoforms:

  Logical: (default = FALSE) Only retrieve
  [isoforms](https://www.uniprot.org/help/alternative_products) of your
  supplied UniProt entity?

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list that contains UniProt protein informations with your supplied
accession.

## Corresponding API Resources

"GET https://ebi.ac.uk/proteins/api/proteins/{accession}"  
"GET https://ebi.ac.uk/proteins/api/proteins/interaction/{accession}"  
"GET https://ebi.ac.uk/proteins/api/proteins/{accession}/isoforms"

## References

- The UniProt Consortium , UniProt: the Universal Protein Knowledgebase
  in 2025, Nucleic Acids Research, 2024;, gkae1010,
  https://doi.org/10.1093/nar/gkae1010

- Andrew Nightingale, Ricardo Antunes, Emanuele Alpi, Borisas
  Bursteinas, Leonardo Gonzales, Wudong Liu, Jie Luo, Guoying Qi, Edd
  Turner, Maria Martin, The Proteins API: accessing key integrated
  protein and genome information, Nucleic Acids Research, Volume 45,
  Issue W1, 3 July 2017, Pages W539–W544,
  https://doi.org/10.1093/nar/gkx237

- [Proteins API Documentation](https://www.ebi.ac.uk/proteins/api/doc/)

- [Citations note on UniProt
  website](https://www.uniprot.org/help/publications)

## See also

Other "UniProt - Proteins":
[`rba_uniprot_proteins_crossref()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_crossref.md),
[`rba_uniprot_proteins_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_proteins_search.md)

## Examples

``` r
# \donttest{
rba_uniprot_proteins(accession = "P01730")
# }
# \donttest{
rba_uniprot_proteins(accession = "P01730", interaction = TRUE)
# }
# \donttest{
rba_uniprot_proteins(accession = "Q29983", isoforms = TRUE)
# }
```
