# Retrieve Epitopes by Accession

Use this function to retrieve epitope annotations linked to a UniProt
entry.

## Usage

``` r
rba_uniprot_epitope(accession, ...)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers).

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A list containing the UniProt epitope features details for the given
accession.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/epitope/{accession}"

## See also

Other "UniProt - Epitopes":
[`rba_uniprot_epitope_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope_search.md),
[`rba_uniprot_rna_edit()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_rna_edit.md)

## Examples

``` r
# \donttest{
rba_uniprot_epitope(accession = "P36222")
# }
```
