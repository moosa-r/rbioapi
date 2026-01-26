# Retrieve Epitope by Accession

Use this function to retrieve [RNA-editing
events](https://www.uniprot.org/help/rna_editing) (conversion,
insertion, deletion of nucleotides) annotations linked to a UniProt
entry.

## Usage

``` r
rba_uniprot_rna_edit(accession, ...)
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

A list containing the UniProt RNA-editing features details for the given
accession.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/rna-edit/{accession}"

## See also

Other "UniProt - Epitopes":
[`rba_uniprot_epitope()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope.md),
[`rba_uniprot_epitope_search()`](https://rbioapi.moosa-r.com/reference/rba_uniprot_epitope_search.md)

## Examples

``` r
# \donttest{
  rba_uniprot_rna_edit(accession = "Q16851")
# }
```
