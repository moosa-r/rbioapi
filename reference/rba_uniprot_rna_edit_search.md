# Search RNA Editing in UniProt

UniProt Curates [RNA-editing
events](https://www.uniprot.org/help/rna_editing) (conversion,
insertion, deletion of nucleotides). Use this function to search RNA
editing records in UniProt using various criteria such as accession,
taxon ID, or variant location.

## Usage

``` r
rba_uniprot_rna_edit_search(
  accession = NULL,
  taxid = NULL,
  variantlocation = NULL,
  ...
)
```

## Arguments

- accession:

  [UniProtKB primary or secondary
  accession](https://www.uniprot.org/help/accession_numbers)(s). You can
  supply up to 100 accession numbers.

- taxid:

  (Numeric) NIH-NCBI [Taxon ID](https://www.uniprot.org/taxonomy/). You
  can supply up to 20 taxon IDs.

- variantlocation:

  Character: RNA editing variant location(s). You can supply up to 20
  taxon IDs.

- ...:

  rbioapi option(s). See
  [`rba_options`](https://rbioapi.moosa-r.com/reference/rba_options.md)'s
  arguments manual for more information on available options.

## Value

A List where each element corresponds to one UniProt entity returned by
your search query. The element itself is a sub-list containing all
information that UniProt has about that entity.

## Corresponding API Resources

"GET https://www.ebi.ac.uk/proteins/api/rna-editing"

## Examples

``` r
# \donttest{
  rba_uniprot_rna_edit_search(accession = c("Q16851", "Q16849"))
# }
```
